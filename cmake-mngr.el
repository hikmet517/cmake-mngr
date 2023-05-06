;;; cmake-mngr.el --- Manage cmake projects -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Hikmet Altıntaş

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Maintainer: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Created: 29 Aug 2020
;; Keywords: tools, extensions
;; URL: https://github.com/hikmet517/cmake-mngr
;; Version: 0.2
;; Package-Requires: ((seq "2.23") (tabulated-list "1.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; A convenient interface to cmake commands.

;;; TODO:
;; suggestions using cmake vars' types
;; add install support

;;; Code:


;;;; Libraries

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'ansi-color)
(require 'compile)


;;;; Variables

(defvar cmake-mngr-projects (list)
  "Currently opened CMake projects.  alist of (string . hash-table).")


;;;; Constants

(defconst cmake-mngr-cache-buffer-name "*cmake-mngr-cache-variables <%s>*"
  "Buffer name for cache variables.")

(defconst cmake-mngr-configure-buffer-name "*cmake-mngr-configure <%s>*"
  "Buffer name for the output of configure command.")

(defconst cmake-mngr-build-buffer-name "*cmake-mngr-build <%s>*"
  "Buffer name for the output of build command.")

(defconst cmake-mngr-project-regexp
  (rx line-start
      (* whitespace)
      "project"
      (* (any whitespace "\n"))
      "("))

;;;; User options

(defgroup cmake-mngr nil
  "Cmake-mngr customization."
  :group 'cmake-mngr
  :prefix "cmake-mngr-"
  :link '(url-link "https://github.com/hikmet517/cmake-mngr.el"))

(defcustom cmake-mngr-build-dir-search-list '("build" "bin" "out")
  "List of directories to search for CMake build directory.
Should be non-nil."
  :type '(repeat directory)
  :group 'cmake-mngr)

(defcustom cmake-mngr-global-configure-args '("-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
  "Argument to pass during configuration."
  :type '(repeat string)
  :group 'cmake-mngr)

(defcustom cmake-mngr-global-build-args '()
  "Argument to pass during build."
  :type '(repeat string)
  :group 'cmake-mngr)

(defcustom cmake-mngr-build-command-prepends '()
  "Commands that will be executed before build.
These commands will be concatenated using && operator."
  :type '(repeat string)
  :group 'cmake-mngr)

(defcustom cmake-mngr-configure-command-prepends '()
  "Commands that will be executed before configure.
These commands will be concatenated using && operator."
  :type '(repeat string)
  :group 'cmake-mngr)


;;;; Functions

;; ANSI-colors in the compilation buffer
;; https://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
(defun cmake-mngr--colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (goto-char compilation-filter-start)
    (move-beginning-of-line nil)
    (ansi-color-apply-on-region (point) (point-max))))

(add-hook 'compilation-filter-hook #'cmake-mngr--colorize-compilation)


(defun cmake-mngr--parse-cache-file (filepath)
  "Parse given CMakeCache.txt file in FILEPATH as a list of elements.
Output is in the form (ID [KEY TYPE VALUE])."
  (when (file-readable-p filepath)
    (let ((content (with-temp-buffer
                     (insert-file-contents filepath)
                     (split-string (buffer-string) "\n" t)))
          (res '())
          (iter 0))
      (dolist (line content)
        (when (and (not (string-prefix-p "#" line))
                   (not (string-prefix-p "//" line))
                   (seq-contains-p line ?=))
          (let* ((kv (split-string line "=" t))
                 (kt (split-string (car kv) ":" t)))
            (push (list iter (vector (or (car kt) "")
                                     (or (cadr kt) "")
                                     (or (cadr kv) "")))
                  res)
            (setq iter (1+ iter)))))
      (sort res (lambda (s1 s2) (string-lessp (aref (cadr s1) 0)
                                              (aref (cadr s2) 0)))))))


(defun cmake-mngr--get-available-generators ()
  "Find available generators by parsing the output of \"cmake --help\"."
  (when-let* ((str (shell-command-to-string "cmake --help"))
              (ss (replace-regexp-in-string "\n[[:space:]]*=" "=" str))
              (found (string-match "The following generators are available" ss))
              (slist (cdr (split-string (substring ss found) "\n" t)))
              (filt (seq-filter (lambda (s) (seq-contains-p s ?=)) slist))
              (gens (mapcar (lambda (s) (string-trim (car (split-string s "=" t)))) filt)))
    (mapcar (lambda (s) (string-trim-left s "* ")) gens)))


(defun cmake-mngr--get-available-targets ()
  "Find available targets by parsing \"cmake --build build-dir --help\"."
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((build-dir (gethash "Build Dir" project))
           (targets '())
           (cmd (concat "cmake --build " (shell-quote-argument build-dir) " --target help"))
           (str (shell-command-to-string cmd)))
      (dolist (line (split-string str "\n" t))
        (when (string-prefix-p "... " line)
          (push (car (split-string (substring line 4))) targets)))
      (reverse targets))))


(defun cmake-mngr--get-parent-dir (path)
  "Get parent dir of given PATH."
  (let ((path-with-no-slash (string-trim-right path "/")))
    (if path-with-no-slash
        (file-name-directory path-with-no-slash)
      nil)))

(defun cmake-mngr--check-cmake-file-is-project (filepath)
  "Check the file given with FILEPATH to see if it is main cmake file.

Checks if project() statement exists in file.
If it does, returns the point where the match occurred, else returns nil."
  (when (file-readable-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (goto-char (point-min))
      (re-search-forward cmake-mngr-project-regexp nil t))))

(defun cmake-mngr--find-project-dir (filepath)
  "Find CMake project root for buffer with the path FILEPATH."
  (let ((dir-iter filepath)
        (dir-found nil)
        (should-exit nil))
    (while (not should-exit)
      (let ((file (expand-file-name "CMakeLists.txt" dir-iter)))
        (when (cmake-mngr--check-cmake-file-is-project file)
          (setq dir-found dir-iter))
        (setq dir-iter (cmake-mngr--get-parent-dir dir-iter))
        (when (not dir-iter)
          (setq should-exit t))))
    dir-found))


(defun cmake-mngr--get-sub-dirs (dir)
  "Get full paths of subdirectories of given directory DIR."
  (let* ((subs (directory-files dir))
         (subs-filt (seq-filter (lambda (s)
                                  (and (not (equal s "."))
                                       (not (equal s ".."))
                                       (file-directory-p (expand-file-name s dir))))
                                subs)))
    (mapcar (lambda (s) (file-name-as-directory (expand-file-name s dir))) subs-filt)))


(defun cmake-mngr--find-project-build-dir (project-dir)
  "Get CMake project build directory by searching the path of PROJECT-DIR.

First, search for sub-directories that contain 'CMakeCache.txt', If there is
none, look for if any of the directories listed in
`cmake-mngr-build-dir-search-list' exists.  If nothing found return nil."
  (let ((build-dir nil)
        (sub-dirs (cmake-mngr--get-sub-dirs project-dir)))
    ;; search for sub directories that contain cache file
    (setq build-dir (seq-find (lambda (s)
                                (file-exists-p (expand-file-name "CMakeCache.txt" s)))
                              sub-dirs))
    ;; search for cmake-mngr-build-dir-search-list
    (unless build-dir
      (setq build-dir (seq-find #'file-exists-p
                                (mapcar (lambda (s)
                                          (file-name-as-directory
                                           (expand-file-name s project-dir)))
                                        cmake-mngr-build-dir-search-list))))
    build-dir))


(defun cmake-mngr--get-project ()
  "Get project's data structure for current buffer.

If it is already found before (added to `cmake-mngr-projects') returns this.
Otherwise, searches directory structure of current buffer.
If found, data is added to `cmake-mngr-projects' and returned,
otherwise returns nil."
  (declare-function dired-current-directory "dired" ())
  (let* ((filepath (cond
                    ((equal major-mode 'dired-mode)
                     (progn
                       (require 'dired)
                       (dired-current-directory)))
                    ((derived-mode-p 'comint-mode)
                     default-directory)
                    (t
                     (buffer-file-name))))
         (project-data (when filepath
                         (cdr (assoc
                               (file-name-directory filepath)
                               cmake-mngr-projects
                               'string-prefix-p)))))
    (when (and (not project-data) filepath)
      (let ((project-dir (cmake-mngr--find-project-dir filepath)))
        (when project-dir
          (let* ((root-name (file-name-base
                             (directory-file-name project-dir)))
                 (build-dir (cmake-mngr--find-project-build-dir project-dir)))
            (setq project-data (make-hash-table :test 'equal))
            (puthash "Root Name" root-name project-data)
            (puthash "Project Dir" project-dir project-data)
            (puthash "Generator" nil project-data)
            (puthash "Build Dir" build-dir project-data)
            (puthash "Target" nil project-data)
            (puthash "Custom Vars" (make-hash-table :test 'equal) project-data)
            (push (cons project-dir project-data) cmake-mngr-projects)))))
    project-data))


;;;###autoload
(defun cmake-mngr-create-symlink-to-compile-commands ()
  "Create a symlink in project root that points to \"compile_commands.json\".
This may be needed for language servers to work."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((project-dir (gethash "Project Dir" project))
           (build-dir (gethash "Build Dir" project))
           (json-file (when build-dir
                        (expand-file-name "compile_commands.json" build-dir))))
      (if (and build-dir
               json-file
               (file-exists-p build-dir)
               (file-exists-p json-file))
          (let ((default-directory project-dir))
            (start-process "create-symlink" nil "ln" "-s" (string-trim json-file project-dir) "-t" "."))
        (user-error "Cannot found build directory or 'compile_commands.json'")))))


(define-derived-mode cmake-mngr-variables-mode tabulated-list-mode "CMake Variables"
  "Major mode for viewing CMake Variables."
  (visual-line-mode +1)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format
        `[("Variable" 40 t)
          ("Type" 15 t)
          ("Value" 0 t)])
  ;; (setq tabulated-list-padding 2)
  (tabulated-list-init-header))


;;;###autoload
(defun cmake-mngr-show-cache-variables ()
  "Show CMake cache variables in a buffer."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (when-let* ((build-dir (gethash "Build Dir" project))
                (buf-name (format cmake-mngr-cache-buffer-name (gethash "Root Name" project)))
                (cache-file (expand-file-name "CMakeCache.txt" build-dir))
                (cache-vars (cmake-mngr--parse-cache-file cache-file)))
      (when cache-vars
        (let ((buf (get-buffer-create buf-name)))
          (display-buffer buf)
          (set-buffer buf)
          (cmake-mngr-variables-mode)
          (setq tabulated-list-entries cache-vars)
          (tabulated-list-print))))))


;;;###autoload
(defun cmake-mngr-configure ()
  "Configure current project."
  (interactive)
  (let* ((project (cmake-mngr--get-project))
         (build-dir (when project (gethash "Build Dir" project)))
         (buf-name (format cmake-mngr-configure-buffer-name (gethash "Root Name" project))))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (unless build-dir
      (setq build-dir (cmake-mngr-set-build-directory)))
    (when (and build-dir (not (file-exists-p build-dir)))
      (make-directory build-dir))
    (when (and build-dir (file-exists-p build-dir))
      (let* ((args (append (list "-S" (gethash "Project Dir" project)
                                 "-B" (gethash "Build Dir" project))
                           (let ((gen (gethash "Generator" project)))
                             (when gen (list "-G" gen)))))
             (custom-args (let ((c (list)))
                            (maphash (lambda (k v) (push (format "-D%s=%s" k v) c))
                                     (gethash "Custom Vars" project))
                            c))
             (all-args (append args cmake-mngr-global-configure-args custom-args))
             (config-cmd (concat "cmake " (combine-and-quote-strings all-args)))
             (cmd (if cmake-mngr-configure-command-prepends
                      (string-join (append cmake-mngr-configure-command-prepends
                                           (list config-cmd))
                                   " && ")
                    config-cmd))
             (default-directory (gethash "Build Dir" project)))
        (message "Cmake configure command: %s" cmd)
        (async-shell-command cmd buf-name)))))


(defun cmake-mngr--build-buffer-name (_name-of-mode)
  "Buffer name function for `compilation-buffer-name-function'."
  (when-let ((project (cmake-mngr--get-project)))
    (format cmake-mngr-build-buffer-name (gethash "Root Name" project))))


;;;###autoload
(defun cmake-mngr-build ()
  "Build current project."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((build-dir (gethash "Build Dir" project))
           (cache-file (when build-dir (expand-file-name "CMakeCache.txt" build-dir))))
      (when (not build-dir)
        (when (yes-or-no-p "Build directory is not set, set now? ")
          (setq build-dir (cmake-mngr-set-build-directory))))
      (when (and build-dir
                 (or (not (file-exists-p build-dir))
                     (not cache-file)
                     (not (file-exists-p cache-file))))
        (when (yes-or-no-p "Need to configure first, configure now? ")
          (cmake-mngr-configure)))
      (when (and build-dir
                 (file-exists-p build-dir)
                 (file-exists-p cache-file))
        (let* ((args (append (list "--build" build-dir)
                             (let ((tgt (gethash "Target" project)))
                               (when tgt (list "--target" tgt)))
                             cmake-mngr-global-build-args))
               (build-cmd (concat "cmake " (combine-and-quote-strings args)))
               (cmd (if cmake-mngr-build-command-prepends
                        (string-join (append cmake-mngr-build-command-prepends
                                             (list build-cmd))
                                     " && ")
                      build-cmd))
               (compilation-buffer-name-function 'cmake-mngr--build-buffer-name)
               (default-directory build-dir))
          (message "Cmake build command: %s" cmd)
          (compile cmd))))))


;;;###autoload
(defun cmake-mngr-select-build-type ()
  "Get CMake build type from user."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let ((type (completing-read "Select cmake build type: "
                                 '("Debug" "Release" "MinSizeRel" "RelWithDebInfo")
                                 nil 'REQUIRE-MATCH nil nil
                                 (gethash "CMAKE_BUILD_TYPE"
                                          (gethash "Custom Vars" project))))
          (custom (gethash "Custom Vars" project)))
      (when (and custom type)
        (puthash "CMAKE_BUILD_TYPE" type custom)
        (when (yes-or-no-p "Need to reconfigure, configure now? ")
          (cmake-mngr-configure))))))


;;;###autoload
(defun cmake-mngr-set-generator ()
  "Set generator for current project."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((generators (cmake-mngr--get-available-generators))
           (choice (completing-read "Select a generator: " generators)))
      (when (and choice (not (string= choice "")))
        (puthash "Generator" choice project)
        (when (yes-or-no-p "Need to reconfigure, configure now? ")
          (cmake-mngr-clear-cache)
          (cmake-mngr-configure))))))


;;;###autoload
(defun cmake-mngr-set-target ()
  "Set target for current project."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((targets (cmake-mngr--get-available-targets))
           (choice (completing-read "Select a target: " targets)))
      (when (and choice (not (string= choice "")))
        (puthash "Target" choice project)))))


;;;###autoload
(defun cmake-mngr-set-build-directory ()
  "Set CMake build directory."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((proj-dir (gethash "Project Dir" project))
           (build-dir (gethash "Build Dir" project))
           (build-dir-name (when build-dir
                             (directory-file-name (string-trim-left build-dir proj-dir))))
           (choice (completing-read
                    (if build-dir (format "Select cmake build directory (default %s): "
                                          build-dir-name)
                      "Select cmake build directory: ")
                    cmake-mngr-build-dir-search-list ; COLLECTION
                    nil nil nil nil ; PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST
                    build-dir-name)))
      (when (and choice (not (equal choice "")))
        (setq build-dir (file-name-as-directory (expand-file-name choice proj-dir)))
        (puthash "Build Dir" build-dir project)
        build-dir))))


;;;###autoload
(defun cmake-mngr-set-variable ()
  "Set a CMake variable as KEY=VALUE.

These variables will be passed to cmake during configuration as -DKEY=VALUE."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let* ((build-dir (gethash "Build Dir" project))
           (cache-file (when build-dir (expand-file-name "CMakeCache.txt" build-dir)))
           (cache-vars (when cache-file (cmake-mngr--parse-cache-file cache-file)))
           (vars (when cache-vars (mapcar #'cadr cache-vars)))
           (key (completing-read "Variable: "
                                 (when vars (mapcar #'seq-first vars))))
           (row (when (and vars key) (seq-find (lambda (s) (string= key (aref s 0))) vars)))
           (dflt (when row (aref row 2)))
           (val (completing-read (if dflt
                                     (format "Value (%s): " dflt)
                                   "Value: ")
                                 (cond
                                  ((and row (string= (aref row 1) "BOOL"))
                                   (list "ON" "OFF"))
                                  ((and row (string= (aref row 0) "CMAKE_BUILD_TYPE"))
                                   (list "Debug" "Release" "RelWithDebInfo" "MinSizeRel")))
                                 nil nil nil nil dflt)))
      (let ((custom-vars (gethash "Custom Vars" project)))
        (when (and custom-vars key val)
          (puthash key val custom-vars)
          (when (yes-or-no-p "Need to reconfigure, configure now? ")
            (cmake-mngr-configure)))))))


;;;###autoload
(defun cmake-mngr-clear-build-directory ()
  "Delete current build directory and all the files inside."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let ((build-dir (gethash "Build Dir" project)))
      (when (and build-dir (file-exists-p build-dir))
        (delete-directory build-dir t)))))


;;;###autoload
(defun cmake-mngr-clear-cache ()
  "Delete \"CMakeCache.txt\"."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (user-error "Cannot find CMake project for this file"))
    (let ((build-dir (gethash "Build Dir" project)))
      (when build-dir
        (let ((file (expand-file-name "CMakeCache.txt" build-dir)))
          (when (file-exists-p file)
            (delete-file file)))))))


;;;###autoload
(defun cmake-mngr-reset ()
  "Reset internal data.  For debugging."
  (interactive)
  (setq cmake-mngr-projects '()))


(provide 'cmake-mngr)
;;; cmake-mngr.el ends here
