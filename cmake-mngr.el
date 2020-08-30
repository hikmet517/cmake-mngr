;;; package --- Manage cmake projects

;; Author: Hikmet Altıntaş (hikmet1517@gmail.com)
;; Keywords: tools, extensions
;; URL: "https://github.com/hikmet517/cmake-mngr.el"

;;; Commentary:
;; TODO: Write here

;;; Code:

;;;; Variables
(defvar cmake-mngr-projects (list)
  "Currently opened cmake projects.")


;;;; User options
(defgroup cmake-mngr nil
  "Cmake-mngr customization."
  :group 'cmake-mngr
  :prefix "cmake-mngr-"
  :link '(url-link "https://github.com/hikmet517/cmake-mngr.el"))

(defcustom cmake-mngr-build-dir-search-list '("build" "bin" "out")
  "List of directories to search for cmake build directory.
Should be non-nil."
  :type '(repeat directory)
  :group 'cmake-mngr)

(defcustom cmake-mngr-global-configure-args '("-DCMAKE_EXPORT_COMPILE_COMMANDS=1")
  "Argument to pass during configuration."
  :type '(repeat string)
  :group 'cmake-mngr)


;;;; Functions
(defun cmake-mngr--parse-cache-file (filepath)
  "Parse given CMakeCache.txt file in FILEPATH as '(key type value)."
  (when (file-readable-p filepath)
    (let ((content (with-temp-buffer
                     (insert-file-contents filepath)
                     (split-string (buffer-string) "\n" t))))
      (setq content (seq-filter (lambda (s)
                                  (not (or
                                        (string-prefix-p "#" s)
                                        (string-prefix-p "//" s))))
                                content))
      ;; if there is a line that does not contain '=', return nil
      (unless (seq-find  (lambda (s) (not (seq-contains-p s ?=))) content)
        (mapcar (lambda (s)
                  (let* ((kv (split-string s "=" t))
                         (kt (split-string (car kv) ":" t)))
                    (list (car kt) ;; (cadr kt)
                          (cadr kv))))
                content)))))


(defun cmake-mngr--find-project-dir (filepath)
  "Find cmake project root for buffer with the path FILEPATH."
  (let ((dirpath filepath)
        (dir-found nil)
        (is-top nil))
    (while (and dirpath (not is-top))
      (setq dirpath (file-name-directory (string-trim-right dirpath "/")))
      (if (file-exists-p (expand-file-name "CMakeLists.txt" dirpath))
          (setq dir-found dirpath)
        (when dir-found
          (setq is-top t))))
    dir-found))


(defun cmake-mngr--get-project-build-dir (project-dir)
  "Get cmake project build directory by searching the path of PROJECT-DIR.

First, it searches for directories listed in
`cmake-mngr-build-dir-search-list'.  If any of them exists in filesystem,
selects this.  Otherwise, selects the first directory in the list."
  (let ((build-dir nil))
    (dolist (d cmake-mngr-build-dir-search-list)
      (let ((dd (concat project-dir d "/")))
        (when (and (file-exists-p dd) (not build-dir))
          (setq build-dir dd))))
    (if (not build-dir)
        (concat project-dir (car cmake-mngr-build-dir-search-list) "/")
      build-dir)))


(defun cmake-mngr--get-project ()
  "Get project's data structure for current buffer.

If it already found before (added to `cmake-mngr-projects') returns
this.  Otherwise, searches directory structure of current buffer.  If
found data is added `cmake-mngr-projects', otherwise returns nil."
  (let* ((filepath (buffer-file-name))
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
                 (build-dir (cmake-mngr--get-project-build-dir project-dir))
                 (cache-vars (cmake-mngr--parse-cache-file
                              (concat build-dir "CMakeCache.txt")))
                 (cache-data (make-hash-table :test 'equal)))
            (setq project-data (make-hash-table :test 'equal))
            (puthash "Root Name" root-name project-data)
            (puthash "Project Dir" project-dir project-data)
            (puthash "Build Dir" build-dir project-data)
            (puthash "Custom Vars" (make-hash-table :test 'equal) project-data)
            (dolist (c cache-vars)
              (puthash (car c) (cadr c) cache-data))
            (puthash "Cache Vars" cache-data project-data)
            (push (cons project-dir project-data) cmake-mngr-projects)))))
    project-data))


(defun cmake-mngr-show-cache-variables ()
  "Show cmake cache variable in a buffer."
  (interactive)
  (let ((project (cmake-mngr--get-project))
        (buffer (generate-new-buffer "*cmake-cache-variables*")))
    (unless project
      (error "Cannot find cmake project for this file"))
    (switch-to-buffer buffer)
    (maphash (lambda (k v)
               (insert k "=" (if v v "") "\n"))
             (gethash "Cache Vars" project))
    (setq buffer-read-only t)))


(defun cmake-mngr-configure ()
  "Configure current project."
  (interactive)
  (let* ((project (cmake-mngr--get-project))
         (build-dir (when project (gethash "Build Dir" project))))
    (unless project
      (error "Cannot find cmake project for this file"))
    (unless (file-exists-p build-dir)
      (mkdir build-dir))
    (let ((cmd (format "cmake -S \"%s\" -B \"%s\""
                       (gethash "Project Dir" project)
                       (gethash "Build Dir" project))))
      (maphash (lambda (k v) (setq cmd (concat cmd " -D" k "=" v)))
               (gethash "Custom Vars" project))
      (dolist (v cmake-mngr-global-configure-args)
        (setq cmd (concat cmd " " v)))
      (setq cmd (concat "echo " cmd "; " cmd))
      (async-shell-command cmd "*cmake configure*")
      (message "Done"))))


(defun cmake-mngr-build ()
  "Build current project."
  (interactive)
  (error "Not implemented"))


(defun cmake-mngr-select-build-type ()
  "Get cmake build type from user."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (error "Cannot find cmake project for this file"))
    (let ((type (completing-read "Select cmake build type: "
                                 '("Debug" "Release" "MinSizeRel" "RelWithDebInfo")
                                 nil
                                 t))
          (custom (gethash "Custom Vars" project)))
      (when (and custom type)
        (puthash "CMAKE_BUILD_TYPE" type custom)))))


(defun cmake-mngr-set-build-directory ()
  "Set cmake build directory."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (error "Cannot find cmake project for this file"))
    (let* ((proj-dir (gethash "Project Dir" project))
           (build-dir (gethash "Build Dir" project))
           (build-dir-name (directory-file-name (string-trim-left build-dir proj-dir)))
           (choice (completing-read
                    (format "Select cmake build directory (default %s): "
                            build-dir-name) ;; PROMPT
                    cmake-mngr-build-dir-search-list ;; COLLECTION
                    nil nil nil nil ;; PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST
                    build-dir-name ;; DEF
                    )))
      (when (and proj-dir choice)
        (puthash "Build Dir" (concat proj-dir choice) project)))))


(defun cmake-mngr-set-variable ()
  "Set a cmake variable as key=value."
  (interactive)
  (let ((project (cmake-mngr--get-project)))
    (unless project
      (error "Cannot find cmake project for this file"))
    (let* ((vars (gethash "Cache Vars" project))
           (key (completing-read "Variable: "
                                 (when vars (hash-table-keys vars))))
           (dflt (when (and vars key) (gethash key vars)))
           (val (completing-read "Value: "
                                 (when dflt (list dflt)))))
      (let ((custom (gethash "Custom Vars" project)))
        (when (and custom key val)
          (puthash key val custom))))))


;;;###autoload
(define-minor-mode cmake-mngr-mode
  "Minor mode to manage cmake projects"
  :lighter " cmake-mngr")

(provide 'cmake-mngr)

;;; cmake-mngr.el ends here


;; random tests
;;
;; (gethash "Build Dir" (cdar cmake-mngr-projects))
;;
