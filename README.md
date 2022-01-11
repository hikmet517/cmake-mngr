# cmake-mngr.el

<!--
(let ((symbols '()))
  (mapatoms (lambda (s)
              (when (string-prefix-p "cmake-mngr-"
                                     (symbol-name s))
                (push s symbols))))
  (setq symbols (sort symbols (lambda (s1 s2) (string-lessp (symbol-name s1)
                                                            (symbol-name s2)))))
  (let ((commands (seq-filter (lambda (s) (and (commandp s)
                                               (not (string= (symbol-name s)
                                                             "cmake-mngr-variables-mode"))))
                              symbols))
        (variables (seq-filter #'user-variable-p symbols)))

    ;; commands
    (progn
      (forward-line 2)
      (insert "\n")
      (insert "## Commands\n")
      (dolist (f commands)
        (insert "- **" (symbol-name f) "**: ")
        (insert (replace-regexp-in-string "\n\n" "\n" (documentation f)) "\n\n")))

    ;; variables
    (progn
      (forward-line 2)
      (insert "\n")
      (insert "## Variables\n")
      (dolist (v variables)
        (insert "- **" (symbol-name v) "**: ")
        (insert (replace-regexp-in-string
                 "\n\n"
                 "\n"
                 (documentation-property v 'variable-documentation))
                "\n\n")))))
-->

## Commands
- **cmake-mngr-build**: Build current project.

- **cmake-mngr-clear-build-directory**: Remove current build directory and all the files inside.

- **cmake-mngr-clear-cache**: Remove CMakeCache.txt

- **cmake-mngr-configure**: Configure current project.

- **cmake-mngr-create-symlink-to-compile-commands**: Create a symlink in project root that points to ’compile_commands.json’
(needed for ‘lsp’ to work).

- **cmake-mngr-reset**: Reset internal data.

- **cmake-mngr-select-build-type**: Get CMake build type from user.

- **cmake-mngr-set-build-directory**: Set CMake build directory.

- **cmake-mngr-set-generator**: Set generator for current project.

- **cmake-mngr-set-target**: Set target for current project.

- **cmake-mngr-set-variable**: Set a CMake variable as KEY=VALUE.
These variables will be passed to cmake during configuration as -DKEY=VALUE.

- **cmake-mngr-show-cache-variables**: Show CMake cache variables in a buffer.


## Variables
- **cmake-mngr-build-dir-search-list**: List of directories to search for CMake build directory.
Should be non-nil.

- **cmake-mngr-global-build-args**: Argument to pass during build.

- **cmake-mngr-global-configure-args**: Argument to pass during configuration.
