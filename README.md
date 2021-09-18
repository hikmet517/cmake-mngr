# cmake-mngr.el
Provides following commands.
## Commands
<!--
(progn
  (forward-line 2)
  (insert "\n")
  (let ((funs '(cmake-mngr-configure
                cmake-mngr-build
                cmake-mngr-show-cache-variables
                cmake-mngr-select-build-type
                cmake-mngr-set-generator
                cmake-mngr-set-target
                cmake-mngr-set-build-directory
                cmake-mngr-set-variable
                cmake-mngr-clear-build-directory
                cmake-mngr-reset
                cmake-mngr-create-symlink-to-compile-commands)))
    (dolist (f funs)
      (insert "- **" (symbol-name f) "**: ")
      (insert (replace-regexp-in-string "\n\n" "\n" (documentation f)) "\n\n"))))
-->

- **cmake-mngr-configure**: Configure current project.

- **cmake-mngr-build**: Build current project.

- **cmake-mngr-show-cache-variables**: Show CMake cache variables in a buffer.

- **cmake-mngr-select-build-type**: Get CMake build type from user.

- **cmake-mngr-set-generator**: Set generator for current project.

- **cmake-mngr-set-target**: Set target for current project.

- **cmake-mngr-set-build-directory**: Set CMake build directory.

- **cmake-mngr-set-variable**: Set a CMake variable as KEY=VALUE.
These variables will be passed to cmake during configuration as -DKEY=VALUE.

- **cmake-mngr-clear-build-directory**: Remove current build directory and all the files inside.

- **cmake-mngr-reset**: Reset internal data.

- **cmake-mngr-create-symlink-to-compile-commands**: Create a symlink in project root that points to ’compile_commands.json’
(needed for ‘lsp’ to work).
