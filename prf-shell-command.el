;;; prf-shell-command.el --- Better shell-command API.

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp, shell
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((with-shell-interpreter "0.1.0"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'tramp)
(require 'tramp-sh)

(require 'with-shell-interpreter)



;; (NON-INTERACTIVE) SHELL COMMANDS

(cl-defun prf/shell-command-to-string (command &key path interpreter command-switch)
  "Call CMD w/ `shell-command-to-string' on host and location described by PATH"
  (with-shell-interpreter
   :form (shell-command-to-string command)
   :path path
   :interpreter interpreter
   :command-switch command-switch))

(cl-defun prf/async-shell-command (command &key output-buffer error-buffer
                                           path interpreter command-switch)
  "Call CMD w/ `async-shell-command' on host and location described by PATH"
  (with-shell-interpreter
   :form (async-shell-command command output-buffer error-buffer)
   :path path
   :interpreter interpreter
   :command-switch command-switch))

(cl-defun prf/shell-command (command &key output-buffer error-buffer
                                     path interpreter command-switch)
  "Call CMD w/ `shell-command' on host and location described by PATH"
  (with-shell-interpreter
   :form (shell-command command output-buffer error-buffer)
   :path path
   :interpreter interpreter
   :command-switch command-switch))




(provide 'prf-shell-command)

;;; prf-shell-command.el ends here.
