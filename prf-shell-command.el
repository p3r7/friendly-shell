;;; prf-shell-command.el --- Better shell-command API -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/prf-shell
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(with-shell-interpreter "20200319.1351"))
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'cl-lib)

(require 'tramp)
(require 'tramp-sh)

(require 'with-shell-interpreter)



;; (NON-INTERACTIVE) SHELL COMMANDS

(cl-defun prf-shell-command-to-string (command &key path interpreter command-switch)
  "Call COMMAND w/ `shell-command-to-string' with :interpreter on host and location described by :path.
For more details about all the keyword arguments, see `with-shell-interpreter'"
  (with-shell-interpreter
    :form (shell-command-to-string command)
    :path path
    :interpreter interpreter
    :command-switch command-switch))

(cl-defun prf-async-shell-command (command &key output-buffer error-buffer
                                           path interpreter command-switch)
  "Call COMMAND w/ `async-shell-command' with :interpreter on host and location described by :path.
An :output-buffer and :error-buffer can be specified.
For more details about all the keyword arguments, see `with-shell-interpreter'"
  (with-shell-interpreter
    :form (async-shell-command command output-buffer error-buffer)
    :path path
    :interpreter interpreter
    :command-switch command-switch))

(cl-defun prf-shell-command (command &key output-buffer error-buffer
                                     path interpreter command-switch)
  "Call COMMAND w/ `shell-command' with :interpreter on host and location described by :path.
An :output-buffer and :error-buffer can be specified.
For more details about all the keyword arguments, see `with-shell-interpreter'"
  (with-shell-interpreter
    :form (shell-command command output-buffer error-buffer)
    :path path
    :interpreter interpreter
    :command-switch command-switch))




(provide 'prf-shell-command)

;;; prf-shell-command.el ends here
