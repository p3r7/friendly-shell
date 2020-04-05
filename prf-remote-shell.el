;;; prf-remote-shell.el --- Human-friendly remote interactive shells.

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/prf-shell
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(prf-shell "0.1.0")(with-shell-interpreter "0.1.0")(friendly-tramp-path "0.1.0"))
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

(require 'friendly-tramp-path)
(require 'with-shell-interpreter)
(require 'prf-shell)



;; VARS

(defvar prf-remote-shell-buffer-name-regexp "^\\*\\(.*\\)@\\(.*\\)\\*\\(.*\\)$")



;; INTERACTIVE SHELLS

(cl-defun prf-remote-shell (&key path
                                 interpreter interpreter-args
                                 command-switch
                                 w32-arg-quote)
  "Open a remote shell to host (extracted from :path).
:path can is in the form HOST_CNNX[:LOCALNAME],
where HOST_CNNX can be any of:
 - HOST
 - USER@HOST
 - /METHOD:USER@HOST
 - /METHOD:HOST

As such, this is a user-friendly wrapper around `prf-shell' for remote connections."
  (interactive)
  (let* ((path (or path (read-string "Host: ")))
         (path (with-shell-interpreter--normalize-path path))
         (vec (friendly-tramp-path-disect path))
         (method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (domain (tramp-file-name-domain vec))
         (host (tramp-file-name-host vec))
         (port (tramp-file-name-port vec))
         (localname (tramp-file-name-localname vec)))

    (if (>= emacs-major-version 26)
        ;; new DOMAIN and PORT parameters
        (setq path (tramp-make-tramp-file-name method user 'nil host 'nil localname))
      (setq path (tramp-make-tramp-file-name method user host localname)))

    (prf-shell :path path
               :interpreter interpreter
               :interpreter-args interpreter-args
               :command-switch command-switch
               :w32-arg-quote w32-arg-quote)))



;; INIT

(defun prf-remote-shell-register-display-same-window ()
  "Register remote shell buffers to display in same window.
Assumes `prf-remote-shell-buffer-name-regexp' and `prf/shell-buffer-remote-name-construction-fn' are kept with their default values."
  (when (>= emacs-major-version 25)
    ;; NB: before Emacs 25, shell-mode buffers would display in same window.
    (add-to-list 'display-buffer-alist `(,prf-remote-shell-buffer-name-regexp display-buffer-same-window))))




(provide 'prf-remote-shell)

;;; prf-remote-shell.el ends here
