;;; prf-remote-shell.el --- Human-friendly remote interactive shells.

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: tramp, shell
;; URL: https://github.com/p3r7/prf-tramp
;; Package-Requires: ((prf-shell "0.1.0")(prf-tramp "0.1.0")(prf-tramp-friendly-parsing "0.1.0"))
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

(require 'prf-shell)
(require 'prf-tramp)
(require 'prf-tramp-friendly-parsing)



;; VARS

(defvar prf/remote-shell-buffer-name-regexp "^\\*\\(.*\\)@\\(.*\\)\\*\\(.*\\)$")



;; INTERACTIVE SHELLS

(cl-defun prf/remote-shell (&key path
                                 interpreter interpreter-args command-switch
                                 w32-arg-quote)
  "Open a remote shell to host (extracted from PATH).
PATH can is in the form HOST_CNNX[:LOCALNAME],
where HOST_CNNX can be any of:
 - HOST
 - USER@HOST
 - /METHOD:USER@HOST
 - /METHOD:HOST
 - empty (for local shell)

As such, this is a uder-friendly wrapper around `prf/shell' for remote connections."
  (interactive)
  (let* ((path (or path (read-string "Host: ")))
         (path (prf/tramp/sanitize-path path))
         (vec (prf/tramp/path/dissect path))
         (method (tramp-file-name-method vec))
         (user (tramp-file-name-user vec))
         (domain (tramp-file-name-domain vec))
         (host (tramp-file-name-host vec))
         (port (tramp-file-name-port vec))
         (localname (tramp-file-name-localname vec)))

    ;; default values
    (if (eq (length method) 0)
        (setq method tramp-default-method))
    (if (eq (length user) 0)
        (setq user tramp-default-user))
    (if (eq (length localname) 0)
        (setq localname "/"))

    (if (>= emacs-major-version 26)
        ;; new DOMAIN and PORT parameters
        (setq path (tramp-make-tramp-file-name method user 'nil host 'nil localname))
      (setq path (tramp-make-tramp-file-name method user host localname)))

    (prf/shell :path path
               :interpreter interpreter
               :interpreter-args interpreter-args
               :command-switch command-switch
               :w32-arg-quote w32-arg-quote)))



;; INIT

(defun prf/remote-shell-register-display-same-window ()
  "Register remote shell buffers to display in same window.
Assumes `prf/remote-shell-buffer-name-regexp' and `prf/shell-buffer-remote-name-construction-fn' are kept with their default values."
  (when (>= emacs-major-version 25)
    ;; NB: before Emacs 25, shell-mode buffers would display in same window.
    (add-to-list 'display-buffer-alist `(,prf/remote-shell-buffer-name-regexp display-buffer-same-window))))




(provide 'prf-remote-shell)

;;; prf-remote-shell.el ends here.
