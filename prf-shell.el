;;; prf-shell.el --- Better shell-mode API.

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/prf-shell
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(with-shell-interpreter "0.1.0"))
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

(require 'cl-lib)

(require 'tramp)
(require 'tramp-sh)

(require 'with-shell-interpreter)



;; VARS

(defvar prf-shell-spawn-in-same-win 't
  "If 't, shell buffers will spawn in the same window.")

(defvar prf-shell-default-buffer-name "shell"
  "Default buffer name for local shells.")
(defvar prf-shell-buffer-local-name-construction-fn #'prf-shell--generate-buffer-name-local
  "Function to generate local shell buffer names.")
(defvar prf-shell-buffer-remote-name-construction-fn #'prf-shell--generate-buffer-name-remote
  "Function to generate remote shell buffer names.")



;; INTERACTIVE SHELLS

(cl-defun prf-shell (&key path
                          interpreter interpreter-args
                          command-switch
                          w32-arg-quote
                          buffer-name)
  "Create a shell at given PATH, using given INTERPRETER binary."
  (interactive)

  (with-shell-interpreter
    :path path
    :interpreter interpreter
    :interpreter-args interpreter-args
    :w32-arg-quote w32-arg-quote
    :form
    (let* ((path (or path default-directory))
           (is-remote (file-remote-p path))
           (interpreter (or interpreter
                            (if is-remote
                                with-shell-interpreter-default-remote
                              shell-file-name)))
           (interpreter (with-shell-interpreter--normalize-path interpreter))
           (interpreter-name (with-shell-interpreter--get-interpreter-name interpreter))
           (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args")))
           (shell-buffer-basename (or buffer-name
                                      (prf-shell--generate-buffer-name is-remote interpreter default-directory)))
           (shell-buffer-name (generate-new-buffer-name (concat "*" shell-buffer-basename "*")))
           (current-prefix-arg '(4))
           (comint-process-echoes t))
      (when prf-shell-spawn-in-same-win
        (prf-shell--maybe-register-buffer-display-same-win shell-buffer-basename))
      (shell shell-buffer-name)

      (with-current-buffer shell-buffer-name
        (make-local-variable 'explicit-shell-file-name)
        (make-local-variable explicit-interpreter-args-var)

        ;; NB: those are necessary when launching `shell-command' and friends from the interactive shell buffer
        (make-local-variable 'shell-file-name)
        (make-local-variable 'shell-command-switch)

        (make-local-variable 'w32-quote-process-args)))))



;; PRIVATE UTILS: BUFFER NAME

(defun prf-shell--generate-buffer-name (is-remote interpreter path)
  "Generate a buffer name accordint to INTERPRETER, PATH and whether IS-REMOTE or not."
  (let ((fn (if is-remote
                prf-shell-buffer-remote-name-construction-fn
              prf-shell-buffer-local-name-construction-fn)))
    (funcall fn interpreter path)))

(defun prf-shell--generate-buffer-name-local (&optional interpreter _path)
  "Generate a buffer name for local shell, according to INTERPRETER."
  (if interpreter
      (with-shell-interpreter--get-interpreter-name interpreter)
    prf-shell-default-buffer-name))

(defun prf-shell--generate-buffer-name-remote (interpreter path)
  "Generate a buffer name for remote shell, according to INTERPRETER and PATH."
  (let ((vec (tramp-dissect-file-name path)))
    (prf-shell--generate-buffer-name-remote-from-vec vec)))

(defun prf-shell--generate-buffer-name-remote-from-vec (vec)
  "Generate a buffer name for remote shell, from VEC (split tramp path)."
  (let (user host)
    (concat
     (tramp-file-name-user vec) "@" (tramp-file-name-host vec))))



;; PRIVATE UTILS: BUFFER DIPLAY BEHAVIOUR

(defun prf-shell--maybe-register-buffer-display-same-win (basename)
  "If necessary, register buffer buffers containing BASENAME as spawning in the same window."
  (let ((entry `(,(concat "^\\*" basename "\\*\\(.*\\)$") display-buffer-same-window)))
    ;; NB: before Emacs 25, shell-mode buffers would display in same window.
    (when (and (>= emacs-major-version 25)
               (not (member entry display-buffer-alist)))
      (add-to-list 'display-buffer-alist entry))))




(provide 'prf-shell)

;;; prf-shell.el ends here
