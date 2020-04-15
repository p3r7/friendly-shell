;;; prf-shell.el --- Better shell-mode API -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/prf-shell
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(with-shell-interpreter "0.1.0"))
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



;; VARS

(defvar prf-shell-spawn-in-same-win 't
  "If 't, shell buffers will spawn in the same window.")

(defvar prf-shell-default-buffer-name "shell"
  "Default buffer name for local shells.")
(defvar prf-shell-buffer-local-name-construction-fn #'prf-shell--generate-buffer-name-local
  "Function to generate local shell buffer names.")
(defvar prf-shell-buffer-remote-name-construction-fn #'prf-shell--generate-buffer-name-remote
  "Function to generate remote shell buffer names.")

;; NB: only bound on Windows build of Emacs
(unless (boundp 'w32-quote-process-args)
  ;; tame lexical binding warnings
  (defvar w32-quote-process-args))



;; INTERACTIVE SHELLS

(cl-defun prf-shell (&key path
                          interpreter interpreter-args
                          command-switch
                          w32-arg-quote
                          allow-local-vars
                          buffer-name)
  "Create a shell at given PATH, using given INTERPRETER binary."
  (interactive)

  (with-shell-interpreter
    :path path
    :interpreter interpreter
    :interpreter-args interpreter-args
    :command-switch command-switch
    :w32-arg-quote w32-arg-quote
    :allow-local-vars allow-local-vars
    :form
    (let* (
           ;; duplicated code from `with-shell-interpreter', but necessayr as not special vars and lexical binding is on
           (path (or path default-directory))
           (is-remote (file-remote-p path))
           (ignore-local-vars (not allow-local-vars))
           (interpreter (with-shell-interpreter--get-interpreter-value is-remote ignore-local-vars interpreter))
           (interpreter-name (with-shell-interpreter--get-interpreter-name interpreter))
           (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args")))
           ;; shell buffer name
           (shell-buffer-basename (or buffer-name
                                      (prf-shell--generate-buffer-name is-remote interpreter default-directory)))
           (shell-buffer-name (generate-new-buffer-name (concat "*" shell-buffer-basename "*")))
           (shell-buffer (get-buffer-create shell-buffer-name))
           ;; special vars
           (current-prefix-arg '(4))
           (comint-process-echoes t)
           ;; copies of special vars set by with-shell-interpreter
           (og-explicit-shell-file-name explicit-shell-file-name)
           (og-shell-file-name shell-file-name)
           (og-explicit-interpreter-args (symbol-value explicit-interpreter-args-var))
           (og-shell-command-switch shell-command-switch)
           (og-w32-quote-process-args w32-quote-process-args))

      (when prf-shell-spawn-in-same-win
        (prf-shell--maybe-register-buffer-display-same-win shell-buffer-basename))

      (with-current-buffer shell-buffer
        ;; NB: when making those var buffer-local, we seem to be forced to bind them to the buffer beforehand
        ;; otherwise, starting from 2nd launched shell, lexical binding will be ignored
        (set (make-local-variable 'explicit-shell-file-name) og-explicit-shell-file-name)
        (set (make-local-variable 'shell-file-name) og-shell-file-name)
        (set (make-local-variable explicit-interpreter-args-var) og-explicit-interpreter-args)
        ;; NB: necessary when launching `shell-command' and friends from the interactive shell buffer
        (set (make-local-variable 'shell-command-switch) og-shell-command-switch)
        (when (boundp 'w32-quote-process-args)
          (set (make-local-variable 'shell-command-switch) og-w32-quote-process-args))
        (set (make-local-variable 'comint-process-echoes) t))

      (shell shell-buffer)

      (with-current-buffer shell-buffer
        ;; NB: comint / shell undoes some of our bindings, so we need to set them back
        (set (make-local-variable 'explicit-shell-file-name) og-explicit-shell-file-name)
        (set (make-local-variable 'shell-file-name) og-shell-file-name)
        (set (make-local-variable explicit-interpreter-args-var) og-explicit-interpreter-args)
        (set (make-local-variable 'shell-command-switch) og-shell-command-switch)
        (when (boundp 'w32-quote-process-args)
          (set (make-local-variable 'shell-command-switch) og-w32-quote-process-args))

        ;; assumes echoes input (e.g. 'stty echo' for dash), for TRAMP to do proper dirtrack
        ;; we hide this echoed line to the end user
        ;; REVIEW: should use a keyword to activate is conditionally
        (setq comint-process-echoes t))

      shell-buffer)))



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

(defun prf-shell--generate-buffer-name-remote (&optional _interpreter path)
  "Generate a buffer name for remote shell, according to PATH."
  (let ((vec (tramp-dissect-file-name path)))
    (prf-shell--generate-buffer-name-remote-from-vec vec)))

(defun prf-shell--generate-buffer-name-remote-from-vec (vec)
  "Generate a buffer name for remote shell, from VEC (split tramp path)."
  (concat
   (tramp-file-name-user vec) "@" (tramp-file-name-host vec)))



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
