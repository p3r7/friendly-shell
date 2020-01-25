

# Defining Commands

## Shell-Spawning Commands

#### Git bash

```el
(defun shell/git-bash (&optional path)
  (interactive)
  (prf/tramp/shell path "C:/Program Files/Git/bin/bash.exe"))
```

#### Cygwin bash

```el
(defun prf/tramp/shell/cygwin-bash (&optional path)
  (interactive)
  (prf/tramp/shell path "c:/cygwin64/bin/bash.exe"  (list "--init-file" (concat "/home/" (getenv "USERNAME") "/.bashrc"))))
```
