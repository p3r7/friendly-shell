

## Single Shell Commands

#### Shutdown Raspberry Pi

Assuming the Raspberry Pi is equipped with a [X735 Power Management Board](http://www.suptronics.com/miniPCkits/x735_V2.0.html).

```el
(prf-async-shell-command
 "x730shutdown.sh"
 :output-buffer "*shutdown - raspberry*"
 :path "/ssh:pi@raspberry|sudo::/")
```

Otherwise:

```el
(prf-async-shell-command
 "halt"
 :output-buffer "*shutdown - raspberry*"
 :path "/ssh:pi@raspberry|sudo::/")
```

#### youtube-dl on a NAS

Assuming we're login in as the same user on the NAS with hostname `nas`.

```el
(defun shell-cmd/yt-dl-on-nas (&optional url)
  (interactive)
  (let ((url (or url (read-string "URL: "))))
    (prf-async-shell-command
     (concat "youtube-dl " url)
     :output-buffer (concat "*yt-dl - " url "*")
     :path "/ssh:nas:/volume1/download/")))
```

## Interactive Shell-Spawning Commands

#### Git bash (under Windows)

```el
(defun shell/git-bash (&optional path)
  (interactive)
  (prf-shell path "C:/Program Files/Git/bin/bash.exe"))
```

#### Cygwin bash (under Windows)

```el
(defun prf/tramp/shell/cygwin-bash (&optional path)
  (interactive)
  (prf-shell path "c:/cygwin64/bin/bash.exe"  (list "--init-file" (concat "/home/" (getenv "USERNAME") "/.bashrc"))))
```
