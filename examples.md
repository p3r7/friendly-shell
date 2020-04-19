

## Single Shell Commands

#### Shutdown Raspberry Pi

Assuming the Raspberry Pi is equipped with a [X735 Power Management Board](http://www.suptronics.com/miniPCkits/x735_V2.0.html).

```el
(friendly-shell-command-async
 "x730shutdown.sh"
 :output-buffer "*shutdown - raspberry*"
 :path "/ssh:pi@raspberry|sudo::/")
```

Otherwise:

```el
(friendly-shell-command-async
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
    (friendly-shell-command-async
     (concat "youtube-dl " url)
     :output-buffer (concat "*yt-dl - " url "*")
     :path "/ssh:nas:/volume1/download/")))
```

## Interactive Shell-Spawning Commands

#### Git bash (under Windows)

```el
(defun shell/git-bash (&optional path)
  (interactive)
  (friendly-shell :path path
                  :interpreter "C:/Program Files/Git/bin/bash.exe"))
```

#### Cygwin bash (under Windows)

```el
(defun prf/tramp/shell/cygwin-bash (&optional path)
  (interactive)
  (friendly-shell :path path
                  :interpreter "c:/cygwin64/bin/bash.exe"
                  :interpreter-args (list "--init-file" (concat "/home/" (getenv "USERNAME") "/.bashrc"))))
```
