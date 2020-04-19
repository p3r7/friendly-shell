# friendly-shell

This repository holds several packages:

 - `friendly-shell-command`: Better shell-command API ([blog post](https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands))
 - `friendly-shell`: Better interactive shell API ([blog post](https://www.eigenbahn.com/2020/01/21/painless-emacs-interactive-shells))
 - `friendly-remote-shell`: Human-friendly spawning of remote shells

Examples can be found in [examples.md](examples.md).

They rely heavily on package [with-shell-interpreter](https://github.com/p3r7/with-shell-interpreter).

`friendly-remote-shell` relies additionally on helper packages from [p3r7/friendly-tramp-path](https://github.com/p3r7/friendly-tramp-path).

NB: These packages used to be called `prf-shell-command`, `prf-shell` and `prf-remote-shell`. You can still grab the legacy code with version [0.1.0](https://github.com/p3r7/friendly-shell/releases/tag/0.1.0).


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package with-shell-interpreter)

(use-package friendly-shell-command
  :quelpa (friendly-shell-command :fetcher github :repo "p3r7/friendly-shell")
  :after (tramp with-shell-interpreter))

(use-package friendly-shell
  :quelpa (friendly-shell :fetcher github :repo "p3r7/friendly-shell")
  :after (tramp with-shell-interpreter))
```

And for `friendly-remote-shell`:

```el
(use-package friendly-tramp-path
  :after tramp))

(use-package friendly-remote-shell
  :quelpa (friendly-remote-shell :fetcher github :repo "p3r7/friendly-shell")
  :after (friendly-shell friendly-tramp-path))
```


## Functions

### Interractive Shells

Command for spawning interactive shells or helper function to create new interactive shell-spawning commands.

* friendly-shell | [friendly-shell](#friendlyshell--path-interpreter-interpreter-args-command-switch-w32-arg-quote) `(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`
* friendly-remote-shell | [friendly-remote-shell](#friendlyremoteshell--path-interpreter-interpreter-args-command-switch-w32-arg-quote) `(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`


### Shell Commands

Helpers functions to create commands launching (non-interactive) shell commands.

* friendly-shell-command | [friendly-shell-command-to-string](#friendlyshell-command-to-string-path-cmd--interpreter-interpreter-args-command-switch) `(path cmd & interpreter interpreter-args command-switch)`
* friendly-shell-command | [friendly-shell-command-async](#friendlyshell-command-async-path-cmd--interpreter-interpreter-args-command-switch) `(path cmd & interpreter interpreter-args command-switch)`


## Interractive Shells

#### friendly-shell `(& path interpreter interpreter-args command-switch w32-arg-quote)`

Command to spawn a shell at current location (`default-directory`).

Can also be called as a function with provided arguments, even though we recommend using `friendly-tramp/shell-cl` for this purpose.

#### friendly-remote-shell `(& path interpreter interpreter-args command-switch w32-arg-quote)`

Same as `friendly-shell` but will prompt for path.

Will parse it expecting it to be a remote path.


## Shell Commands

#### friendly-shell-command-to-string `(path cmd & interpreter interpreter-args command-switch)`

Calls CMD with `shell-command-to-string` at given PATH.

PATH can be local or remote.


#### friendly-shell-command-async `(path cmd & interpreter interpreter-args command-switch)`

Calls CMD with `async-shell-command` at given PATH.

PATH can be local or remote.


## Similar projects

#### [Howard Abrams' dot emacs](https://github.com/howardabrams/dot-files)

Howard has a lot of goodies related to remote interactive shells and eshell.

Notably:

 - command [eshell-here ](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-here) that behaves like `friendly-shell` called interactively, except spawning eshell instead of shell.
 - command [eshell-there](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-there) that is very similar to `friendly-remote-shell`, except spawning eshell instead of shell
 - [various commands](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-favorites) for spawning remote shells given host aliases. He also defines a `remote-shell-command` that behaves like `friendly-shell-command`.


#### [killdash9/better-shell](https://github.com/killdash9/better-shell)

This package provides command `better-shell-for-current-dir` which is very similar to `friendly-shell` called interactively. One key difference is that it implements a mechanism for reusing existing shell buffers if appropriate.

It also provides command `better-shell-remote-open` which behaves like `friendly-remote-shell` except it provides autocompletion from `~/.ssh/known_hosts`.


#### [randymorris/tramp-term](https://github.com/randymorris/tramp-term.el)

Bindings between term.el and TRAMP to ease the creation of remote term buffers.
