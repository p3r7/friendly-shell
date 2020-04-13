# prf-shell

This repository holds several packages:

 - `prf-shell-command`: Better shell-command API ([blog post](https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands))
 - `prf-shell`: Better interactive shell API ([blog post](https://www.eigenbahn.com/2020/01/21/painless-emacs-interactive-shells))
 - `prf-remote-shell`: Human-friendly spawning of remote shells

Examples can be found in [examples.md](examples.md).

They rely heavily on package [with-shell-interpreter](https://github.com/p3r7/with-shell-interpreter).

`prf-remote-shell` relies additionally on helper packages from [p3r7/friendly-tramp-path](https://github.com/p3r7/friendly-tramp-path).


## Installation

Not yet on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package with-shell-interpreter)

(use-package prf-shell-command
  :quelpa (prf-shell-command :fetcher github :repo "p3r7/prf-shell")
  :after (tramp with-shell-interpreter))

(use-package prf-shell
  :quelpa (prf-shell :fetcher github :repo "p3r7/prf-shell")
  :after (tramp with-shell-interpreter))
```

And for `prf-remote-shell`:

```el
(use-package friendly-tramp-path
  :after tramp))

(use-package prf-remote-shell
  :quelpa (prf-remote-shell :fetcher github :repo "p3r7/prf-shell")
  :after (prf-shell friendly-tramp-path))
```


## Functions

### Interractive Shells

Command for spawning interactive shells or helper function to create new interactive shell-spawning commands.

* prf-shell | [prf-shell](#prfshell--path-interpreter-interpreter-args-command-switch-w32-arg-quote) `(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`
* prf-remote-shell | [prf-remote-shell](#prfremoteshell--path-interpreter-interpreter-args-command-switch-w32-arg-quote) `(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`


### Shell Commands

Helpers functions to create commands launching (non-interactive) shell commands.

* prf-shell-command | [prf-shell-command-to-string](#prfshell-command-to-string-path-cmd--interpreter-interpreter-args-command-switch) `(path cmd & interpreter interpreter-args command-switch)`
* prf-shell-command | [prf-async-shell-command](#prfasync-shell-command-path-cmd--interpreter-interpreter-args-command-switch) `(path cmd & interpreter interpreter-args command-switch)`


## Interractive Shells

#### prf-shell `(& path interpreter interpreter-args command-switch w32-arg-quote)`

Command to spawn a shell at current location (`default-directory`).

Can also be called as a function with provided arguments, even though we recommend using `prf-tramp/shell-cl` for this purpose.

#### prf-remote-shell `(& path interpreter interpreter-args command-switch w32-arg-quote)`

Same as `prf-shell` but will prompt for path.

Will parse it expecting it to be a remote path.


## Shell Commands

#### prf-shell-command-to-string `(path cmd & interpreter interpreter-args command-switch)`

Calls CMD with `shell-command-to-string` at given PATH.

PATH can be local or remote.


#### prf-async-shell-command `(path cmd & interpreter interpreter-args command-switch)`

Calls CMD with `async-shell-command` at given PATH.

PATH can be local or remote.


## Similar projects

#### [Howard Abrams' dot emacs](https://github.com/howardabrams/dot-files)

Howard has a lot of goodies related to remote interactive shells and eshell.

Notably:

 - command [eshell-here ](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-here) that behaves like `prf-shell` called interactively, except spawning eshell instead of shell.
 - command [eshell-there](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-there) that is very similar to `prf-remote-shell`, except spawning eshell instead of shell
 - [various commands](https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org#shell-favorites) for spawning remote shells given host aliases. He also defines a `remote-shell-command` that behaves like `prf-shell-command`.


#### [killdash9/better-shell](https://github.com/killdash9/better-shell)

This package provides command `better-shell-for-current-dir` which is very similar to `prf-shell` called interactively. One key difference is that it implements a mechanism for reusing existing shell buffers if appropriate.

It also provides command `better-shell-remote-open` which behaves like `prf-remote-shell` except it provides autocompletion from `~/.ssh/known_hosts`.


#### [randymorris/tramp-term](https://github.com/randymorris/tramp-term.el)

Bindings between term.el and TRAMP to ease the creation of remote term buffers.
