# friendly-shell

Wrappers functions around standard Emacs shell API functions, with saner defaults and additional keyword arguments.

They are split into 3 packages:

 - [![MELPA](https://melpa.org/packages/friendly-shell-command-badge.svg)](https://melpa.org/#/friendly-shell-command) `friendly-shell-command`: wraps shell command API from simple.el ([blog post](https://www.eigenbahn.com/2020/01/19/painless-emacs-shell-commands))
 - [![MELPA](https://melpa.org/packages/friendly-shell-badge.svg)](https://melpa.org/#/friendly-shell) `friendly-shell`: wraps interactive shell API from shell.el ([blog post](https://www.eigenbahn.com/2020/01/21/painless-emacs-interactive-shells))
 - [![MELPA](https://melpa.org/packages/friendly-remote-shell-badge.svg)](https://melpa.org/#/friendly-remote-shell) `friendly-remote-shell`: command for human-friendly spawning of remote shells

Examples can be found in [examples.md](examples.md).

They rely heavily on package [with-shell-interpreter](https://github.com/p3r7/with-shell-interpreter).

`friendly-remote-shell` relies additionally on helper package [friendly-tramp-path](https://github.com/p3r7/friendly-tramp-path).

NB: These packages used to be called `prf-shell-command`, `prf-shell` and `prf-remote-shell`. You can still grab the legacy code with version [0.1.0](https://github.com/p3r7/friendly-shell/releases/tag/0.1.0).


## Installation

All the packages are available on [Melpa](https://melpa.org/).

```el
(use-package friendly-shell-command)

(use-package friendly-shell)

(use-package friendly-remote-shell)
```


## General usage

Each of functions provided by the package have the same behavior as their wrapped counterparts but come with additional keyword arguments.

Here are the additional keywords that are shared by all the wrapper functions:

| keyword argument    | implicit var being let-bound                   | description                                                                       |
|---------------------|------------------------------------------------|-----------------------------------------------------------------------------------|
| _:path_             | `default-directory`                            | The path from which to launch command / shell.                                    |
| _:interpreter_      | `explicit-shell-file-name` / `shell-file-name` | Name or absolute path of shell interpreter executable.                            |
| _:interpreter-args_ | `explicit-INTEPRETER-args`                     | Login args to call interpreter with for login.                                    |
| _:command-switch_   | `shell-command-switch`                         | Command switch arg for asking interpreter to run a shell command.                 |
| _:w32-arg-quote_    | `w32-quote-process-args`                       | Character to use for quoting shell arguments (only on the Windows build of Emacs) |

If _:path_ is remote, the command will be executed with the remote host interpeter.

See README of [with-shell-interpreter](https://github.com/p3r7/with-shell-interpreter) for more details.

Concrete examples can be found in [examples.md](examples.md).


## Functions index

### Shell Commands

Helpers functions to create commands launching (non-interactive) shell commands.

* friendly-shell-command | [friendly-shell-command-to-string](#friendly-shell-command-to-string) `(cmd & :path :interpreter :command-switch)`
* friendly-shell-command | [friendly-shell-command](#friendly-shell-command) `(cmd & :output-buffer :error-buffer :path :interpreter :interpreter-args :command-switch :callback :kill-buffer)`
* friendly-shell-command | [friendly-shell-command-async](#friendly-shell-command-async) `(cmd & :output-buffer :error-buffer :path :interpreter :interpreter-args :command-switch :callback :kill-buffer :sentinel)`


### Interactive Shells

Command for spawning interactive shells or helper function to create new interactive shell-spawning commands.

* friendly-shell | [friendly-shell](#friendly-shell) `(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`
* friendly-remote-shell | [friendly-remote-shell](#friendly-remote-shell) `(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`


## Shell Commands

Emacs provides some standard function for launching shell commands (from `simple.el`):

| function                                                       | execution    | return value                      | spawned buffers   |
| --                                                             | :--:         | --                                | --                |
| _shell-command-to-string_ `(command)`                          | synchronous  | stdout                            |                   |
| _shell-command_ `(command & output-buffer error-buffer)`       | synchronous  | return code                       | stdout and stderr |
| _async-shell-command_ `(command & output-buffer error-buffer)` | asynchronous | _window_ containing output-buffer | stdout and stderr |

Package `friendly-shell-command` provide a wrapper around each of those.

They have the same behavior (sync/async, return value, spawned buffers) as their wrapped counterparts.


#### friendly-shell-command-to-string

`(cmd & :path :interpreter :command-switch)`

Calls CMD with `shell-command-to-string` with _:interpreter_ at given _:path_.


#### friendly-shell-command

`(cmd & :output-buffer :error-buffer :path :interpreter :interpreter-args :command-switch :callback :kill-buffer)`

Calls CMD synchronously with `shell-command` with _:interpreter_ at given _:path_.

In addition to the [common keywords](#General-usage), the following additional keyword can be used:

| keyword          | description                                          | default value             |
|------------------|------------------------------------------------------|---------------------------|
| _:output-buffer_ | Buffer to output to.                                 | `*Shell Command Output*`  |
| _:error-buffer_  | Buffer to output stderr to.                          | value of `:output-buffer` |
| _:kill-buffer_   | If non-nil, will output buffer after execution       | `nil`                     |
| _:callback_      | Function to run at the end of the command execution. | n/a                       |

Please note that `:callback` function should not take any argument (0-arity).


#### friendly-shell-command-async

`(cmd & :output-buffer :error-buffer :path :interpreter :interpreter-args :command-switch :callback :kill-buffer :sentinel)`

Calls CMD asynchronously with `async-shell-command` with _:interpreter_ at given _:path_.

In addition to the [common keywords](#General-usage), the following additional keyword can be used:

| keyword          | description                                          | default value             |
|------------------|------------------------------------------------------|---------------------------|
| _:output-buffer_ | Buffer to output to.                                 | `*Shell Command Output*`  |
| _:error-buffer_  | Buffer to output stderr to.                          | value of `:output-buffer` |
| _:kill-buffer_   | If non-nil, will output buffer after execution       | `nil`                     |
| _:callback_      | Function to run at the end of the command execution. | n/a                       |
| _:sentinel_      | Process sentinel to bind to the command process.     | n/a                       |

Please note that _:callback_ function should not take any argument (0-arity).

For the _:sentinel_ argument, you can read more about process sentinels in [the Emacs manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Sentinels.html).


## Interactive Shells

#### friendly-shell

`(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`

Spawn a shell with `shell` with _:interpreter_ at given _:path_.

When used as a command (i.e. called interactively), spawns a shell at current location (`default-directory`) with the default interpreter (`shell-file-name` or `with-shell-interpreter-default-remote` if on a remote server).

Contrarily to default `shell` behavior, the value of interpreter and its args are kept as buffer-local vars (`explicit-shell-file-name`, `explicit-<interpreter>-args` ,`shell-command-switch`...).

This allows reusing the same interpreter config when launching shell commands from the spawned buffer.


#### friendly-remote-shell

`(& :path :interpreter :interpreter-args :command-switch :w32-arg-quote)`

Same as `friendly-shell` but accept a more permissive remote path format (thanks to [p3r7/friendly-tramp-path](https://github.com/p3r7/friendly-tramp-path)).

If called as a command, will prompt user for _:path_, assuming it's a remote host.

For example:

    M-x friendly-shell
    Host: pi@raspberry


## Complementary packages

I highly encourage to use package [shx](https://github.com/riscy/shx-for-emacs) which provides various enhancement for interactive shell buffers.

One of the major improvement is the ability to resurrect dead shell buffers.


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
