<p align="center">
  <a href= http://chat.lerax.me>
    <img src="http://www.lisperati.com/lisplogo_warning2_256.png" width="128" />
  </a>
</p>

# Lisp Chat 

<a href="https://app.codesponsor.io/link/jza5upfrcVSndCUAeCgb4ScS/ryukinix/lisp-chat" rel="nofollow"><img src="https://app.codesponsor.io/embed/jza5upfrcVSndCUAeCgb4ScS/ryukinix/lisp-chat.svg" style="width: 888px; height: 68px;" alt="Sponsor" /></a>

An experimental chat irc-like written in Lisp.

![lisp-chat-screenshot](lisp-chat.png)


# Installation

You'll need in your system:

* [SBCL]
* [Quicklisp Client]

[SBCL]: http://www.sbcl.org/
[Quicklisp Client]: https://github.com/quicklisp/quicklisp-client

Make sure that `~/.sbclrc` has a entry calling to the Quicklisp setup.

```common-lisp
(load "~/.quicklisp/setup.lisp")
```

Since that `~/.quicklisp` is the Quicklisp instalation distribution, clone this
repository with:

`git clone https://www.github.com/ryukinix/lisp-chat.git ~/.quicklisp/local-projects/lisp-chat`

## Tip for Quicklisp

As I said, we'll need a proper instalation of Quicklisp to getting this running. To accomplish that,
if you are not familiar with the CL ecossystem, I'll guide you in a simple way: clone the [Quicklisp Client] and execute the `setup.lisp` with `sbcl --script setup.lisp`.

In general Quicklisp distribution provide the folder `~/quicklisp` instead
of `~/.quicklisp`, but I personally don't like it much. Make sure to synchronize with the quicklisp correct folder. Maybe
will need change the `.sblcrc` file to accomplish that, since for default as I said quicklisp set `~/quicklisp` folder.

# Usage


Load the server
```bash
$ ./lisp-chat-server
```

Get a client
```bash
$ ./lisp-chat
```


You can easilly setup your own configuration of `*port*` and
`*domain*` for running a new instance of the server and client. For
now we have a instance of lisp-chat as example running. If you really
want to runs locally, you can just change the `*domain*` variable to `"localhost"` as:

```lisp
(require 'lisp-chat)
(setq lisp-chat-config:*domain* "localhost")
```


# For Non-lispers

If you want test this and don't have the Lisp environment with SBCL
and Quicklisp, I have two alternative choices for you:

* Python client
* Netcat client (wtf?)

On Python client, I wrote in a way only using ths stdlib avoiding pain to handle the dependency hell, so you can just call that:

```bash
$ python client.py
```

So finally... netcat. Yes! You can even just use `netcat`! An user called `Chris` in past days
just logged in the server with the following message:

```
|16:30:37| [Chris]: Used netcad
|16:30:41| [Chris]: netcat*
|16:30:50| [Chris]: bye
```

So you can type `netcat chat.lerax.me 5558` and go on! I tested on my machine and
works fine! The main reason is because the communication between
server and client just use raw data. For better synchronization with
text data from server while you typing, I suggest you to
use a readline wrapper like [`rlwrap`](https://github.com/hanslub42/rlwrap) calling as `rlwrap netcat
ryukinix.tk 5558`.
