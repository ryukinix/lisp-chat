# Lisp Chat

<a href="https://app.codesponsor.io/link/jza5upfrcVSndCUAeCgb4ScS/ryukinix/lisp-chat" rel="nofollow"><img src="https://app.codesponsor.io/embed/jza5upfrcVSndCUAeCgb4ScS/ryukinix/lisp-chat.svg" style="width: 888px; height: 68px;" alt="Sponsor" /></a>

An experimental chat irc-like written in Lisp.

![lisp-chat-screenshot](lisp-chat.png)


# Installation

You'll need in your system:

* [SBCL](http://www.sbcl.org/)
* [Quicklisp](https://github.com/quicklisp/quicklisp-client)

Make sure that `~/.sbclrc` has a entry calling to the Quicklisp setup.

```common-lisp
(load "~/.quicklisp/setup.lisp")
```

Since that `~/.quicklisp` is the Quicklisp instalation distribution, clone this
repository on:

`git clone https://www.github.com/ryukinix/lisp-chat.git ~/.quicklisp/local-projects/lisp-chat`

## Tip for Quicklisp

Clone the attached repository of Quicklisp and execute the
`setup.lisp` with `sbcl --script setup.lisp`.

In general Quicklisp distribution is provide on `~/quicklisp` instead
of `~/.quicklisp`, but I personally don't like it much. So you can or
edit your `.sbclrc` file to complains that and adjust the clone path
or just rename the folder and use as I use.

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
now we have a instance of lisp-chat as example running. If you sure
want to runs locally, you can just change.


# For Non-lispers

If you wish test this and don't have the Lisp environment with SBCL
and Quicklisp, you can try the client version written in Python using
only the stdlib.

```bash
$ python client.py
```

You can even just use `netcat`! A user called `Chris` in past days
just logged in the server with the following message:

```
|16:30:37| [Chris]: Used netcad
|16:30:41| [Chris]: netcat*
|16:30:50| [Chris]: bye
```

So you can just type `netcat ryukinix.tk 5558` and go on! I tested and
works fine! The main reason is because the communication between
server and client just use raw data. For better synchronization with
text data from server while you typing, I suggest you to
use [`rlwrap`](https://github.com/hanslub42/rlwrap) as `rlwrap netcat
ryukinix.tk 5558`.
