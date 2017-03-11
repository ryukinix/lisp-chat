# Lisp Chat

An experimental chat irc-like written in Lisp.

![lisp-chat-screenshot](lisp-chat.png)


# Installation

You need in your system:

* [SBCL](http://www.sbcl.org/)
* [Quicklisp](https://github.com/quicklisp/quicklisp-client)

And make sure that `~/.sbclrc` has a entry calling to the quicklisp setup.

```common-lisp
(load "~/quicklisp/setup.lisp")
```

# Usage

Load the server
```bash
$ sbcl --load server.lisp
```

Get a client
```bash
$ sbcl --load client.lisp
```

# Tip for Quicklisp

Clone the attached repository of Quicklisp and execute the `setup.lisp` with
`sbcl --script setup.lisp`
