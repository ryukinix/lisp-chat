<p align="center">
    <img src="logo/horizontal.png" alt="lisp-chat" height="150px">
</p>


[![Quicklisp dist](http://quickdocs.org/badge/lisp-chat.svg)](http://quickdocs.org/lisp-chat/)

# Lisp Chat

An experimental chat irc-like written in Lisp.

![lisp-chat-screenshot](lisp-chat.png)


# Installation
Install [roswell][ros] and add `~/.roswell/bin/` to the `PATH` variable.

After that just type:

``` bash
ros install ryukinix/lisp-chat
```

Lisp-chat it's on Quicklisp as well, tested on the following
implementations:

* SBCL
* CCL
* ECL

# Usage


Load the server
```bash
$ lisp-chat-server localhost
```

Create a client
```bash
$ lisp-chat localhost
```

Online version with connection through web sockets instead raw tcp sockets:

```
$ lisp-chat wss://chat.manoel.dev/ws
```

# Web Interface

A web interface is now available! You can access the public instance at:
[https://chat.manoel.dev](https://chat.manoel.dev)

When running the server locally, the web interface is accessible at `http://localhost:5559`.

![lisp-chat-web](lisp-chat-web.png)

# Alternative clients

To test this with alternative clients, you can use these options:

* **Emacs client**: An ERC-like interface with colorized usernames, mentions, and WebSocket/TCP support. See [emacs/README.md](emacs/README.md) for details.
* Terminal readline-based [python client](./bin/client.py)
* Terminal ncurses [python client](./bin/client_curses.py)
* Netcat client (wtf?)
* [Lispinto Chat](https://github.com/mateusfccp/lispinto-chat): a Flutter client that runs on macOS, Android, iOS and [web](https://labs.mateusfccp.me/lispinto-chat).

On Python client, I wrote in a way only using ths stdlib avoiding pain
to handle the dependency hell, so you can just call that:

```bash
$ python client.py
```

So finally... netcat. Yes! You can even just use `netcat`! An user
called `Chris` in past days just logged in the server with the
following message:

```
|16:30:37| [Chris]: Used netcad
|16:30:41| [Chris]: netcat*
|16:30:50| [Chris]: bye
```

So you can type `netcat <server> 5558` and go on! I tested on
my machine and works fine! The main reason is because the
communication between server and client just use raw data. For better
synchronization with text data from server while you typing, I suggest
you to use a readline wrapper like
[`rlwrap`](https://github.com/hanslub42/rlwrap) calling as `rlwrap
netcat <server> 5558`.

<p align="center">
  <a href="https://chat.manoel.dev">
    <img src="http://www.lisperati.com/lisplogo_warning2_256.png" width="128" />
  </a>
</p>
