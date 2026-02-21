# Lisp Chat Emacs Client

An Emacs Lisp client for Lisp Chat with an ERC-like interface.

## Prerequisites

- Emacs 26.1 or later.
- `websocket` package (installable via MELPA: `M-x package-install RET websocket RET`).

## Installation

Add the `emacs` directory to your `load-path` in your `init.el`:

```elisp
(add-to-list 'load-path "/path/to/lisp-chat/emacs")
(require 'lisp-chat)
```

## Usage

### Connecting

Run `M-x lisp-chat`.

- It will prompt for an address (defaults to `wss://chat.manoel.dev/ws`).
- If you provide a `ws://` or `wss://` URL, it connects via **WebSockets**.
- If you provide a hostname, it connects via **TCP** (default port 8000).

### Interface

The client uses a persistent input area at the bottom of the buffer:

- **Type your message** directly in the buffer after the prompt.
- **`RET`**: Send the current message or confirm your username.
- **`C-c C-q`**: Disconnect and close the buffer.

## Features

- **ERC-like Interface**: Persistent input line at the bottom, chat log above.
- **Smart Scrolling**: Automatically follows new messages if you are at the prompt, but allows scrolling up to read history.
- **Colorized Usernames**: Users have consistent colors based on their names.
- **Mentions**: `@user` mentions are highlighted.
- **User List**: The header line shows a colorized list of currently online users.
- **Automatic History**: Automatically loads the last 100 messages upon joining.
- **Unified Protocol**: One command handles both TCP and WebSocket transparently.
