# lisp-chat API Reference

The `lisp-chat` server provides a REST API for executing chat commands
over HTTP. This allows for programmatic interaction with the chat
server, enabling bots, web integrations, or CLI tools to interact with
the system.

## Base URL

All currenty API requests for commands are made via `POST` to:
`http://<host>:<websocket-port>/api/commands/<command>`

*Note: The API port is the same as the WebSocket port (default: 5559).*

---

## Authentication

Some API commands require an active session. Authenticated requests must include a `Client-Session` header.

### How to get a Client-Session
1. Join the chat through a standard client (Web, TUI, or Emacs).
2. Execute the command: `/session`
3. The server will respond with your unique session UUID.
4. Use this UUID in the `Client-Session` header of your HTTP requests.

**Example Header:**
`Client-Session: 550e8400-e29b-41d4-a716-446655440000`

---

## Request Format

Requests must be `POST` with a JSON body. The body can contain:
- `args`: A list of positional arguments.
- `kwargs`: A dictionary of keyword arguments.
- `channel`: (Optional) Override the active channel context for this specific command.

**Example Payload:**
```json
POST /api/commands/search
{
  "args": ["https"],
  "kwargs": {
    "user": "lerax",
    "limit": "5"
  },
  "channel": "#testing"
}
```

The command above will get the last 5 messages of lerax where https
are mnetioned in the content in the channel #testing.

---

## Response Format

The API returns a JSON object containing a `result` string, which is the exact output the command would have sent to a chat client.

**Success (200 OK):**
```json
{
  "result": "Server online since 16:11:13 of Thursday, 2026-03-05 (GMT-3)"
}
```

**Error (4xx/5xx):**
```json
{
  "error": "Unauthorized: valid Client-Session header required"
}
```

---

## Example Commands

### Public Commands (No Authentication Required)
These commands do not require a `Client-Session` header.

| Command      | Path                     | Description                                   |
|:-------------|:-------------------------|:----------------------------------------------|
| **version**  | `/api/commands/version`  | Returns the server version.                   |
| **uptime**   | `/api/commands/uptime`   | Returns how long the server has been running. |
| **users**    | `/api/commands/users`    | Lists users currently in the channel.         |
| **channels** | `/api/commands/channels` | Lists all active public channels.             |

**Curl Example:**
```bash
curl -X POST http://localhost:5559/api/commands/uptime -H "Content-Type: application/json" -d "{}"
```

### Authenticated Commands
These require a valid `Client-Session` header.

| Command   | Path                  | Description                                 |
|:----------|:----------------------|:--------------------------------------------|
| **log**   | `/api/commands/log`   | Fetch recent message history.               |
| **whois** | `/api/commands/whois` | Get info about a specific user.             |
| **join**  | `/api/commands/join`  | Join a new channel (changes session state). |

**Curl Example (Log with Date Format):**
```bash
curl -X POST http://localhost:5559/api/commands/log \
     -H "Content-Type: application/json" \
     -H "Client-Session: <your-session-id>" \
     -d '{"kwargs": {"date-format": "date"}}'
```

---

## Blocked Commands
For security reasons, the following commands are blocked from the HTTP API:
- `/lisp`: Direct S-expression evaluation is restricted to interactive clients.
- `/session`: To prevent session hijacking, you cannot request a session ID through the API itself.
