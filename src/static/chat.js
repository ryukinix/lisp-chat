const chat = document.getElementById('chat');
const form = document.getElementById('input-area');
const input = document.getElementById('message-input');
const userList = document.getElementById('user-list');

let ws;
let loggedIn = false;
let username = '';
let fetchUsersInterval;
let backgroundRequestsPending = 0;

function addMessage(text) {
    const linesArray = text.split('\n');
    for (const line of linesArray) {
        if (line === "> Type your username: " && username) {
            ws.send(username);
            loggedIn = true;
            // Restart periodic updates if not already running
            if (!fetchUsersInterval) {
                fetchUsersInterval = setInterval(requestUserList, 5000);
                setTimeout(requestUserList, 500);
            }
            continue;
        }

        // Check if it's a response from @server

        // Format: |HH:MM:SS| [from]: content

        const match = line.match(/^\|(\d{2}:\d{2}):(\d{2})\| \[(.*?)\]: (.*)$/);

        if (match) {
            const [_, timeHM, timeS, from, content] = match;
            if (from === '@server') {
                const isSystemMessage = content.includes("joined to the party") ||
                    content.includes("exited from the party") ||
                    content.includes("Your new nick is");
                const isHelpOrUptime = content.startsWith("Server online since") ||
                    content.startsWith("/users, /help");

                if (isSystemMessage) {
                    requestUserList(); // Refresh sidebar on join/part/nick
                } else if (!isHelpOrUptime) {
                    // This is likely the response to /users
                    updateUserList(content);
                    if (backgroundRequestsPending > 0) {
                        backgroundRequestsPending--;
                        continue; // Swallow background updates

                    }

                }

            }

            const div = document.createElement('div');


            div.className = 'message';

            const timeSpan = document.createElement('span');
            timeSpan.className = 'timestamp';
            timeSpan.innerHTML = `${timeHM}<span class="timestamp-seconds">:${timeS}</span>`;

            const fromSpan = document.createElement('span');
            fromSpan.className = from == '@server'? 'msg-from-server': 'msg-from';
            fromSpan.textContent = `[${from}]: `; // Corrected from "[${from}]: "

            const contentSpan = document.createElement('span');
            contentSpan.className = 'msg-content';
            contentSpan.textContent = content;

            div.appendChild(timeSpan);
            div.appendChild(fromSpan);
            div.appendChild(contentSpan);
            chat.appendChild(div);
            continue;
        }

        const div = document.createElement('div');
        div.className = 'message';
        div.textContent = line;
        chat.appendChild(div);
    }
    chat.scrollTop = chat.scrollHeight;
}

function updateUserList(usersString) {
    const users = usersString.split(',').map(u => u.trim()).filter(u => u.length > 0);
    userList.innerHTML = '';
    users.forEach(user => {
        const li = document.createElement('li');
        li.className = 'user-item';
        li.textContent = user;
        userList.appendChild(li);
    });
}

function requestUserList() {
    if (ws && ws.readyState === WebSocket.OPEN && loggedIn) {
        backgroundRequestsPending++;
        ws.send('/users');
    }
}

function connect() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    ws = new WebSocket(`${protocol}//${window.location.host}/ws`);

    ws.onopen = () => {
        addMessage("Connected to server.");
    };

    ws.onmessage = (event) => {
        addMessage(event.data);
    };

    ws.onclose = () => {
        loggedIn = false;
        if (fetchUsersInterval) clearInterval(fetchUsersInterval);
        addMessage("Disconnected. Reconnecting in 3s...");
        setTimeout(connect, 3000);
    };

    ws.onerror = (err) => {
        console.error("WS Error", err);
    };
}

form.addEventListener('submit', (e) => {
    e.preventDefault();
    if (input.value && ws && ws.readyState === WebSocket.OPEN) {
        if (input.value == "/clear") {
            chat.innerHTML = ""
            input.value = "";
            return
        }
        if (!loggedIn) {
            username = input.value;
            loggedIn = true;
            // Start periodic user list updates after login
            fetchUsersInterval = setInterval(requestUserList, 5000);
            setTimeout(requestUserList, 500); // Initial fetch
        }
        ws.send(input.value);
        input.value = '';
    }
});
input.addEventListener("focus", () => {
    setTimeout(() => {
        input.scrollIntoView({
            behavior: "smooth",
            block: "center"
        });
    }, 300); // delay lets the keyboard open first
});

connect();
