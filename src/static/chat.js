const chat = document.getElementById("chat");
const form = document.getElementById("input-area");
const input = document.getElementById("message-input");
const userList = document.getElementById("user-list");

let ws;
let loggedIn = false;
let username = "";
let fetchUsersInterval;
let backgroundRequestsPending = 0;

const keepAliveWorker = new Worker(URL.createObjectURL(new Blob([`
    let interval;
    self.onmessage = e => {
        if (e.data === 'start') {
            if (!interval) interval = setInterval(() => self.postMessage('tick'), 5000);
        } else if (e.data === 'stop') {
            clearInterval(interval);
            interval = null;
        }
    };
`], {type: 'application/javascript'})));

keepAliveWorker.onmessage = () => requestUserList();

const availableColors = [
    "#ff7675", "#fab1a0", "#fdcb6e", "#e17055", "#d63031",
    "#00b894", "#00cec9", "#0984e3", "#6c5ce7", "#e84393",
    "#ffeaa7", "#55efc4", "#81ecec", "#74b9ff", "#a29bfe"
];

function getUserColor(name) {
    if (name === "@server") return "#bb2222";
    let hash = 0;
    const hashShift = 12;
    for (let i = 0; i < name.length; i++) {
        hash = name.charCodeAt(i) + ((hash << hashShift) - hash);
    }
    const index = Math.abs(hash) % availableColors.length;
    return availableColors[index];
}

function updateUsernamePrefix() {
    let prefix = document.getElementById("username-prefix");
    if (!prefix) {
        prefix = document.createElement("span");
        prefix.id = "username-prefix";
        form.insertBefore(prefix, input);
    }
    if (username && loggedIn) {
        prefix.textContent = `[${username}]: `;
        prefix.style.color = getUserColor(username);
        prefix.style.fontWeight = "bold";
        prefix.style.marginRight = "5px";
        input.placeholder = "";
    } else {
        prefix.textContent = "";
    }
}

function addMessage(text) {
    const linesArray = text.split("\n");
    for (const line of linesArray) {
        if (line === "> Type your username: " && username) {
            ws.send(username);
            loggedIn = true;
            updateUsernamePrefix();
            // Restart periodic updates if not already running
            if (!fetchUsersInterval) {
                keepAliveWorker.postMessage('start');
                fetchUsersInterval = true;
                setTimeout(requestUserList, 500);
            }
            continue;
        }

        // Check if it's a response from @server

        // Format: |HH:MM:SS| [from]: content

        const match = line.match(/^\|(\d{2}:\d{2}):(\d{2})\| \[(.*?)\]: (.*)$/);

        if (match) {
            const [_, timeHM, timeS, from, content] = match;
            if (from === "@server") {
                const isSystemMessage = content.includes("joined to the party") ||
                    content.includes("exited from the party") ||
                    content.includes("Your new nick is");
                const isUsersListResponse = content.startsWith("users: ")

                if (content.includes("Your new nick is")) {
                    const nickMatch = content.match(/Your new nick is: (.*)/);
                    if (nickMatch) {
                        username = nickMatch[1].trim();
                        updateUsernamePrefix();
                    }
                }

                if (isSystemMessage) {
                    requestUserList(); // Refresh sidebar on join/part/nick
                } else if (isUsersListResponse) {
                    // This is likely the response to /users
                    updateUserList(content);
                    if (backgroundRequestsPending > 0) {
                        backgroundRequestsPending--;
                        continue; // Swallow background updates

                    }

                }

            }

            const div = document.createElement("div");


            div.className = "message";

            const timeSpan = document.createElement("span");
            timeSpan.className = "timestamp";
            timeSpan.innerHTML = `${timeHM}<span class="timestamp-seconds">:${timeS}</span>`;

            const fromSpan = document.createElement("span");
            fromSpan.textContent = `[${from}]: `;
            fromSpan.style.color = getUserColor(from);

            const contentSpan = document.createElement("span");
            contentSpan.className = "msg-content";
            contentSpan.textContent = content;

            div.appendChild(timeSpan);
            div.appendChild(fromSpan);
            div.appendChild(contentSpan);
            chat.appendChild(div);
            continue;
        }

        const div = document.createElement("div");
        div.className = "message";
        div.textContent = line;
        chat.appendChild(div);
    }
    chat.scrollTop = chat.scrollHeight;
}

function updateUserList(usersString) {
    const users = usersString.replace("users: ", "").split(",").map(u => u.trim()).filter(u => u.length > 0);
    userList.innerHTML = "";
    users.forEach(user => {
        const li = document.createElement("li");
        li.className = "user-item";
        li.textContent = user;
        li.style.color = getUserColor(user);
        userList.appendChild(li);
    });
}

function requestUserList() {
    if (ws && ws.readyState === WebSocket.OPEN && loggedIn) {
        backgroundRequestsPending++;
        ws.send("/users");
    }
}

function connect() {
    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    ws = new WebSocket(`${protocol}//${window.location.host}/ws`);

    ws.onopen = () => {
        addMessage("Connected to server.");
    };

    ws.onmessage = (event) => {
        addMessage(event.data);
    };

    ws.onclose = () => {
        loggedIn = false;
        updateUsernamePrefix();
        if (fetchUsersInterval) {
            keepAliveWorker.postMessage('stop');
            fetchUsersInterval = null;
        }
        addMessage("Disconnected. Reconnecting in 3s...");
        setTimeout(connect, 3000);
    };

    ws.onerror = (err) => {
        console.error("WS Error", err);
    };
}

form.addEventListener("submit", (e) => {
    e.preventDefault();
    if (input.value && ws && ws.readyState === WebSocket.OPEN) {
        if (input.value == "/clear") {
            chat.innerHTML = ""
            input.value = "";
            return
        }
        if (!loggedIn) {
            username = input.value.trim();
            loggedIn = true;
            updateUsernamePrefix();
            // Start periodic user list updates after login
            keepAliveWorker.postMessage('start');
            fetchUsersInterval = true;
            setTimeout(requestUserList, 500); // Initial fetch
        }
        ws.send(input.value);
        input.value = "";
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
