const chat = document.getElementById("chat");
const form = document.getElementById("input-area");
const input = document.getElementById("message-input");
const userList = document.getElementById("user-list");

function setCookie(name, value, days) {
    let expires = "";
    if (days) {
        const date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        expires = "; expires=" + date.toUTCString();
    }
    document.cookie = name + "=" + (value || "") + expires + "; path=/";
}

function getCookie(name) {
    const nameEQ = name + "=";
    const ca = document.cookie.split(';');
    for (let i = 0; i < ca.length; i++) {
        let c = ca[i];
        while (c.charAt(0) == ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
    }
    return null;
}

let ws;
let loggedIn = false;
let username = getCookie("username") || "";
let fetchUsersInterval;
let backgroundRequestsPending = 0;
const messageCache = new Set();
const messageHistory = [];
const MAX_CACHE_SIZE = 200;
const LOG_HISTORY_SIZE = 50;

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

function linkify(text) {
    const urlPattern = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/ig;
    return text.replace(urlPattern, '<a href="$1" target="_blank" rel="noopener noreferrer">$1</a>');
}

function updateUsernamePrefix() {
    let prefix = document.getElementById("username-prefix");
    if (!prefix) {
        prefix = document.createElement("span");
        prefix.id = "username-prefix";
        form.insertBefore(prefix, input);
    }
    if (username && loggedIn) {
        prefix.textContent = `[${username}]:`;
        prefix.style.color = getUserColor(username);
        prefix.style.fontWeight = "bold";
        input.placeholder = "";
    } else {
        prefix.textContent = "";
    }
}

let anchorElement = null;
let anchorSeconds = 0;

function addMessage(text) {
    // Smart Scroll: Check if user is near the bottom BEFORE adding content.
    // Use a generous threshold (e.g. 100px) to handle minor deviations.
    const scrollThreshold = 100;
    const isAtBottom = (chat.scrollHeight - chat.scrollTop - chat.clientHeight) <= scrollThreshold;

    const linesArray = text.split("\n");
    for (const line of linesArray) {
        if (line === "> Type your username: " && username) {
            ws.send(username);
            loggedIn = true;
            updateUsernamePrefix();
            // Fetch recent history to fill potential gaps from disconnection
            ws.send(`/log ${LOG_HISTORY_SIZE}`);
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
            // De-duplication: skip if we've already seen this exact message line
            const [_, timeHM, timeS, from, content] = match;
            if (messageCache.has(line)) continue;
            if (from != "@server") {
                messageCache.add(line);
                messageHistory.push(line);
            }
            if (messageHistory.length > MAX_CACHE_SIZE) {
                const old = messageHistory.shift();
                messageCache.delete(old);
            }

            if (from === "@server") {
                const isSystemMessage = content.includes("joined to the party") ||
                    content.includes("exited from the party") ||
                    content.includes("Your new nick is");
                const isUsersListResponse = content.startsWith("users: ")

                if (content.includes("Your new nick is")) {
                    const nickMatch = content.match(/Your new nick is: (.*)/);
                    if (nickMatch) {
                        username = nickMatch[1].trim();
                        setCookie("username", username, 30);
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
            contentSpan.innerHTML = linkify(content);

            div.appendChild(timeSpan);
            div.appendChild(fromSpan);
            div.appendChild(contentSpan);

            const [h, m] = timeHM.split(':').map(Number);
            const s = Number(timeS);
            const seconds = h * 3600 + m * 60 + s;

            // Anchor logic: "joined" message marks the start of the session.
            // Messages appearing to be older than the anchor (history) go before it.
            if (from === "@server" && content.includes(`"${username}" joined to the party`)) {
                anchorElement = div;
                anchorSeconds = seconds;
                chat.appendChild(div);
                continue;
            }

            if (anchorElement && anchorElement.isConnected) {
                // Check if message is chronologically "before" the anchor.
                // Handle day wrap: if msg is > 12h ahead of anchor, assume it's from yesterday.
                const isBefore = (seconds < anchorSeconds) || (seconds > anchorSeconds + 43200);

                if (isBefore) {
                    chat.insertBefore(div, anchorElement);
                    continue;
                }
            }

            chat.appendChild(div);
            continue;
        }

        const div = document.createElement("div");
        div.className = "message";
        div.innerHTML = linkify(line);
        chat.appendChild(div);
    }

    // Only auto-scroll if the user was already at the bottom
    if (isAtBottom) {
        chat.scrollTop = chat.scrollHeight;
    }
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
    if (ws) {
        ws.onclose = null;
        ws.onmessage = null;
        ws.onerror = null;
        ws.onopen = null;
        ws.close();
    }

    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    ws = new WebSocket(`${protocol}//${window.location.host}/ws`);

    ws.onopen = () => {
        addMessage("Connected to server.");
        anchorElement = null;
    };

    ws.onmessage = (event) => {
        addMessage(event.data);
    };

    ws.onclose = (event) => {
        if (ws && ws !== event.target) return;

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
            chat.innerHTML = "";
            messageCache.clear();
            messageHistory.length = 0;
            input.value = "";
            return;
        }
        if (!loggedIn) {
            username = input.value.trim();
            setCookie("username", username, 30);
            loggedIn = true;
            updateUsernamePrefix();
            ws.send(input.value); // sends username
            ws.send(`/log ${LOG_HISTORY_SIZE}`);
            input.value = "";
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
