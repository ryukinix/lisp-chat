const chat = document.getElementById("chat");
const form = document.getElementById("input-area");
const input = document.getElementById("message-input");
const userList = document.getElementById("user-list");
const LOG_HISTORY_SIZE = 100;
const MAX_CACHE_SIZE = 200;
const messageCache = new Set();
const messageHistory = [];
const availableColors = [
    "#ff7675", "#fab1a0", "#fdcb6e", "#e17055", "#d63031",
    "#00b894", "#00cec9", "#0984e3", "#6c5ce7", "#e84393",
    "#ffeaa7", "#55efc4", "#81ecec", "#74b9ff", "#a29bfe"
];

let backgroundRequestsPending = 0;
let fetchUsersInterval;
let loggedIn = false;
let ws;


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
let username = getCookie("username") || "";


const keepAliveWorker = new Worker(URL.createObjectURL(new Blob([`
    let interval;
    self.onmessage = e => {
        if (e.data === 'start') {
            if (!interval) interval = setInterval(() => self.postMessage('tick'), 30000);
        } else if (e.data === 'stop') {
            clearInterval(interval);
            interval = null;
        }
    };
`], {type: 'application/javascript'})));

keepAliveWorker.onmessage = () => requestUserList(true);



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

function escapeHTML(text) {
    return text
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

function formatMessage(text) {
    if (!text) return "";

    const urls = [];
    const urlPattern = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;\*]*[-A-Z0-9+&@#\/%=~_|])/ig;

    // 1. Protect URLs
    let processed = text.replace(urlPattern, (match) => {
        const id = urls.length;
        urls.push(match);
        return `URLPLACEHOLDER${id}URL`;
    });

    // 2. Escape HTML
    processed = escapeHTML(processed);

    // 3. Markdown (on escaped text)
    // Code first to avoid processing inside code tags
    processed = processed.replace(/`(.*?)`/g, '<code>$1</code>');
    // Bold
    processed = processed.replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>');
    processed = processed.replace(/__(.*?)__/g, '<strong>$1</strong>');
    // Italic
    processed = processed.replace(/\*(.*?)\*/g, '<em>$1</em>');
    processed = processed.replace(/_(.*?)_/g, '<em>$1</em>');
    // Strikethrough
    processed = processed.replace(/~~(.*?)~~/g, '<del>$1</del>');

    // 4. Mentions
    processed = processed.replace(/(^|\s)@([A-zÀ-ú0-9_\-]+)/g, (match, prefix, user) => {
        const color = getUserColor(user);
        return `${prefix}<span style="color: ${color}">@${user}</span>`;
    });

    // 5. Restore URLs
    return processed.replace(/URLPLACEHOLDER(\d+)URL/g, (match, id) => {
        const url = urls[parseInt(id)];
        const escapedUrl = escapeHTML(url);
        return `<a href="${escapedUrl}" target="_blank" rel="noopener noreferrer">${escapedUrl}</a>`;
    });
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
        input.disabled = false;
    } else {
        prefix.textContent = "";
        if (username) {
            input.placeholder = "Connecting...";
            input.disabled = true;
        } else {
            input.placeholder = "Type your username...";
            input.disabled = false;
        }
    }
}

function showNotification(text) {
    const container = document.getElementById("notifications");
    if (!container) return;

    const notification = document.createElement("div");
    notification.className = "notification";
    notification.innerHTML = formatMessage(text);
    notification.onclick = () => notification.remove();

    container.prepend(notification);

    setTimeout(() => {
        notification.remove();
    }, 8000);
}

function addMessage(text) {
    const isAtBottom = checkChatIsAtBottom();
    const linesArray = text.split("\n");
    for (const line of linesArray) {
        if (handleAuthHandshake(line)) continue;

        const match = line.match(/^\|(?:(\d{4}-\d{2}-\d{2}) )?(\d{2}:\d{2}):(\d{2})\| \[(.*?)\]: (.*)$/);
        if (match) {
            processStructuredMessage(line, match);
        } else {
            addRawMessage(line);
        }
    }

    if (isAtBottom) {
        chat.scrollTop = chat.scrollHeight;
    }
}

function checkChatIsAtBottom() {
    const scrollThreshold = 100;
    return (chat.scrollHeight - chat.scrollTop - chat.clientHeight) <= scrollThreshold;
}

function handleAuthHandshake(line) {
    if (line === "> Type your username: ") {
        if (username) {
            ws.send(username);
            loggedIn = true;
            updateUsernamePrefix();
            // Fetch recent history to fill potential gaps from disconnection
            ws.send(`/log :depth ${LOG_HISTORY_SIZE} :date-format date`);
            // Restart periodic updates if not already running
            if (!fetchUsersInterval) {
                keepAliveWorker.postMessage('start');
                fetchUsersInterval = true;
                setTimeout(() => requestUserList(true), 500);
            }
        } else {
            loggedIn = false;
            updateUsernamePrefix();
        }
        return true;
    }
    if (line === "> Name cannot be empty. Try again: ") {
        loggedIn = false;
        updateUsernamePrefix();
        input.placeholder = "Name cannot be empty. Try your username: ";
        return true;
    }
    return false;
}

function getTodayDate() {
    // Options for formatting the date to get YYYY-MM-DD
    const options = {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        timeZone: 'America/Sao_Paulo' // A common city in GMT-3
    };

    const today = new Date();
    // Format the date parts and rearrange them to YYYY-MM-DD
    const parts = today.toLocaleDateString('en-CA', options).split('/');
    // Note: 'en-CA' locale conveniently provides YYYY-MM-DD format
    return parts.join('-');
}

function processStructuredMessage(line, match) {
    const [_, date, timeHM, timeS, from, content] = match;
    const effectiveDate = date || getTodayDate();
    const normalizedLine = `|${effectiveDate} ${timeHM}:${timeS}| [${from}]: ${content}`;

    if (isMessageCached(normalizedLine, from)) return;

    if (from === "@server") {
        const shouldRender = processServerMessage(content, !date);
        if (!shouldRender) return;
    }

    const div = createMessageElement(effectiveDate, timeHM, timeS, from, content);
    const seconds = calculateSeconds(timeHM, timeS);

    insertMessageNode(div, from, content, seconds, !!date);
}

function isMessageCached(line, from) {
    if (messageCache.has(line)) return true;

    if (from != "@server") {
        messageCache.add(line);
        messageHistory.push(line);
    }

    if (messageHistory.length > MAX_CACHE_SIZE) {
        const old = messageHistory.shift();
        messageCache.delete(old);
    }
    return false;
}

function processServerMessage(content, isRealTime) {
    const isJoin = content.includes("joined to the party");
    const isExit = content.includes("exited from the party");
    const isNickChange = content.includes("Your new nick is");
    const isSystemMessage = isJoin || isExit || isNickChange;
    const isUsersListResponse = content.startsWith("users: ");

    if (isNickChange) {
        const nickMatch = content.match(/Your new nick is: @(.*)/);
        if (nickMatch) {
            username = nickMatch[1].trim();
            setCookie("username", username, 30);
            updateUsernamePrefix();
        }
    }

    if (isSystemMessage) {
        requestUserList(true);
        if ((isJoin || isExit) && isRealTime) {
            showNotification(content);
        }
        if (isJoin || isExit) return false;
    } else if (isUsersListResponse) {
        updateUserList(content);
        if (backgroundRequestsPending > 0) {
            backgroundRequestsPending--;
            return false; // Swallow background updates
        }
    }
    return true;
}

function createMessageElement(date, timeHM, timeS, from, content) {
    const div = document.createElement("div");
    div.className = "message";
    div.dataset.date = date;

    const timeSpan = document.createElement("span");
    timeSpan.className = "timestamp";
    timeSpan.innerHTML = `${timeHM}<span class="timestamp-seconds">:${timeS}</span>`;

    const fromSpan = document.createElement("span");
    fromSpan.textContent = `[${from}]: `;
    fromSpan.style.color = getUserColor(from);

    const contentSpan = document.createElement("span");
    contentSpan.className = "msg-content";
    contentSpan.innerHTML = formatMessage(content);

    div.appendChild(timeSpan);
    div.appendChild(fromSpan);
    div.appendChild(contentSpan);

    return div;
}

function calculateSeconds(timeHM, timeS) {
    const [h, m] = timeHM.split(':').map(Number);
    const s = Number(timeS);
    return h * 3600 + m * 60 + s;
}

function ensureDateDivider(el) {
    const date = el.dataset.date;
    if (!date) return;

    const getAdjacentMessage = (node, direction) => {
        let adj = direction === "prev" ? node.previousElementSibling : node.nextElementSibling;
        while (adj && !adj.classList.contains("message")) {
            adj = direction === "prev" ? adj.previousElementSibling : adj.nextElementSibling;
        }
        return adj;
    };

    const prevMsg = getAdjacentMessage(el, "prev");
    const nextMsg = getAdjacentMessage(el, "next");

    // Clean up all existing dividers between el and its neighboring messages
    let node = el.previousElementSibling;
    while (node && node !== prevMsg && node.classList.contains("date-divider")) {
        let toRemove = node;
        node = node.previousElementSibling;
        toRemove.remove();
    }
    node = el.nextElementSibling;
    while (node && node !== nextMsg && node.classList.contains("date-divider")) {
        let toRemove = node;
        node = node.nextElementSibling;
        toRemove.remove();
    }

    // Ensure divider before el if it's the first message of the day
    const prevDate = prevMsg ? prevMsg.dataset.date : null;
    if (date !== prevDate) {
        const divider = document.createElement("div");
        divider.className = "date-divider";
        divider.dataset.date = date;
        divider.textContent = ` ${date} `;
        el.parentElement.insertBefore(divider, el);
    }

    // Ensure divider before next message if it starts a new day relative to el
    if (nextMsg) {
        const nextDate = nextMsg.dataset.date;
        if (nextDate !== date) {
            const nextDivider = document.createElement("div");
            nextDivider.className = "date-divider";
            nextDivider.dataset.date = nextDate;
            nextDivider.textContent = ` ${nextDate} `;
            nextMsg.parentElement.insertBefore(nextDivider, nextMsg);
        }
    }
}

function insertMessageNode(div, from, content, seconds, hasDate) {
    chat.appendChild(div);
    ensureDateDivider(div);
}

function addRawMessage(line) {
    const div = document.createElement("div");
    div.className = "message";
    div.dataset.date = getTodayDate();
    div.innerHTML = formatMessage(line);
    chat.appendChild(div);
    ensureDateDivider(div);
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

function requestUserList(isBackground = false) {
    if (ws && ws.readyState === WebSocket.OPEN && loggedIn) {
        if (isBackground) {
            backgroundRequestsPending++;
        }
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
    const search = window.location.search.substring(1);
    let wsUrl = `${protocol}//${window.location.host}/ws`;
    if (search) {
        if (search.includes("=")) {
            wsUrl += `?${search}`;
        } else {
            wsUrl += `?channel=${search}`;
        }
    }
    ws = new WebSocket(wsUrl);

    ws.onopen = () => {
        showNotification("Connected to server.");
        loggedIn = false;
        updateUsernamePrefix();
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
        showNotification("Disconnected. Reconnecting in 3s...");
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
            ws.send(`/log :depth ${LOG_HISTORY_SIZE} :date-format date`);
            input.value = "";
            // Start periodic user list updates after login
            keepAliveWorker.postMessage('start');
            fetchUsersInterval = true;
            setTimeout(() => requestUserList(true), 500); // Initial fetch
        }
        ws.send(input.value);
        input.value = "";
        input.focus();
    }
});

const sendButton = form.querySelector("button");
if (sendButton) {
    sendButton.addEventListener("pointerdown", (e) => {
        if (input.value.trim().length > 0) {
            // Prevent button from taking focus, keeping keyboard open
            e.preventDefault();
            form.requestSubmit();
        }
    });
}
let lastViewportHeight = window.visualViewport ? window.visualViewport.height : window.innerHeight;

if (window.visualViewport) {
    const updateViewport = () => {
        const wasAtBottom = checkChatIsAtBottom();
        const isShrinking = window.visualViewport.height < lastViewportHeight;

        document.body.style.height = `${window.visualViewport.height}px`;
        document.body.style.top = `${window.visualViewport.offsetTop}px`;
        document.body.style.left = `${window.visualViewport.offsetLeft}px`;
        document.body.style.width = `${window.visualViewport.width}px`;

        if (wasAtBottom || (isShrinking && document.activeElement === input)) {
            chat.scrollTop = chat.scrollHeight;
        }

        lastViewportHeight = window.visualViewport.height;
    };
    window.visualViewport.addEventListener('resize', updateViewport);
    window.visualViewport.addEventListener('scroll', updateViewport);
    updateViewport();
}

input.addEventListener("focus", () => {
    setTimeout(() => {
        chat.scrollTop = chat.scrollHeight;
    }, 300); // delay lets the keyboard open first
});

connect();
