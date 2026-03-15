import * as config from './config.js';
import * as utils from './utils.js';
import * as formatting from './formatting.js';
import * as notifications from './notifications.js';
import * as users from './users.js';
import * as auth from './auth.js';
import * as network from './network.js';

export const chat = document.getElementById("chat");

const messageCache = new Set();
const messageHistory = [];

export function clearMessages() {
    chat.innerHTML = "";
    messageCache.clear();
    messageHistory.length = 0;
}

export function checkChatIsAtBottom() {
    const scrollThreshold = 100;
    return (chat.scrollHeight - chat.scrollTop - chat.clientHeight) <= scrollThreshold;
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
            auth.setUsername(nickMatch[1].trim());
            auth.updateUsernamePrefix();
        }
    }

    if (isSystemMessage) {
        network.requestUserList(true);
        if ((isJoin || isExit) && isRealTime) {
            notifications.showNotification(content);
        }
        if (isJoin || isExit) return false;
    } else if (isUsersListResponse) {
        users.updateUserList(content);
        if (network.userRequestsPending > 0) {
            network.resetUserRequestsPending();
            return true;
        }
        return false;
    }
    return true;
}

function isMessageCached(line, from) {
    if (messageCache.has(line)) return true;

    if (from != "@server") {
        messageCache.add(line);
        messageHistory.push(line);
    }

    if (messageHistory.length > config.MAX_CACHE_SIZE) {
        const old = messageHistory.shift();
        messageCache.delete(old);
    }
    return false;
}

function createMessageElement(date, timeHM, timeS, from, content) {
    const div = document.createElement("div");
    div.className = "message";
    div.dataset.date = date;
    div.dataset.timeHm = timeHM;
    div.dataset.timeS = timeS;
    div.dataset.from = from;

    const timeSpan = document.createElement("span");
    timeSpan.className = "timestamp";
    timeSpan.innerHTML = `${timeHM}<span class="timestamp-seconds">:${timeS}</span>`;

    const fromSpan = document.createElement("span");
    fromSpan.textContent = `[${from}]: `;
    fromSpan.style.color = utils.getUserColor(from);

    const contentSpan = document.createElement("span");
    contentSpan.className = "msg-content";
    contentSpan.innerHTML = formatting.formatMessage(content);

    div.appendChild(timeSpan);
    div.appendChild(fromSpan);
    div.appendChild(contentSpan);

    return div;
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

    const prevDate = prevMsg ? prevMsg.dataset.date : null;
    if (date !== prevDate) {
        const divider = document.createElement("div");
        divider.className = "date-divider";
        divider.dataset.date = date;
        divider.textContent = ` ${date} `;
        el.parentElement.insertBefore(divider, el);
    }

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

function processStructuredMessage(line, match) {
    const [_, date, timeHM, timeS, from, content] = match;
    const effectiveDate = date || utils.getTodayDate();
    const normalizedLine = `|${effectiveDate} ${timeHM}:${timeS}| [${from}]: ${content}`;

    if (isMessageCached(normalizedLine, from)) return;

    if (from === "@server") {
        const shouldRender = processServerMessage(content, !date);
        if (!shouldRender) return;
    }

    const lastMsg = chat.lastElementChild;
    if (lastMsg && lastMsg.classList.contains("message") &&
        lastMsg.dataset.date === effectiveDate &&
        lastMsg.dataset.timeHm === timeHM &&
        lastMsg.dataset.timeS === timeS &&
        lastMsg.dataset.from === from) {

        const contentSpan = lastMsg.querySelector(".msg-content");
        contentSpan.appendChild(document.createElement("br"));
        const newContent = document.createElement("span");
        newContent.innerHTML = formatting.formatMessage(content);
        while(newContent.firstChild) {
            contentSpan.appendChild(newContent.firstChild);
        }
        return;
    }

    const div = createMessageElement(effectiveDate, timeHM, timeS, from, content);
    const seconds = utils.calculateSeconds(timeHM, timeS);

    insertMessageNode(div, from, content, seconds, !!date);
}

function addRawMessage(line) {
    const div = document.createElement("div");
    div.className = "message";
    div.dataset.date = utils.getTodayDate();
    div.innerHTML = formatting.formatMessage(line);
    chat.appendChild(div);
    ensureDateDivider(div);
}

export function addMessage(text) {
    const isAtBottom = checkChatIsAtBottom();
    const linesArray = text.split("\n");
    for (const line of linesArray) {
        if (auth.handleAuthHandshake(line)) continue;

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
