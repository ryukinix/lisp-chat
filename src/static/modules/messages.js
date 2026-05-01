import config from './config.js';
import utils from './utils.js';
import formatting from './formatting.js';
import notifications from './notifications.js';
import users from './users.js';
import auth from './auth.js';
import network from './network.js';

const chat = document.getElementById("chat");

const messageCache = new Set();
const messageHistory = [];

function clearMessages() {
    chat.innerHTML = "";
    messageCache.clear();
    messageHistory.length = 0;
}

function checkChatIsAtBottom() {
    const scrollThreshold = 100;
    return (chat.scrollHeight - chat.scrollTop - chat.clientHeight) <= scrollThreshold;
}

function processServerMessage(content, isRealTime) {
    const isJoin = content.includes("joined to the party");
    const isExit = content.includes("exited from the party");
    const isNickChange = content.includes("Your new nick is");
    const isSessionId = content.includes("Your session ID is:");
    const isSystemMessage = isJoin || isExit || isNickChange || isSessionId;
    const isUsersListResponse = content.startsWith("users: ");

    if (isSessionId) {
        const sessionIdMatch = content.match(/Your session ID is: (.*)/);
        if (sessionIdMatch) {
            auth.setSessionId(sessionIdMatch[1].trim());
            return false;
        }
    }

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
        if (network.getUserRequestsPending() > 0) {
            network.resetUserRequestsPending();
            return true;
        }
        return false;
    }
    return true;
}

function isMessageCached(line, from, doNotCache = false) {
    if (messageCache.has(line)) return true;

    if (!doNotCache && from != "@server") {
        messageCache.add(line);
        messageHistory.push(line);

        if (messageHistory.length > config.MAX_CACHE_SIZE) {
            const old = messageHistory.shift();
            messageCache.delete(old);
        }
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

    const timeLink = document.createElement("a");
    timeLink.className = "timestamp";
    timeLink.innerHTML = `${timeHM}<span class="timestamp-seconds">:${timeS}</span>`;
    timeLink.style.cursor = "pointer";
    timeLink.style.textDecoration = "none";
    timeLink.style.color = "inherit";
    timeLink.title = "Click to reply / Right click to copy link";
    
    let channel = window.location.search.substring(1).split('&')[0];
    if (!channel || channel.includes('=')) {
        channel = "general";
    }
    const reference = `<#${channel.replace('#', '')}: ${date} ${timeHM}:${timeS} [${from}]>`;
    const url = new URL(window.location.origin + window.location.pathname);
    url.searchParams.set('channel', channel.replace('#', ''));
    url.searchParams.set('message_ref', reference);
    timeLink.href = url.toString();

    timeLink.addEventListener("click", (e) => {
        e.preventDefault();
        const msgInput = document.getElementById("message-input");
        
        // Remove existing reference if any
        let currentText = msgInput.value.replace(/<[^>]+>\s*/g, '');
        msgInput.value = `${reference} ${currentText}`;
        msgInput.focus();
    });

    const fromSpan = document.createElement("span");
    fromSpan.textContent = `[${from}]: `;
    fromSpan.style.color = utils.getUserColor(from);

    const contentSpan = document.createElement("span");
    contentSpan.className = "msg-content";
    contentSpan.dataset.rawContent = content;
    contentSpan.innerHTML = formatting.formatMessage(content);

    div.appendChild(timeLink);
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

function insertMessageNode(div, anchor) {
    chat.insertBefore(div, anchor);
    ensureDateDivider(div);
}

function processStructuredMessage(line, match, anchor, prepend) {
    const [_, date, timeHM, timeS, from, content] = match;
    const effectiveDate = date || utils.getTodayDate();
    const normalizedLine = `|${effectiveDate} ${timeHM}:${timeS}| [${from}]: ${content}`;

    if (isMessageCached(normalizedLine, from, prepend)) return;

    if (from === "@server") {
        const shouldRender = processServerMessage(content, !date);
        if (!shouldRender) return;
    }

    const existingMsgs = chat.querySelectorAll(`.message[data-date="${effectiveDate}"][data-time-hm="${timeHM}"][data-time-s="${timeS}"][data-from="${from}"]`);
    for (const msg of existingMsgs) {
        const contentSpan = msg.querySelector(".msg-content");
        if (contentSpan) {
            const rawContent = contentSpan.dataset.rawContent;
            if (rawContent && rawContent.includes(content)) return;
            if (contentSpan.textContent.includes(content)) return;
        }
    }

    const lastMsg = anchor ? anchor.previousElementSibling : chat.lastElementChild;
    if (lastMsg && lastMsg.classList.contains("message") &&
        lastMsg.dataset.date === effectiveDate &&
        lastMsg.dataset.timeHm === timeHM &&
        lastMsg.dataset.timeS === timeS &&
        lastMsg.dataset.from === from) {

        const contentSpan = lastMsg.querySelector(".msg-content");
        if (contentSpan.dataset.rawContent !== undefined) {
            contentSpan.dataset.rawContent += "\\n" + content;
        }
        contentSpan.appendChild(document.createElement("br"));
        const newContent = document.createElement("span");
        newContent.innerHTML = formatting.formatMessage(content);
        while(newContent.firstChild) {
            contentSpan.appendChild(newContent.firstChild);
        }
        return;
    }

    const div = createMessageElement(effectiveDate, timeHM, timeS, from, content);

    insertMessageNode(div, anchor);

    // Check if this message is the one referenced in the URL
    const urlParams = new URLSearchParams(window.location.search);
    const messageRef = urlParams.get('message_ref');
    if (messageRef) {
        const refMatch = messageRef.match(/<(#?[A-zÀ-ú0-9_\-]+):\s*(\d{4}-\d{2}-\d{2})\s*(\d{2}:\d{2}):(\d{2})\s*\[(.*?)\]>/);
        if (refMatch) {
            const [_, refChannel, refDate, refTimeHM, refTimeS, refFrom] = refMatch;
            if (effectiveDate === refDate && timeHM === refTimeHM && timeS === refTimeS && from === refFrom) {
                div.classList.add('shared-focus');
                setTimeout(() => div.scrollIntoView({ behavior: 'smooth', block: 'center' }), 100);
                
                // Remove focus on interaction
                const removeFocus = () => {
                    div.classList.remove('shared-focus');
                    chat.removeEventListener('scroll', removeFocus);
                    chat.removeEventListener('click', removeFocus);
                    const msgInput = document.getElementById('message-input');
                    if (msgInput) {
                        msgInput.removeEventListener('input', removeFocus);
                        msgInput.removeEventListener('keydown', removeFocus);
                    }
                };

                // Add listeners after a short delay so the initial scroll doesn't trigger it
                setTimeout(() => {
                    chat.addEventListener('scroll', removeFocus, { once: true });
                    chat.addEventListener('click', removeFocus, { once: true });
                    const msgInput = document.getElementById('message-input');
                    if (msgInput) {
                        msgInput.addEventListener('input', removeFocus, { once: true });
                        msgInput.addEventListener('keydown', removeFocus, { once: true });
                    }
                }, 1000);

                // Clear message_ref from URL after first match to avoid re-triggering if new messages arrive
                const newUrl = new URL(window.location);
                newUrl.searchParams.delete('message_ref');
                window.history.replaceState({}, '', newUrl);
            }
        }
    }
}

function addRawMessage(line, anchor) {
    const div = document.createElement("div");
    div.className = "message";
    div.dataset.date = utils.getTodayDate();
    
    const contentSpan = document.createElement("span");
    contentSpan.className = "msg-content";
    contentSpan.dataset.rawContent = line;
    contentSpan.innerHTML = formatting.formatMessage(line);
    
    div.appendChild(contentSpan);
    insertMessageNode(div, anchor);
}

function addMessage(text, prepend = false) {
    const isAtBottom = checkChatIsAtBottom();
    const linesArray = text.split(/\r?\n/);
    let anchor = null;
    if (prepend) {
        anchor = chat.firstElementChild;
        while (anchor && !anchor.classList.contains("message")) {
            anchor = anchor.nextElementSibling;
        }
    }
    let previousScrollHeight = chat.scrollHeight;

    for (const line of linesArray) {
        if (!line.trim()) continue;
        if (auth.handleAuthHandshake(line)) continue;

        const match = line.match(/^\|(?:(\d{4}-\d{2}-\d{2}) )?(\d{2}:\d{2}):(\d{2})\| \[(.*?)\]: (.*)$/);
        if (match) {
            processStructuredMessage(line, match, anchor, prepend);
        } else {
            addRawMessage(line, anchor);
        }
    }

    if (prepend && chat.scrollHeight > previousScrollHeight) {
        chat.scrollTop += chat.scrollHeight - previousScrollHeight;
    } else if (isAtBottom && !prepend) {
        chat.scrollTop = chat.scrollHeight;
    }
}

export default { chat, clearMessages, checkChatIsAtBottom, addMessage };
