import { LOG_HISTORY_SIZE, desktopMinWidth } from './config.js';
import {
    username, loggedIn, setLoggedIn, setUsername,
    updateUsernamePrefix, hideLoginPanel
} from './auth.js';
import {
    ws, connect, requestUserList, keepAliveWorker, setFetchUsersInterval,
    incrementUserRequestsPending
} from './network.js';
import { initAutocomplete, closeAutocomplete } from './autocomplete.js';
import { addMessage, clearMessages, checkChatIsAtBottom, chat } from './messages.js';

const form = document.getElementById("input-area");
const input = document.getElementById("message-input");

form.addEventListener("submit", (e) => {
    e.preventDefault();
    const value = input.value;
    const trimmed = value.trim();
    if (value && ws && ws.readyState === WebSocket.OPEN) {
        closeAutocomplete();
        if (trimmed === "/clear") {
            clearMessages();
            input.value = "";
            return;
        }
        if (!loggedIn) {
            setUsername(trimmed);
            setLoggedIn(true);
            updateUsernamePrefix();
            hideLoginPanel();
            ws.send(value);
            ws.send(`/log :depth ${LOG_HISTORY_SIZE} :date-format date`);
            input.value = "";
            keepAliveWorker.postMessage('start');
            setFetchUsersInterval(true);
            setTimeout(() => requestUserList(true), 500);
            return;
        }
        const firstWord = trimmed.split(/\s+/)[0];
        if (firstWord === "/users") {
            incrementUserRequestsPending();
        }
        ws.send(value);
        input.value = "";
        input.focus();
    }
});

const sendButton = form.querySelector("button");
if (sendButton) {
    sendButton.addEventListener("pointerdown", (e) => {
        if (input.value.trim().length > 0) {
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
    }, 300);
});

if (window.innerWidth > desktopMinWidth) {
    input.focus();
}

initAutocomplete(input);
connect(addMessage);
