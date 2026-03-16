import config from './modules/config.js';
import auth from './modules/auth.js';
import network from './modules/network.js';
import autocomplete from './modules/autocomplete.js';
import messages from './modules/messages.js';

const form = document.getElementById("input-area");
const input = document.getElementById("message-input");

form.addEventListener("submit", (e) => {
    e.preventDefault();
    const value = input.value;
    const trimmed = value.trim();
    if (value && network.getWs() && network.getWs().readyState === WebSocket.OPEN) {
        autocomplete.closeAutocomplete();
        if (trimmed === "/clear") {
            messages.clearMessages();
            input.value = "";
            return;
        }
        if (!auth.getLoggedIn()) {
            auth.setUsername(trimmed);
            auth.setLoggedIn(true);
            auth.updateUsernamePrefix();
            auth.hideLoginPanel();
            network.getWs().send(value);
            network.getWs().send(`/log :depth ${config.LOG_HISTORY_SIZE} :date-format date`);
            input.value = "";
            network.keepAliveWorker.postMessage('start');
            network.setFetchUsersInterval(true);
            setTimeout(() => network.requestUserList(true), 500);
            return;
        }
        const firstWord = trimmed.split(/\s+/)[0];
        if (firstWord === "/users") {
            network.incrementUserRequestsPending();
        }
        network.getWs().send(value);
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
        const wasAtBottom = messages.checkChatIsAtBottom();
        const isShrinking = window.visualViewport.height < lastViewportHeight;

        document.body.style.height = `${window.visualViewport.height}px`;
        document.body.style.top = `${window.visualViewport.offsetTop}px`;
        document.body.style.left = `${window.visualViewport.offsetLeft}px`;
        document.body.style.width = `${window.visualViewport.width}px`;

        if (wasAtBottom || (isShrinking && document.activeElement === input)) {
            messages.chat.scrollTop = messages.chat.scrollHeight;
        }

        lastViewportHeight = window.visualViewport.height;
    };
    window.visualViewport.addEventListener('resize', updateViewport);
    window.visualViewport.addEventListener('scroll', updateViewport);
    updateViewport();
}

input.addEventListener("focus", () => {
    setTimeout(() => {
        messages.chat.scrollTop = messages.chat.scrollHeight;
    }, 300);
});

if (window.innerWidth > config.DESKTOP_MIN_WIDTH) {
    input.focus();
}

autocomplete.initAutocomplete(input);
network.connect(messages.addMessage);
