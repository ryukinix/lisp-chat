import config from './modules/config.js';
import auth from './modules/auth.js';
import network from './modules/network.js';
import autocomplete from './modules/autocomplete.js';
import messages from './modules/messages.js';
import inputModule from './modules/input.js';
import notifications from './modules/notifications.js';
import history from './modules/history.js';
import reply from "./modules/reply.js";

// Utility to convert Base64-URL to Uint8Array for the browser API
function urlBase64ToUint8Array(base64String) {
    const padding = '='.repeat((4 - base64String.length % 4) % 4);
    const base64 = (base64String + padding).replace(/\-/g, '+').replace(/_/g, '/');
    const rawData = window.atob(base64);
    const outputArray = new Uint8Array(rawData.length);
    for (let i = 0; i < rawData.length; ++i) {
        outputArray[i] = rawData.charCodeAt(i);
    }
    return outputArray;
}

async function setupPushNotifications(username) {
    if ('serviceWorker' in navigator && 'PushManager' in window) {
        try {
            const swReg = await navigator.serviceWorker.register('./sw.js');
            console.log('Service Worker Registered!', swReg);
            
            // 1. Fetch public key from Lisp
            const vKeyReq = await fetch('/api/vapid-public-key');
            if (!vKeyReq.ok) return;
            const vKey = await vKeyReq.text();
            
            // 2. Subscribe Browser
            const subscription = await swReg.pushManager.subscribe({
                userVisibleOnly: true,
                applicationServerKey: urlBase64ToUint8Array(vKey.trim())
            });
            
            // 3. Send to Lisp Server
            await fetch(`/api/subscribe-push?user=${encodeURIComponent(username)}`, {
                method: 'POST',
                body: JSON.stringify(subscription),
                headers: { 'Content-Type': 'application/json' }
            });
            console.log("Web push subscription succeeded");
        } catch (err) {
            console.log('Web push setup failed: ', err);
        }
    }
}

// Attach a listener to auth module to handle push subscription when logged in
const originalPerformLogin = auth.performLogin;
auth.performLogin = function(username) {
    originalPerformLogin.call(auth, username);
    setupPushNotifications(username);
};

function updatePageTitle() {
    let channel = window.location.search.substring(1).split('&')[0];
    if (!channel || channel.includes('=')) {
        channel = "general";
    }
    const channelName = channel.replace(/^#/, '');
    document.title = `Lisp Chat Web: #${channelName}`;
}

const form = document.getElementById("input-area");
const input = document.getElementById("message-input");

inputModule.initInputHistory(input);
inputModule.setupInputOverlay(input);
reply.setupReplyFocus();

form.addEventListener("submit", (e) => {
    e.preventDefault();
    const value = input.value;
    const trimmed = value.trim();
    if (value && network.getWs() && network.getWs().readyState === WebSocket.OPEN) {
        autocomplete.closeAutocomplete();
        if (trimmed) {
            inputModule.addMessageToHistory(value);
        }
        if (trimmed === "/clear") {
            messages.clearMessages();
            history.resetReachedEnd();
            input.value = "";
            return;
        }
        if (!auth.getLoggedIn()) {
            auth.performLogin(trimmed);
            input.value = "";
            return;
        }
        const firstWord = trimmed.split(/\s+/)[0];
        if (firstWord === "/users") {
            network.incrementUserRequestsPending();
        }
        if (firstWord === "/join") {
            messages.clearMessages();
            history.resetReachedEnd();
            network.getWs().send(value);
            network.getWs().send(`/log :depth ${config.LOG_HISTORY_SIZE} :date-format date`);

            const parts = trimmed.split(/\s+/);
            if (parts.length > 1) {
                const newChannel = parts[1].replace(/^#/, '');
                const cleanPath = window.location.pathname.replace(/\/index\.html$/, '/');
                const url = new URL(window.location.origin + cleanPath);
                url.search = newChannel;
                window.history.pushState({}, '', url);
                updatePageTitle();
            }

            input.value = "";
            input.focus();
            return;
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

updatePageTitle();
history.initHistoryLoading();
autocomplete.initAutocomplete(input);
network.connect(messages.addMessage);

if ('serviceWorker' in navigator) {
    navigator.serviceWorker.addEventListener('message', event => {
        if (event.data && event.data.type === 'NAVIGATE_CHANNEL') {
            const newChannel = event.data.channel.replace(/^#/, '');
            const cleanPath = window.location.pathname.replace(/\/index\.html$/, '/');
            const url = new URL(window.location.origin + cleanPath);
            url.search = newChannel;
            window.history.pushState({}, '', url);
            updatePageTitle();

            messages.clearMessages();
            history.resetReachedEnd();
            if (network.getWs() && network.getWs().readyState === WebSocket.OPEN) {
                 network.getWs().send(`/join #${newChannel}`);
                 network.getWs().send(`/log :depth ${config.LOG_HISTORY_SIZE} :date-format date`);
            }
        }
    });
}
