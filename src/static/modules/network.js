import notifications from './notifications.js';
import auth from './auth.js';
import settings from './settings.js';

let ws;
let fetchUsersInterval;
let userRequestsPending = 0;
let reconnectAttempts = 0;
let reconnectTimer = null;
let savedOnMessageCallback = null;

function getWs() { return ws; }
function setWs(newWs) { ws = newWs; }
function getFetchUsersInterval() { return fetchUsersInterval; }
function getUserRequestsPending() { return userRequestsPending; }

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

function setFetchUsersInterval(value) {
    fetchUsersInterval = value;
}

function incrementUserRequestsPending() {
    userRequestsPending++;
}

function resetUserRequestsPending() {
    userRequestsPending = 0;
}

function requestUserList(isBackground = false) {
    if (ws && ws.readyState === WebSocket.OPEN && auth.getLoggedIn()) {
        ws.send("/users");
    }
}

keepAliveWorker.onmessage = () => requestUserList(true);

function connect(onMessageCallback) {
    if (onMessageCallback) savedOnMessageCallback = onMessageCallback;
    const cb = onMessageCallback || savedOnMessageCallback;

    if (ws) {
        ws.onclose = null;
        ws.onmessage = null;
        ws.onerror = null;
        ws.onopen = null;
        ws.close();
    }

    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    let wsUrl = `${protocol}//${window.location.host}/ws`;
    
    // Calculate the timezone offset in hours (e.g. UTC-3 is -3)
    // Auto-detect from browser locale
    const tzOffset = -(new Date().getTimezoneOffset() / 60);
    
    let channel = window.location.search.substring(1).split('&')[0];
    if (!channel || channel.includes('=')) {
        channel = '';
    }

    const wsParams = new URLSearchParams();
    if (channel) {
        wsParams.set('channel', channel);
    }
    wsParams.set('tz', tzOffset);
    wsParams.set('expand_reply', 'false');
    
    wsUrl += `?${wsParams.toString()}`;
    
    ws = new WebSocket(wsUrl);

    ws.onopen = () => {
        notifications.showNotification("Connected to server.");
        reconnectAttempts = 0;
        auth.setLoggedIn(false);
        auth.updateUsernamePrefix();
    };

    ws.onmessage = (event) => {
        if (cb) cb(event.data);
    };

    ws.onclose = (event) => {
        if (ws && ws !== event.target) return;

        auth.setLoggedIn(false);
        auth.updateUsernamePrefix();
        if (fetchUsersInterval) {
            keepAliveWorker.postMessage('stop');
            setFetchUsersInterval(null);
        }

        const reconnectEnabled = settings.get('reconnectEnabled');
        const maxAttempts = settings.get('maxReconnectAttempts');

        // When auto-reconnect is disabled, only reconnect if the page
        // is currently visible. If hidden, defer until it becomes visible.
        if (!reconnectEnabled) {
            if (document.visibilityState === 'visible') {
                notifications.showNotification("Disconnected. Reconnecting...");
                reconnectTimer = setTimeout(() => connect(), 3000);
            } else {
                notifications.showNotification("Disconnected. Will reconnect when page is active.");
            }
            return;
        }

        reconnectAttempts++;

        if (maxAttempts > 0 && reconnectAttempts > maxAttempts) {
            notifications.showNotification(`Disconnected. Max reconnection attempts (${maxAttempts}) reached.`);
            return;
        }

        notifications.showNotification(`Disconnected. Reconnecting in 3s... (attempt ${reconnectAttempts})`);
        reconnectTimer = setTimeout(() => connect(), 3000);
    };

    ws.onerror = (err) => {
        console.error("WS Error", err);
    };
}

// Reconnect when page becomes visible if auto-reconnect is disabled
// and the socket is currently disconnected
document.addEventListener('visibilitychange', () => {
    if (document.visibilityState !== 'visible') return;

    // If auto-reconnect is enabled, the normal onclose handler already
    // handles reconnection. Only intervene when it's disabled.
    if (settings.get('reconnectEnabled')) return;

    // Clear any pending reconnect timer (we're reconnecting now)
    if (reconnectTimer) {
        clearTimeout(reconnectTimer);
        reconnectTimer = null;
    }

    if (!ws || ws.readyState === WebSocket.CLOSED) {
        notifications.showNotification("Page active. Reconnecting...");
        reconnectAttempts = 0;
        connect();
    }
});

export default {
    getWs, setWs, getFetchUsersInterval, getUserRequestsPending,
    keepAliveWorker, setFetchUsersInterval, incrementUserRequestsPending,
    resetUserRequestsPending, requestUserList, connect
};
