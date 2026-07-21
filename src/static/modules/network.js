import notifications from './notifications.js';
import auth from './auth.js';
import settings from './settings.js';

let ws;
let fetchUsersInterval;
let userRequestsPending = 0;
let reconnectAttempts = 0;
let reconnectTimer = null;

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
    // Uses settings if a fixed timezone is configured, otherwise auto-detect
    const tzOffset = settings.getTimezoneOffset();
    
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
        if (onMessageCallback) onMessageCallback(event.data);
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

        if (!reconnectEnabled) {
            notifications.showNotification("Disconnected. Auto-reconnect is disabled.");
            return;
        }

        reconnectAttempts++;

        if (maxAttempts > 0 && reconnectAttempts > maxAttempts) {
            notifications.showNotification(`Disconnected. Max reconnection attempts (${maxAttempts}) reached.`);
            return;
        }

        notifications.showNotification(`Disconnected. Reconnecting in 3s... (attempt ${reconnectAttempts})`);
        reconnectTimer = setTimeout(() => connect(onMessageCallback), 3000);
    };

    ws.onerror = (err) => {
        console.error("WS Error", err);
    };
}

export default {
    getWs, setWs, getFetchUsersInterval, getUserRequestsPending,
    keepAliveWorker, setFetchUsersInterval, incrementUserRequestsPending,
    resetUserRequestsPending, requestUserList, connect
};
