import * as notifications from './notifications.js';
import * as auth from './auth.js';

export let ws;
export let fetchUsersInterval;
export let userRequestsPending = 0;

export const keepAliveWorker = new Worker(URL.createObjectURL(new Blob([`
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

export function setFetchUsersInterval(value) {
    fetchUsersInterval = value;
}

export function incrementUserRequestsPending() {
    userRequestsPending++;
}

export function resetUserRequestsPending() {
    userRequestsPending = 0;
}

export function requestUserList(isBackground = false) {
    if (ws && ws.readyState === WebSocket.OPEN && auth.loggedIn) {
        ws.send("/users");
    }
}

keepAliveWorker.onmessage = () => requestUserList(true);

export function connect(onMessageCallback) {
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
        notifications.showNotification("Connected to server.");
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
        notifications.showNotification("Disconnected. Reconnecting in 3s...");
        setTimeout(() => connect(onMessageCallback), 3000);
    };

    ws.onerror = (err) => {
        console.error("WS Error", err);
    };
}
