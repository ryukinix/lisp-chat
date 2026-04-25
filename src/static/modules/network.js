import notifications from './notifications.js';
import auth from './auth.js';

let ws;
let fetchUsersInterval;
let userRequestsPending = 0;

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
    const search = window.location.search.substring(1);
    let wsUrl = `${protocol}//${window.location.host}/ws`;
    
    // Calculate the timezone offset in hours (e.g. UTC-3 is -3)
    const tzOffset = -(new Date().getTimezoneOffset() / 60);
    
    if (search) {
        if (search.includes("=")) {
            wsUrl += `?${search}&tz=${tzOffset}`;
        } else {
            wsUrl += `?channel=${search}&tz=${tzOffset}`;
        }
    } else {
        wsUrl += `?tz=${tzOffset}`;
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

export default {
    getWs, setWs, getFetchUsersInterval, getUserRequestsPending,
    keepAliveWorker, setFetchUsersInterval, incrementUserRequestsPending,
    resetUserRequestsPending, requestUserList, connect
};
