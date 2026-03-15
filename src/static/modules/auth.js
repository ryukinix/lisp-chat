import * as utils from './utils.js';
import * as config from './config.js';
import * as network from './network.js';

export let username = utils.getCookie("username") || "";
export let loggedIn = false;

export function setLoggedIn(value) {
    loggedIn = value;
}

export function setUsername(value) {
    username = value;
    utils.setCookie("username", username, 30);
}

export function updateUsernamePrefix() {
    const form = document.getElementById("input-area");
    const input = document.getElementById("message-input");
    if (!form || !input) return;

    let prefix = document.getElementById("username-prefix");
    if (!prefix) {
        prefix = document.createElement("span");
        prefix.id = "username-prefix";
        form.insertBefore(prefix, input);
    }

    if (username && loggedIn) {
        prefix.textContent = `[${username}]:`;
        prefix.style.color = utils.getUserColor(username);
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

    if (!input.disabled && window.innerWidth > config.DESKTOP_MIN_WIDTH) {
        input.focus();
    }
}

export function showLoginPanel() {
    if (document.getElementById("login-panel")) return;
    const main = document.getElementById("main");
    if (!main) return;
    const panel = document.createElement("div");
    panel.id = "login-panel";
    panel.innerHTML = `
        <h2>Login</h2>
        <p>Set a username</p>
    `;
    main.appendChild(panel);
}

export function hideLoginPanel() {
    const panel = document.getElementById("login-panel");
    if (panel) panel.remove();
}

export function handleAuthHandshake(line) {
    const input = document.getElementById("message-input");
    if (line === "> Type your username: ") {
        if (username) {
            network.ws.send(username);
            setLoggedIn(true);
            updateUsernamePrefix();
            network.ws.send(`/log :depth ${config.LOG_HISTORY_SIZE} :date-format date`);
            if (!network.fetchUsersInterval) {
                network.keepAliveWorker.postMessage('start');
                network.setFetchUsersInterval(true);
                setTimeout(() => network.requestUserList(true), 500);
            }
        } else {
            setLoggedIn(false);
            updateUsernamePrefix();
            showLoginPanel();
        }
        return true;
    }
    if (line === "> Name cannot be empty. Try again: ") {
        setLoggedIn(false);
        updateUsernamePrefix();
        input.placeholder = "Name cannot be empty. Try your username: ";
        return true;
    }
    return false;
}
