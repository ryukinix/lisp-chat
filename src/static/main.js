import config from './modules/config.js';
import auth from './modules/auth.js';
import network from './modules/network.js';
import autocomplete from './modules/autocomplete.js';
import messages from './modules/messages.js';
import inputModule from './modules/input.js';
import notifications from './modules/notifications.js';
import history from './modules/history.js';
import reply from "./modules/reply.js";
import settings from './modules/settings.js';
import users from './modules/users.js';
import push from './modules/push.js';
import lightbox from './modules/lightbox.js';
import channelsModal from './modules/channels.js';
import dmsModal from './modules/dms.js';

function getCurrentDmPeer() {
    const params = new URLSearchParams(window.location.search);
    const dm = params.get('dm');
    return dm ? dm.replace(/^@/, '').trim() : '';
}

function getCurrentChannelName() {
    let channel = window.location.search.substring(1).split('&')[0];
    if (!channel || channel.includes('=')) {
        channel = "general";
    } else {
        try { channel = decodeURIComponent(channel); } catch (e) { /* keep raw */ }
    }
    return channel.replace(/^#/, '');
}

function updatePageTitle() {
    const dmPeer = getCurrentDmPeer();
    if (dmPeer) {
        document.title = `Lisp Chat Web: DM @${dmPeer}`;
        push.clearChannelNotifications(`dm:${dmPeer}`);
        return;
    }
    const channelName = getCurrentChannelName();
    document.title = `Lisp Chat Web: #${channelName}`;
    push.clearChannelNotifications(channelName);
}

function navigateToDm(peerName) {
    const peer = (peerName || '').replace(/^@/, '').trim();
    if (!peer) return;

    const cleanPath = window.location.pathname.replace(/\/index\.html$/, '/');
    const url = new URL(window.location.origin + cleanPath);
    url.search = `dm=${encodeURIComponent(peer)}`;
    window.history.pushState({}, '', url);
    updatePageTitle();

    messages.clearMessages();
    history.resetReachedEnd();
    if (network.getWs() && network.getWs().readyState === WebSocket.OPEN) {
        network.getWs().send(`/dm ${peer}`);
        network.getWs().send(`/log :depth ${config.LOG_HISTORY_SIZE} :date-format date`);
    } else {
        network.connect(messages.addMessage);
    }
    input.focus();
}

function navigateToChannel(channelName) {
    const newChannel = (channelName || 'general').replace(/^#/, '').trim();
    if (!newChannel) return;

    if (!getCurrentDmPeer() && newChannel.toLowerCase() === getCurrentChannelName().toLowerCase()) {
        input.focus();
        return;
    }

    const cleanPath = window.location.pathname.replace(/\/index\.html$/, '/');
    const url = new URL(window.location.origin + cleanPath);
    url.search = encodeURIComponent(newChannel);
    window.history.pushState({}, '', url);
    updatePageTitle();

    messages.clearMessages();
    history.resetReachedEnd();
    if (network.getWs() && network.getWs().readyState === WebSocket.OPEN) {
        network.getWs().send(`/join #${newChannel}`);
        network.getWs().send(`/log :depth ${config.LOG_HISTORY_SIZE} :date-format date`);
    } else {
        network.connect(messages.addMessage);
    }
    input.focus();
}

const form = document.getElementById("input-area");
const input = document.getElementById("message-input");

inputModule.initInputHistory(input);
inputModule.setupInputOverlay(input);
reply.setupReplyFocus();
lightbox.init();
users.initUserListClick((username) => {
    const me = (auth.getUsername() || '').trim();
    if (!username || (me && username.toLowerCase() === me.toLowerCase())) {
        return;
    }
    navigateToDm(username);
});

// Initialize settings module (applies theme and image-preview classes)
settings.init();

// Listen for settings changes and apply dynamic classes at runtime
settings.addListener((newSettings) => {
    settings.applyAll();
    users.refreshUserListColors();
    auth.updateUsernamePrefix();
    inputModule.updateCaretColor();
});

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
        if (trimmed.startsWith("/settings")) {
            settings.openModal();
            input.value = "";
            return;
        }
        if (!auth.getLoggedIn()) {
            auth.performLogin(trimmed);
            input.value = "";
            return;
        }
        const firstWord = trimmed.split(/\s+/)[0];
        if (firstWord === "/channels") {
            const opts = channelsModal.parseChannelsCommand(trimmed);
            channelsModal.openModal({
                ...opts,
                currentChannel: getCurrentDmPeer() ? '' : getCurrentChannelName(),
                onSelect: navigateToChannel
            });
            input.value = "";
            return;
        }
        if (firstWord === "/dms") {
            dmsModal.openModal({
                currentPeer: getCurrentDmPeer(),
                onSelect: navigateToDm,
                onClear: (peer) => {
                    if (network.getWs() && network.getWs().readyState === WebSocket.OPEN) {
                        network.getWs().send(`/dm-clear ${peer}`);
                        if (getCurrentDmPeer().toLowerCase() === peer.toLowerCase()) {
                            messages.clearMessages();
                        }
                    }
                }
            });
            input.value = "";
            return;
        }
        if (firstWord === "/dm") {
            const parts = trimmed.split(/\s+/);
            if (parts.length === 2) {
                navigateToDm(parts[1]);
                input.value = "";
                return;
            }
        }
        if (firstWord === "/dm-clear") {
            const parts = trimmed.split(/\s+/);
            const target = (parts[1] || getCurrentDmPeer()).replace(/^@/, '').toLowerCase();
            if (target && target === getCurrentDmPeer().toLowerCase()) {
                messages.clearMessages();
            }
        }
        if (firstWord === "/users") {
            network.incrementUserRequestsPending();
        }
        if (firstWord === "/join") {
            const parts = trimmed.split(/\s+/);
            if (parts.length > 1) {
                navigateToChannel(parts[1]);
            } else {
                network.getWs().send(value);
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

window.addEventListener('focus', () => {
    push.clearChannelNotifications(getCurrentChannelName());
});
document.addEventListener('visibilitychange', () => {
    if (!document.hidden) {
        push.clearChannelNotifications(getCurrentChannelName());
    }
});

// Quick channel switcher shortcut (Ctrl+K / Cmd+K)
document.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key.toLowerCase() === 'k') {
        if (lightbox.isOpen()) return;
        e.preventDefault();
        if (channelsModal.isOpen()) {
            channelsModal.closeModal();
        } else {
            channelsModal.openModal({
                all: true,
                currentChannel: getCurrentChannelName(),
                onSelect: navigateToChannel
            });
        }
    }
});

if ('serviceWorker' in navigator) {
    navigator.serviceWorker.addEventListener('message', event => {
        if (event.data && event.data.type === 'NAVIGATE_CHANNEL' && event.data.channel) {
            if (event.data.channel.toLowerCase().startsWith('dm:') && event.data.from) {
                navigateToDm(event.data.from);
                return;
            }
            navigateToChannel(event.data.channel);
        }
    });
}
