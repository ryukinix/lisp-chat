// Channel quick switcher modal triggered by /channels or Ctrl+K

import api from './api.js';
import utils from './utils.js';

let overlay = null;
let filterInput = null;
let showAllCheckbox = null;
let listEl = null;
let statusEl = null;

let allChannels = [];
let filteredItems = [];
let selectedIndex = 0;
let currentChannelName = 'general';
let currentAll = false;
let currentUsernames = false;
let onSelectCallback = null;
let previouslyFocused = null;
let fetchToken = 0;

function isTruthyKeywordValue(val) {
    if (!val) return false;
    const lower = val.toLowerCase();
    return lower === 't' || lower === 'true' || lower === '1' || lower === 'yes';
}

function parseChannelsCommand(commandText) {
    const tokens = (commandText || '').trim().split(/\s+/).slice(1);
    let all = false;
    let usernames = false;

    for (let i = 0; i < tokens.length; i++) {
        const tok = tokens[i].toLowerCase();
        if (tok === ':all') {
            all = isTruthyKeywordValue(tokens[i + 1]);
            i++;
        } else if (tok === ':usernames') {
            usernames = isTruthyKeywordValue(tokens[i + 1]);
            i++;
        }
    }

    return { all, usernames };
}

function parseChannelsResult(resultText) {
    if (!resultText) return [];
    const lines = resultText.split('\n').slice(1);
    const parsed = [];

    for (const rawLine of lines) {
        const line = rawLine.trim();
        if (!line) continue;
        const colonIdx = line.indexOf(':');
        if (colonIdx === -1) continue;

        const rawChan = line.slice(0, colonIdx).trim();
        const detail = line.slice(colonIdx + 1).trim();
        const name = rawChan.replace(/^#/, '');
        if (!name) continue;

        let userCount = 0;
        const countMatch = detail.match(/^(\d+)\s+user/i);
        if (countMatch) {
            userCount = parseInt(countMatch[1], 10);
        } else if (detail && detail.toLowerCase() !== '0 users') {
            userCount = detail.split(',').filter((u) => u.trim().length > 0).length;
        }

        parsed.push({
            channel: `#${name}`,
            name,
            detail,
            userCount
        });
    }

    // Sort active channels (more users) first, then alphabetically
    parsed.sort((a, b) => {
        if (b.userCount !== a.userCount) return b.userCount - a.userCount;
        return a.name.localeCompare(b.name);
    });

    return parsed;
}

function isOpen() {
    return Boolean(overlay && !overlay.hidden);
}

function closeModal() {
    if (!overlay || overlay.hidden) return;
    overlay.hidden = true;
    document.removeEventListener('keydown', handleKeyDown);
    if (previouslyFocused && typeof previouslyFocused.focus === 'function') {
        previouslyFocused.focus();
    }
    previouslyFocused = null;
}

function selectItem(item) {
    if (!item || !item.name) return;
    const targetName = item.name;
    const cb = onSelectCallback;
    closeModal();
    if (typeof cb === 'function') {
        cb(targetName);
    }
}

function updateSelection() {
    if (!listEl) return;
    const rows = listEl.querySelectorAll('.channel-item');
    rows.forEach((row, idx) => {
        const active = idx === selectedIndex;
        row.classList.toggle('selected', active);
        row.setAttribute('aria-selected', active ? 'true' : 'false');
        if (active && typeof row.scrollIntoView === 'function') {
            row.scrollIntoView({ block: 'nearest' });
        }
    });
}

function renderList() {
    if (!listEl) return;
    const query = (filterInput ? filterInput.value : '').trim().replace(/^#/, '').toLowerCase();

    filteredItems = allChannels.filter((item) => {
        if (!query) return true;
        return item.name.toLowerCase().includes(query) ||
            item.detail.toLowerCase().includes(query);
    });

    const exactMatch = query && allChannels.some((item) => item.name.toLowerCase() === query);
    if (query && !exactMatch) {
        filteredItems.push({
            channel: `#${query}`,
            name: query,
            detail: 'join channel',
            userCount: 0,
            isNew: true
        });
    }

    if (selectedIndex >= filteredItems.length) {
        selectedIndex = Math.max(0, filteredItems.length - 1);
    }

    listEl.innerHTML = '';

    if (filteredItems.length === 0) {
        statusEl.textContent = 'No channels found.';
        statusEl.hidden = false;
        return;
    }

    statusEl.hidden = true;

    filteredItems.forEach((item, idx) => {
        const li = document.createElement('li');
        li.className = 'channel-item' + (idx === selectedIndex ? ' selected' : '');
        li.setAttribute('role', 'option');
        li.setAttribute('aria-selected', idx === selectedIndex ? 'true' : 'false');

        const isCurrent = item.name.toLowerCase() === currentChannelName.toLowerCase();
        const badge = isCurrent ? '<span class="channel-current-badge">current</span>' : '';
        const newTag = item.isNew ? '<span class="channel-new-badge">new</span>' : '';

        li.innerHTML = `
            <span class="channel-item-name">#${utils.escapeHTML(item.name)}${badge}${newTag}</span>
            <span class="channel-item-meta">${utils.escapeHTML(item.detail)}</span>
        `;

        li.addEventListener('mouseenter', () => {
            selectedIndex = idx;
            updateSelection();
        });

        li.addEventListener('click', () => {
            selectItem(item);
        });

        listEl.appendChild(li);
    });
}

async function loadChannels() {
    const token = ++fetchToken;
    if (statusEl) {
        statusEl.textContent = 'Loading channels...';
        statusEl.hidden = false;
    }
    if (listEl) {
        listEl.innerHTML = '';
    }

    try {
        const data = await api.channels(currentAll, currentUsernames);
        if (token !== fetchToken) return;
        allChannels = parseChannelsResult(data && data.result ? data.result : '');
        selectedIndex = 0;
        renderList();
    } catch (err) {
        if (token !== fetchToken) return;
        console.error('Failed to load channels:', err);
        allChannels = [];
        filteredItems = [];
        if (statusEl) {
            statusEl.textContent = 'Failed to load channels.';
            statusEl.hidden = false;
        }
    }
}

function handleKeyDown(e) {
    if (!isOpen()) return;
    if (e.key === 'Escape') {
        e.preventDefault();
        closeModal();
    } else if (e.key === 'ArrowDown' || (e.ctrlKey && e.key.toLowerCase() === 'n')) {
        e.preventDefault();
        if (filteredItems.length > 0) {
            selectedIndex = (selectedIndex + 1) % filteredItems.length;
            updateSelection();
        }
    } else if (e.key === 'ArrowUp' || (e.ctrlKey && e.key.toLowerCase() === 'p')) {
        e.preventDefault();
        if (filteredItems.length > 0) {
            selectedIndex = (selectedIndex - 1 + filteredItems.length) % filteredItems.length;
            updateSelection();
        }
    } else if (e.key === 'Enter') {
        if (filteredItems.length > 0 && selectedIndex >= 0 && selectedIndex < filteredItems.length) {
            e.preventDefault();
            selectItem(filteredItems[selectedIndex]);
        }
    }
}

function ensureOverlay() {
    if (overlay) return overlay;

    overlay = document.createElement('div');
    overlay.id = 'channels-modal-overlay';
    overlay.className = 'channels-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', 'Switch channel');
    overlay.hidden = true;

    overlay.innerHTML = `
        <div class="channels-modal">
            <div class="channels-header">
                <h3>Channels</h3>
                <button type="button" class="channels-close-btn" aria-label="Close channels switcher">✕</button>
            </div>
            <div class="channels-controls">
                <input type="text" class="channels-filter" placeholder="Filter channels..." autocomplete="off">
                <label class="channels-all-toggle">
                    <input type="checkbox" id="channels-show-all">
                    All channels (<code>:all t</code>)
                </label>
            </div>
            <div class="channels-status" hidden></div>
            <ul class="channels-list" role="listbox" aria-label="Available channels"></ul>
            <div class="channels-hint">↑↓ navigate · Enter join · Esc close</div>
        </div>
    `;

    filterInput = overlay.querySelector('.channels-filter');
    showAllCheckbox = overlay.querySelector('#channels-show-all');
    listEl = overlay.querySelector('.channels-list');
    statusEl = overlay.querySelector('.channels-status');
    const closeBtn = overlay.querySelector('.channels-close-btn');

    closeBtn.addEventListener('click', closeModal);
    overlay.addEventListener('click', (e) => {
        if (e.target === overlay) {
            closeModal();
        }
    });

    filterInput.addEventListener('input', () => {
        selectedIndex = 0;
        renderList();
    });

    showAllCheckbox.addEventListener('change', () => {
        currentAll = showAllCheckbox.checked;
        loadChannels();
    });

    document.body.appendChild(overlay);
    return overlay;
}

function openModal({ all = false, usernames = false, currentChannel = 'general', onSelect = null } = {}) {
    ensureOverlay();
    previouslyFocused = document.activeElement;
    currentAll = Boolean(all);
    currentUsernames = Boolean(usernames);
    currentChannelName = (currentChannel || 'general').replace(/^#/, '');
    onSelectCallback = onSelect;

    filterInput.value = '';
    showAllCheckbox.checked = currentAll;
    overlay.hidden = false;
    document.addEventListener('keydown', handleKeyDown);
    filterInput.focus();

    return loadChannels();
}

export default {
    openModal,
    closeModal,
    isOpen,
    parseChannelsCommand,
    parseChannelsResult
};
