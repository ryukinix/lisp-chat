// Direct messages (1:1 DM) switcher modal triggered by /dms

import api from './api.js';
import utils from './utils.js';

let overlay = null;
let filterInput = null;
let listEl = null;
let statusEl = null;

let allDms = [];
let filteredItems = [];
let selectedIndex = 0;
let currentPeerName = '';
let onSelectCallback = null;
let onClearCallback = null;
let previouslyFocused = null;
let fetchToken = 0;

function parseDmsResult(resultText) {
    if (!resultText) return [];
    const lines = resultText.split('\n');
    const parsed = [];

    for (const rawLine of lines) {
        const line = rawLine.trim();
        if (!line || line.toLowerCase() === 'dms:' || line.toLowerCase() === 'dms: none') continue;
        const colonIdx = line.indexOf(':');
        if (colonIdx === -1) continue;

        const rawPeer = line.slice(0, colonIdx).trim();
        const detail = line.slice(colonIdx + 1).trim();
        const peer = rawPeer.replace(/^@/, '');
        if (!peer) continue;

        const unreadMatch = detail.match(/\((\d+)\s+unread\)/i);
        const unread = unreadMatch ? parseInt(unreadMatch[1], 10) : 0;

        parsed.push({ peer, detail, unread });
    }
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
    if (!item || !item.peer) return;
    const peer = item.peer;
    const cb = onSelectCallback;
    closeModal();
    if (typeof cb === 'function') {
        cb(peer);
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
    const query = (filterInput ? filterInput.value : '').trim().replace(/^@/, '').toLowerCase();

    filteredItems = allDms.filter((item) => {
        if (!query) return true;
        return item.peer.toLowerCase().includes(query);
    });

    const exactMatch = query && allDms.some((item) => item.peer.toLowerCase() === query);
    if (query && !exactMatch) {
        filteredItems.push({
            peer: query,
            detail: 'start DM',
            unread: 0,
            isNew: true
        });
    }

    if (selectedIndex >= filteredItems.length) {
        selectedIndex = Math.max(0, filteredItems.length - 1);
    }

    listEl.innerHTML = '';

    if (filteredItems.length === 0) {
        statusEl.textContent = 'No private conversations yet. Type a username to start one.';
        statusEl.hidden = false;
        return;
    }

    statusEl.hidden = true;

    filteredItems.forEach((item, idx) => {
        const li = document.createElement('li');
        li.className = 'channel-item' + (idx === selectedIndex ? ' selected' : '');
        li.setAttribute('role', 'option');
        li.setAttribute('aria-selected', idx === selectedIndex ? 'true' : 'false');

        const isCurrent = currentPeerName && item.peer.toLowerCase() === currentPeerName.toLowerCase();
        const badge = isCurrent ? '<span class="channel-current-badge">current</span>' : '';
        const newTag = item.isNew ? '<span class="channel-new-badge">new</span>' : '';
        const clearBtnHtml = !item.isNew
            ? `<button type="button" class="channels-close-btn dm-delete-btn" title="Delete history with @${utils.escapeHTML(item.peer)}" aria-label="Delete DM history">✕</button>`
            : '';

        li.innerHTML = `
            <span class="channel-item-name">@${utils.escapeHTML(item.peer)}${badge}${newTag}</span>
            <span class="channel-item-meta">${utils.escapeHTML(item.detail)} ${clearBtnHtml}</span>
        `;

        li.addEventListener('mouseenter', () => {
            selectedIndex = idx;
            updateSelection();
        });

        li.addEventListener('click', (e) => {
            if (e.target && e.target.classList && e.target.classList.contains('dm-delete-btn')) {
                e.stopPropagation();
                if (typeof onClearCallback === 'function') {
                    Promise.resolve(onClearCallback(item.peer)).then(() => loadDms());
                }
                return;
            }
            selectItem(item);
        });

        listEl.appendChild(li);
    });
}

async function loadDms() {
    const token = ++fetchToken;
    if (statusEl) {
        statusEl.textContent = 'Loading private conversations...';
        statusEl.hidden = false;
    }
    if (listEl) {
        listEl.innerHTML = '';
    }

    try {
        const data = await api.dms();
        if (token !== fetchToken) return;
        allDms = parseDmsResult(data && data.result ? data.result : '');
        selectedIndex = 0;
        renderList();
    } catch (err) {
        if (token !== fetchToken) return;
        console.error('Failed to load DMs:', err);
        allDms = [];
        filteredItems = [];
        if (statusEl) {
            statusEl.textContent = 'Failed to load private conversations.';
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
    overlay.id = 'dms-modal-overlay';
    overlay.className = 'channels-overlay';
    overlay.setAttribute('role', 'dialog');
    overlay.setAttribute('aria-modal', 'true');
    overlay.setAttribute('aria-label', 'Private conversations');
    overlay.hidden = true;

    overlay.innerHTML = `
        <div class="channels-modal">
            <div class="channels-header">
                <h3>Private Messages</h3>
                <button type="button" class="channels-close-btn" aria-label="Close private messages">✕</button>
            </div>
            <div class="channels-controls">
                <input type="text" class="channels-filter" placeholder="Filter or type @username..." autocomplete="off">
            </div>
            <div class="channels-status" hidden></div>
            <ul class="channels-list" role="listbox" aria-label="Private conversations"></ul>
            <div class="channels-hint">↑↓ navigate · Enter open DM · ✕ delete history · Esc close</div>
        </div>
    `;

    filterInput = overlay.querySelector('.channels-filter');
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

    document.body.appendChild(overlay);
    return overlay;
}

function openModal({ currentPeer = '', onSelect = null, onClear = null } = {}) {
    ensureOverlay();
    previouslyFocused = document.activeElement;
    currentPeerName = (currentPeer || '').replace(/^@/, '');
    onSelectCallback = onSelect;
    onClearCallback = onClear;

    filterInput.value = '';
    overlay.hidden = false;
    document.addEventListener('keydown', handleKeyDown);
    filterInput.focus();

    return loadDms();
}

export default {
    openModal,
    closeModal,
    isOpen,
    parseDmsResult
};
