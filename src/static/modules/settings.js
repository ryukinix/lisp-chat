import utils from './utils.js';
import auth from './auth.js';

// Default settings
const DEFAULTS = {
    imagePreview: true,
    theme: 'dark',
    timezone: 'auto',
    reconnectEnabled: true,
    maxReconnectAttempts: 0,  // 0 = infinite
};

// In-memory runtime state (synced with cookies)
let settings = { ...DEFAULTS };

// Pending changes in the modal (not yet saved)
let pendingSettings = null;

function init() {
    loadSettings();
    applyAll();
}

function loadSettings() {
    const saved = utils.getCookie('lispchat_settings');
    if (saved) {
        try {
            const parsed = JSON.parse(decodeURIComponent(saved));
            settings = { ...DEFAULTS, ...parsed };
        } catch (e) {
            settings = { ...DEFAULTS };
        }
    }
}

function saveSettings() {
    const json = JSON.stringify(settings);
    utils.setCookie('lispchat_settings', encodeURIComponent(json), 365);
}

function getSettings() {
    return settings;
}

function get(key) {
    return settings[key];
}

function set(key, value) {
    settings[key] = value;
    saveSettings();
}

function applyTheme() {
    const theme = settings.theme;
    if (theme === 'light') {
        document.body.classList.add('light-theme');
        window.isLightTheme = true;
    } else {
        document.body.classList.remove('light-theme');
        window.isLightTheme = false;
    }
}

function applyImagePreview() {
    if (settings.imagePreview) {
        document.body.classList.remove('image-preview-disabled');
    } else {
        document.body.classList.add('image-preview-disabled');
    }
}

function applyAll() {
    applyTheme();
    applyImagePreview();
}

function toggleTheme() {
    settings.theme = settings.theme === 'dark' ? 'light' : 'dark';
    saveSettings();
    applyTheme();
}

// --- Modal ---

function openModal() {
    // Remove existing modal if any
    closeModal();

    pendingSettings = { ...settings };

    const overlay = document.createElement('div');
    overlay.id = 'settings-overlay';
    overlay.className = 'settings-overlay';

    const modal = document.createElement('div');
    modal.className = 'settings-modal';

    const timezoneOptions = [
        { value: 'auto', label: 'Auto (browser detection)' },
        { value: 'UTC', label: 'UTC' },
        { value: '-12', label: 'UTC-12' },
        { value: '-11', label: 'UTC-11' },
        { value: '-10', label: 'UTC-10' },
        { value: '-9', label: 'UTC-9' },
        { value: '-8', label: 'UTC-8' },
        { value: '-7', label: 'UTC-7' },
        { value: '-6', label: 'UTC-6' },
        { value: '-5', label: 'UTC-5' },
        { value: '-4', label: 'UTC-4' },
        { value: '-3', label: 'UTC-3' },
        { value: '-2', label: 'UTC-2' },
        { value: '-1', label: 'UTC-1' },
        { value: '0', label: 'UTC+0' },
        { value: '1', label: 'UTC+1' },
        { value: '2', label: 'UTC+2' },
        { value: '3', label: 'UTC+3' },
        { value: '4', label: 'UTC+4' },
        { value: '5', label: 'UTC+5' },
        { value: '6', label: 'UTC+6' },
        { value: '7', label: 'UTC+7' },
        { value: '8', label: 'UTC+8' },
        { value: '9', label: 'UTC+9' },
        { value: '10', label: 'UTC+10' },
        { value: '11', label: 'UTC+11' },
        { value: '12', label: 'UTC+12' },
    ];

    modal.innerHTML = `
        <h2>Settings</h2>
        <div class="settings-section">
            <label class="settings-label">
                <input type="checkbox" id="setting-imagePreview" ${pendingSettings.imagePreview ? 'checked' : ''}>
                Inline image preview
            </label>
        </div>
        <div class="settings-section">
            <label class="settings-label">
                <input type="checkbox" id="setting-themeDark" ${pendingSettings.theme === 'dark' ? 'checked' : ''}>
                Dark theme (uncheck for light)
            </label>
        </div>
        <div class="settings-section">
            <label class="settings-label">
                <input type="checkbox" id="setting-reconnectEnabled" ${pendingSettings.reconnectEnabled ? 'checked' : ''}>
                Reconnection in background
            </label>
        </div>
        <div class="settings-section">
            <label class="settings-label">Max reconnection attempts (0 = infinite)</label>
            <input type="number" id="setting-maxReconnectAttempts" class="settings-input" value="${pendingSettings.maxReconnectAttempts}" min="0" max="100">
        </div>
        <div class="settings-section">
            <label class="settings-label">Timezone</label>
            <select id="setting-timezone" class="settings-select">
                ${timezoneOptions.map(opt => `<option value="${opt.value}" ${pendingSettings.timezone === opt.value ? 'selected' : ''}>${opt.label}</option>`).join('')}
            </select>
        </div>
        <div class="settings-section">
            <label class="settings-label">Nickname</label>
            <input type="text" id="setting-nickname" class="settings-input" value="${utils.escapeHTML(pendingSettings.nickname || auth.getUsername() || '')}" placeholder="Current nickname">
        </div>
        <div class="settings-buttons">
            <button type="button" id="settings-cancel" class="settings-cancel-btn">Cancel</button>
            <button type="button" id="settings-save" class="settings-save-btn">Save</button>
        </div>
    `;

    overlay.appendChild(modal);
    document.body.appendChild(overlay);

    // Close on overlay click (outside modal)
    overlay.addEventListener('click', (e) => {
        if (e.target === overlay) closeModal();
    });

    // Button handlers
    const cancelBtn = document.getElementById('settings-cancel');
    const saveBtn = document.getElementById('settings-save');

    cancelBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        closeModal();
    });

    saveBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        e.preventDefault();

        // Read pending values from inputs
        pendingSettings.imagePreview = document.getElementById('setting-imagePreview').checked;
        const nicknameInput = document.getElementById('setting-nickname').value.trim();
        const oldNickname = auth.getUsername();
        const nicknameChanged = nicknameInput && nicknameInput !== oldNickname;
        if (nicknameChanged) {
            pendingSettings.nickname = nicknameInput;
        }
        pendingSettings.timezone = document.getElementById('setting-timezone').value;
        pendingSettings.theme = document.getElementById('setting-themeDark').checked ? 'dark' : 'light';
        pendingSettings.reconnectEnabled = document.getElementById('setting-reconnectEnabled').checked;
        pendingSettings.maxReconnectAttempts = parseInt(document.getElementById('setting-maxReconnectAttempts').value) || 0;

        // Capture new nickname before clearing pendingSettings
        const newNickname = nicknameChanged ? nicknameInput : null;

        // Apply settings
        settings = { ...settings, ...pendingSettings };
        saveSettings();
        applyAll();
        closeModal();

        // Send nick change command and update local state if nickname was changed
        if (newNickname) {
            // Lazy import to avoid circular dependency
            import('./network.js').then(module => {
                const network = module.default;
                const ws = network.getWs();
                if (ws && ws.readyState === WebSocket.OPEN) {
                    ws.send(`/nick ${newNickname}`);
                }
                auth.setUsername(newNickname);
                auth.updateUsernamePrefix();
                notifyListeners();
            });
            return;
        }

        // Notify listeners
        notifyListeners();
    });
}

function closeModal() {
    const overlay = document.getElementById('settings-overlay');
    if (overlay) overlay.remove();
    pendingSettings = null;
}

// --- Change listeners ---

const listeners = [];

function addListener(fn) {
    listeners.push(fn);
}

function notifyListeners() {
    for (const fn of listeners) {
        try { fn(settings); } catch (e) { console.error('Settings listener error:', e); }
    }
}

// --- Timezone helper ---

function getTimezoneOffset() {
    if (settings.timezone === 'auto') {
        return -(new Date().getTimezoneOffset() / 60);
    }
    return parseInt(settings.timezone);
}

export default {
    init, loadSettings, saveSettings, getSettings, get, set,
    openModal, closeModal, applyAll, applyTheme, toggleTheme,
    addListener, getTimezoneOffset
};