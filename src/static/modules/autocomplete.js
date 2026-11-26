import utils from './utils.js';
import api from './api.js';
import emojis from './emojis.js';
import settings from './settings.js';

let autocompleteCache = {
    '/': null,
    '@': null,
    '#': null,
    ':': null
};
let autocompleteActiveIndex = -1;
let currentAutocompleteTrigger = null;
let currentAutocompleteFragment = "";
let currentFilteredItems = [];

function getAutocompletePopup() {
    return document.getElementById("autocomplete-popup");
}

async function fetchAutoCompleteData(trigger) {
    if (trigger === '@') {
        const userList = document.getElementById("user-list");
        return Array.from(userList.querySelectorAll('.user-item')).map(li => li.dataset.username);
    }

    if (trigger === ':') {
        const lang = settings.get('emojiLanguage') || 'en';
        return emojis.getEmojis ? emojis.getEmojis(lang) : (emojis[lang] || emojis);
    }

    if (autocompleteCache[trigger] !== null) return autocompleteCache[trigger];

    try {
        let data;
        let result = "";
        let items = [];

        if (trigger === '/') {
            data = await api.fetchCommand("help");
            result = data.result || "";
            items = result.replace("Available commands: ", "").split(",").map(c => c.trim().substring(1)).filter(c => c.length > 0);
            // Add client-only commands not returned by server /help
            const clientCommands = ['settings'];
            for (const cmd of clientCommands) {
                if (!items.includes(cmd)) {
                    items.push(cmd);
                }
            }
        } else if (trigger === '#') {
            data = await api.channels(true);
            result = data.result || "";
            items = result.split('\n').slice(1).map(line => line.split(':')[0].trim().substring(1)).filter(c => c.length > 0);
        }

        autocompleteCache[trigger] = items;
        return items;
    } catch (e) {
        console.error("Failed to fetch autocomplete data", e);
        return [];
    }
}

function renderAutocomplete(filteredItems, input) {
    const autocompletePopup = getAutocompletePopup();
    if (filteredItems.length === 0) {
        closeAutocomplete();
        return;
    }

    currentFilteredItems = filteredItems;
    autocompletePopup.innerHTML = "";
    filteredItems.forEach((item, index) => {
        const div = document.createElement("div");
        div.className = "autocomplete-item";

        if (currentAutocompleteTrigger === ':') {
            const emojiChar = typeof item === 'object' ? item.emoji : '';
            const emojiName = typeof item === 'object' ? item.name : item;

            const spanChar = document.createElement("span");
            spanChar.className = "autocomplete-emoji";
            spanChar.textContent = emojiChar;

            const spanName = document.createElement("span");
            spanName.className = "autocomplete-name";
            spanName.textContent = ":" + emojiName + ":";

            div.appendChild(spanChar);
            div.appendChild(spanName);
        } else {
            const name = typeof item === 'object' ? item.name : item;
            div.textContent = currentAutocompleteTrigger + name;
            if (currentAutocompleteTrigger === '@') {
                div.style.color = utils.getUserColor(name);
            }
        }

        if (index === autocompleteActiveIndex) {
            div.classList.add("selected");
        }
        div.onclick = () => {
            autocompleteActiveIndex = index;
            handleAutocompleteSelection(item, input);
        };
        autocompletePopup.appendChild(div);
    });

    autocompletePopup.classList.remove("hidden");
}

function handleAutocompleteSelection(item, input) {
    const value = input.value;
    const selectionStart = input.selectionStart;
    const textBeforeCursor = value.substring(0, selectionStart);
    const textAfterCursor = value.substring(selectionStart);

    const lastTriggerIndex = textBeforeCursor.lastIndexOf(currentAutocompleteTrigger);

    if (lastTriggerIndex !== -1) {
        const beforeTrigger = textBeforeCursor.substring(0, lastTriggerIndex);
        let insertText;
        if (currentAutocompleteTrigger === ':') {
            const emojiChar = typeof item === 'object' ? item.emoji : item;
            insertText = emojiChar + " ";
        } else {
            const text = typeof item === 'object' ? item.name : item;
            insertText = currentAutocompleteTrigger + text + " ";
        }

        input.value = beforeTrigger + insertText + textAfterCursor;
        const newPos = beforeTrigger.length + insertText.length;
        input.setSelectionRange(newPos, newPos);
    }

    input.focus();
    closeAutocomplete();
}

function closeAutocomplete() {
    const autocompletePopup = getAutocompletePopup();
    if (autocompletePopup) {
        autocompletePopup.classList.add("hidden");
        autocompletePopup.innerHTML = "";
    }
    autocompleteActiveIndex = -1;
    currentAutocompleteTrigger = null;
    currentAutocompleteFragment = "";
    currentFilteredItems = [];
}

function updateAutocompleteSelection(items) {
    items.forEach((item, index) => {
        if (index === autocompleteActiveIndex) {
            item.classList.add("selected");
            item.scrollIntoView({ block: "nearest" });
        } else {
            item.classList.remove("selected");
        }
    });
}

function initAutocomplete(input) {
    input.addEventListener("input", async () => {
        const value = input.value;
        const selectionStart = input.selectionStart;

        const textBeforeCursor = value.substring(0, selectionStart);
        const words = textBeforeCursor.split(/\s+/);
        const lastWord = words[words.length - 1];

        if (lastWord && (lastWord.startsWith('/') || lastWord.startsWith('@') || lastWord.startsWith('#') || lastWord.startsWith(':'))) {
            // Avoid matching :: (e.g. package prefixes) or already closed emojis :smile:
            if (lastWord.startsWith('::') || (lastWord.startsWith(':') && lastWord.length > 1 && lastWord.endsWith(':'))) {
                closeAutocomplete();
                return;
            }

            const trigger = lastWord[0];
            const fragment = lastWord.substring(1).toLowerCase();

            if (trigger === '/' && textBeforeCursor.trim() !== lastWord) {
                 closeAutocomplete();
                 return;
            }

            currentAutocompleteTrigger = trigger;
            currentAutocompleteFragment = fragment;

            const data = await fetchAutoCompleteData(trigger);

            if (input.value !== value || input.selectionStart !== selectionStart) return;

            let filtered = data.filter(item => {
                const text = typeof item === 'object' ? item.name : item;
                return text.toLowerCase().startsWith(fragment);
            });

            if (trigger === ':') {
                filtered = filtered.slice(0, 50);
            }

            if (autocompleteActiveIndex >= filtered.length) {
                autocompleteActiveIndex = filtered.length > 0 ? 0 : -1;
            } else if (autocompleteActiveIndex === -1 && filtered.length > 0) {
                // Do not preselect on bare colon without search query to avoid hijacking Enter
                autocompleteActiveIndex = (trigger === ':' && fragment === "") ? -1 : 0;
            }

            renderAutocomplete(filtered, input);
        } else {
            closeAutocomplete();
        }
    });

    input.addEventListener("keydown", (e) => {
        const autocompletePopup = getAutocompletePopup();
        if (autocompletePopup.classList.contains("hidden")) return;

        const items = autocompletePopup.querySelectorAll('.autocomplete-item');
        if (items.length === 0) return;

        if (e.key === "ArrowDown") {
            e.preventDefault();
            autocompleteActiveIndex = (autocompleteActiveIndex + 1) % items.length;
            updateAutocompleteSelection(items);
        } else if (e.key === "ArrowUp") {
            e.preventDefault();
            autocompleteActiveIndex = (autocompleteActiveIndex - 1 + items.length) % items.length;
            updateAutocompleteSelection(items);
        } else if (e.key === "Enter" || e.key === "Tab") {
            e.preventDefault();
            if (autocompleteActiveIndex >= 0 && autocompleteActiveIndex < currentFilteredItems.length) {
                handleAutocompleteSelection(currentFilteredItems[autocompleteActiveIndex], input);
            }
        } else if (e.key === "Escape") {
            e.preventDefault();
            closeAutocomplete();
        }
    });

    document.addEventListener("click", (e) => {
        const autocompletePopup = getAutocompletePopup();
        if (autocompletePopup && !autocompletePopup.contains(e.target) && e.target !== input) {
            closeAutocomplete();
        }
    });
}

export default {
    getAutocompletePopup, fetchAutoCompleteData, renderAutocomplete,
    handleAutocompleteSelection, closeAutocomplete, updateAutocompleteSelection,
    initAutocomplete
};
