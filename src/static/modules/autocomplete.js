import utils from './utils.js';
import api from './api.js';

let autocompleteCache = {
    '/': null,
    '@': null,
    '#': null
};
let autocompleteActiveIndex = -1;
let currentAutocompleteTrigger = null;
let currentAutocompleteFragment = "";

function getAutocompletePopup() {
    return document.getElementById("autocomplete-popup");
}

async function fetchAutoCompleteData(trigger) {
    if (trigger === '@') {
        const userList = document.getElementById("user-list");
        return Array.from(userList.querySelectorAll('.user-item')).map(li => li.textContent);
    }

    if (autocompleteCache[trigger] !== null) return autocompleteCache[trigger];

    let command = "";
    let kwargs = {};
    if (trigger === '/') {
        command = "help";
    } else if (trigger === '#') {
        command = "channels";
        kwargs = { "all": true };
    }

    try {
        const data = await api.fetchCommand(command, kwargs);
        const result = data.result || "";

        let items = [];
        if (trigger === '/') {
            items = result.replace("Available commands: ", "").split(",").map(c => c.trim().substring(1)).filter(c => c.length > 0);
        } else if (trigger === '#') {
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

    autocompletePopup.innerHTML = "";
    filteredItems.forEach((item, index) => {
        const div = document.createElement("div");
        div.className = "autocomplete-item";
        div.textContent = currentAutocompleteTrigger + item;
        if (currentAutocompleteTrigger === '@') {
            div.style.color = utils.getUserColor(item);
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
        const insertText = currentAutocompleteTrigger + item + " ";

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

        if (lastWord && (lastWord.startsWith('/') || lastWord.startsWith('@') || lastWord.startsWith('#'))) {
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

            const filtered = data.filter(item => item.toLowerCase().startsWith(fragment));

            if (autocompleteActiveIndex >= filtered.length) {
                autocompleteActiveIndex = 0;
            } else if (autocompleteActiveIndex === -1 && filtered.length > 0) {
                autocompleteActiveIndex = 0;
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
            if (autocompleteActiveIndex >= 0 && autocompleteActiveIndex < items.length) {
                const selectedText = items[autocompleteActiveIndex].textContent.substring(1);
                handleAutocompleteSelection(selectedText, input);
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
