import formatting from './formatting.js';

const messageHistory = [];
let historyIndex = 0;
let currentDraft = "";

function initInputHistory(inputElement) {
    inputElement.addEventListener("keydown", (e) => {
        const autocompletePopup = document.getElementById("autocomplete-popup");
        if (autocompletePopup && !autocompletePopup.classList.contains("hidden")) return;

        if (e.key === "ArrowUp") {
            if (messageHistory.length > 0) {
                e.preventDefault();
                if (historyIndex === messageHistory.length) {
                    currentDraft = inputElement.value;
                }
                if (historyIndex > 0) {
                    historyIndex--;
                    inputElement.value = messageHistory[historyIndex];
                }
            }
        } else if (e.key === "ArrowDown") {
            if (messageHistory.length > 0) {
                e.preventDefault();
                if (historyIndex < messageHistory.length - 1) {
                    historyIndex++;
                    inputElement.value = messageHistory[historyIndex];
                } else if (historyIndex === messageHistory.length - 1) {
                    historyIndex++;
                    inputElement.value = currentDraft;
                }
            }
        }
    });
}

function addMessageToHistory(message) {
    messageHistory.push(message);
    historyIndex = messageHistory.length;
    currentDraft = "";
}

function setupInputOverlay(inputElement) {
    const wrapper = document.createElement('div');
    wrapper.style.position = 'relative';
    wrapper.style.flex = '1';
    wrapper.style.display = 'flex';
    wrapper.style.alignItems = 'stretch';
    wrapper.className = 'input-wrapper';

    const overlay = document.createElement('div');
    overlay.className = 'input-overlay';
    overlay.style.position = 'absolute';
    overlay.style.top = '0';
    overlay.style.left = '0';
    overlay.style.right = '0';
    overlay.style.bottom = '0';
    overlay.style.pointerEvents = 'none';
    overlay.style.whiteSpace = 'pre';
    overlay.style.overflow = 'hidden';
    overlay.style.padding = '10px';
    overlay.style.fontFamily = 'monospace';
    overlay.style.fontSize = '12px';
    overlay.style.color = '#fff';
    overlay.style.boxSizing = 'border-box';
    overlay.style.border = '1px solid transparent';
    overlay.style.lineHeight = 'normal';

    inputElement.parentNode.insertBefore(wrapper, inputElement);
    wrapper.appendChild(overlay);
    wrapper.appendChild(inputElement);

    inputElement.style.color = 'transparent';
    inputElement.style.caretColor = window.isLightTheme ? '#222' : '#fff';
    inputElement.style.background = 'transparent';
    inputElement.style.position = 'relative';
    inputElement.style.zIndex = '2';
    
    wrapper.style.background = '#333';
    wrapper.style.border = '1px solid #555';
    inputElement.style.border = 'none';
    inputElement.style.outline = 'none';
    inputElement.style.margin = '0';
    inputElement.style.width = '100%';

    function updateOverlay() {
        const text = inputElement.value;
        const html = formatting.formatMessage(text, true);

        overlay.innerHTML = html;
        
        // Sync scroll
        overlay.scrollLeft = inputElement.scrollLeft;
    }

    inputElement.addEventListener('input', updateOverlay);
    inputElement.addEventListener('scroll', () => {
        overlay.scrollLeft = inputElement.scrollLeft;
    });
    
    // Also update when value is changed programmatically
    const originalDescriptor = Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, 'value');
    Object.defineProperty(inputElement, 'value', {
        get: function() {
            return originalDescriptor.get.call(this);
        },
        set: function(val) {
            originalDescriptor.set.call(this, val);
            updateOverlay();
        }
    });

    updateOverlay();
}

function updateCaretColor() {
    const inputElement = document.getElementById('message-input');
    if (inputElement) {
        inputElement.style.caretColor = window.isLightTheme ? '#222' : '#fff';
    }
}

export default {
    initInputHistory,
    addMessageToHistory,
    setupInputOverlay,
    updateCaretColor
};
