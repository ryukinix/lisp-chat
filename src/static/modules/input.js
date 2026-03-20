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

export default {
    initInputHistory,
    addMessageToHistory
};
