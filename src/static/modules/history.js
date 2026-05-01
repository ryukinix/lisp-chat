import config from './config.js';
import messages from './messages.js';
import notifications from './notifications.js';
import api from './api.js';

const textButton = "⬆"
const textNewerButton = "⬇"
let hasReachedEnd = false;
let isHistoricalContext = false;
let hasReachedNewest = false;

function setHistoricalContext(value) {
    isHistoricalContext = value;
    hasReachedNewest = false;
    updateNewerButtonVisibility();
}

function updateNewerButtonVisibility() {
    const loadNewerBtn = document.getElementById("load-newer-btn");
    if (!loadNewerBtn) return;
    
    if (isHistoricalContext && !hasReachedNewest && messages.chat.scrollTop + messages.chat.clientHeight >= messages.chat.scrollHeight - 100) {
        loadNewerBtn.classList.remove("hidden");
    } else {
        loadNewerBtn.classList.add("hidden");
    }
}

function initHistoryLoading() {
    const loadMoreBtn = document.getElementById("load-more-btn");
    const loadNewerBtn = document.getElementById("load-newer-btn");
    if (loadNewerBtn) {
        loadNewerBtn.textContent = textNewerButton;
    }
    
    if (!loadMoreBtn) return;

    messages.chat.addEventListener('scroll', () => {
        if (!hasReachedEnd) {
            if (messages.chat.scrollTop === 0 && messages.chat.scrollHeight > messages.chat.clientHeight) {
                loadMoreBtn.classList.remove("hidden");
            } else {
                loadMoreBtn.classList.add("hidden");
            }
        }
        updateNewerButtonVisibility();
    });

    let isLoadingHistory = false;
    loadMoreBtn.addEventListener('click', async () => {
        if (isLoadingHistory || hasReachedEnd) return;
        isLoadingHistory = true;
        const oldestMsg = messages.chat.querySelector('.message');
        if (!oldestMsg) {
            isLoadingHistory = false;
            loadMoreBtn.textContent = textButton;
            return;
        }

        const date = oldestMsg.dataset.date;
        const timeHm = oldestMsg.dataset.timeHm;
        const timeS = oldestMsg.dataset.timeS;

        if (!date || !timeHm || !timeS) {
            isLoadingHistory = false;
            loadMoreBtn.textContent = textButton;
            return;
        }
        const isoDate = `${date}T${timeHm}:${timeS}`;

        try {
            const data = await api.log(config.LOG_HISTORY_SIZE, isoDate);
            const result = data.result || "";
            const numMessagesBefore = messages.chat.querySelectorAll('.message').length;
            
            if (result.trim()) {
                messages.addMessage(result, true);
            }
            
            const numMessagesAfter = messages.chat.querySelectorAll('.message').length;
            if (numMessagesBefore === numMessagesAfter) {
                hasReachedEnd = true;
                notifications.showNotification("You reached the chat history end!");
                loadMoreBtn.classList.add("hidden");
            }
        } catch (e) {
            console.error("Failed to load history", e);
        }

        isLoadingHistory = false;
        loadMoreBtn.textContent = textButton;
    });

    let isLoadingNewer = false;
    if (loadNewerBtn) {
        loadNewerBtn.addEventListener('click', async () => {
            if (isLoadingNewer || hasReachedNewest) return;
            isLoadingNewer = true;
            
            const newestMsg = messages.chat.querySelectorAll('.message');
            const lastMsg = newestMsg[newestMsg.length - 1];
            
            if (!lastMsg) {
                isLoadingNewer = false;
                return;
            }

            const date = lastMsg.dataset.date;
            const timeHm = lastMsg.dataset.timeHm;
            const timeS = lastMsg.dataset.timeS;

            if (!date || !timeHm || !timeS) {
                isLoadingNewer = false;
                return;
            }
            const isoDate = `${date}T${timeHm}:${timeS}`;

            try {
                // Fetch messages *after* the current newest message.
                const data = await api.log(config.LOG_HISTORY_SIZE, undefined, isoDate);
                const result = data.result || "";
                
                const numMessagesBefore = messages.chat.querySelectorAll('.message').length;
                
                if (result.trim()) {
                    // addMessage adds to bottom if prepend is false
                    messages.addMessage(result, false); 
                }
                
                const numMessagesAfter = messages.chat.querySelectorAll('.message').length;
                if (numMessagesBefore === numMessagesAfter) {
                    hasReachedNewest = true;
                    isHistoricalContext = false; // We reached the end, no longer historical.
                    updateNewerButtonVisibility();
                    notifications.showNotification("You are now viewing the latest messages.");
                }
            } catch (e) {
                console.error("Failed to load newer messages", e);
            }

            isLoadingNewer = false;
            updateNewerButtonVisibility();
        });
    }
}

function resetReachedEnd() {
    hasReachedEnd = false;
    isHistoricalContext = false;
    hasReachedNewest = false;
    updateNewerButtonVisibility();
}

export default { initHistoryLoading, resetReachedEnd, setHistoricalContext };
