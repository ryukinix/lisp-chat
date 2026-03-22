import config from './config.js';
import messages from './messages.js';
import notifications from './notifications.js';
import api from './api.js';

const textButton = "⬆"
let hasReachedEnd = false;

function initHistoryLoading() {
    const loadMoreBtn = document.getElementById("load-more-btn");
    if (!loadMoreBtn) return;

    messages.chat.addEventListener('scroll', () => {
        if (hasReachedEnd) return;
        if (messages.chat.scrollTop === 0 && messages.chat.scrollHeight > messages.chat.clientHeight) {
            loadMoreBtn.classList.remove("hidden");
        } else {
            loadMoreBtn.classList.add("hidden");
        }
    });

    let isLoadingHistory = false;
    loadMoreBtn.addEventListener('click', async () => {
        console.log("CLICKED LOAD MORE!");
        if (isLoadingHistory || hasReachedEnd) {
            console.log("ALREADY LOADING OR REACHED END", isLoadingHistory, hasReachedEnd);
            return;
        }
        isLoadingHistory = true;
        const oldestMsg = messages.chat.querySelector('.message');
        if (!oldestMsg) {
            console.log("NO OLDEST MSG");
            isLoadingHistory = false;
            loadMoreBtn.textContent = textButton;
            return;
        }

        const date = oldestMsg.dataset.date;
        const timeHm = oldestMsg.dataset.timeHm;
        const timeS = oldestMsg.dataset.timeS;

        if (!date || !timeHm || !timeS) {
            console.log("MISSING DATASETS", date, timeHm, timeS);
            isLoadingHistory = false;
            loadMoreBtn.textContent = textButton;
            return;
        }
        const isoDate = `${date}T${timeHm}:${timeS}`;

        try {
            const data = await api.log(config.LOG_HISTORY_SIZE, isoDate);
            const result = data.result || "";
            const numMessagesBefore = messages.chat.querySelectorAll('.message').length;
            
            console.log("HISTORY LOAD:", result, "BEFORE:", numMessagesBefore);
            
            if (result.trim()) {
                messages.addMessage(result, true);
            }
            
            const numMessagesAfter = messages.chat.querySelectorAll('.message').length;
            console.log("HISTORY LOAD AFTER:", numMessagesAfter);
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
}

function resetReachedEnd() {
    hasReachedEnd = false;
}

export default { initHistoryLoading, resetReachedEnd };
