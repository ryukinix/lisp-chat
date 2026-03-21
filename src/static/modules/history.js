import config from './config.js';
import messages from './messages.js';
import notifications from './notifications.js';
import api from './api.js';

const textButton = "⬆"

function initHistoryLoading() {
    const loadMoreBtn = document.getElementById("load-more-btn");
    if (!loadMoreBtn) return;

    messages.chat.addEventListener('scroll', () => {
        if (messages.chat.scrollTop === 0 && messages.chat.scrollHeight > messages.chat.clientHeight) {
            loadMoreBtn.classList.remove("hidden");
        } else {
            loadMoreBtn.classList.add("hidden");
        }
    });

    let isLoadingHistory = false;
    loadMoreBtn.addEventListener('click', async () => {
        if (isLoadingHistory) return;
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
            if (!result.trim()) {
                notifications.showNotification("You reached the chat history end!");
                loadMoreBtn.style.display = 'none';
            } else {
                messages.addMessage(result, true);
            }
        } catch (e) {
            console.error("Failed to load history", e);
        }

        isLoadingHistory = false;
        loadMoreBtn.textContent = textButton;
    });
}

export default { initHistoryLoading };
