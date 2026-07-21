function updateReplyReferenceStates() {
    const chat = document.getElementById('chat');
    if (!chat) return;

    const references = chat.querySelectorAll('.message-reference');
    const messages = chat.querySelectorAll('.message');

    // Create a fast lookup map for messages currently in the DOM
    const messageMap = new Set();
    for (const msg of messages) {
        if (msg.dataset.date && msg.dataset.timeHm && msg.dataset.from) {
            // Include seconds for exact match
            if (msg.dataset.timeS) {
                messageMap.add(`${msg.dataset.date}|${msg.dataset.timeHm}:${msg.dataset.timeS}|${msg.dataset.from}`);
            }
            // Add fallback match without seconds
            messageMap.add(`${msg.dataset.date}|${msg.dataset.timeHm}|${msg.dataset.from}`);
        }
    }

    for (const ref of references) {
        const date = ref.dataset.date;
        const timeHM = ref.dataset.timeHm;
        const timeS = ref.dataset.timeS;
        const from = ref.dataset.from;

        const exactKey = `${date}|${timeHM}:${timeS}|${from}`;
        const fallbackKey = `${date}|${timeHM}|${from}`;

        if (messageMap.has(exactKey) || messageMap.has(fallbackKey)) {
            if (ref.classList.contains('external')) {
                ref.classList.remove('external');
                ref.classList.add('local');
                ref.title = "Click to focus message";
            }
        } else {
            if (ref.classList.contains('local')) {
                ref.classList.remove('local');
                ref.classList.add('external');
                ref.title = "Click to open message link";
            }
        }
    }
}

function setupReplyFocus() {
    document.getElementById('chat').addEventListener('click', (e) => {
        // If we click inside the reference but not exactly on the element (like clicking the span inside)
        const target = e.target.closest('.message-reference');
        if (target) {
            const date = target.dataset.date;
            const timeHM = target.dataset.timeHm;
            const timeS = target.dataset.timeS;
            const from = target.dataset.from;

            // Find the message in the DOM
            const chat = document.getElementById('chat');
            const messages = chat.querySelectorAll('.message');

            let matchedMessage = null;

            for (const msg of messages) {
                if (msg.dataset.date === date &&
                    msg.dataset.timeHm === timeHM &&
                    msg.dataset.timeS === timeS &&
                    msg.dataset.from === from) {
                    matchedMessage = msg;
                    break;
                }

                // Fallback: sometimes timeS may not perfectly match if it wasn't tracked precisely,
                // try to match with date, hm and user
                if (!matchedMessage &&
                    msg.dataset.date === date &&
                    msg.dataset.timeHm === timeHM &&
                    msg.dataset.from === from) {
                    matchedMessage = msg;
                }
            }

            if (matchedMessage) {
                matchedMessage.scrollIntoView({ behavior: 'smooth', block: 'center' });

                matchedMessage.classList.add('focused');

                // Remove highlight when user scrolls, clicks, or types
                const removeFocus = () => {
                    matchedMessage.classList.remove('focused');
                    chat.removeEventListener('scroll', removeFocus);
                    chat.removeEventListener('click', removeFocus);
                    const msgInput = document.getElementById('message-input');
                    if (msgInput) {
                        msgInput.removeEventListener('input', removeFocus);
                        msgInput.removeEventListener('keydown', removeFocus);
                    }
                };

                // Add listeners after a short delay so the initial
                // scrollIntoView doesn't immediately trigger removal
                setTimeout(() => {
                    chat.addEventListener('scroll', removeFocus, { once: true });
                    chat.addEventListener('click', removeFocus, { once: true });
                    const msgInput = document.getElementById('message-input');
                    if (msgInput) {
                        msgInput.addEventListener('input', removeFocus, { once: true });
                        msgInput.addEventListener('keydown', removeFocus, { once: true });
                    }
                }, 800);
            } else {
                let channel = target.dataset.channel;
                if (!channel) {
                    channel = window.location.search.substring(1).split('&')[0];
                    if (!channel || channel.includes('=')) {
                        channel = "general";
                    }
                }
                const refChannel = channel.replace('#', '');
                const reference = `<#${refChannel}: ${date} ${timeHM}:${timeS} [${from}]>`;
                
                const cleanPath = window.location.pathname.replace(/\/index\.html$/, '/');
                const url = new URL(window.location.origin + cleanPath);
                url.search = `?${refChannel}&message_ref=${encodeURIComponent(reference)}`;
                window.location.href = url.toString();
            }
        }
    });
}

export default { setupReplyFocus, updateReplyReferenceStates };
