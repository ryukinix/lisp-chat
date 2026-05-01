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

                // Remove the class after animation completes so it can be triggered again
                setTimeout(() => {
                    matchedMessage.classList.remove('focused');
                }, 3000);
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
                
                const url = new URL(window.location.origin + window.location.pathname);
                url.searchParams.set('channel', refChannel);
                url.searchParams.set('message_ref', reference);
                window.location.href = url.toString();
            }
        }
    });
}

export default { setupReplyFocus };
