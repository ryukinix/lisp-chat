export function setupReplyFocus() {
    document.getElementById('chat').addEventListener('click', (e) => {
        if (e.target && e.target.classList.contains('message-reference')) {
            const date = e.target.dataset.date;
            const timeHM = e.target.dataset.timeHm;
            const timeS = e.target.dataset.timeS;
            const from = e.target.dataset.from;
            
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
            }
        }
    });
}

export default { setupReferenceFocus };
