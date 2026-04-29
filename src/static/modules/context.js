import formatting from './formatting.js';
import utils from './utils.js';
import network from './network.js';

export function setupContextModal() {
    const modal = document.getElementById('context-modal');
    const span = document.getElementsByClassName('close-modal')[0];

    span.onclick = function() {
        modal.style.display = 'none';
    }

    window.onclick = function(event) {
        if (event.target == modal) {
            modal.style.display = 'none';
        }
    }
    
    document.getElementById('chat').addEventListener('click', async (e) => {
        if (e.target && e.target.classList.contains('message-reference')) {
            const datetime = e.target.dataset.datetime;
            const channel = e.target.dataset.channel;
            
            modal.style.display = 'block';
            const contextContainer = document.getElementById('context-messages');
            contextContainer.innerHTML = 'Loading context...';
            
            try {
                // Fetch context via POST to /api/commands/search
                // We use global search around the specific datetime
                const payload = {
                    args: [e.target.dataset.user],
                    kwargs: {
                        around: datetime,
                        global: "t"
                    }
                };
                
                const response = await fetch('/api/commands/search', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json'
                    },
                    body: JSON.stringify(payload)
                });
                
                if (response.ok) {
                    const data = await response.json();
                    const text = data.result || "";
                    
                    if (text.includes("error:") || text.includes("empty result")) {
                        contextContainer.innerHTML = 'Could not load context.';
                        return;
                    }
                    
                    contextContainer.innerHTML = '';
                    const lines = text.split('\\n');
                    
                    for (const line of lines) {
                        if (!line.trim()) continue;
                        const match = line.match(/^\|.*?\| \[(.*?)\]: (.*)$/);
                        if (match) {
                           const div = document.createElement("div");
                           div.className = "context-message";
                           
                           // if it's the exact message we clicked, highlight it
                           const isTarget = line.includes(datetime.split('T')[1]); // naive check by time
                           if (isTarget) {
                               div.style.backgroundColor = 'rgba(255, 255, 255, 0.1)';
                           }
                           
                           div.innerHTML = formatting.formatMessage(line);
                           contextContainer.appendChild(div);
                        } else {
                           const div = document.createElement("div");
                           div.className = "context-message";
                           div.innerHTML = formatting.formatMessage(line);
                           contextContainer.appendChild(div);
                        }
                    }
                } else {
                    contextContainer.innerHTML = 'Error loading context.';
                }
            } catch (error) {
                console.error(error);
                contextContainer.innerHTML = 'Error loading context.';
            }
        }
    });

}

export default { setupContextModal };
