import * as formatting from './formatting.js';

export function showNotification(text) {
    const container = document.getElementById("notifications");
    if (!container) return;

    const notification = document.createElement("div");
    notification.className = "notification";
    notification.innerHTML = formatting.formatMessage(text);
    notification.onclick = () => notification.remove();

    container.prepend(notification);

    setTimeout(() => {
        notification.remove();
    }, 8000);
}
