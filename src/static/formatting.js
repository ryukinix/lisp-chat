import { escapeHTML, getUserColor } from './utils.js';

export function formatMessage(text) {
    if (!text) return "";

    const urls = [];
    const urlPattern = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;\*]*[-A-Z0-9+&@#\/%=~_|])/ig;

    let processed = text.replace(urlPattern, (match) => {
        const id = urls.length;
        urls.push(match);
        return `URLPLACEHOLDER${id}URL`;
    });

    processed = escapeHTML(processed);

    processed = processed.replace(/`(.*?)`/g, '<code>$1</code>');
    processed = processed.replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>');
    processed = processed.replace(/__(.*?)__/g, '<strong>$1</strong>');
    processed = processed.replace(/\*(.*?)\*/g, '<em>$1</em>');
    processed = processed.replace(/_(.*?)_/g, '<em>$1</em>');
    processed = processed.replace(/~~(.*?)~~/g, '<del>$1</del>');

    processed = processed.replace(/(^|\s)#([A-zÀ-ú0-9_\-]+)/g, (match, prefix, channel) => {
        return `${prefix}<a href="?${channel}">#${channel}</a>`;
    });

    processed = processed.replace(/(^|\s)@([A-zÀ-ú0-9_\-]+)/g, (match, prefix, user) => {
        const color = getUserColor(user);
        return `${prefix}<span style="color: ${color}">@${user}</span>`;
    });

    processed = processed.replace(/(^|\s)(\/[a-zA-Z0-9_\-]+)/g, (match, prefix, command) => {
        return `${prefix}<span class="command">${command}</span>`;
    });

    return processed.replace(/URLPLACEHOLDER(\d+)URL/g, (match, id) => {
        const url = urls[parseInt(id)];
        const escapedUrl = escapeHTML(url);
        return `<a href="${escapedUrl}" target="_blank" rel="noopener noreferrer">${escapedUrl}</a>`;
    });
}