import utils from './utils.js';

// Image file extensions for auto-detection
const IMAGE_EXTENSIONS = /\.(jpe?g|png|gif|webp|svg|bmp|ico|avif)(\?[^/\s]*)?$/i;

function isImageUrl(url) {
    return IMAGE_EXTENSIONS.test(url);
}

// Runtime toggle for image preview (controlled by settings module)
let imagePreviewEnabled = true;

function setImagePreviewEnabled(enabled) {
    imagePreviewEnabled = enabled;
}

function formatMessage(text, preserveRaw = false) {
    if (!text) return "";

    const urls = [];
    const forcedImages = new Set();
    const urlPattern = /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#/%?=~_|!:,.;\*]*[-A-Z0-9+&@#/%=~_|])/ig;

    // Pre-process image+URL syntax: strip the marker and flag as forced image
    let processed = text.replace(/image\+((?:https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;\*]*[-A-Z0-9+&@#\/%=~_|])/ig, (match, url) => {
        const id = urls.length;
        urls.push(url);
        forcedImages.add(id);
        return `URLPLACEHOLDER${id}URL`;
    });

    // Extract remaining normal URLs
    processed = processed.replace(urlPattern, (match) => {
        const id = urls.length;
        urls.push(match);
        return `URLPLACEHOLDER${id}URL`;
    });

    processed = utils.escapeHTML(processed);

    // 1. References
    processed = processed.replace(/&lt;(#?[A-zÀ-ú0-9_\-]+):\s*(\d{4}-\d{2}-\d{2})\s*(\d{2}:\d{2}):(\d{2})\s*\[(.*?)\]&gt;/g, (match, channel, date, timeHM, timeS, user) => {
        if (preserveRaw) {
            return `<span style="color: #aaa; text-decoration: underline;">${match}</span>`;
        }
        const userColor = utils.getUserColor(user);
        return `<span class="message-reference external" data-channel="${channel}" data-date="${date}" data-time-hm="${timeHM}" data-time-s="${timeS}" data-from="${user}" title="Click to open message link">«reply to <span style="color: ${userColor}">@${user}</span>»</span>`;
    });

    // 2. Channels
    processed = processed.replace(/(^|\s)#([A-zÀ-ú0-9_\-]+)(?![^<]*>)/g, (match, prefix, channel) => {
        return `${prefix}<a href="?${channel}">#${channel}</a>`;
    });

    // 3. Mentions
    processed = processed.replace(/(^|\s)@([A-zÀ-ú0-9_\-]+)(?![^<]*>)/g, (match, prefix, user) => {
        const color = utils.getUserColor(user);
        return `${prefix}<span style="color: ${color}">@${user}</span>`;
    });

    // 4. Commands
    processed = processed.replace(/(^|\s)(\/[a-zA-Z0-9_\-]+)(?![^<]*>)/g, (match, prefix, command) => {
        return `${prefix}<span class="command">${command}</span>`;
    });

    // 5. Markdown
    processed = processed.replace(/`(.*?)`/g, preserveRaw ? '<code style="padding: 0;">`$1`</code>' : '<code>$1</code>');
    processed = processed.replace(/\*\*(.*?)\*\*/g, preserveRaw ? '<strong>**$1**</strong>' : '<strong>$1</strong>');
    processed = processed.replace(/__(.*?)__/g, preserveRaw ? '<strong>__$1__</strong>' : '<strong>$1</strong>');
    processed = processed.replace(/\*(.*?)\*/g, preserveRaw ? '<em>*$1*</em>' : '<em>$1</em>');
    processed = processed.replace(/_(.*?)_/g, preserveRaw ? '<em>_$1_</em>' : '<em>$1</em>');
    processed = processed.replace(/~~(.*?)~~/g, preserveRaw ? '<del>~~$1~~</del>' : '<del>$1</del>');

    // 6. URLs (with optional inline image preview)
    return processed.replace(/URLPLACEHOLDER(\d+)URL/g, (match, id) => {
        const url = urls[parseInt(id)];
        const escapedUrl = utils.escapeHTML(url);
        const shouldPreview = imagePreviewEnabled && (forcedImages.has(parseInt(id)) || isImageUrl(url));

        if (shouldPreview && !preserveRaw) {
            return `<a href="${escapedUrl}" target="_blank" rel="noopener noreferrer">${escapedUrl}</a><img src="${escapedUrl}" alt="image preview" class="image-preview" loading="lazy" onerror="this.style.display='none'">`;
        }
        return `<a href="${escapedUrl}" target="_blank" rel="noopener noreferrer">${escapedUrl}</a>`;
    });
}

export default { formatMessage, isImageUrl, setImagePreviewEnabled };