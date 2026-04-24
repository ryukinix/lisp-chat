import config from './config.js';

function setCookie(name, value, days) {
    let expires = "";
    if (days) {
        const date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        expires = "; expires=" + date.toUTCString();
    }
    document.cookie = name + "=" + (value || "") + expires + "; path=/";
}

function getCookie(name) {
    const nameEQ = name + "=";
    const ca = document.cookie.split(';');
    for (let i = 0; i < ca.length; i++) {
        let c = ca[i];
        while (c.charAt(0) == ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
    }
    return null;
}

function getUserColor(name) {
    if (name === "@server") return "#bb2222";
    let hash = 0;
    const hashShift = 12;
    for (let i = 0; i < name.length; i++) {
        hash = name.charCodeAt(i) + ((hash << hashShift) - hash);
    }
    const index = Math.abs(hash) % config.AVAILABLE_COLORS.length;
    return config.AVAILABLE_COLORS[index];
}

function escapeHTML(text) {
    return text
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

function getTodayDate() {
    const today = new Date();
    const year = today.getFullYear();
    const month = String(today.getMonth() + 1).padStart(2, '0');
    const day = String(today.getDate()).padStart(2, '0');
    return `${year}-${month}-${day}`;
}

function getServerTodayDate() {
    const now = new Date();
    const utc3Time = now.getTime() - (3 * 60 * 60 * 1000);
    const d = new Date(utc3Time);
    const year = d.getUTCFullYear();
    const month = String(d.getUTCMonth() + 1).padStart(2, '0');
    const day = String(d.getUTCDate()).padStart(2, '0');
    return `${year}-${month}-${day}`;
}

function convertToLocalTime(serverDate, timeHM, timeS) {
    const isoString = `${serverDate}T${timeHM}:${timeS}-03:00`;
    const d = new Date(isoString);

    if (isNaN(d.getTime())) {
        return { localDate: serverDate, localTimeHM: timeHM, localTimeS: timeS };
    }

    const year = d.getFullYear();
    const month = String(d.getMonth() + 1).padStart(2, '0');
    const day = String(d.getDate()).padStart(2, '0');

    const hours = String(d.getHours()).padStart(2, '0');
    const minutes = String(d.getMinutes()).padStart(2, '0');
    const seconds = String(d.getSeconds()).padStart(2, '0');

    return {
        localDate: `${year}-${month}-${day}`,
        localTimeHM: `${hours}:${minutes}`,
        localTimeS: `${seconds}`
    };
}

function calculateSeconds(timeHM, timeS) {
    const [h, m] = timeHM.split(':').map(Number);
    const s = Number(timeS);
    return h * 3600 + m * 60 + s;
}

export default { setCookie, getCookie, getUserColor, escapeHTML, getTodayDate, getServerTodayDate, convertToLocalTime, calculateSeconds };
