import auth from './auth.js';

function isPushSupported() {
    return typeof window !== 'undefined' &&
        'serviceWorker' in navigator &&
        'PushManager' in window &&
        'Notification' in window;
}

// Convert a Base64-URL VAPID public key into a Uint8Array for PushManager.subscribe
function urlBase64ToUint8Array(base64String) {
    const padding = '='.repeat((4 - base64String.length % 4) % 4);
    const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
    const rawData = window.atob(base64);
    const outputArray = new Uint8Array(rawData.length);
    for (let i = 0; i < rawData.length; ++i) {
        outputArray[i] = rawData.charCodeAt(i);
    }
    return outputArray;
}

function keysEqual(a, b) {
    if (!a || !b || a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}

async function getActiveRegistration() {
    await navigator.serviceWorker.register('./sw.js');
    return navigator.serviceWorker.ready;
}

async function subscribeAndRegister(requestPermissionIfDefault = false) {
    if (!isPushSupported()) {
        return { ok: false, reason: 'unsupported' };
    }

    try {
        if (Notification.permission === 'denied') {
            return { ok: false, reason: 'denied' };
        }
        if (Notification.permission === 'default') {
            if (!requestPermissionIfDefault) {
                return { ok: false, reason: 'default' };
            }
            const perm = await Notification.requestPermission();
            if (perm !== 'granted') {
                return { ok: false, reason: perm };
            }
        }

        const swReg = await getActiveRegistration();

        // Fetch current VAPID public key from server
        const vKeyReq = await fetch('/api/notifications/vapid-public-key', { cache: 'no-store' });
        if (!vKeyReq.ok) {
            return { ok: false, reason: 'vapid-key-unavailable' };
        }
        const vKey = (await vKeyReq.text()).trim();
        if (!vKey) {
            return { ok: false, reason: 'vapid-key-empty' };
        }
        const currentKeyArray = urlBase64ToUint8Array(vKey);

        // Reuse existing browser subscription if its applicationServerKey still matches
        let subscription = await swReg.pushManager.getSubscription();
        if (subscription) {
            const existingKey = subscription.options && subscription.options.applicationServerKey
                ? new Uint8Array(subscription.options.applicationServerKey)
                : null;
            if (!keysEqual(currentKeyArray, existingKey)) {
                await subscription.unsubscribe();
                subscription = null;
            }
        }

        if (!subscription) {
            subscription = await swReg.pushManager.subscribe({
                userVisibleOnly: true,
                applicationServerKey: currentKeyArray
            });
        }

        const sessionId = auth.getSessionId();
        const resp = await fetch(`/api/notifications/subscribe-push?session_id=${encodeURIComponent(sessionId)}`, {
            method: 'POST',
            body: JSON.stringify(subscription),
            headers: { 'Content-Type': 'application/json' }
        });

        if (!resp.ok) {
            return { ok: false, reason: 'server-error' };
        }
        return { ok: true };
    } catch (err) {
        console.error('Web push setup failed:', err);
        return { ok: false, reason: err && err.message ? err.message : 'error' };
    }
}

async function enablePushNotifications() {
    return subscribeAndRegister(true);
}

async function syncPushSubscription() {
    // Only auto-sync silently when permission was already granted by the user
    if (!isPushSupported() || Notification.permission !== 'granted') {
        return { ok: false, reason: 'not-granted' };
    }
    return subscribeAndRegister(false);
}

async function disablePushNotifications() {
    if (!isPushSupported()) {
        return { ok: false, reason: 'unsupported' };
    }
    try {
        const swReg = await getActiveRegistration();
        const subscription = await swReg.pushManager.getSubscription();
        const endpoint = subscription ? subscription.endpoint : null;
        if (subscription) {
            await subscription.unsubscribe();
        }
        const sessionId = auth.getSessionId();
        await fetch(`/api/notifications/unsubscribe-push?session_id=${encodeURIComponent(sessionId)}`, {
            method: 'POST',
            body: JSON.stringify(endpoint ? { endpoint } : {}),
            headers: { 'Content-Type': 'application/json' }
        });
        return { ok: true };
    } catch (err) {
        console.error('Web push unsubscribe failed:', err);
        return { ok: false, reason: err && err.message ? err.message : 'error' };
    }
}

export default {
    isPushSupported,
    urlBase64ToUint8Array,
    enablePushNotifications,
    syncPushSubscription,
    disablePushNotifications
};
