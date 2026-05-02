const CACHE_NAME = 'lisp-chat-v2';
const ASSETS = [
  './',
  './index.html',
  './style.css',
  './main.js',
  './logo.png'
];

self.addEventListener('install', (event) => {
  self.skipWaiting();
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then((cache) => cache.addAll(ASSETS))
  );
});

self.addEventListener('activate', (event) => {
  // Clear old caches when a new service worker is activated to ensure users get the latest version
  event.waitUntil(
    caches.keys().then((cacheNames) => {
      return Promise.all(
        cacheNames.map((cacheName) => {
          if (cacheName !== CACHE_NAME) {
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});

self.addEventListener('fetch', (event) => {
  if (event.request.method !== 'GET') return;
  
  event.respondWith(
    caches.match(event.request)
      .then((response) => response || fetch(event.request).then((fetchResponse) => {
        // Optional: Cache dynamically fetched modules if you want, but for now just pass through
        return fetchResponse;
      }))
  );
});

let username = null;
let pollInterval = null;

self.addEventListener('message', (event) => {
    if (event.data && event.data.type === 'SET_USERNAME') {
        username = event.data.username;
        if (!pollInterval && username) {
            startPolling();
        }
    } else if (event.data && event.data.type === 'CLEAR_USERNAME') {
        username = null;
        if (pollInterval) {
            clearInterval(pollInterval);
            pollInterval = null;
        }
    }
});

function startPolling() {
    pollInterval = setInterval(async () => {
        if (!username) return;

        try {
            const response = await fetch(`/api/notifications?user=${encodeURIComponent(username)}&clear=true`);
            if (response.ok) {
                const data = await response.json();
                if (data.notifications && data.notifications.length > 0) {
                    for (const notif of data.notifications) {
                        self.registration.showNotification(`Mention from @${notif.from}`, {
                            body: notif.content,
                            icon: '/logo.png',
                            data: { channel: notif.channel }
                        });
                    }
                }
            }
        } catch (e) {
            console.error("Failed to poll notifications:", e);
        }
    }, 5000); // Poll every 5 seconds. Note: Mobile OSes (Android/iOS) heavily restrict background Service Worker execution. This will mostly work on Desktop or when the PWA is active.
}

self.addEventListener('notificationclick', (event) => {
    event.notification.close();
    const channel = event.notification.data ? event.notification.data.channel : null;
    let url = '/';
    if (channel) {
        url = `/?${encodeURIComponent(channel)}`;
    }

    event.waitUntil(
        clients.matchAll({ type: 'window' }).then((windowClients) => {
            for (var i = 0; i < windowClients.length; i++) {
                var client = windowClients[i];
                if (client.url.includes('/') && 'focus' in client) {
                    // Update the channel if necessary via postMessage or just focus
                    client.postMessage({ type: 'NAVIGATE_CHANNEL', channel: channel });
                    return client.focus();
                }
            }
            if (clients.openWindow) {
                return clients.openWindow(url);
            }
        })
    );
});

