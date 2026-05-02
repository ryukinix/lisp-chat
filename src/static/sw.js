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

self.addEventListener('push', function(event) {
  let title = "New message in Lisp Chat!";
  let body = "";
  if (event.data) {
     body = event.data.text();
  }
  event.waitUntil(
    self.registration.showNotification(title, {
      body: body,
      icon: '/logo.png'
    })
  );
});

self.addEventListener('notificationclick', (event) => {
    event.notification.close();
    let url = '/';
    event.waitUntil(
        clients.matchAll({ type: 'window' }).then((windowClients) => {
            for (var i = 0; i < windowClients.length; i++) {
                var client = windowClients[i];
                if (client.url.includes('/') && 'focus' in client) {
                    return client.focus();
                }
            }
            if (clients.openWindow) {
                return clients.openWindow(url);
            }
        })
    );
});

