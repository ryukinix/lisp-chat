const CACHE_NAME = 'lisp-chat-v3';
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
    }).then(() => self.clients.claim())
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
  let title = "New mention in Lisp Chat";
  let body = "";
  let channel = "#general";
  let url = null;
  if (event.data) {
    try {
      const data = event.data.json();
      if (data.title) title = data.title;
      if (data.body) body = data.body;
      if (data.channel) channel = data.channel;
      if (data.url) url = data.url;
    } catch (e) {
      body = event.data.text();
    }
  }
  const channelName = channel.replace(/^#/, '');
  const targetPath = url || ('/?' + encodeURIComponent(channelName));
  event.waitUntil(
    self.registration.showNotification(title, {
      body: body,
      icon: '/logo.png',
      badge: '/logo.png',
      tag: 'lisp-chat-' + channel,
      data: { channel: channel, url: targetPath }
    })
  );
});

self.addEventListener('notificationclick', (event) => {
  event.notification.close();
  const channel = (event.notification.data && event.notification.data.channel) || '#general';
  const channelName = channel.replace(/^#/, '');
  const rawUrl = (event.notification.data && event.notification.data.url) || ('/?' + encodeURIComponent(channelName));
  const targetUrl = new URL(rawUrl, self.location.origin).href;
  event.waitUntil(
    clients.matchAll({ type: 'window', includeUncontrolled: true }).then((windowClients) => {
      for (let i = 0; i < windowClients.length; i++) {
        const client = windowClients[i];
        if (client.url.startsWith(self.location.origin) && 'focus' in client) {
          client.postMessage({ type: 'NAVIGATE_CHANNEL', channel: channel, url: targetUrl });
          return client.focus();
        }
      }
      if (clients.openWindow) {
        return clients.openWindow(targetUrl);
      }
    })
  );
});

