const CACHE_NAME = 'lisp-chat-v5';
const PUSH_META_CACHE = 'lisp-chat-push-meta';
const PUSH_SESSION_KEY = '/__lisp_chat_push_session__';
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
          if (cacheName !== CACHE_NAME && cacheName !== PUSH_META_CACHE) {
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

function getDmPeerFromUrl(urlString) {
  try {
    const parsed = new URL(urlString);
    const dm = parsed.searchParams.get('dm');
    return dm ? dm.replace(/^@/, '').toLowerCase() : '';
  } catch (e) {
    return '';
  }
}

function getChannelFromUrl(urlString) {
  try {
    const parsed = new URL(urlString);
    let ch = parsed.search.substring(1).split('&')[0];
    if (!ch || ch.includes('=')) return 'general';
    try { ch = decodeURIComponent(ch); } catch (e) { /* keep raw */ }
    return ch.replace(/^#/, '').toLowerCase();
  } catch (e) {
    return 'general';
  }
}

function urlBase64ToUint8Array(base64String) {
  const padding = '='.repeat((4 - base64String.length % 4) % 4);
  const base64 = (base64String + padding).replace(/-/g, '+').replace(/_/g, '/');
  const rawData = atob(base64);
  const outputArray = new Uint8Array(rawData.length);
  for (let i = 0; i < rawData.length; ++i) {
    outputArray[i] = rawData.charCodeAt(i);
  }
  return outputArray;
}

self.addEventListener('push', function(event) {
  let title = "New mention in Lisp Chat";
  let body = "";
  let channel = "#general";
  let from = "";
  let url = null;
  if (event.data) {
    try {
      const data = event.data.json();
      if (data.title) title = data.title;
      if (data.body) body = data.body;
      if (data.channel) channel = data.channel;
      if (data.from) from = data.from;
      if (data.url) url = data.url;
    } catch (e) {
      body = event.data.text();
    }
  }
  const isDm = channel.toLowerCase().startsWith('dm:');
  const channelName = channel.replace(/^#/, '');
  const targetChannelLower = channelName.toLowerCase();
  const targetPath = url || (isDm && from ? ('/?dm=' + encodeURIComponent(from)) : ('/?' + encodeURIComponent(channelName)));

  event.waitUntil(
    clients.matchAll({ type: 'window', includeUncontrolled: true }).then((windowClients) => {
      const isViewingChannel = windowClients.some((client) => {
        if (!client.url.startsWith(self.location.origin)) return false;
        const active = client.focused || client.visibilityState === 'visible';
        if (!active) return false;
        if (isDm) {
          return from && getDmPeerFromUrl(client.url) === from.toLowerCase();
        }
        return !getDmPeerFromUrl(client.url) && getChannelFromUrl(client.url) === targetChannelLower;
      });
      if (isViewingChannel) {
        return;
      }
      return self.registration.showNotification(title, {
        body: body,
        icon: '/logo.png',
        badge: '/logo.png',
        tag: isDm && from ? ('lisp-chat-dm:' + from.toLowerCase()) : ('lisp-chat-' + channel),
        renotify: true,
        data: { channel: channel, from: from, url: targetPath }
      });
    })
  );
});

self.addEventListener('pushsubscriptionchange', (event) => {
  event.waitUntil((async () => {
    try {
      const cache = await caches.open(PUSH_META_CACHE);
      const metaResp = await cache.match(PUSH_SESSION_KEY);
      if (!metaResp) return;
      const meta = await metaResp.json();
      const sessionId = meta && meta.sessionId;
      if (!sessionId) return;

      let newSub = event.newSubscription;
      if (!newSub) {
        const vKeyResp = await fetch('/api/notifications/vapid-public-key', { cache: 'no-store' });
        if (!vKeyResp.ok) return;
        const vKey = (await vKeyResp.text()).trim();
        if (!vKey) return;
        newSub = await self.registration.pushManager.subscribe({
          userVisibleOnly: true,
          applicationServerKey: urlBase64ToUint8Array(vKey)
        });
      }

      if (event.oldSubscription && event.oldSubscription.endpoint &&
          event.oldSubscription.endpoint !== newSub.endpoint) {
        await fetch('/api/notifications/unsubscribe-push?session_id=' + encodeURIComponent(sessionId), {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ endpoint: event.oldSubscription.endpoint })
        });
      }

      await fetch('/api/notifications/subscribe-push?session_id=' + encodeURIComponent(sessionId), {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(newSub)
      });
    } catch (e) {
      // Best-effort silent re-subscription; client will also sync on next open
    }
  })());
});

self.addEventListener('notificationclick', (event) => {
  event.notification.close();
  const channel = (event.notification.data && event.notification.data.channel) || '#general';
  const from = (event.notification.data && event.notification.data.from) || '';
  const channelName = channel.replace(/^#/, '');
  const rawUrl = (event.notification.data && event.notification.data.url) || ('/?' + encodeURIComponent(channelName));
  const targetUrl = new URL(rawUrl, self.location.origin).href;
  event.waitUntil(
    clients.matchAll({ type: 'window', includeUncontrolled: true }).then((windowClients) => {
      for (let i = 0; i < windowClients.length; i++) {
        const client = windowClients[i];
        if (client.url.startsWith(self.location.origin) && 'focus' in client) {
          client.postMessage({ type: 'NAVIGATE_CHANNEL', channel: channel, from: from, url: targetUrl });
          return client.focus();
        }
      }
      if (clients.openWindow) {
        return clients.openWindow(targetUrl);
      }
    })
  );
});

