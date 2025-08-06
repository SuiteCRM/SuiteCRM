/**
 * Service Worker for SuiteCRM Mobile Real Estate
 * Handles offline functionality and caching
 */

const CACHE_NAME = 'suitecrm-mobile-v1';
const urlsToCache = [
    '/',
    '/themes/MobileRealEstate/css/mobile-realestate.css',
    '/themes/MobileRealEstate/js/mobile-app.js',
    '/themes/MobileRealEstate/images/icon-192.png',
    '/themes/MobileRealEstate/images/property-placeholder.jpg',
    '/include/javascript/jquery/jquery-min.js',
    '/offline.html'
];

// Install event - cache resources
self.addEventListener('install', event => {
    event.waitUntil(
        caches.open(CACHE_NAME)
            .then(cache => cache.addAll(urlsToCache))
    );
});

// Fetch event - serve from cache when offline
self.addEventListener('fetch', event => {
    event.respondWith(
        caches.match(event.request)
            .then(response => {
                // Cache hit - return response
                if (response) {
                    return response;
                }
                
                // Clone the request
                const fetchRequest = event.request.clone();
                
                return fetch(fetchRequest).then(response => {
                    // Check if valid response
                    if (!response || response.status !== 200 || response.type !== 'basic') {
                        return response;
                    }
                    
                    // Clone the response
                    const responseToCache = response.clone();
                    
                    caches.open(CACHE_NAME)
                        .then(cache => {
                            cache.put(event.request, responseToCache);
                        });
                    
                    return response;
                });
            })
            .catch(() => {
                // Offline fallback
                if (event.request.destination === 'document') {
                    return caches.match('/offline.html');
                }
            })
    );
});

// Activate event - clean up old caches
self.addEventListener('activate', event => {
    const cacheWhitelist = [CACHE_NAME];
    
    event.waitUntil(
        caches.keys().then(cacheNames => {
            return Promise.all(
                cacheNames.map(cacheName => {
                    if (cacheWhitelist.indexOf(cacheName) === -1) {
                        return caches.delete(cacheName);
                    }
                })
            );
        })
    );
});

// Background sync for offline data
self.addEventListener('sync', event => {
    if (event.tag === 'sync-properties') {
        event.waitUntil(syncOfflineData());
    }
});

// Push notifications
self.addEventListener('push', event => {
    const options = {
        body: event.data.text(),
        icon: '/themes/MobileRealEstate/images/icon-192.png',
        badge: '/themes/MobileRealEstate/images/badge-72.png',
        vibrate: [200, 100, 200],
        data: {
            dateOfArrival: Date.now(),
            primaryKey: 1
        }
    };
    
    event.waitUntil(
        self.registration.showNotification('New Property Alert', options)
    );
});

// Notification click handler
self.addEventListener('notificationclick', event => {
    event.notification.close();
    
    event.waitUntil(
        clients.openWindow('/')
    );
});

// Helper function to sync offline data
async function syncOfflineData() {
    const cache = await caches.open(CACHE_NAME);
    const requests = await cache.keys();
    
    const offlineRequests = requests.filter(request => 
        request.url.includes('offline=true')
    );
    
    for (const request of offlineRequests) {
        try {
            const response = await fetch(request);
            if (response.ok) {
                await cache.delete(request);
            }
        } catch (error) {
            console.error('Sync failed:', error);
        }
    }
}