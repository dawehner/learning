self.addEventListener('install', async () => {
  const cache = await caches.open('my-test');
  cache.addAll([
    '/index.html',
    '/offline.html',
  ]);
});

self.addEventListener('fetch', (event) => {
  // fallback to offline.html for any HTML request, so we can display at least something.
  if (event.request.headers.get('accept').includes('text/html')) {
    event.respondWith(
      caches.match(event.request)
        .then(function(response) {
          // Cache hit - return response
          if (response) {
            return response;
          }
          return fetch(event.request);
        }
      )
      .catch(() => {
        return caches.match('offline.html');
      })
    );
  }
})