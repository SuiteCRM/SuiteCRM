\$cache\_modified\_check {#variable.cache.modified.check}
========================

If set to TRUE, Smarty will respect the If-Modified-Since header sent
from the client. If the cached file timestamp has not changed since the
last visit, then a `'304: Not Modified'` header will be sent instead of
the content. This works only on cached content without
[`{insert}`](#language.function.insert) tags.

See also [`$caching`](#variable.caching),
[`$cache_lifetime`](#variable.cache.lifetime), and the [caching
section](#caching).
