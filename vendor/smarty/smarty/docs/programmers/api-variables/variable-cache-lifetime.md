\$cache\_lifetime {#variable.cache.lifetime}
=================

This is the length of time in seconds that a template cache is valid.
Once this time has expired, the cache will be regenerated.

-   `$caching` must be turned on (either
    Smarty::CACHING\_LIFETIME\_CURRENT or
    Smarty::CACHING\_LIFETIME\_SAVED) for `$cache_lifetime` to have any
    purpose.

-   A `$cache_lifetime` value of -1 will force the cache to never
    expire.

-   A value of 0 will cause the cache to always regenerate (good for
    testing only, to disable caching a more efficient method is to set
    [`$caching`](#variable.caching) = Smarty::CACHING\_OFF).

-   If you want to give certain templates their own cache lifetime, you
    could do this by setting [`$caching`](#variable.caching) =
    Smarty::CACHING\_LIFETIME\_SAVED, then set `$cache_lifetime` to a
    unique value just before calling [`display()`](#api.display) or
    [`fetch()`](#api.fetch).

If [`$force_compile`](#variable.force.compile) is enabled, the cache
files will be regenerated every time, effectively disabling caching. You
can clear all the cache files with the
[`clear_all_cache()`](#api.clear.all.cache) function, or individual
cache files (or groups) with the [`clear_cache()`](#api.clear.cache)
function.
