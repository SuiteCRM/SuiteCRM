\$caching {#variable.caching}
=========

This tells Smarty whether or not to cache the output of the templates to
the [`$cache_dir`](#variable.cache.dir). By default this is set to the
constant Smarty::CACHING\_OFF. If your templates consistently generate
the same content, it is advisable to turn on `$caching`, as this may
result in significant performance gains.

You can also have [multiple](#caching.multiple.caches) caches for the
same template.

-   A constant value of Smarty::CACHING\_LIFETIME\_CURRENT or
    Smarty::CACHING\_LIFETIME\_SAVED enables caching.

-   A value of Smarty::CACHING\_LIFETIME\_CURRENT tells Smarty to use
    the current [`$cache_lifetime`](#variable.cache.lifetime) variable
    to determine if the cache has expired.

-   A value of Smarty::CACHING\_LIFETIME\_SAVED tells Smarty to use the
    [`$cache_lifetime`](#variable.cache.lifetime) value at the time the
    cache was generated. This way you can set the
    [`$cache_lifetime`](#variable.cache.lifetime) just before
    [fetching](#api.fetch) the template to have granular control over
    when that particular cache expires. See also
    [`isCached()`](#api.is.cached).

-   If [`$compile_check`](#variable.compile.check) is enabled, the
    cached content will be regenerated if any of the templates or config
    files that are part of this cache are changed.

-   If [`$force_compile`](#variable.force.compile) is enabled, the
    cached content will always be regenerated.

See also [`$cache_dir`](#variable.cache.dir),
[`$cache_lifetime`](#variable.cache.lifetime),
[`$cache_modified_check`](#variable.cache.modified.check),
[`is_cached()`](#api.is.cached) and the [caching section](#caching).
