\$cache\_locking {#variable.cache.locking}
================

Cache locking avoids concurrent cache generation. This means resource
intensive pages can be generated only once, even if they\'ve been
requested multiple times in the same moment.

Cache locking is disabled by default. To enable it set `$cache_locking`
to TRUE.

See also [`$locking_timeout`](#variable.locking.timeout)
