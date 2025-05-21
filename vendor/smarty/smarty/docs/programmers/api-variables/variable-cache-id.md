\$cache\_id {#variable.cache.id}
===========

Persistent cache\_id identifier. As an alternative to passing the same
`$cache_id` to each and every function call, you can set this
`$cache_id` and it will be used implicitly thereafter.

With a `$cache_id` you can have multiple cache files for a single call
to [`display()`](#api.display) or [`fetch()`](#api.fetch) depending for
example from different content of the same template. See the [caching
section](#caching) for more information.
