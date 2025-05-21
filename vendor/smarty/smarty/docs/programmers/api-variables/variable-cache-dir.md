\$cache\_dir {#variable.cache.dir}
============

This is the name of the directory where template caches are stored. By
default this is `./cache`, meaning that Smarty will look for the
`cache/` directory in the same directory as the executing php script.
**This directory must be writeable by the web server**, [see
install](#installing.smarty.basic) for more info.

You can also use your own [custom cache implementation](#caching.custom)
to control cache files, which will ignore this setting. See also
[`$use_sub_dirs`](#variable.use.sub.dirs).

> **Note**
>
> This setting must be either a relative or absolute path. include\_path
> is not used for writing files.

> **Note**
>
> It is not recommended to put this directory under the web server
> document root.

> **Note**
>
> As of Smarty 3.1 the attribute \$cache\_dir is no longer accessible
> directly. Use [`getCacheDir()`](#api.get.cache.dir) and
> [`setCacheDir()`](#api.set.cache.dir) instead.

See also [`getCacheDir()`](#api.get.cache.dir),
[`setCacheDir()`](#api.set.cache.dir), [`$caching`](#variable.caching),
[`$use_sub_dirs`](#variable.use.sub.dirs),
[`$cache_lifetime`](#variable.cache.lifetime),
[`$cache_modified_check`](#variable.cache.modified.check) and the
[caching section](#caching).
