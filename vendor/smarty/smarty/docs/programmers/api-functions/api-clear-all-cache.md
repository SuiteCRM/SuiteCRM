clearAllCache()

clears the entire template cache

Description
===========

void

clearAllCache

int

expire\_time

As an optional parameter, you can supply a minimum age in seconds the
cache files must be before they will get cleared.

> **Note**
>
> Since Smarty version 3.1.14 it is possible to delete cache files by
> their individual expiration time at creation by passing constant
> SMARTY::CLEAR\_EXPIRED as `expire_time` parameter.


    <?php
    // clear the entire cache
    $smarty->clearAllCache();

    // clears all files over one hour old
    $smarty->clearAllCache(3600);
    ?>

       

See also [`clearCache()`](#api.clear.cache),
[`isCached()`](#api.is.cached) and the [caching](#caching) page.
