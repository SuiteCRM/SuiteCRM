isCached()

returns true if there is a valid cache for this template

Description
===========

bool

isCached

string

template

string

cache\_id

string

compile\_id

-   This only works if [`$caching`](#variable.caching) is set to one of
    `Smarty::CACHING_LIFETIME_CURRENT` or
    `Smarty::CACHING_LIFETIME_SAVED` to enable caching. See the [caching
    section](#caching) for more info.

-   You can also pass a `$cache_id` as an optional second parameter in
    case you want [multiple caches](#caching.multiple.caches) for the
    given template.

-   You can supply a [`$compile id`](#variable.compile.id) as an
    optional third parameter. If you omit that parameter the persistent
    [`$compile_id`](#variable.compile.id) is used if its set.

-   If you do not want to pass a `$cache_id` but want to pass a
    [`$compile_id`](#variable.compile.id) you have to pass NULL as a
    `$cache_id`.

> **Note**
>
> If `isCached()` returns TRUE it actually loads the cached output and
> stores it internally. Any subsequent call to
> [`display()`](#api.display) or [`fetch()`](#api.fetch) will return
> this internally stored output and does not try to reload the cache
> file. This prevents a race condition that may occur when a second
> process clears the cache between the calls to `isCached()` and to
> [`display()`](#api.display) in the example above. This also means
> calls to [`clearCache()`](#api.clear.cache) and other changes of the
> cache-settings may have no effect after `isCached()` returned TRUE.


    <?php
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    if(!$smarty->isCached('index.tpl')) {
    // do database calls, assign vars here
    }

    $smarty->display('index.tpl');
    ?>

       


    <?php
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    if(!$smarty->isCached('index.tpl', 'FrontPage')) {
      // do database calls, assign vars here
    }

    $smarty->display('index.tpl', 'FrontPage');
    ?>

       

See also [`clearCache()`](#api.clear.cache),
[`clearAllCache()`](#api.clear.all.cache), and [caching
section](#caching).
