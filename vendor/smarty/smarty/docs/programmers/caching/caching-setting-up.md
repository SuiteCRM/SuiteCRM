Setting Up Caching {#caching.setting.up}
==================

The first thing to do is enable caching by setting
[`$caching`](#variable.caching) to one of
`Smarty::CACHING_LIFETIME_CURRENT` or `Smarty::CACHING_LIFETIME_SAVED`.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    // uses the value of $smarty->cacheLifetime() to determine
    // the number of seconds a cache is good for
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    $smarty->display('index.tpl');
    ?>

        

With caching enabled, the function call to `display('index.tpl')` will
render the template as usual, but also saves a copy of its output to a
file (a cached copy) in the [`$cache_dir`](#variable.cache.dir). On the
next call to `display('index.tpl')`, the cached copy will be used
instead of rendering the template again.

> **Note**
>
> The files in the [`$cache_dir`](#variable.cache.dir) are named similar
> to the template name. Although they end in the `.php` extension, they
> are not intended to be directly executable. Do not edit these files!

Each cached page has a limited lifetime determined by
[`$cache_lifetime`](#variable.cache.lifetime). The default value is 3600
seconds, or one hour. After that time expires, the cache is regenerated.
It is possible to give individual caches their own expiration time by
setting [`$caching`](#variable.caching) to
`Smarty::CACHING_LIFETIME_SAVED`. See
[`$cache_lifetime`](#variable.cache.lifetime) for more details.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    // retain current cache lifetime for each specific display call
    $smarty->setCaching(Smarty::CACHING_LIFETIME_SAVED);

    // set the cache_lifetime for index.tpl to 5 minutes
    $smarty->setCacheLifetime(300);
    $smarty->display('index.tpl');

    // set the cache_lifetime for home.tpl to 1 hour
    $smarty->setCacheLifetime(3600);
    $smarty->display('home.tpl');

    // NOTE: the following $cache_lifetime setting will not work when $caching
    // is set to Smarty::CACHING_LIFETIME_SAVED.
    // The cache lifetime for home.tpl has already been set
    // to 1 hour, and will no longer respect the value of $cache_lifetime.
    // The home.tpl cache will still expire after 1 hour.
    $smarty->setCacheLifetime(30); // 30 seconds
    $smarty->display('home.tpl');
    ?>

        

If [`$compile_check`](#variable.compile.check) is enabled (default),
every template file and config file that is involved with the cache file
is checked for modification. If any of the files have been modified
since the cache was generated, the cache is immediately regenerated.
This is a computational overhead, so for optimum performance set
[`$compile_check`](#variable.compile.check) to FALSE.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);
    $smarty->setCompileCheck(false);

    $smarty->display('index.tpl');
    ?>

        

If [`$force_compile`](#variable.force.compile) is enabled, the cache
files will always be regenerated. This effectively disables caching,
however this also seriously degrades performance.
[`$force_compile`](#variable.force.compile) is meant to be used for
[debugging](#chapter.debugging.console) purposes. The appropriate way to
disable caching is to set [`$caching`](#variable.caching) to
Smarty::CACHING\_OFF.

The [`isCached()`](#api.is.cached) function can be used to test if a
template has a valid cache or not. If you have a cached template that
requires something like a database fetch, you can use this to skip that
process.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    if(!$smarty->isCached('index.tpl')) {
        // No cache available, do variable assignments here.
        $contents = get_database_contents();
        $smarty->assign($contents);
    }

    $smarty->display('index.tpl');
    ?>

        

You can keep parts of a page dynamic (disable caching) with the
[`{nocache}{/nocache}`](#language.function.nocache) block function, the
[`{insert}`](#language.function.insert) function, or by using the
`nocache` parameter for most template functions.

Let\'s say the whole page can be cached except for a banner that is
displayed down the side of the page. By using the
[`{insert}`](#language.function.insert) function for the banner, you can
keep this element dynamic within the cached content. See the
documentation on [`{insert}`](#language.function.insert) for more
details and examples.

You can clear all the cache files with the
[`clearAllCache()`](#api.clear.all.cache) function, or individual cache
files [and groups](#caching.groups) with the
[`clearCache()`](#api.clear.cache) function.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    // clear only cache for index.tpl
    $smarty->clearCache('index.tpl');

    // clear out all cache files
    $smarty->clearAllCache();

    $smarty->display('index.tpl');
    ?>

        
