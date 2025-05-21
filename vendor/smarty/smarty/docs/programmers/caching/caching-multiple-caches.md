Multiple Caches Per Page {#caching.multiple.caches}
========================

You can have multiple cache files for a single call to
[`display()`](#api.display) or [`fetch()`](#api.fetch). Let\'s say that
a call to `display('index.tpl')` may have several different output
contents depending on some condition, and you want separate caches for
each one. You can do this by passing a `$cache_id` as the second
parameter to the function call.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    $my_cache_id = $_GET['article_id'];

    $smarty->display('index.tpl', $my_cache_id);
    ?>

         

Above, we are passing the variable `$my_cache_id` to
[`display()`](#api.display) as the `$cache_id`. For each unique value of
`$my_cache_id`, a separate cache will be generated for `index.tpl`. In
this example, `article_id` was passed in the URL and is used as the
`$cache_id`.

> **Note**
>
> Be very cautious when passing values from a client (web browser) into
> Smarty or any PHP application. Although the above example of using the
> article\_id from the URL looks handy, it could have bad consequences.
> The `$cache_id` is used to create a directory on the file system, so
> if the user decided to pass an extremely large value for article\_id,
> or write a script that sends random article\_id\'s at a rapid pace,
> this could possibly cause problems at the server level. Be sure to
> sanitize any data passed in before using it. In this instance, maybe
> you know the article\_id has a length of ten characters and is made up
> of alpha-numerics only, and must be a valid article\_id in the
> database. Check for this!

Be sure to pass the same `$cache_id` as the second parameter to
[`isCached()`](#api.is.cached) and [`clearCache()`](#api.clear.cache).


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    $my_cache_id = $_GET['article_id'];

    if(!$smarty->isCached('index.tpl',$my_cache_id)) {
        // No cache available, do variable assignments here.
        $contents = get_database_contents();
        $smarty->assign($contents);
    }

    $smarty->display('index.tpl',$my_cache_id);
    ?>

         

You can clear all caches for a particular `$cache_id` by passing NULL as
the first parameter to [`clearCache()`](#api.clear.cache).


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    // clear all caches with "sports" as the $cache_id
    $smarty->clearCache(null,'sports');

    $smarty->display('index.tpl','sports');
    ?>

         

In this manner, you can "group" your caches together by giving them the
same `$cache_id`.
