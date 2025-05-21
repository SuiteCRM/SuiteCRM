Cache Groups {#caching.groups}
============

You can do more elaborate grouping by setting up `$cache_id` groups.
This is accomplished by separating each sub-group with a vertical bar
`|` in the `$cache_id` value. You can have as many sub-groups as you
like.

-   You can think of cache groups like a directory hierarchy. For
    instance, a cache group of `'a|b|c'` could be thought of as the
    directory structure `'/a/b/c/'`.

-   `clearCache(null,'a|b|c')` would be like removing the files
    `'/a/b/c/*'`. `clearCache(null,'a|b')` would be like removing the
    files `'/a/b/*'`.

-   If you specify a [`$compile_id`](#variable.compile.id) such as
    `clearCache(null,'a|b','foo')` it is treated as an appended cache
    group `'/a/b/c/foo/'`.

-   If you specify a template name such as
    `clearCache('foo.tpl','a|b|c')` then Smarty will attempt to remove
    `'/a/b/c/foo.tpl'`.

-   You CANNOT remove a specified template name under multiple cache
    groups such as `'/a/b/*/foo.tpl'`, the cache grouping works
    left-to-right ONLY. You will need to group your templates under a
    single cache group hierarchy to be able to clear them as a group.

Cache grouping should not be confused with your template directory
hierarchy, the cache grouping has no knowledge of how your templates are
structured. So for example, if you have a template structure like
`themes/blue/index.tpl` and you want to be able to clear all the cache
files for the "blue" theme, you will need to create a cache group
structure that mimics your template file structure, such as
`display('themes/blue/index.tpl','themes|blue')`, then clear them with
`clearCache(null,'themes|blue')`.


    <?php
    require('Smarty.class.php');
    $smarty = new Smarty;

    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    // clear all caches with 'sports|basketball' as the first two cache_id groups
    $smarty->clearCache(null,'sports|basketball');

    // clear all caches with "sports" as the first cache_id group. This would
    // include "sports|basketball", or "sports|(anything)|(anything)|(anything)|..."
    $smarty->clearCache(null,'sports');

    // clear the foo.tpl cache file with "sports|basketball" as the cache_id
    $smarty->clearCache('foo.tpl','sports|basketball');


    $smarty->display('index.tpl','sports|basketball');
    ?>

          
