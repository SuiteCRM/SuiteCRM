Controlling Cacheability of Output {#caching.cacheable}
==================================

If caching is enabled normally the whole final output of the page gets
cached. However Smarty3 offers several options how to exclude sections
of your output from caching.

> **Note**
>
> Be sure any variables used within a non-cached section are also
> assigned from PHP when the page is loaded from the cache.

Cacheability of Template Section {#cacheability.sections}
--------------------------------

A larger section of your template can easily excluded from caching by
using the [`{nocache}`](#language.function.nocache) and
[`{/nocache}`](#language.function.nocache) tags.



    Today's date is
    {nocache}
    {$smarty.now|date_format}
    {/nocache}

       

The above code will output the current date on a cached page.

Cacheability of Tags {#cacheability.tags}
--------------------

Caching for an individual tag can be disabled by adding the \"nocache\"
option flag to the tag.


    Today's date is
    {$smarty.now|date_format nocache}

        

Cacheability of Variables {#cacheability.variables}
-------------------------

You can [`assign()`](#api.assign) variables as not cachable. Any tag
which uses such variable will be automatically executed in nocache mode.

> **Note**
>
> If a tag is executed in nocache mode you must make sure that all other
> variables used by that tag are also assigned from PHP when the page is
> loaded from the cache.

> **Note**
>
> The nocache status of an assigned variable will effect the compiled
> template code. If you change the status you must manually delete
> existing compiled and cached template files to force a recompile.


    // assign $foo as nocahe variable
    $smarty->assign('foo',time(),true);


    Dynamic time value is {$foo}

        

Cacheability of Plugins {#cacheability.plugins}
-----------------------

The cacheability of plugins can be declared when registering them. The
third parameter to [`registerPlugin()`](#api.register.plugin) is called
`$cacheable` and defaults to TRUE.

When registering a plugin with `$cacheable=false` the plugin is called
everytime the page is displayed, even if the page comes from the cache.
The plugin function behaves a little like an
[`{insert}`](#plugins.inserts) function.

> **Note**
>
> The `$cacheable` status will effect the compiled template code. If you
> change the status you must manually delete existing compiled and
> cached template files to force a recompile.

In contrast to [`{insert}`](#plugins.inserts) the attributes to the
plugins are not cached by default. They can be declared to be cached
with the fourth parameter `$cache_attrs`. `$cache_attrs` is an array of
attribute-names that should be cached, so the plugin-function get value
as it was the time the page was written to cache everytime it is fetched
from the cache.


    <?php
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    function remaining_seconds($params, $smarty) {
        $remain = $params['endtime'] - time();
        if($remain >= 0){
            return $remain . ' second(s)';
        }else{
            return 'done';
        }
    }

    $smarty->registerPlugin('function','remaining', 'remaining_seconds', false, array('endtime'));

    if (!$smarty->isCached('index.tpl')) {
        // fetch $obj from db and assign...
        $smarty->assignByRef('obj', $obj);
    }

    $smarty->display('index.tpl');
    ?>

       

where `index.tpl` is:


    Time Remaining: {remaining endtime=$obj->endtime}

       

The number of seconds till the endtime of `$obj` is reached changes on
each display of the page, even if the page is cached. Since the endtime
attribute is cached the object only has to be pulled from the database
when page is written to the cache but not on subsequent requests of the
page.


    index.php:

    <?php
    $smarty->setCaching(Smarty::CACHING_LIFETIME_CURRENT);

    function smarty_block_dynamic($param, $content, $smarty) {
        return $content;
    }
    $smarty->registerPlugin('block','dynamic', 'smarty_block_dynamic', false);

    $smarty->display('index.tpl');
    ?>

       

where `index.tpl` is:


    Page created: {'0'|date_format:'%D %H:%M:%S'}

    {dynamic}

    Now is: {'0'|date_format:'%D %H:%M:%S'}

    ... do other stuff ...

    {/dynamic}

       

When reloading the page you will notice that both dates differ. One is
"dynamic" one is "static". You can do everything between
`{dynamic}...{/dynamic}` and be sure it will not be cached like the rest
of the page.

> **Note**
>
> The above example shall just demonstrate how a dynamic block plugins
> works. See
> [`Cacheability of Template Section`](#cacheability.sections) on how to
> disable caching of a template section by the built-in
> [`{nocache}`](#language.function.nocache) and
> [`{/nocache}`](#language.function.nocache) tags.
