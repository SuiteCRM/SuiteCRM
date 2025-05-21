registerCacheResource()

dynamically register CacheResources

Description
===========

void

registerCacheResource

string

name

Smarty\_CacheResource

resource\_handler

Use this to dynamically register a [CacheResource
plugin](#caching.custom) with Smarty. Pass in the `name` of the
CacheResource and the object extending Smarty\_CacheResource. See
[Custom Cache Implementation](#caching.custom) for more information on
how to create custom CacheResources.

> **Note**
>
> In Smarty2 this used to be a callback function called
> `$cache_handler_func`. Smarty3 replaced this callback by the
> `Smarty_CacheResource` module.


    <?php
    $smarty->registerCacheResource('mysql', new Smarty_CacheResource_Mysql());
    ?>

       

See also [`unregisterCacheResource()`](#api.unregister.cacheresource)
and the [Custom CacheResource Implementation](#caching.custom) section.
