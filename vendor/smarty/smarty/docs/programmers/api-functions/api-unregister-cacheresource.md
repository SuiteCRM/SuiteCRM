unregisterCacheResource()

dynamically unregister a CacheResource plugin

Description
===========

void

unregisterCacheResource

string

name

Pass in the `name` of the CacheResource.


    <?php

    $smarty->unregisterCacheResource('mysql');

    ?>

       

See also [`registerCacheResource()`](#api.register.cacheresource) and
the [Custom CacheResource Implementation](#caching.custom) section.
