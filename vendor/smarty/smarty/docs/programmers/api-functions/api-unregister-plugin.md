unregisterPlugin

dynamically unregister plugins

Description
===========

void

unregisterPlugin

string

type

string

name

This method unregisters plugins which previously have been registered by
[registerPlugin()](#api.register.plugin), It uses the following
parameters:

<!-- -->


    <?php

    // we don't want template designers to have access to function plugin "date_now" 
    $smarty->unregisterPlugin("function","date_now");

    ?>

       

See also [`registerPlugin()`](#api.register.plugin).
