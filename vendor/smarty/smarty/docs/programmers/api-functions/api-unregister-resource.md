unregisterResource()

dynamically unregister a resource plugin

Description
===========

void

unregisterResource

string

name

Pass in the `name` of the resource.


    <?php

    $smarty->unregisterResource('db');

    ?>

       

See also [`registerResource()`](#api.register.resource) and [template
resources](#resources)
