getRegisteredObject()

returns a reference to a registered object

Description
===========

array

getRegisteredObject

string

object\_name

This is useful from within a custom function when you need direct access
to a [registered object](#api.register.object). See the
[objects](#advanced.features.objects) page for more info.


    <?php
    function smarty_block_foo($params, $smarty)
    {
      if (isset($params['object'])) {
        // get reference to registered object
        $obj_ref = $smarty->getRegisteredObject($params['object']);
        // use $obj_ref is now a reference to the object
      }
    }
    ?>

       

See also [`registerObject()`](#api.register.object),
[`unregisterObject()`](#api.unregister.object) and [objects
page](#advanced.features.objects)
