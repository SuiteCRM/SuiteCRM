clearAssign()

clears the value of an assigned variable

Description
===========

void

clearAssign

mixed

var

This can be a single value, or an array of values.


    <?php
    // clear a single variable
    $smarty->clearAssign('Name');

    // clears multiple variables
    $smarty->clearAssign(array('Name', 'Address', 'Zip'));
    ?>

       

See also [`clearAllAssign()`](#api.clear.all.assign),
[`clearConfig()`](#api.clear.config),
[`getTemplateVars()`](#api.get.template.vars), [`assign()`](#api.assign)
and [`append()`](#api.append)
