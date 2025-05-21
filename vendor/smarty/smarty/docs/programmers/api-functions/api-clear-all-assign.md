clearAllAssign()

clears the values of all assigned variables

Description
===========

void

clearAllAssign


    <?php
    // passing name/value pairs
    $smarty->assign('Name', 'Fred');
    $smarty->assign('Address', $address);

    // will output above
    print_r( $smarty->getTemplateVars() );

    // clear all assigned variables
    $smarty->clearAllAssign();

    // will output nothing
    print_r( $smarty->getTemplateVars() );

    ?>

       

See also [`clearAssign()`](#api.clear.assign),
[`clearConfig()`](#api.clear.config),
[`getTemplateVars()`](#api.get.template.vars), [`assign()`](#api.assign)
and [`append()`](#api.append)
