getTemplateVars()

returns assigned variable value(s)

Description
===========

array

getTemplateVars

string

varname

If no parameter is given, an array of all [assigned](#api.assign)
variables are returned.


    <?php
    // get assigned template var 'foo'
    $myVar = $smarty->getTemplateVars('foo');

    // get all assigned template vars
    $all_tpl_vars = $smarty->getTemplateVars();

    // take a look at them
    print_r($all_tpl_vars);
    ?>

       

See also [`assign()`](#api.assign),
[`{assign}`](#language.function.assign), [`append()`](#api.append),
[`clearAssign()`](#api.clear.assign),
[`clearAllAssign()`](#api.clear.all.assign) and
[`getConfigVars()`](#api.get.config.vars)
