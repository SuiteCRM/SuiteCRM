getConfigVars()

returns the given loaded config variable value

Description
===========

array

getConfigVars

string

varname

If no parameter is given, an array of all loaded [config
variables](#language.config.variables) is returned.


    <?php

    // get loaded config template var #foo#
    $myVar = $smarty->getConfigVars('foo');

    // get all loaded config template vars
    $all_config_vars = $smarty->getConfigVars();

    // take a look at them
    print_r($all_config_vars);
    ?>

       

See also [`clearConfig()`](#api.clear.config),
[`{config_load}`](#language.function.config.load),
[`configLoad()`](#api.config.load) and
[`getTemplateVars()`](#api.get.template.vars).
