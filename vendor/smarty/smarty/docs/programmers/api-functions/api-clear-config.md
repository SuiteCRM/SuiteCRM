clearConfig()

clears assigned config variables

Description
===========

void

clearConfig

string

var

This clears all assigned [config variables](#language.config.variables).
If a variable name is supplied, only that variable is cleared.


    <?php
    // clear all assigned config variables.
    $smarty->clearConfig();

    // clear one variable
    $smarty->clearConfig('foobar');
    ?>

       

See also [`getConfigVars()`](#api.get.config.vars),
[`config variables`](#language.config.variables),
[`config files`](#config.files),
[`{config_load}`](#language.function.config.load),
[`configLoad()`](#api.config.load) and
[`clearAssign()`](#api.clear.assign).
