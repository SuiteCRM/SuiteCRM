Changing settings by template {#advanced.features.template.settings}
=============================

Normally you configure the Smarty settings by modifying the
[`Smarty class variables`](#api.variables). Furthermore you can register
plugins, filters etc. with [`Smarty functions`](#api.functions).
Modifications done to the Smarty object will be global for all
templates.

However the Smarty class variables and functions can be accessed or
called by individual template objects. Modification done to a template
object will apply only for that template and its included subtemplates.


    <?php
    $tpl = $smarty->createTemplate('index.tpl);
    $tpl->cache_lifetime = 600;
    //or
    $tpl->setCacheLifetime(600);
    $smarty->display($tpl);
    ?>

        


    <?php
    $tpl = $smarty->createTemplate('index.tpl);
    $tpl->registerPlugin('modifier','mymodifier');
    $smarty->display($tpl);
    ?>

        
