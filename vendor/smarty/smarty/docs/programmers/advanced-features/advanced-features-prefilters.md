Prefilters {#advanced.features.prefilters}
==========

Template prefilters are PHP functions that your templates are ran
through *before they are compiled*. This is good for preprocessing your
templates to remove unwanted comments, keeping an eye on what people are
putting in their templates, etc.

Prefilters can be either [registered](#api.register.filter) or loaded
from the [plugins directory](#variable.plugins.dir) by using
[`loadFilter()`](#api.load.filter) function or by setting the
[`$autoload_filters`](#variable.autoload.filters) variable.

Smarty will pass the template source code as the first argument, and
expect the function to return the resulting template source code.

This will remove all the html comments in the template source.


    <?php
    // put this in your application
    function remove_dw_comments($tpl_source, Smarty_Internal_Template $template)
    {
        return preg_replace("/<!--#.*-->/U",'',$tpl_source);
    }

    // register the prefilter
    $smarty->registerFilter('pre','remove_dw_comments');
    $smarty->display('index.tpl');
    ?>

      

See also [`registerFilter()`](#api.register.filter),
[postfilters](#advanced.features.postfilters) and
[`loadFilter()`](#api.load.filter).
