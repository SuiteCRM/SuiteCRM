Postfilters {#advanced.features.postfilters}
===========

Template postfilters are PHP functions that your templates are ran
through *after they are compiled*. Postfilters can be either
[registered](#api.register.filter) or loaded from the [plugins
directory](#variable.plugins.dir) by using the
[`loadFilter()`](#api.load.filter) function or by setting the
[`$autoload_filters`](#variable.autoload.filters) variable. Smarty will
pass the compiled template code as the first argument, and expect the
function to return the result of the processing.


    <?php
    // put this in your application
    function add_header_comment($tpl_source, Smarty_Internal_Template $template)
    {
        return "<?php echo \"<!-- Created by Smarty! -->\n\"; ?>\n".$tpl_source;
    }

    // register the postfilter
    $smarty->registerFilter('post','add_header_comment');
    $smarty->display('index.tpl');
    ?>

      

The postfilter above will make the compiled Smarty template `index.tpl`
look like:


    <!-- Created by Smarty! -->
    {* rest of template content... *}

      

See also [`registerFilter()`](#api.register.filter),
[prefilters](#advanced.features.prefilters),
[outputfilters](#advanced.features.outputfilters), and
[`loadFilter()`](#api.load.filter).
