Output Filters {#advanced.features.outputfilters}
==============

When the template is invoked via [`display()`](#api.display) or
[`fetch()`](#api.fetch), its output can be sent through one or more
output filters. This differs from
[`postfilters`](#advanced.features.postfilters) because postfilters
operate on compiled templates before they are saved to the disk, whereas
output filters operate on the template output when it is executed.

Output filters can be either [registered](#api.register.filter) or
loaded from the [plugins directory](#variable.plugins.dir) by using the
[`loadFilter()`](#api.load.filter) method or by setting the
[`$autoload_filters`](#variable.autoload.filters) variable. Smarty will
pass the template output as the first argument, and expect the function
to return the result of the processing.


    <?php
    // put this in your application
    function protect_email($tpl_output, Smarty_Internal_Template $template)
    {
        $tpl_output =
           preg_replace('!(\S+)@([a-zA-Z0-9\.\-]+\.([a-zA-Z]{2,3}|[0-9]{1,3}))!',
                        '$1%40$2', $tpl_output);
        return $tpl_output;
    }

    // register the outputfilter
    $smarty->registerFilter("output","protect_email");
    $smarty->display("index.tpl');

    // now any occurrence of an email address in the template output will have
    // a simple protection against spambots
    ?>

        

See also [`registerFilter()`](#api.register.filter),
[`loadFilter()`](#api.load.filter),
[`$autoload_filters`](#variable.autoload.filters),
[postfilters](#advanced.features.postfilters) and
[`$plugins_dir`](#variable.plugins.dir).
