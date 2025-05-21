Output Filters {#plugins.outputfilters}
==============

Output filter plugins operate on a template\'s output, after the
template is loaded and executed, but before the output is displayed.

string

smarty\_outputfilter\_

name

string

\$template\_output

object

\$template

The first parameter to the output filter function is the template output
that needs to be processed, and the second parameter is the instance of
Smarty invoking the plugin. The plugin is supposed to do the processing
and return the results.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     outputfilter.protect_email.php
     * Type:     outputfilter
     * Name:     protect_email
     * Purpose:  Converts @ sign in email addresses to %40 as
     *           a simple protection against spambots
     * -------------------------------------------------------------
     */
     function smarty_outputfilter_protect_email($output, Smarty_Internal_Template $template)
     {
         return preg_replace('!(\S+)@([a-zA-Z0-9\.\-]+\.([a-zA-Z]{2,3}|[0-9]{1,3}))!',
                             '$1%40$2', $output);
     }
    ?>

         

See also [`registerFilter()`](#api.register.filter),
[`unregisterFilter()`](#api.unregister.filter).
