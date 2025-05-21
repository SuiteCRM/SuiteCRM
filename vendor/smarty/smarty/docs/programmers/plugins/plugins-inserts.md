Inserts {#plugins.inserts}
=======

Insert plugins are used to implement functions that are invoked by
[`{insert}`](#language.function.insert) tags in the template.

string

smarty\_insert\_

name

array

\$params

object

\$template

The first parameter to the function is an associative array of
attributes passed to the insert.

The insert function is supposed to return the result which will be
substituted in place of the `{insert}` tag in the template.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     insert.time.php
     * Type:     time
     * Name:     time
     * Purpose:  Inserts current date/time according to format
     * -------------------------------------------------------------
     */
    function smarty_insert_time($params, Smarty_Internal_Template $template)
    {
        if (empty($params['format'])) {
            trigger_error("insert time: missing 'format' parameter");
            return;
        }
        return strftime($params['format']);
    }
    ?>

         
