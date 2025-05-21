Template Functions {#plugins.functions}
==================

void

smarty\_function\_

name

array

\$params

object

\$template

All [attributes](#language.syntax.attributes) passed to template
functions from the template are contained in the `$params` as an
associative array.

The output (return value) of the function will be substituted in place
of the function tag in the template, eg the
[`{fetch}`](#language.function.fetch) function. Alternatively, the
function can simply perform some other task without any output, eg the
[`{assign}`](#language.function.assign) function.

If the function needs to assign some variables to the template or use
some other Smarty-provided functionality, it can use the supplied
`$template` object to do so eg `$template->foo()`.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     function.eightball.php
     * Type:     function
     * Name:     eightball
     * Purpose:  outputs a random magic answer
     * -------------------------------------------------------------
     */
    function smarty_function_eightball($params, Smarty_Internal_Template $template)
    {
        $answers = array('Yes',
                         'No',
                         'No way',
                         'Outlook not so good',
                         'Ask again soon',
                         'Maybe in your reality');

        $result = array_rand($answers);
        return $answers[$result];
    }
    ?>

which can be used in the template as:

    Question: Will we ever have time travel?
    Answer: {eightball}.
        


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     function.assign.php
     * Type:     function
     * Name:     assign
     * Purpose:  assign a value to a template variable
     * -------------------------------------------------------------
     */
    function smarty_function_assign($params, Smarty_Internal_Template $template)
    {
        if (empty($params['var'])) {
            trigger_error("assign: missing 'var' parameter");
            return;
        }

        if (!in_array('value', array_keys($params))) {
            trigger_error("assign: missing 'value' parameter");
            return;
        }

        $template->assign($params['var'], $params['value']);     
        
    }
    ?>

          

See also: [`registerPlugin()`](#api.register.plugin),
[`unregisterPlugin()`](#api.unregister.plugin).
