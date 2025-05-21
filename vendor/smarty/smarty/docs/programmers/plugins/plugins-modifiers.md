Modifiers {#plugins.modifiers}
=========

[Modifiers](#language.modifiers) are little functions that are applied
to a variable in the template before it is displayed or used in some
other context. Modifiers can be chained together.

mixed

smarty\_modifier\_

name

mixed

\$value

\[mixed

\$param1

, \...\]

The first parameter to the modifier plugin is the value on which the
modifier is to operate. The rest of the parameters are optional,
depending on what kind of operation is to be performed.

The modifier has to [return](https://www.php.net/return) the result of its
processing.

This plugin basically aliases one of the built-in PHP functions. It does
not have any additional parameters.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     modifier.capitalize.php
     * Type:     modifier
     * Name:     capitalize
     * Purpose:  capitalize words in the string
     * -------------------------------------------------------------
     */
    function smarty_modifier_capitalize($string)
    {
        return ucwords($string);
    }
    ?>


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     modifier.truncate.php
     * Type:     modifier
     * Name:     truncate
     * Purpose:  Truncate a string to a certain length if necessary,
     *           optionally splitting in the middle of a word, and
     *           appending the $etc string.
     * -------------------------------------------------------------
     */
    function smarty_modifier_truncate($string, $length = 80, $etc = '...',
                                      $break_words = false)
    {
        if ($length == 0)
            return '';

        if (strlen($string) > $length) {
            $length -= strlen($etc);
            $fragment = substr($string, 0, $length+1);
            if ($break_words)
                $fragment = substr($fragment, 0, -1);
            else
                $fragment = preg_replace('/\s+(\S+)?$/', '', $fragment);
            return $fragment.$etc;
        } else
            return $string;
    }
    ?>

         

See also [`registerPlugin()`](#api.register.plugin),
[`unregisterPlugin()`](#api.unregister.plugin).
