Compiler Functions {#plugins.compiler.functions}
==================

Compiler functions are called only during compilation of the template.
They are useful for injecting PHP code or time-sensitive static content
into the template. If there is both a compiler function and a [custom
function](#language.custom.functions) registered under the same name,
the compiler function has precedence.

mixed

smarty\_compiler\_

name

array

\$params

object

\$smarty

The compiler function is passed two parameters: the params array which
contains precompiled strings for the attribute values and the Smarty
object. It\'s supposed to return the code to be injected into the
compiled template including the surrounding PHP tags.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     compiler.tplheader.php
     * Type:     compiler
     * Name:     tplheader
     * Purpose:  Output header containing the source file name and
     *           the time it was compiled.
     * -------------------------------------------------------------
     */
    function smarty_compiler_tplheader($params, Smarty $smarty)
    {
        return "<?php\necho '" . $smarty->_current_file . " compiled at " . date('Y-m-d H:M'). "';\n?>";
    }
    ?>

This function can be called from the template as:


    {* this function gets executed at compile time only *}
    {tplheader}

         

The resulting PHP code in the compiled template would be something like
this:


    <?php
    echo 'index.tpl compiled at 2002-02-20 20:02';
    ?>

         

See also [`registerPlugin()`](#api.register.plugin),
[`unregisterPlugin()`](#api.unregister.plugin).
