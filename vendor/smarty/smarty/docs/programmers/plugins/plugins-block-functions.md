Block Functions {#plugins.block.functions}
===============

void

smarty\_block\_

name

array

\$params

mixed

\$content

object

\$template

boolean

&\$repeat

Block functions are functions of the form: `{func} .. {/func}`. In other
words, they enclose a template block and operate on the contents of this
block. Block functions take precedence over [custom
functions](#language.custom.functions) of the same name, that is, you
cannot have both custom function `{func}` and block function
`{func}..{/func}`.

-   By default your function implementation is called twice by Smarty:
    once for the opening tag, and once for the closing tag. (See
    `$repeat` below on how to change this.)

-   Starting with Smarty 3.1 the returned value of the opening tag call
    is displayed as well.

-   Only the opening tag of the block function may have
    [attributes](#language.syntax.attributes). All attributes passed to
    template functions from the template are contained in the `$params`
    variable as an associative array. The opening tag attributes are
    also accessible to your function when processing the closing tag.

-   The value of the `$content` variable depends on whether your
    function is called for the opening or closing tag. In case of the
    opening tag, it will be NULL, and in case of the closing tag it will
    be the contents of the template block. Note that the template block
    will have already been processed by Smarty, so all you will receive
    is the template output, not the template source.

-   The parameter `$repeat` is passed by reference to the function
    implementation and provides a possibility for it to control how many
    times the block is displayed. By default `$repeat` is TRUE at the
    first call of the block-function (the opening tag) and FALSE on all
    subsequent calls to the block function (the block\'s closing tag).
    Each time the function implementation returns with `$repeat` being
    TRUE, the contents between `{func}...{/func}` are evaluated and the
    function implementation is called again with the new block contents
    in the parameter `$content`.

If you have nested block functions, it\'s possible to find out what the
parent block function is by accessing `$smarty->_tag_stack` variable.
Just do a [`var_dump()`](https://www.php.net/var_dump) on it and the
structure should be apparent.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     block.translate.php
     * Type:     block
     * Name:     translate
     * Purpose:  translate a block of text
     * -------------------------------------------------------------
     */
    function smarty_block_translate($params, $content, Smarty_Internal_Template $template, &$repeat)
    {
        // only output on the closing tag
        if(!$repeat){
            if (isset($content)) {
                $lang = $params['lang'];
                // do some intelligent translation thing here with $content
                return $translation;
            }
        }
    }
    ?>

         

See also: [`registerPlugin()`](#api.register.plugin),
[`unregisterPlugin()`](#api.unregister.plugin).
