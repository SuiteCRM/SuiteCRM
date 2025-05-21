registerDefaultPluginHandler()

register a function which gets called on undefined tags

Description
===========

void

registerDefaultPluginHandler

mixed

callback

Register a default plugin handler which gets called if the compiler can
not find a definition for a tag otherwise. It uses the following
parameters:

If during compilation Smarty encounters tag which is not defined
internal, registered or located in the plugins folder it tries to
resolve it by calling the registered default plugin handler. The handler
may be called several times for same undefined tag looping over valid
plugin types.


    <?php

    $smarty = new Smarty();
    $smarty->registerDefaultPluginHandler('my_plugin_handler');

    /**
     * Default Plugin Handler
     *
     * called when Smarty encounters an undefined tag during compilation
     * 
     * @param string                     $name      name of the undefined tag
     * @param string                     $type     tag type (e.g. Smarty::PLUGIN_FUNCTION, Smarty::PLUGIN_BLOCK, 
                                                   Smarty::PLUGIN_COMPILER, Smarty::PLUGIN_MODIFIER, Smarty::PLUGIN_MODIFIERCOMPILER)
     * @param Smarty_Internal_Template   $template     template object
     * @param string                     &$callback    returned function name 
     * @param string                     &$script      optional returned script filepath if function is external
     * @param bool                       &$cacheable    true by default, set to false if plugin is not cachable (Smarty >= 3.1.8)
     * @return bool                      true if successfull
     */
    function my_plugin_handler ($name, $type, $template, &$callback, &$script, &$cacheable)
    {
        switch ($type) {
            case Smarty::PLUGIN_FUNCTION:
                switch ($name) {
                    case 'scriptfunction':
                        $script = './scripts/script_function_tag.php';
                        $callback = 'default_script_function_tag';
                        return true;
                    case 'localfunction':
                        $callback = 'default_local_function_tag';
                        return true;
                    default:
                    return false;
                }
            case Smarty::PLUGIN_COMPILER:
                switch ($name) {
                    case 'scriptcompilerfunction':
                        $script = './scripts/script_compiler_function_tag.php';
                        $callback = 'default_script_compiler_function_tag';
                        return true;
                    default:
                    return false;
                }
            case Smarty::PLUGIN_BLOCK:
                switch ($name) {
                    case 'scriptblock':
                        $script = './scripts/script_block_tag.php';
                        $callback = 'default_script_block_tag';
                        return true;
                    default:
                    return false;
                }
            default:
            return false;
        }
     }

    ?>

      

> **Note**
>
> The return callback must be static; a function name or an array of
> class and method name.
>
> Dynamic callbacks like objects methods are not supported.
