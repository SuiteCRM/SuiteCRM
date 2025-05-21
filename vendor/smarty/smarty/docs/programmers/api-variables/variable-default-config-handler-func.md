\$default\_config\_handler\_func {#variable.default.config.handler.func}
================================

This function is called when a config file cannot be obtained from its
resource.

> **Note**
>
> The default handler is currently only invoked for file resources. It
> is not triggered when the resource itself cannot be found, in which
> case a SmartyException is thrown.


    <?php

    $smarty = new Smarty();
    $smarty->default_config_handler_func = 'my_default_config_handler_func';

    /**
     * Default Config Handler
     *
     * called when Smarty's file: resource is unable to load a requested file
     * 
     * @param string   $type     resource type (e.g. "file", "string", "eval", "resource")
     * @param string   $name     resource name (e.g. "foo/bar.tpl")
     * @param string  &$content  config's content
     * @param integer &$modified config's modification time
     * @param Smarty   $smarty   Smarty instance
     * @return string|boolean   path to file or boolean true if $content and $modified 
     *                          have been filled, boolean false if no default config 
     *                          could be loaded
     */
    function my_default_config_handler_func($type, $name, &$content, &$modified, Smarty $smarty) {
        if (false) {
            // return corrected filepath
            return "/tmp/some/foobar.tpl";
        } elseif (false) {
            // return a config directly
            $content = 'someVar = "the config source"';
            $modified = time();
            return true;
        } else {
            // tell smarty that we failed
            return false;
        }
    }

    ?>

      
