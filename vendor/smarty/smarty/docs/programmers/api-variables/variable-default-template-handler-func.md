\$default\_template\_handler\_func {#variable.default.template.handler.func}
==================================

This function is called when a template cannot be obtained from its
resource.

> **Note**
>
> The default handler is currently only invoked for file resources. It
> is not triggered when the resource itself cannot be found, in which
> case a SmartyException is thrown.


    <?php

    $smarty = new Smarty();
    $smarty->default_template_handler_func = 'my_default_template_handler_func';

    /**
     * Default Template Handler
     *
     * called when Smarty's file: resource is unable to load a requested file
     * 
     * @param string   $type     resource type (e.g. "file", "string", "eval", "resource")
     * @param string   $name     resource name (e.g. "foo/bar.tpl")
     * @param string  &$content  template's content
     * @param integer &$modified template's modification time
     * @param Smarty   $smarty   Smarty instance
     * @return string|boolean   path to file or boolean true if $content and $modified 
     *                          have been filled, boolean false if no default template 
     *                          could be loaded
     */
    function my_default_template_handler_func($type, $name, &$content, &$modified, Smarty $smarty) {
        if (false) {
            // return corrected filepath
            return "/tmp/some/foobar.tpl";
        } elseif (false) {
            // return a template directly
            $content = "the template source";
            $modified = time();
            return true;
        } else {
            // tell smarty that we failed
            return false;
        }
    }

    ?>

      
