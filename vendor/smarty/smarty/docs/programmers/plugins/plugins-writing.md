Writing Plugins {#plugins.writing}
===============

Plugins can be either loaded by Smarty automatically from the filesystem
or they can be registered at runtime via one of the register\_\* API
functions. They can also be unregistered by using unregister\_\* API
functions.

For the plugins that are registered at runtime, the name of the plugin
function(s) does not have to follow the naming convention.

If a plugin depends on some functionality provided by another plugin (as
is the case with some plugins bundled with Smarty), then the proper way
to load the needed plugin is this:


    <?php
    function smarty_function_yourPlugin(array $params, Smarty_Internal_Template $template)
    {
      // load plugin depended upon
      $template->smarty->loadPlugin('smarty_shared_make_timestamp');
      // plugin code
    }
    ?>

        

As a general rule, the currently evaluated template\'s
Smarty\_Internal\_Template object is always passed to the plugins as the
last parameter with two exceptions:

-   modifiers do not get passed the Smarty\_Internal\_Template object at
    all

-   blocks get passed `$repeat` after the Smarty\_Internal\_Template
    object to keep backwards compatibility to older versions of Smarty.
