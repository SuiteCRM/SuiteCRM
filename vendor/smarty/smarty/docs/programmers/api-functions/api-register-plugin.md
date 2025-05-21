registerPlugin()

dynamically register plugins

Description
===========

void

registerPlugin

string

type

string

name

mixed

callback

bool

cacheable

mixed

cache\_attrs

This method registers functions or methods defined in your script as
plugin. It uses the following parameters:

-   `cacheable` and `cache_attrs` can be omitted in most cases. See
    [controlling cacheability of plugins output](#caching.cacheable) on
    how to use them properly.

<!-- -->


    <?php
    $smarty->registerPlugin("function","date_now", "print_current_date");

    function print_current_date($params, $smarty)
    {
      if(empty($params["format"])) {
        $format = "%b %e, %Y";
      } else {
        $format = $params["format"];
      }
      return strftime($format,time());
    }
    ?>

       

And in the template


    {date_now}

    {* or to format differently *}
    {date_now format="%Y/%m/%d"}


    <?php
    // function declaration
    function do_translation ($params, $content, $smarty, &$repeat, $template)
    {
      if (isset($content)) {
        $lang = $params["lang"];
        // do some translation with $content
        return $translation;
      }
    }

    // register with smarty
    $smarty->registerPlugin("block","translate", "do_translation");
    ?>

       

Where the template is:


    {translate lang="br"}Hello, world!{/translate}

       


    <?php

    // let's map PHP's stripslashes function to a Smarty modifier.
    $smarty->registerPlugin("modifier","ss", "stripslashes");

    ?>

In the template, use `ss` to strip slashes.


    <?php
    {$var|ss}
    ?>

See also [`unregisterPlugin()`](#api.unregister.plugin), [plugin
functions](#plugins.functions), [plugin block
functions](#plugins.block.functions), [plugin compiler
functions](#plugins.compiler.functions), and the [creating plugin
modifiers](#plugins.modifiers) section.
