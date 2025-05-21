Prefilters/Postfilters {#plugins.prefilters.postfilters}
======================

Prefilter and postfilter plugins are very similar in concept; where they
differ is in the execution \-- more precisely the time of their
execution.

string

smarty\_prefilter\_

name

string

\$source

object

\$template

Prefilters are used to process the source of the template immediately
before compilation. The first parameter to the prefilter function is the
template source, possibly modified by some other prefilters. The plugin
is supposed to return the modified source. Note that this source is not
saved anywhere, it is only used for compilation.

string

smarty\_postfilter\_

name

string

\$compiled

object

\$template

Postfilters are used to process the compiled output of the template (the
PHP code) immediately after the compilation is done but before the
compiled template is saved to the filesystem. The first parameter to the
postfilter function is the compiled template code, possibly modified by
other postfilters. The plugin is supposed to return the modified version
of this code.


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     prefilter.pre01.php
     * Type:     prefilter
     * Name:     pre01
     * Purpose:  Convert html tags to be lowercase.
     * -------------------------------------------------------------
     */
     function smarty_prefilter_pre01($source, Smarty_Internal_Template $template)
     {
         return preg_replace('!<(\w+)[^>]+>!e', 'strtolower("$1")', $source);
     }
    ?>

         


    <?php
    /*
     * Smarty plugin
     * -------------------------------------------------------------
     * File:     postfilter.post01.php
     * Type:     postfilter
     * Name:     post01
     * Purpose:  Output code that lists all current template vars.
     * -------------------------------------------------------------
     */
     function smarty_postfilter_post01($compiled, Smarty_Internal_Template $template)
     {
         $compiled = "<pre>\n<?php print_r(\$template->getTemplateVars()); ?>\n</pre>" . $compiled;
         return $compiled;
     }
    ?>

         

See also [`registerFilter()`](#api.register.filter) and
[`unregisterFilter()`](#api.unregister.filter).
