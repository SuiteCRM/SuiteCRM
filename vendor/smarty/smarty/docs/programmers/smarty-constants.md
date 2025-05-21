Constants {#smarty.constants}
=========

SMARTY\_DIR {#constant.smarty.dir}
===========

This is the **full system path** to the location of the Smarty class
files. If this is not defined in your script, then Smarty will attempt
to determine the appropriate value automatically. If defined, the path
**must end with a trailing slash/**.


    <?php
    // set path to Smarty directory *nix style
    define('SMARTY_DIR', '/usr/local/lib/php/Smarty-v.e.r/libs/');

    // path to Smarty windows style
    define('SMARTY_DIR', 'c:/webroot/libs/Smarty-v.e.r/libs/');

    // include the smarty class, note 'S' is upper case
    require_once(SMARTY_DIR . 'Smarty.class.php');
    ?>

         

See also [`$smarty.const`](../designers/language-variables/language-variables-smarty.md).
