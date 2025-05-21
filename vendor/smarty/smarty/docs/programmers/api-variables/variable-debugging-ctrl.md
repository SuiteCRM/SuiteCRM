\$debugging\_ctrl {#variable.debugging.ctrl}
=================

This allows alternate ways to enable debugging. `NONE` means no
alternate methods are allowed. `URL` means when the keyword
`SMARTY_DEBUG` is found in the `QUERY_STRING`, debugging is enabled for
that invocation of the script. If [`$debugging`](#variable.debugging) is
TRUE, this value is ignored.


    <?php
    // shows debug console only on localhost ie
    // http://localhost/script.php?foo=bar&SMARTY_DEBUG
    $smarty->debugging = false; // the default
    $smarty->debugging_ctrl = ($_SERVER['SERVER_NAME'] == 'localhost') ? 'URL' : 'NONE';
    ?>

See also [debugging console](#chapter.debugging.console) section,
[`$debugging`](#variable.debugging) and
[`$smarty_debug_id`](#variable.smarty.debug.id).
