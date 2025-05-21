clearCompiledTemplate()

clears the compiled version of the specified template resource

Description
===========

void

clearCompiledTemplate

string

tpl\_file

string

compile\_id

int

exp\_time

This clears the compiled version of the specified template resource, or
all compiled template files if one is not specified. If you pass a
[`$compile_id`](#variable.compile.id) only the compiled template for
this specific [`$compile_id`](#variable.compile.id) is cleared. If you
pass an exp\_time, then only compiled templates older than `exp_time`
seconds are cleared, by default all compiled templates are cleared
regardless of their age. This function is for advanced use only, not
normally needed.


    <?php
    // clear a specific template resource
    $smarty->clearCompiledTemplate('index.tpl');

    // clear entire compile directory
    $smarty->clearCompiledTemplate();
    ?>

       

See also [`clearCache()`](#api.clear.cache).
