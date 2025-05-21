setCacheDir()

set the directory where the rendered template\'s output is stored

Description
===========

Smarty

setCacheDir

string

cache\_dir


    <?php

    // set directory where rendered template's output is stored
    $smarty->setCacheDir('./cache');

    // chaining of method calls
    $smarty->setTemplateDir('./templates')
           ->setCompileDir('./templates_c')
           ->setCacheDir('./cache');

    ?>

       

See also [`getCacheDir()`](#api.get.cache.dir) and
[`$cache_dir`](#variable.cache.dir).
