loadFilter()

load a filter plugin

Description
===========

void

loadFilter

string

type

string

name

The first argument specifies the type of the filter to load and can be
one of the following: `pre`, `post` or `output`. The second argument
specifies the `name` of the filter plugin.


    <?php

    // load prefilter named 'trim'
    $smarty->loadFilter('pre', 'trim');

    // load another prefilter named 'datefooter'
    $smarty->loadFilter('pre', 'datefooter');

    // load output filter named 'compress'
    $smarty->loadFilter('output', 'compress');

    ?>

       

See also [`registerFilter()`](#api.register.filter),
[`$autoload_filters`](#variable.autoload.filters) and [advanced
features](#advanced.features).
