\$compile\_id {#variable.compile.id}
=============

Persistent compile identifier. As an alternative to passing the same
`$compile_id` to each and every function call, you can set this
`$compile_id` and it will be used implicitly thereafter.

If you use the same template with different [pre- and/or
post-filters](#plugins.prefilters.postfilters) you must use a unique
`$compile_id` to keep the compiled template files separated.

For example a [prefilter](#plugins.prefilters.postfilters) that
localizes your templates (that is: translates language dependent parts)
at compile time, then you could use the current language as
`$compile_id` and you will get a set of compiled templates for each
language you use.


    <?php
    $smarty->compile_id = 'en';
    ?>

      

Another application would be to use the same compile directory across
multiple domains / multiple virtual hosts.


    <?php

    $smarty->compile_id = $_SERVER['SERVER_NAME'];
    $smarty->compile_dir = '/path/to/shared_compile_dir';

    ?>

      

> **Note**
>
> In Smarty 3 a `$compile_id` is no longer required to keep templates
> with same name in different [`$template_dir`
> folders](#variable.template.dir) separated. The [`$template_dir` file
> path](#variable.template.dir) is encoded in the file name of compiled
> and cached template files.
