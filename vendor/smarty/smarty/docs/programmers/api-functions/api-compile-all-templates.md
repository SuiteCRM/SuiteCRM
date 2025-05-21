compileAllTemplates()

compiles all known templates

Description
===========

string

compileAllTemplates

string

extension

boolean

force

integer

timelimit

integer

maxerror

This function compiles template files found in the
[`$template_dir`](#variable.template.dir) folder. It uses the following
parameters:

-   `extension` is an optional string which defines the file extension
    for the template files. The default is \".tpl\".

-   `force` is an optional boolean which controls if only modified
    (false) or all (true) templates shall be compiled. The default is
    \"false\".

-   `timelimit` is an optional integer to set a runtime limit in seconds
    for the compilation process. The default is no limit.

-   `maxerror` is an optional integer to set an error limit. If more
    templates failed to compile the function will be aborted. The
    default is no limit.

> **Note**
>
> This function may not create desired results in all configurations.
> Use is on own risk.

> **Note**
>
> If any template requires registered plugins, filters or objects you
> must register all of them before running this function.

> **Note**
>
> If you are using template inheritance this function will create
> compiled files of parent templates which will never be used.


    <?php
    include('Smarty.class.php');
    $smarty = new Smarty;

    // force compilation of all template files
    $smarty->compileAllTemplates('.tpl',true);

    ?>

        
