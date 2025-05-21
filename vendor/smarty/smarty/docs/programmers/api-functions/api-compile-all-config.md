compileAllConfig()

compiles all known config files

Description
===========

string

compileAllConfig

string

extension

boolean

force

integer

timelimit

integer

maxerror

This function compiles config files found in the
[`$config_dir`](#variable.config.dir) folder. It uses the following
parameters:

-   `extension` is an optional string which defines the file extension
    for the config files. The default is \".conf\".

-   `force` is an optional boolean which controls if only modified
    (false) or all (true) config files shall be compiled. The default is
    \"false\".

-   `timelimit` is an optional integer to set a runtime limit in seconds
    for the compilation process. The default is no limit.

-   `maxerror` is an optional integer to set an error limit. If more
    config files failed to compile the function will be aborted. The
    default is no limit.

> **Note**
>
> This function may not create desired results in all configurations.
> Use is on own risk.


    <?php
    include('Smarty.class.php');
    $smarty = new Smarty;

    // force compilation of all config files
    $smarty->compileAllConfig('.config',true);

    ?>

        
