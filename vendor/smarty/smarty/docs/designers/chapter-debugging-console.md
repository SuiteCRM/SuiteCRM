# Debugging Console

There is a debugging console included with Smarty. The console informs
you of all the [included](./language-builtin-functions/language-function-include.md) templates,
[assigned](../programmers/api-functions/api-assign.md) variables and
[config](./language-variables/language-config-variables.md) file variables for the current
invocation of the template. A template file named `debug.tpl` is
included with the distribution of Smarty which controls the formatting
of the console.

Set [`$debugging`](../programmers/api-variables/variable-debugging.md) to TRUE in Smarty, and if needed
set [`$debug_tpl`](../programmers/api-variables/variable-debug-template.md) to the template resource
path to `debug.tpl` (this is in [`SMARTY_DIR`](../programmers/smarty-constants.md) by
default). When you load the page, a Javascript console window will pop
up and give you the names of all the included templates and assigned
variables for the current page.

To see the available variables for a particular template, see the
[`{debug}`](./language-builtin-functions/language-function-debug.md) template function. To disable the
debugging console, set [`$debugging`](../programmers/api-variables/variable-debugging.md) to FALSE. You
can also temporarily turn on the debugging console by putting
`SMARTY_DEBUG` in the URL if you enable this option with
[`$debugging_ctrl`](../programmers/api-variables/variable-debugging-ctrl.md).

> **Note**
>
> The debugging console does not work when you use the
> [`fetch()`](../programmers/api-functions/api-fetch.md) API, only when using
> [`display()`](../programmers/api-functions/api-display.md). It is a set of javascript statements
> added to the very bottom of the generated template. If you do not like
> javascript, you can edit the `debug.tpl` template to format the output
> however you like. Debug data is not cached and `debug.tpl` info is not
> included in the output of the debug console.

> **Note**
>
> The load times of each template and config file are in seconds, or
> fractions thereof.

See also [troubleshooting](../appendixes/troubleshooting.md).
