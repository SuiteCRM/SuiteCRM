\$debugging {#variable.debugging}
===========

This enables the [debugging console](#chapter.debugging.console). The
console is a javascript popup window that informs you of the
[included](#language.function.include) templates, variables
[assigned](#api.assign) from php and [config file
variables](#language.config.variables) for the current script. It does
not show variables assigned within a template with the
[`{assign}`](#language.function.assign) function.

The console can also be enabled from the url with
[`$debugging_ctrl`](#variable.debugging.ctrl).

See also [`{debug}`](#language.function.debug),
[`$debug_tpl`](#variable.debug_template), and
[`$debugging_ctrl`](#variable.debugging.ctrl).
