# Built-in Functions

Smarty comes with several built-in functions. These built-in functions
are the integral part of the smarty template engine. They are compiled
into corresponding inline PHP code for maximum performance.

You cannot create your own [custom functions](../language-custom-functions/index.md) with the same name; and you
should not need to modify the built-in functions.

A few of these functions have an `assign` attribute which collects the
result the function to a named template variable instead of being
output; much like the [`{assign}`](language-function-assign.md) function.

- [{append}](language-function-append.md)
- [{assign} or {$var=...}](language-function-assign.md)
- [{block}](language-function-block.md)
- [{call}](language-function-call.md)
- [{capture}](language-function-capture.md)
- [{config_load}](language-function-config-load.md)
- [{debug}](language-function-debug.md)
- [{extends}](language-function-extends.md)
- [{for}](language-function-for.md)
- [{foreach}, {foreachelse}](language-function-foreach.md)
- [{function}](language-function-function.md)
- [{if}, {elseif}, {else}](language-function-if.md)
- [{include}](language-function-include.md)
- [{insert}](language-function-insert.md)
- [{ldelim}, {rdelim}](language-function-ldelim.md)
- [{literal}](language-function-literal.md)
- [{nocache}](language-function-nocache.md)
- [{section}, {sectionelse}](language-function-section.md)
- [{setfilter}](language-function-setfilter.md)
- [{strip}](language-function-strip.md)
- [{while}](language-function-while.md)

