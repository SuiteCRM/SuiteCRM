# Functions

Every Smarty tag either prints a [variable](./language-syntax-variables.md) or
invokes some sort of function. These are processed and displayed by
enclosing the function and its [attributes](./language-syntax-attributes.md)
within delimiters like so: `{funcname attr1="val1" attr2="val2"}`.

## Examples

```smarty
{config_load file="colors.conf"}

{include file="header.tpl"}
{insert file="banner_ads.tpl" title="My Site"}

{if $logged_in}
    Welcome, <span style="color:{#fontColor#}">{$name}!</span>
{else}
    hi, {$name}
{/if}

{include file="footer.tpl"}
```
      
-   Both [built-in functions](../language-builtin-functions/index.md) and [custom
    functions](../language-custom-functions/index.md) have the same syntax within
    templates.

-   Built-in functions are the **inner** workings of Smarty, such as
    [`{if}`](../language-builtin-functions/language-function-if.md),
    [`{section}`](../language-builtin-functions/language-function-section.md) and
    [`{strip}`](../language-builtin-functions/language-function-strip.md). There should be no need to
    change or modify them.

-   Custom functions are **additional** functions implemented via
    [plugins](../../programmers/plugins.md). They can be modified to your liking, or you can
    create new ones. [`{html_options}`](../language-custom-functions/language-function-html-options.md)
    is an example of a custom function.

See also [`registerPlugin()`](../../programmers/api-functions/api-register-plugin.md)
