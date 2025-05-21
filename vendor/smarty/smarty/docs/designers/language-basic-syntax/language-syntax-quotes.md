# Embedding Vars in Double Quotes

-   Smarty will recognize [assigned](../../programmers/api-functions/api-assign.md)
    [variables](./language-syntax-variables.md) embedded in "double
    quotes" so long as the variable name contains only numbers, letters
    and under_scores. See [naming](https://www.php.net/language.variables)
    for more detail.

-   With any other characters, for example a period(.) or
    `$object->reference`, then the variable must be surrounded by `` `backticks` ``.

-   In addition, Smarty does allow embedded Smarty tags in double-quoted
    strings. This is useful if you want to include variables with
    modifiers, plugin or PHP function results.

## Examples
```smarty
{func var="test $foo test"}              // sees $foo
{func var="test $foo_bar test"}          // sees $foo_bar
{func var="test `$foo[0]` test"}         // sees $foo[0]
{func var="test `$foo[bar]` test"}       // sees $foo[bar]
{func var="test $foo.bar test"}          // sees $foo (not $foo.bar)
{func var="test `$foo.bar` test"}        // sees $foo.bar
{func var="test `$foo.bar` test"|escape} // modifiers outside quotes!
{func var="test {$foo|escape} test"}     // modifiers inside quotes!
{func var="test {time()} test"}          // PHP function result
{func var="test {counter} test"}         // plugin result
{func var="variable foo is {if !$foo}not {/if} defined"} // Smarty block function

{* will replace $tpl_name with value *}
{include file="subdir/$tpl_name.tpl"}

{* does NOT replace $tpl_name *}
{include file='subdir/$tpl_name.tpl'} // vars require double quotes!

{* must have backticks as it contains a dot "." *}
{cycle values="one,two,`$smarty.config.myval`"}

{* must have backticks as it contains a dot "." *}
{include file="`$module.contact`.tpl"}

{* can use variable with dot syntax *}
{include file="`$module.$view`.tpl"}
```
      
> **Note**
>
> Although Smarty can handle some very complex expressions and syntax,
> it is a good rule of thumb to keep the template syntax minimal and
> focused on presentation. If you find your template syntax getting too
> complex, it may be a good idea to move the bits that do not deal
> explicitly with presentation to PHP by way of plugins or modifiers.

See also [`escape`](../language-modifiers/language-modifier-escape.md).
