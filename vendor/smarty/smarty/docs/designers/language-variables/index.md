# Variables

Smarty has several types of variables. The type of the
variable depends on what symbol it is prefixed or enclosed within.

- [Variables assigned from PHP](language-assigned-variables.md)
- [Variables loaded from config files](language-config-variables.md)
- [{$smarty} reserved variable](language-variables-smarty.md)

Variables in Smarty can be either displayed directly or used as
arguments for [functions](../language-basic-syntax/language-syntax-functions.md),
[attributes](../language-basic-syntax/language-syntax-attributes.md) and
[modifiers](../language-modifiers/index.md), inside conditional expressions, etc.
To print a variable, simply enclose it in the
[delimiters](../../programmers/api-variables/variable-left-delimiter.md) so that it is the only thing
contained between them.

```smarty
{$Name}

{$product.part_no} <b>{$product.description}</b>

{$Contacts[row].Phone}

<body bgcolor="{#bgcolor#}">
```

## Scopes
You can assign variables to specific [variable scopes](language-variable-scopes.md).

      
> **Note**
>
> An easy way to examine assigned Smarty variables is with the
> [debugging console](../chapter-debugging-console.md).

