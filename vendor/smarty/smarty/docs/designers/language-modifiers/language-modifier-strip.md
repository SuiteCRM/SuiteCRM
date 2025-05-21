# strip

This replaces all spaces, newlines and tabs with a single space, or with
the supplied string.

## Basic usage
```smarty
{$myVar|strip}
```

> **Note**
>
> If you want to strip blocks of template text, use the built-in
> [`{strip}`](../language-builtin-functions/language-function-strip.md) function.

## Examples

```php
<?php
$smarty->assign('articleTitle', "Grandmother of\neight makes\t    hole in one.");
$smarty->display('index.tpl');
```

Where template is:

```smarty
{$articleTitle}
{$articleTitle|strip}
{$articleTitle|strip:'&nbsp;'}
```

Will output:

```html
Grandmother of
eight makes        hole in one.
Grandmother of eight makes hole in one.
Grandmother&nbsp;of&nbsp;eight&nbsp;makes&nbsp;hole&nbsp;in&nbsp;one.
```

See also [`{strip}`](../language-builtin-functions/language-function-strip.md) and
[`truncate`](language-modifier-truncate.md).
