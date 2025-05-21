# upper

This is used to uppercase a variable. This is equivalent to the PHP
[`strtoupper()`](https://www.php.net/strtoupper) function.

## Basic usage
```smarty
{$myVar|upper}
```

## Examples

```php
<?php
$smarty->assign('articleTitle', "If Strike isn't Settled Quickly it may Last a While.");
```

Where template is:

```smarty
{$articleTitle}
{$articleTitle|upper}
```

Will output:

```
If Strike isn't Settled Quickly it may Last a While.
IF STRIKE ISN'T SETTLED QUICKLY IT MAY LAST A WHILE.
```

See also [`lower`](lower) and
[`capitalize`](language-modifier-capitalize.md).
