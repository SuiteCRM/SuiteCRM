# lower

This is used to lowercase a variable. This is equivalent to the PHP
[`strtolower()`](https://www.php.net/strtolower) function.

## Basic usage
```smarty
{$myVar|lower}
```

## Examples

```php
<?php

$smarty->assign('articleTitle', 'Two Convicts Evade Noose, Jury Hung.');
```

Where template is:

```smarty
{$articleTitle}
{$articleTitle|lower}
```

This will output:

```
Two Convicts Evade Noose, Jury Hung.
two convicts evade noose, jury hung.
```
       
See also [`upper`](language-modifier-upper.md) and
[`capitalize`](language-modifier-capitalize.md).
