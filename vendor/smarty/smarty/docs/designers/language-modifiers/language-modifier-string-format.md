# string_format

This is a way to format strings, such as decimal numbers and such. Use
the syntax for [`sprintf()`](https://www.php.net/sprintf) for the
formatting.

## Basic usage
```smarty
{$myVar|string_format:"%d"}
```

## Parameters

| Parameter Position | Type   | Required | Description                           |
|--------------------|--------|----------|---------------------------------------|
| 1                  | string | Yes      | This is what format to use. (sprintf) |

## Examples

```php
<?php

$smarty->assign('number', 23.5787446);

```

Where template is:

```smarty
{$number}
{$number|string_format:"%.2f"}
{$number|string_format:"%d"}
```

Will output:

```
23.5787446
23.58
23
```

See also [`date_format`](language-modifier-date-format.md).
