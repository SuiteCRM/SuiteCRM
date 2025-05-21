# cat

This value is concatenated to the given variable.

## Basic usage
```smarty
{$myVar|cat:' units'}
```

## Parameters

| Parameter | Type   | Required | Description                                      |
|-----------|--------|----------|--------------------------------------------------|
| 1         | string | No       | This value to concatenate to the given variable. |

## Examples

```php
<?php

    $smarty->assign('articleTitle', "Psychics predict world didn't end");

```

Where template is:

```smarty
    {$articleTitle|cat:' yesterday.'}
```

Will output:

```
    Psychics predict world didn't end yesterday.
```
       
