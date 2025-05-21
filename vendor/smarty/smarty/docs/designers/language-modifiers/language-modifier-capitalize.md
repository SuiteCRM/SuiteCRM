# capitalize

This is used to capitalize the first letter of all words in a variable.
This is similar to the PHP [`ucwords()`](https://www.php.net/ucwords)
function.

## Basic usage
```smarty
{$myVar|capitalize}
```

## Parameters

| Parameter | Type    | Required | Description                                                                                           |
|-----------|---------|----------|-------------------------------------------------------------------------------------------------------|
| 1         | boolean | No       | This determines whether or not words with digits will be uppercased                                   |
| 2         | boolean | No       | This determines whether or not Capital letters within words should be lowercased, e.g. "aAa" to "Aaa" |


## Examples

```php
<?php

    $smarty->assign('articleTitle', 'next x-men film, x3, delayed.');

```
       

Where the template is:

```smarty
    {$articleTitle}
    {$articleTitle|capitalize}
    {$articleTitle|capitalize:true}
```
       

Will output:

```
    next x-men film, x3, delayed.
    Next X-Men Film, x3, Delayed.
    Next X-Men Film, X3, Delayed.
```
       

See also [`lower`](language-modifier-lower.md) and
[`upper`](language-modifier-upper.md)
