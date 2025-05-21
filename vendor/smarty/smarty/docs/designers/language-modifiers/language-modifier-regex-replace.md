# regex_replace

A regular expression search and replace on a variable. Use the
[`preg_replace()`](https://www.php.net/preg_replace) syntax from the PHP
manual.

## Basic usage
```smarty
{$myVar|regex_replace:"/foo/":"bar"}
```

> **Note**
>
> Although Smarty supplies this regex convenience modifier, it is
> usually better to apply regular expressions in PHP, either via custom
> functions or modifiers. Regular expressions are considered application
> code and are not part of presentation logic.

## Parameters

| Parameter Position | Type   | Required | Description                                    |
|--------------------|--------|----------|------------------------------------------------|
| 1                  | string | Yes      | This is the regular expression to be replaced. |
| 2                  | string | Yes      | This is the string of text to replace with.    |


## Examples

```php
<?php

$smarty->assign('articleTitle', "Infertility unlikely to\nbe passed on, experts say.");

```

Where template is:

```smarty
{* replace each carriage return, tab and new line with a space *}

{$articleTitle}
{$articleTitle|regex_replace:"/[\r\t\n]/":" "}
```
       
Will output:

```
Infertility unlikely to
be passed on, experts say.
Infertility unlikely to be passed on, experts say.
```
       

See also [`replace`](language-modifier-replace.md) and
[`escape`](language-modifier-escape.md).
