# spacify

`spacify` is a way to insert a space between every character of a
variable. You can optionally pass a different character or string to
insert.

## Basic usage
```smarty
{$myVar|spacify}
```

## Parameters

| Parameter Position | Type   | Required | Default     | Description                                                     |
|--------------------|--------|----------|-------------|-----------------------------------------------------------------|
| 1                  | string | No       | *one space* | This what gets inserted between each character of the variable. |

## Examples

```php
<?php

$smarty->assign('articleTitle', 'Something Went Wrong in Jet Crash, Experts Say.');

```

Where template is:

```smarty
{$articleTitle}
{$articleTitle|spacify}
{$articleTitle|spacify:"^^"}
```

Will output:

```
Something Went Wrong in Jet Crash, Experts Say.
S o m e t h i n g   W .... snip ....  s h ,   E x p e r t s   S a y .
S^^o^^m^^e^^t^^h^^i^^n^^g^^ .... snip .... ^^e^^r^^t^^s^^ ^^S^^a^^y^^.
```

See also [`wordwrap`](language-modifier-wordwrap.md) and
[`nl2br`](language-modifier-nl2br.md).
