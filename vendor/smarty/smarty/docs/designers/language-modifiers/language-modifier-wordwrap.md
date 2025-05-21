# wordwrap

Wraps a string to a column width, the default is 80. As an optional
second parameter, you can specify a string of text to wrap the text to
the next line, the default is a carriage return `"\n"`. By default,
`wordwrap` will attempt to wrap at a word boundary. If you want to cut
off at the exact character length, pass the optional third parameter as
TRUE. This is equivalent to the PHP
[`wordwrap()`](https://www.php.net/wordwrap) function.

## Basic usage
```smarty
{$myVar|wordwrap:30}
```

## Parameters

| Parameter Position | Type    | Required | Default | Description                                                                                   |
|--------------------|---------|----------|---------|-----------------------------------------------------------------------------------------------|
| 1                  | integer | No       | 80      | This determines how many columns to wrap to.                                                  |
| 2                  | string  | No       | \\n     | This is the string used to wrap words with.                                                   |
| 3                  | boolean | No       | FALSE   | This determines whether to wrap at a word boundary (FALSE), or at the exact character (TRUE). |

## Examples

```php
<?php

$smarty->assign('articleTitle',
                "Blind woman gets new kidney from dad she hasn't seen in years."
               );

```

Where template is

```smarty
{$articleTitle}

{$articleTitle|wordwrap:30}

{$articleTitle|wordwrap:20}

{$articleTitle|wordwrap:30:"<br />\n"}

{$articleTitle|wordwrap:26:"\n":true}
```

Will output:

```html
Blind woman gets new kidney from dad she hasn't seen in years.

Blind woman gets new kidney
from dad she hasn't seen in
years.

Blind woman gets new
kidney from dad she
hasn't seen in
years.

Blind woman gets new kidney<br />
from dad she hasn't seen in<br />
years.

Blind woman gets new kidn
ey from dad she hasn't se
en in years.
```       

See also [`nl2br`](language-modifier-nl2br.md) and
[`{textformat}`](../language-custom-functions/language-function-textformat.md).
