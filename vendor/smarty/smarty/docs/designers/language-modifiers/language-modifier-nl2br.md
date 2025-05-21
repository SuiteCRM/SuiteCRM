# nl2br

All `"\n"` line breaks will be converted to html `<br />` tags in the
given variable. This is equivalent to the PHP\'s
[`nl2br()`](https://www.php.net/nl2br) function.

## Basic usage
```smarty
{$myVar|nl2br}
```

## Examples

```php
<?php

$smarty->assign('articleTitle',
                "Sun or rain expected\ntoday, dark tonight"
                );

```

Where the template is:

```smarty
{$articleTitle|nl2br}
```
       
Will output:

```
Sun or rain expected<br />today, dark tonight
```

See also [`word_wrap`](language-modifier-wordwrap.md),
[`count_paragraphs`](language-modifier-count-paragraphs.md) and
[`count_sentences`](language-modifier-count-sentences.md).
