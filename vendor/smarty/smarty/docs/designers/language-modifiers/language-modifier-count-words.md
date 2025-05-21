# count_words

This is used to count the number of words in a variable.

## Basic usage
```smarty
{$myVar|count_words}
```

## Examples

```php
<?php

    $smarty->assign('articleTitle', 'Dealers Will Hear Car Talk at Noon.');

```

Where template is:

```smarty
    {$articleTitle}
    {$articleTitle|count_words}
```

This will output:

```
    Dealers Will Hear Car Talk at Noon.
    7
```

See also [`count_characters`](language-modifier-count-characters.md),
[`count_paragraphs`](language-modifier-count-paragraphs.md) and
[`count_sentences`](language-modifier-count-sentences.md).
