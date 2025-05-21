# count_sentences

This is used to count the number of sentences in a variable. A sentence
being delimited by a dot, question- or exclamation-mark (.?!).

## Basic usage
```smarty
{$myVar|count_sentences}
```

## Examples

```php
<?php

    $smarty->assign('articleTitle',
                     'Two Soviet Ships Collide - One Dies.
                     Enraged Cow Injures Farmer with Axe.'
                     );

```

Where template is:

```smarty
    {$articleTitle}
    {$articleTitle|count_sentences}
```

Will output:

```
    Two Soviet Ships Collide - One Dies. Enraged Cow Injures Farmer with Axe.
    2
```
       
See also [`count_characters`](language-modifier-count-characters.md),
[`count_paragraphs`](language-modifier-count-paragraphs.md) and
[`count_words`](language-modifier-count-words.md).
