# count_paragraphs

This is used to count the number of paragraphs in a variable.

## Basic usage
```smarty
{$myVar|count_paragraphs}
```

## Examples

```php
<?php

    $smarty->assign('articleTitle',
                     "War Dims Hope for Peace. Child's Death Ruins Couple's Holiday.\n\n
                     Man is Fatally Slain. Death Causes Loneliness, Feeling of Isolation."
                    );

```

Where template is:

```smarty
    {$articleTitle}
    {$articleTitle|count_paragraphs}
```
       

Will output:

```
    War Dims Hope for Peace. Child's Death Ruins Couple's Holiday.

    Man is Fatally Slain. Death Causes Loneliness, Feeling of Isolation.
    2
```

See also [`count_characters`](language-modifier-count-characters.md),
[`count_sentences`](language-modifier-count-sentences.md) and
[`count_words`](language-modifier-count-words.md).
