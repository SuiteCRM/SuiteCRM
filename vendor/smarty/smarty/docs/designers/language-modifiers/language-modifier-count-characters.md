# count_characters

This is used to count the number of characters in a variable.

## Basic usage
```smarty
{$myVar|count_characters}
```

## Parameters

| Parameter | Type    | Required | Description                                                            |
|-----------|---------|----------|------------------------------------------------------------------------|
| 1         | boolean | No       | This determines whether to include whitespace characters in the count. |

## Examples

```php
<?php

    $smarty->assign('articleTitle', 'Cold Wave Linked to Temperatures.');

```

Where template is:

```smarty
    {$articleTitle}
    {$articleTitle|count_characters}
    {$articleTitle|count_characters:true}
```

Will output:

```
    Cold Wave Linked to Temperatures.
    29
    33
```
       
See also [`count_words`](language-modifier-count-words.md),
[`count_sentences`](language-modifier-count-sentences.md) and
[`count_paragraphs`](language-modifier-count-paragraphs.md).
