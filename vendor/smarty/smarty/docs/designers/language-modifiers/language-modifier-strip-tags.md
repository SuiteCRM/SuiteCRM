# strip_tags

This strips out HTML markup tags, basically anything between `<` and `>`.

## Basic usage
```smarty
{$myVar|strip_tags}
```

## Parameters

| Parameter Position | Type | Required | Default | Description                                                |
|--------------------|------|----------|---------|------------------------------------------------------------|
| 1                  | bool | No       | TRUE    | This determines whether the tags are replaced by ' ' or '' |

## Examples

```php
<?php

$smarty->assign('articleTitle',
                "Blind Woman Gets <font face=\"helvetica\">New
Kidney</font> from Dad she Hasn't Seen in <b>years</b>."
               );

```
       

Where template is:

```smarty
{$articleTitle}
{$articleTitle|strip_tags} {* same as {$articleTitle|strip_tags:true} *}
{$articleTitle|strip_tags:false}
```

Will output:

```html
Blind Woman Gets <font face="helvetica">New Kidney</font> from Dad she Hasn't Seen in <b>years</b>.
Blind Woman Gets  New Kidney  from Dad she Hasn't Seen in  years .
Blind Woman Gets New Kidney from Dad she Hasn't Seen in years.
```

See also [`replace`](language-modifier-replace.md) and
[`regex_replace`](language-modifier-regex-replace.md).
