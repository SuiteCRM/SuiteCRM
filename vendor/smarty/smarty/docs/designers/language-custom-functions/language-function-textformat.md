# {textformat}

`{textformat}` is a [block function](../../programmers/plugins/plugins-block-functions.md) used to
format text. It basically cleans up spaces and special characters, and
formats paragraphs by wrapping at a boundary and indenting lines.

You can set the parameters explicitly, or use a preset style. Currently,
"email" is the only available style.

## Attributes

| Attribute Name | Default          | Description                                                                            |
|----------------|------------------|----------------------------------------------------------------------------------------|
| style          | *n/a*            | Preset style                                                                           |
| indent         | *0*              | The number of chars to indent every line                                               |
| indent\_first  | *0*              | The number of chars to indent the first line                                           |
| indent\_char   | *(single space)* | The character (or string of chars) to indent with                                      |
| wrap           | *80*             | How many characters to wrap each line to                                               |
| wrap\_char     | *\\n*            | The character (or string of chars) to break each line with                             |
| wrap\_cut      | *FALSE*          | If TRUE, wrap will break the line at the exact character instead of at a word boundary |
| assign         | *n/a*            | The template variable the output will be assigned to                                   |

## Examples

```smarty
{textformat wrap=40}

This is foo.
This is foo.
This is foo.
This is foo.
This is foo.
This is foo.

This is bar.

bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.

{/textformat}
```

The above example will output:

```
This is foo. This is foo. This is foo.
This is foo. This is foo. This is foo.

This is bar.

bar foo bar foo foo. bar foo bar foo
foo. bar foo bar foo foo. bar foo bar
foo foo. bar foo bar foo foo. bar foo
bar foo foo. bar foo bar foo foo.
```

```smarty
{textformat wrap=40 indent=4}

This is foo.
This is foo.
This is foo.
This is foo.
This is foo.
This is foo.

This is bar.

bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.

{/textformat}
```

The above example will output:

```
    This is foo. This is foo. This is
    foo. This is foo. This is foo. This
    is foo.
    
    This is bar.
    
    bar foo bar foo foo. bar foo bar foo
    foo. bar foo bar foo foo. bar foo
    bar foo foo. bar foo bar foo foo.
    bar foo bar foo foo. bar foo bar
    foo foo.
```


```smarty
{textformat wrap=40 indent=4 indent_first=4}

This is foo.
This is foo.
This is foo.
This is foo.
This is foo.
This is foo.

This is bar.

bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.

{/textformat}
```

The above example will output:


```
   This is foo. This is foo. This
   is foo. This is foo. This is foo.
   This is foo.

   This is bar.

   bar foo bar foo foo. bar foo bar
   foo foo. bar foo bar foo foo. bar
   foo bar foo foo. bar foo bar foo
   foo. bar foo bar foo foo. bar foo
   bar foo foo.
```
      

```smarty
{textformat style="email"}

This is foo.
This is foo.
This is foo.
This is foo.
This is foo.
This is foo.

This is bar.

bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.
bar foo bar foo     foo.

{/textformat}
```

The above example will output:


```
This is foo. This is foo. This is foo. This is foo. This is foo. This is
foo.

This is bar.

bar foo bar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo
bar foo foo. bar foo bar foo foo. bar foo bar foo foo. bar foo bar foo
foo.
```
     

See also [`{strip}`](../language-builtin-functions/language-function-strip.md) and
[`wordwrap`](../language-modifiers/language-modifier-wordwrap.md).
