# Attributes

Most of the [functions](./language-syntax-functions.md) take attributes that
specify or modify their behavior. Attributes to Smarty functions are
much like HTML attributes. Static values don't have to be enclosed in
quotes, but it is required for literal strings. Variables with or
without modifiers may also be used, and should not be in quotes. You can
even use PHP function results, plugin results and complex expressions.

Some attributes require boolean values (TRUE or FALSE). These can be
specified as `true` and `false`. If an attribute has no value assigned
it gets the default boolean value of true.

## Examples
```smarty
{include file="header.tpl"}

{include file="header.tpl" nocache}  // is equivalent to nocache=true

{include file="header.tpl" attrib_name="attrib value"}

{include file=$includeFile}

{include file=#includeFile# title="My Title"}

{assign var=foo value={counter}}  // plugin result

{assign var=foo value=substr($bar,2,5)}  // PHP function result

{assign var=foo value=$bar|strlen}  // using modifier

{assign var=foo value=$buh+$bar|strlen}  // more complex expression

{html_select_date display_days=true}

{mailto address="smarty@example.com"}

<select name="company_id">
  {html_options options=$companies selected=$company_id}
</select>
```
      
> **Note**
>
> Although Smarty can handle some very complex expressions and syntax,
> it is a good rule of thumb to keep the template syntax minimal and
> focused on presentation. If you find your template syntax getting too
> complex, it may be a good idea to move the bits that do not deal
> explicitly with presentation to PHP by way of plugins or modifiers.
