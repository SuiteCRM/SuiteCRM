# Basic Syntax

A simple Smarty template could look like this:
```smarty
<h1>{$title|escape}</h1>
<ul>
    {foreach $cities as $city}
        <li>{$city.name|escape} ({$city.population})</li>
    {foreachelse}
        <li>no cities found</li>        
    {/foreach}
</ul>
```

All Smarty template tags are enclosed within delimiters. By default
these are `{` and `}`, but they can be
[changed](../../programmers/api-variables/variable-left-delimiter.md).

For the examples in this manual, we will assume that you are using the
default delimiters. In Smarty, all content outside of delimiters is
displayed as static content, or unchanged. When Smarty encounters
template tags, it attempts to interpret them, and displays the
appropriate output in their place.

The basis components of the Smarty syntax are:

- [Comments](language-syntax-comments.md)
- [Variables](language-syntax-variables.md)
- [Functions](language-syntax-functions.md)
- [Attributes](language-syntax-attributes.md)
- [Quotes](language-syntax-quotes.md)
- [Math](language-math.md)
- [Escaping](language-escaping.md)
