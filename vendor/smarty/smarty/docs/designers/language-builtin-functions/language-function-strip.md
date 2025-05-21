# {strip}

Many times web designers run into the issue where white space and
carriage returns affect the output of the rendered HTML (browser
"features"), so you must run all your tags together in the template to
get the desired results. This usually ends up in unreadable or
unmanageable templates.

Anything within `{strip}{/strip}` tags are stripped of the extra spaces
or carriage returns at the beginnings and ends of the lines before they
are displayed. This way you can keep your templates readable, and not
worry about extra white space causing problems.

> **Note**
>
> `{strip}{/strip}` does not affect the contents of template variables,
> see the [strip modifier](../language-modifiers/language-modifier-strip.md) instead.

```smarty
{* the following will be all run into one line upon output *}
{strip}
    <table>
     <tr>
      <td>
       <a href="#">
        This is a test
       </a>
      </td>
     </tr>
    </table>
{/strip}
```
      
The above example will output:

```html
<table><tr><td><a href="#">This is a test</a></td></tr></table>
```

Notice that in the above example, all the lines begin and end with HTML
tags. Be aware that all the lines are run together. If you have plain
text at the beginning or end of any line, they will be run together, and
may not be desired results.

See also the [`strip`](../language-modifiers/language-modifier-strip.md) modifier.
