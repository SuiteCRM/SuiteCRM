# {literal}

`{literal}` tags allow a block of data to be taken literally. This is
typically used around Javascript or stylesheet blocks where {curly
braces} would interfere with the template
[delimiter](../../programmers/api-variables/variable-left-delimiter.md) syntax. Anything within
`{literal}{/literal}` tags is not interpreted, but displayed as-is. If
you need template tags embedded in a `{literal}` block, consider using
[`{ldelim}{rdelim}`](./language-function-ldelim.md) to escape the individual
delimiters instead.

> **Note**
>
> `{literal}{/literal}` tags are normally not necessary, as Smarty
> ignores delimiters that are surrounded by whitespace. Be sure your
> javascript and CSS curly braces are surrounded by whitespace. This is
> new behavior to Smarty 3.

```smarty
<script>
   // the following braces are ignored by Smarty
   // since they are surrounded by whitespace
   function myFoo {
     alert('Foo!');
   }
   // this one will need literal escapement
   {literal}
     function myBar {alert('Bar!');}
   {/literal}
</script>
```

See also [`{ldelim} {rdelim}`](./language-function-ldelim.md) and the
[escaping Smarty parsing](../language-basic-syntax/language-escaping.md) page.
