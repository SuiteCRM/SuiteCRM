# {ldelim}, {rdelim}

`{ldelim}` and `{rdelim}` are used for [escaping](../language-basic-syntax/language-escaping.md)
template delimiters, by default **{** and **}**. You can also use
[`{literal}{/literal}`](./language-function-literal.md) to escape blocks of
text eg Javascript or CSS. See also the complementary
[`{$smarty.ldelim}`](../../programmers/api-variables/variable-left-delimiter.md).

```smarty
{* this will print literal delimiters out of the template *}

{ldelim}funcname{rdelim} is how functions look in Smarty!
```

The above example will output:

```
{funcname} is how functions look in Smarty!
```

Another example with some Javascript

```smarty
<script>
function foo() {ldelim}
    ... code ...
{rdelim}
</script>
```

will output

```html
<script>
function foo() {
    .... code ...
}
</script>
```

```smarty
<script>
    function myJsFunction(){ldelim}
        alert("The server name\n{$smarty.server.SERVER_NAME|escape:javascript}\n{$smarty.server.SERVER_ADDR|escape:javascript}");
    {rdelim}
</script>
<a href="javascript:myJsFunction()">Click here for Server Info</a>
```

See also [`{literal}`](./language-function-literal.md) and [escaping Smarty
parsing](../language-basic-syntax/language-escaping.md).
