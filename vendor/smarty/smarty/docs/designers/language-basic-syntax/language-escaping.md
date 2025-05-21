# Escaping Smarty parsing

It is sometimes desirable or even necessary to have Smarty ignore
sections it would otherwise parse. A classic example is embedding
Javascript or CSS code in a template. The problem arises as those
languages use the { and } characters which are also the default
[delimiters](../language-builtin-functions/language-function-ldelim.md) for Smarty.

> **Note**
>
> A good practice for avoiding escapement altogether is by separating
> your Javascript/CSS into their own files and use standard HTML methods
> to access them. This will also take advantage of browser script
> caching. When you need to embed Smarty variables/functions into your
> Javascript/CSS, then the following applies.

In Smarty templates, the { and } braces will be ignored so long as they
are surrounded by white space. This behavior can be disabled by setting
the Smarty class variable [`$auto_literal`](../../programmers/api-variables/variable-auto-literal.md) to
false.

## Examples

```smarty
<script>
   // the following braces are ignored by Smarty
   // since they are surrounded by whitespace
   function foobar {
    alert('foobar!');
   }
   // this one will need literal escapement
   {literal}
    function bazzy {alert('foobar!');}
   {/literal}
</script>
```  
     
[`{literal}..{/literal}`](../language-builtin-functions/language-function-literal.md) blocks are used
for escaping blocks of template logic. You can also escape the braces
individually with
[`{ldelim}`, `{rdelim}`](../language-builtin-functions/language-function-ldelim.md) tags or
[`{$smarty.ldelim}`,`{$smarty.rdelim}`](../language-variables/language-variables-smarty.md#smartyldelim-smartyrdelim-languagevariablessmartyldelim)
variables.

Smarty's default delimiters { and } cleanly represent presentational
content. However, if another set of delimiters suit your needs better,
you can change them with Smarty's
[`$left_delimiter`](../../programmers/api-variables/variable-left-delimiter.md) and
[`$right_delimiter`](../../programmers/api-variables/variable-right-delimiter.md) values.

> **Note**
>
> Changing delimiters affects ALL template syntax and escapement. Be
> sure to clear out cache and compiled files if you decide to change
> them.

```php
<?php

$smarty->left_delimiter = '<!--{';
$smarty->right_delimiter = '}-->';

$smarty->assign('foo', 'bar');
$smarty->assign('name', 'Albert');
$smarty->display('example.tpl');
```

Where the template is:

```smarty
Welcome <!--{$name}--> to Smarty
<script language="javascript">
  var foo = <!--{$foo}-->;
  function dosomething() {
    alert("foo is " + foo);
  }
  dosomething();
</script>
```
