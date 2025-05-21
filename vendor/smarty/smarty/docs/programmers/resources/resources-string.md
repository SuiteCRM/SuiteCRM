String Template Resources {#resources.string}
=========================

Smarty can render templates from a string by using the `string:` or
`eval:` resource.

-   The `string:` resource behaves much the same as a template file. The
    template source is compiled from a string and stores the compiled
    template code for later reuse. Each unique template string will
    create a new compiled template file. If your template strings are
    accessed frequently, this is a good choice. If you have frequently
    changing template strings (or strings with low reuse value), the
    `eval:` resource may be a better choice, as it doesn\'t save
    compiled templates to disk.

-   The `eval:` resource evaluates the template source every time a page
    is rendered. This is a good choice for strings with low reuse value.
    If the same string is accessed frequently, the `string:` resource
    may be a better choice.

> **Note**
>
> With a `string:` resource type, each unique string generates a
> compiled file. Smarty cannot detect a string that has changed, and
> therefore will generate a new compiled file for each unique string. It
> is important to choose the correct resource so that you do not fill
> your disk space with wasted compiled strings.


    <?php
    $smarty->assign('foo','value');
    $template_string = 'display {$foo} here';
    $smarty->display('string:'.$template_string); // compiles for later reuse
    $smarty->display('eval:'.$template_string); // compiles every time
    ?>

      

From within a Smarty template


    {include file="string:$template_string"} {* compiles for later reuse *}
    {include file="eval:$template_string"} {* compiles every time *}


      

Both `string:` and `eval:` resources may be encoded with
[`urlencode()`](https://www.php.net/urlencode) or
[`base64_encode()`](https://www.php.net/urlencode). This is not necessary
for the usual use of `string:` and `eval:`, but is required when using
either of them in conjunction with
[`Extends Template Resource`](#resources.extends)

     
     <?php
     $smarty->assign('foo','value');
     $template_string_urlencode = urlencode('display {$foo} here');
     $template_string_base64 = base64_encode('display {$foo} here');
     $smarty->display('eval:urlencode:'.$template_string_urlencode); // will decode string using urldecode()
     $smarty->display('eval:base64:'.$template_string_base64); // will decode string using base64_decode()
     ?>
     
       

From within a Smarty template

     
     {include file="string:urlencode:$template_string_urlencode"} {* will decode string using urldecode() *}
     {include file="eval:base64:$template_string_base64"} {* will decode string using base64_decode() *}

     
       
