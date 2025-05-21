Stream Template Resources {#resources.streams}
=========================

Streams allow you to use PHP streams as a template resource. The syntax
is much the same a traditional template resource names.

Smarty will first look for a registered template resource. If nothing is
found, it will check if a PHP stream is available. If a stream is
available, Smarty will use it to fetch the template.

> **Note**
>
> You can further define allowed streams with security enabled.

Using a PHP stream for a template resource from the display() function.

     
     $smarty->display('foo:bar.tpl');
     
       

Using a PHP stream for a template resource from within a template.

     
     {include file="foo:bar.tpl"}
     
       
