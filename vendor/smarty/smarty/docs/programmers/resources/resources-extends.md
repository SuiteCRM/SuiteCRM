Extends Template Resources {#resources.extends}
==========================

The `extends:` resource is used to define child/parent relationships for
template inheritance from the PHP script. For details see section of
[Template Inheritance](#advanced.features.template.inheritance).

As of Smarty 3.1 the `extends:` resource may use any available [template
resource](#resources), including `string:` and `eval:`. When [templates
from strings](#resources.string) are used, make sure they are properly
(url or base64) encoded. Is an `eval:` resource found within an
inheritance chain, its \"don\'t save a compile file\" property is
superseded by the `extends:` resource. The templates within an
inheritance chain are not compiled separately, though. Only a single
compiled template will be generated.

> **Note**
>
> Use this when inheritance is required programmatically. When inheriting
> within PHP, it is not obvious from the child template what inheritance
> took place. If you have a choice, it is normally more flexible and
> intuitive to handle inheritance chains from within the templates.


    <?php
    $smarty->display('extends:parent.tpl|child.tpl|grandchild.tpl'); 

    // inheritance from multiple template sources
    $smarty->display('extends:db:parent.tpl|file:child.tpl|grandchild.tpl|eval:{block name="fooBazVar_"}hello world{/block}'); 
    ?>

      

See also [Template Inheritance](#advanced.features.template.inheritance)
[`{block}`](#language.function.block) and
[`{extends}`](#language.function.extends).
