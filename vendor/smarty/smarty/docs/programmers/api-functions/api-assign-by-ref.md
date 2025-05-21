assignByRef()

assign values by reference

Description
===========

void

assignByRef

string

varname

mixed

var

This is used to [`assign()`](#api.assign) values to the templates by
reference.

> **Note**
>
> With the introduction of PHP5, `assignByRef()` is not necessary for
> most intents and purposes. `assignByRef()` is useful if you want a PHP
> array index value to be affected by its reassignment from a template.
> Assigned object properties behave this way by default.


    <?php
    // passing name/value pairs
    $smarty->assignByRef('Name', $myname);
    $smarty->assignByRef('Address', $address);
    ?>

       

See also [`assign()`](#api.assign),
[`clearAllAssign()`](#api.clear.all.assign), [`append()`](#api.append),
[`{assign}`](#language.function.assign) and
[`getTemplateVars()`](#api.get.template.vars).
