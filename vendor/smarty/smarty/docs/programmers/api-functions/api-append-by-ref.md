appendByRef()

append values by reference

Description
===========

void

appendByRef

string

varname

mixed

var

bool

merge

This is used to [`append()`](#api.append) values to the templates by
reference.

> **Note**
>
> With the introduction of PHP5, `appendByRef()` is not necessary for
> most intents and purposes. `appendByRef()` is useful if you want a PHP
> array index value to be affected by its reassignment from a template.
> Assigned object properties behave this way by default.

NOTE.PARAMETER.MERGE


    <?php
    // appending name/value pairs
    $smarty->appendByRef('Name', $myname);
    $smarty->appendByRef('Address', $address);
    ?>

       

See also [`append()`](#api.append), [`assign()`](#api.assign) and
[`getTemplateVars()`](#api.get.template.vars).
