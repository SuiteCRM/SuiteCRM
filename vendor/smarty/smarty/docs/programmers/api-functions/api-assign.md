assign()

assign variables/objects to the templates

Description
===========

void

assign

mixed

var

void

assign

string

varname

mixed

var

bool

nocache

You can explicitly pass name/value pairs, or associative arrays
containing the name/value pairs.

If you pass the optional third `nocache` parameter of TRUE, the variable
is assigned as nocache variable. See
[`Cacheability of Variables`](#cacheability.variables) for details.

> **Note**
>
> When you assign/register objects to templates, be sure that all
> properties and methods accessed from the template are for presentation
> purposes only. It is very easy to inject application logic through
> objects, and this leads to poor designs that are difficult to manage.
> See the Best Practices section of the Smarty website.


    <?php
    // passing name/value pairs
    $smarty->assign('Name', 'Fred');
    $smarty->assign('Address', $address);

    // passing an associative array
    $smarty->assign(array('city' => 'Lincoln', 'state' => 'Nebraska'));

    // passing an array
    $myArray = array('no' => 10, 'label' => 'Peanuts');
    $smarty->assign('foo',$myArray);

    // passing a row from a database (eg adodb)
    $sql = 'select id, name, email from contacts where contact ='.$id;
    $smarty->assign('contact', $db->getRow($sql));
    ?>

These are accessed in the template with


    {* note the vars are case sensitive like php *}
    {$Name}
    {$Address}
    {$city}
    {$state}

    {$foo.no}, {$foo.label}
    {$contact.id}, {$contact.name},{$contact.email}

To access more complex array assignments see
[`{foreach}`](#language.function.foreach) and
[`{section}`](#language.function.section)

See also [`assignByRef()`](#api.assign.by.ref),
[`getTemplateVars()`](#api.get.template.vars),
[`clearAssign()`](#api.clear.assign), [`append()`](#api.append) and
[`{assign}`](#language.function.assign)
