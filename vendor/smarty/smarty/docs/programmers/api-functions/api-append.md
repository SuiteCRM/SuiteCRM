append()

append an element to an assigned array

Description
===========

void

append

mixed

var

void

append

string

varname

mixed

var

bool

merge

If you append to a string value, it is converted to an array value and
then appended to. You can explicitly pass name/value pairs, or
associative arrays containing the name/value pairs. If you pass the
optional third parameter of TRUE, the value will be merged with the
current array instead of appended.

NOTE.PARAMETER.MERGE


    <?php
    // This is effectively the same as assign()
    $smarty->append('foo', 'Fred');
    // After this line, foo will now be seen as an array in the template
    $smarty->append('foo', 'Albert');

    $array = array(1 => 'one', 2 => 'two');
    $smarty->append('X', $array);
    $array2 = array(3 => 'three', 4 => 'four');
    // The following line will add a second element to the X array
    $smarty->append('X', $array2);

    // passing an associative array
    $smarty->append(array('city' => 'Lincoln', 'state' => 'Nebraska'));
    ?>

       

See also [`appendByRef()`](#api.append.by.ref),
[`assign()`](#api.assign) and
[`getTemplateVars()`](#api.get.template.vars)
