# {html_options}

`{html_options}` is a [custom function](index.md) that
creates the html `<select><option>` group with the assigned data. It
takes care of which item(s) are selected by default as well.

## Attributes

| Attribute Name | Required                            | Description                                                                                                                                                                                             |
|----------------|-------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| values         | Yes, unless using options attribute | An array of values for dropdown                                                                                                                                                                         |
| output         | Yes, unless using options attribute | An array of output for dropdown                                                                                                                                                                         |
| selected       | No                                  | The selected option element(s) as a string or array                                                                                                                                                     |
| options        | Yes, unless using values and output | An associative array of values and output                                                                                                                                                               |
| name           | No                                  | Name of select group                                                                                                                                                                                    |
| strict         | No                                  | Will make the "extra" attributes *disabled* and *readonly* only be set, if they were supplied with either boolean *TRUE* or string *"disabled"* and *"readonly"* respectively (defaults to false) |

-   Required attributes are `values` and `output`, unless you use the
    combined `options` instead.

-   If the optional `name` attribute is given, the `<select></select>`
    tags are created, otherwise ONLY the `<option>` list is generated.

-   If a given value is an array, it will treat it as an html
    `<optgroup>`, and display the groups. Recursion is supported with
    `<optgroup>`.

-   All parameters that are not in the list above are printed as
    name/value-pairs inside the `<select>` tag. They are ignored if the
    optional `name` is not given.

-   All output is XHTML compliant.

## Examples

```php
<?php
$smarty->assign('myOptions', [
                                1800 => 'Joe Schmoe',
                                9904 => 'Jack Smith',
                                2003 => 'Charlie Brown']
                                );
$smarty->assign('mySelect', 9904);
```

The following template will generate a drop-down list. Note the presence
of the `name` attribute which creates the `<select>` tags.

```smarty
{html_options name=foo options=$myOptions selected=$mySelect}
```

Output of the above example would be:

```html
<select name="foo">
    <option value="1800">Joe Schmoe</option>
    <option value="9904" selected="selected">Jack Smith</option>
    <option value="2003">Charlie Brown</option>
</select>   
```

```php
<?php
$smarty->assign('cust_ids', [56,92,13]);
$smarty->assign('cust_names', [
                              'Joe Schmoe',
                              'Jane Johnson',
                              'Charlie Brown']);
$smarty->assign('customer_id', 92);
```

The above arrays would be output with the following template (note the
use of the php [`count()`](https://www.php.net/function.count) function as a
modifier to set the select size).

```smarty
<select name="customer_id" size="{$cust_names|@count}">
   {html_options values=$cust_ids output=$cust_names selected=$customer_id}
</select>
```

The above example would output:

```html
<select name="customer_id" size="3">
    <option value="56">Joe Schmoe</option>
    <option value="92" selected="selected">Jane Johnson</option>
    <option value="13">Charlie Brown</option>
</select>
```

```php
<?php

$sql = 'select type_id, types from contact_types order by type';
$smarty->assign('contact_types',$db->getAssoc($sql));

$sql = 'select contact_id, name, email, contact_type_id
        from contacts where contact_id='.$contact_id;
$smarty->assign('contact',$db->getRow($sql));

```

Where a template could be as follows. Note the use of the
[`truncate`](../language-modifiers/language-modifier-truncate.md) modifier.

```smarty
<select name="type_id">
    <option value='null'>-- none --</option>
    {html_options options=$contact_types|truncate:20 selected=$contact.type_id}
</select>
```

```php
<?php
$arr['Sport'] = array(6 => 'Golf', 9 => 'Cricket',7 => 'Swim');
$arr['Rest']  = array(3 => 'Sauna',1 => 'Massage');
$smarty->assign('lookups', $arr);
$smarty->assign('fav', 7);
```

The script above and the following template

```smarty
{html_options name=foo options=$lookups selected=$fav}
```

would output:

```html
<select name="foo">
    <optgroup label="Sport">
        <option value="6">Golf</option>
        <option value="9">Cricket</option>
        <option value="7" selected="selected">Swim</option>
    </optgroup>
    <optgroup label="Rest">
        <option value="3">Sauna</option>
        <option value="1">Massage</option>
    </optgroup>
</select>
```

See also [`{html_checkboxes}`](./language-function-html-checkboxes.md) and
[`{html_radios}`](./language-function-html-radios.md)
