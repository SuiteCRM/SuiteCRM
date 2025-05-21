# {html_checkboxes}

`{html_checkboxes}` is a [custom function](index.md)
that creates an html checkbox group with provided data. It takes care of
which item(s) are selected by default as well.

## Attributes

| Attribute Name | Required                            | Description                                                                                                                                                                                       |
|----------------|-------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| name           | No                                  | Name of checkbox list (defaults to 'checkbox')                                                                                                                                                    |
| values         | Yes, unless using options attribute | An array of values for checkbox buttons                                                                                                                                                           |
| output         | Yes, unless using options attribute | An array of output for checkbox buttons                                                                                                                                                           |
| selected       | No                                  | The selected checkbox element(s) as a string or array                                                                                                                                             |
| options        | Yes, unless using values and output | An associative array of values and output                                                                                                                                                         |
| separator      | No                                  | String of text to separate each checkbox item                                                                                                                                                     |
| assign         | No                                  | Assign checkbox tags to an array instead of output                                                                                                                                                |
| labels         | No                                  | Add <label\>-tags to the output (defaults to true)                                                                                                                                                |
| label\_ids     | No                                  | Add id-attributes to <label\> and <input\> to the output (defaults to false)                                                                                                                      |
| escape         | No                                  | Escape the output / content (values are always escaped) (defaults to true)                                                                                                                        |
| strict         | No                                  | Will make the "extra" attributes *disabled* and *readonly* only be set, if they were supplied with either boolean *TRUE* or string *"disabled"* and *"readonly"* respectively (defaults to false) |

-   Required attributes are `values` and `output`, unless you use `options` instead.

-   All output is XHTML compliant.

-   All parameters that are not in the list above are printed as
    name/value-pairs inside each of the created <input\>-tags.

## Examples
```php
<?php

$smarty->assign('cust_ids', array(1000,1001,1002,1003));
$smarty->assign('cust_names', array(
                                'Joe Schmoe',
                                'Jack Smith',
                                'Jane Johnson',
                                'Charlie Brown')
                              );
$smarty->assign('customer_id', 1001);
```

where template is

```smarty
{html_checkboxes name='id' values=$cust_ids output=$cust_names selected=$customer_id  separator='<br />'}
```

or where PHP code is:

```php
<?php

$smarty->assign(
    'cust_checkboxes', 
    [
     1000 => 'Joe Schmoe',
     1001 => 'Jack Smith',
     1002 => 'Jane Johnson',
     1003 => 'Charlie Brown',
    ]
);
$smarty->assign('customer_id', 1001);
```

and the template is

```smarty
{html_checkboxes name='id' options=$cust_checkboxes selected=$customer_id separator='<br />'}
```

both examples will output:

```html
<label><input type="checkbox" name="id[]" value="1000" />Joe Schmoe</label><br />
<label><input type="checkbox" name="id[]" value="1001" checked="checked" />Jack Smith</label>
<br />
<label><input type="checkbox" name="id[]" value="1002" />Jane Johnson</label><br />
<label><input type="checkbox" name="id[]" value="1003" />Charlie Brown</label><br />
```
      

```php
<?php

$sql = 'select type_id, types from contact_types order by type';
$smarty->assign('contact_types',$db->getAssoc($sql));

$sql = 'select contact_id, contact_type_id, contact '
       .'from contacts where contact_id=12';
$smarty->assign('contact',$db->getRow($sql));
```

The results of the database queries above would be output with.

```smarty
{html_checkboxes name='contact_type_id' options=$contact_types selected=$contact.contact_type_id separator='<br />'}
```

See also [`{html_radios}`](./language-function-html-radios.md) and
[`{html_options}`](./language-function-html-options.md)
