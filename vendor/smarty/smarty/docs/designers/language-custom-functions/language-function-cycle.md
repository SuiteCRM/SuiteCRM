# {cycle}

`{cycle}` is used to alternate a set of values. This makes it easy to
for example, alternate between two or more colors in a table, or cycle
through an array of values.

## Attributes

| Attribute Name | Required | Description                                                                                                 |
|----------------|----------|-------------------------------------------------------------------------------------------------------------|
| name           | No       | The name of the cycle                                                                                       |
| values         | Yes      | The values to cycle through, either a comma delimited list (see delimiter attribute), or an array of values |
| print          | No       | Whether to print the value or not (defaults to true)                                                        |
| advance        | No       | Whether or not to advance to the next value (defaults to true)                                              |
| delimiter      | No       | The delimiter to use in the values attribute (defaults to ',')                                              |
| assign         | No       | The template variable the output will be assigned to                                                        |
| reset          | No       | The cycle will be set to the first value and not advanced (defaults to false)                               |

-   You can `{cycle}` through more than one set of values in a template
    by supplying a `name` attribute. Give each `{cycle}` a unique
    `name`.

-   You can force the current value not to print with the `print`
    attribute set to FALSE. This would be useful for silently skipping a
    value.

-   The `advance` attribute is used to repeat a value. When set to
    FALSE, the next call to `{cycle}` will print the same value.

-   If you supply the `assign` attribute, the output of the `{cycle}`
    function will be assigned to a template variable instead of being
    output to the template.

## Examples
```smarty
{section name=rows loop=$data}
    <tr class="{cycle values="odd,even"}">
       <td>{$data[rows]}</td>
    </tr>
{/section}
```

The above template would output:

```html
    <tr class="odd">
       <td>1</td>
    </tr>
    <tr class="even">
       <td>2</td>
    </tr>
    <tr class="odd">
       <td>3</td>
    </tr>
```
