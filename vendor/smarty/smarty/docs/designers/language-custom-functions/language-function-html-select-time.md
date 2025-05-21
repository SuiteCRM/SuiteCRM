# {html_select_time}

`{html_select_time}` is a [custom function](index.md)
that creates time dropdowns for you. It can display any or all of: hour,
minute, second and meridian.

The `time` attribute can have different formats. It can be a unique
timestamp, a string of the format `YYYYMMDDHHMMSS` or a string that is
parseable by PHP's [`strtotime()`](https://www.php.net/strtotime).

## Attributes

| Attribute Name        | Default                                                | Description                                                                                                                                                                                                                                                                                                                                                                              |
|-----------------------|--------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| prefix                | Time\_                                                 | What to prefix the var name with                                                                                                                                                                                                                                                                                                                                                         |
| time                  | current [timestamp](https://www.php.net/function.time) | What date/time to pre-select. Accepts  [timestamp](https://www.php.net/function.time), [DateTime](https://www.php.net/class.DateTime), mysql timestamp or any string parsable by [`strtotime()`](https://www.php.net/strtotime). If an array is given, the attributes field\_array and prefix are used to identify the array elements to extract hour, minute, second and meridian from. |
| display\_hours        | TRUE                                                   | Whether or not to display hours                                                                                                                                                                                                                                                                                                                                                          |
| display\_minutes      | TRUE                                                   | Whether or not to display minutes                                                                                                                                                                                                                                                                                                                                                        |
| display\_seconds      | TRUE                                                   | Whether or not to display seconds                                                                                                                                                                                                                                                                                                                                                        |
| display\_meridian     | TRUE                                                   | Whether or not to display meridian (am/pm)                                                                                                                                                                                                                                                                                                                                               |
| use\_24\_hours        | TRUE                                                   | Whether or not to use 24 hour clock                                                                                                                                                                                                                                                                                                                                                      |
| minute\_interval      | 1                                                      | Number interval in minute dropdown                                                                                                                                                                                                                                                                                                                                                       |
| second\_interval      | 1                                                      | Number interval in second dropdown                                                                                                                                                                                                                                                                                                                                                       |
| hour\_format          | \%02d                                                  | What format the hour label should be in (sprintf)                                                                                                                                                                                                                                                                                                                                        |
| hour\_value\_format   | \%20d                                                  | What format the hour value should be in (sprintf)                                                                                                                                                                                                                                                                                                                                        |
| minute\_format        | \%02d                                                  | What format the minute label should be in (sprintf)                                                                                                                                                                                                                                                                                                                                      |
| minute\_value\_format | \%20d                                                  | What format the minute value should be in (sprintf)                                                                                                                                                                                                                                                                                                                                      |
| second\_format        | \%02d                                                  | What format the second label should be in (sprintf)                                                                                                                                                                                                                                                                                                                                      |
| second\_value\_format | \%20d                                                  | What format the second value should be in (sprintf)                                                                                                                                                                                                                                                                                                                                      |
| field\_array          | n/a                                                    | Outputs values to array of this name                                                                                                                                                                                                                                                                                                                                                     |
| all\_extra            | null                                                   | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                      |
| hour\_extra           | null                                                   | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                      |
| minute\_extra         | null                                                   | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                      |
| second\_extra         | null                                                   | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                      |
| meridian\_extra       | null                                                   | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                      |
| field\_separator      | \\n                                                    | String printed between different fields                                                                                                                                                                                                                                                                                                                                                  |
| option\_separator     | \\n                                                    | String printed between different options of a field                                                                                                                                                                                                                                                                                                                                      |
| all\_id               | null                                                   | Adds id-attribute to all select/input tags if given                                                                                                                                                                                                                                                                                                                                      |
| hour\_id              | null                                                   | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                          |
| minute\_id            | null                                                   | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                          |
| second\_id            | null                                                   | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                          |
| meridian\_id          | null                                                   | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                          |
| all\_empty            | null                                                   | If supplied then the first element of any select-box has this value as it's label and "" as it's value. This is useful to make the select-boxes read "Please select" for example.                                                                                                                                                                                                        |
| hour\_empty           | null                                                   | If supplied then the first element of the hour's select-box has this value as it's label and "" as it's value. This is useful to make the select-box read "Please select an hour" for example.                                                                                                                                                                                           |
| minute\_empty         | null                                                   | If supplied then the first element of the minute's select-box has this value as it's label and "" as it's value. This is useful to make the select-box read "Please select an minute" for example.                                                                                                                                                                                       |
| second\_empty         | null                                                   | If supplied then the first element of the second's select-box has this value as it's label and "" as it's value. This is useful to make the select-box read "Please select an second" for example.                                                                                                                                                                                       |
| meridian\_empty       | null                                                   | If supplied then the first element of the meridian's select-box has this value as it's label and "" as it's value. This is useful to make the select-box read "Please select an meridian" for example.                                                                                                                                                                                   |


## Examples

```smarty
{html_select_time use_24_hours=true}
```
     
At 9:20 and 23 seconds in the morning the template above would output:

```html
<select name="Time_Hour">
    <option value="00">00</option>
    <option value="01">01</option>
    ... snipped ....
    <option value="08">08</option>
    <option value="09" selected>09</option>
    <option value="10">10</option>
    ... snipped ....
    <option value="22">22</option>
    <option value="23">23</option>
</select>
<select name="Time_Minute">
    <option value="00">00</option>
    <option value="01">01</option>
    ... snipped ....
    <option value="19">19</option>
    <option value="20" selected>20</option>
    <option value="21">21</option>
    ... snipped ....
    <option value="58">58</option>
    <option value="59">59</option>
</select>
<select name="Time_Second">
    <option value="00">00</option>
    <option value="01">01</option>
    ... snipped ....
    <option value="22">22</option>
    <option value="23" selected>23</option>
    <option value="24">24</option>
    ... snipped ....
    <option value="58">58</option>
    <option value="59">59</option>
</select>
<select name="Time_Meridian">
    <option value="am" selected>AM</option>
    <option value="pm">PM</option>
</select>
```

See also [`$smarty.now`](../language-variables/language-variables-smarty.md#smartynow-languagevariablessmartynow),
[`{html_select_date}`](language-function-html-select-date.md) and the
[date tips page](../../appendixes/tips.md#dates).
