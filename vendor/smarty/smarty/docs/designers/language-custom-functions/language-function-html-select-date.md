# {html_select_date}

`{html_select_date}` is a [custom function](index.md)
that creates date dropdowns. It can display any or all of: year, month,
and day. All parameters that are not in the list below are printed as
name/value-pairs inside the `<select>` tags of day, month and year.

## Attributes

| Attribute Name     | Default            | Description                                                                                                                                                                                                                                                                                                                                                                                                    |
|--------------------|--------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| prefix             | Date_              | What to prefix the var name with                                                                                                                                                                                                                                                                                                                                                                               |
| time               |                    | What date/time to pre-select. Accepts timestamps, DateTime objects or any string parseable by [strtotime()](https://www.php.net/strtotime). If an array is given, the attributes field_array and prefix are used to identify the array elements to extract year, month and day from. Omitting this parameter or supplying a falsy value will select the current date. To prevent date selection, pass in NULL. |
| start_year         | current year       | The first year in the dropdown, either year number, or relative to current year (+/- N)                                                                                                                                                                                                                                                                                                                        |
| end_year           | same as start_year | The last year in the dropdown, either year number, or relative to current year (+/- N)                                                                                                                                                                                                                                                                                                                         |
| display_days       | TRUE               | Whether to display days or not                                                                                                                                                                                                                                                                                                                                                                                 |
| display_months     | TRUE               | Whether to display months or not                                                                                                                                                                                                                                                                                                                                                                               |
| display_years      | TRUE               | Whether to display years or not                                                                                                                                                                                                                                                                                                                                                                                |
| month_names        |                    | List of strings to display for months. array(1 =\> 'Jan', ..., 12 =\> 'Dec')                                                                                                                                                                                                                                                                                                                                   |
| month_format       | \%B                | What format the month should be in (strftime)                                                                                                                                                                                                                                                                                                                                                                  |
| day_format         | \%02d              | What format the day output should be in (sprintf)                                                                                                                                                                                                                                                                                                                                                              |
| day_value_format   | \%d                | What format the day value should be in (sprintf)                                                                                                                                                                                                                                                                                                                                                               |
| year_as_text       | FALSE              | Whether or not to display the year as text                                                                                                                                                                                                                                                                                                                                                                     |
| reverse_years      | FALSE              | Display years in reverse order                                                                                                                                                                                                                                                                                                                                                                                 |
| field_array        |                    | If a name is given, the select boxes will be drawn such that the results will be returned to PHP in the form of name\[Day\], name\[Year\], name\[Month\].                                                                                                                                                                                                                                                      |
| day_size           |                    | Adds size attribute to select tag if given                                                                                                                                                                                                                                                                                                                                                                     |
| month_size         |                    | Adds size attribute to select tag if given                                                                                                                                                                                                                                                                                                                                                                     |
| year_size          |                    | Adds size attribute to select tag if given                                                                                                                                                                                                                                                                                                                                                                     |
| all_extra          |                    | Adds extra attributes to all select/input tags if given                                                                                                                                                                                                                                                                                                                                                        |
| day_extra          |                    | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                                            |
| month_extra        |                    | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                                            |
| year_extra         |                    | Adds extra attributes to select/input tags if given                                                                                                                                                                                                                                                                                                                                                            |
| all_id             |                    | Adds id-attribute to all select/input tags if given                                                                                                                                                                                                                                                                                                                                                            |
| day_id             |                    | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                                                |
| month_id           |                    | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                                                |
| year_id            |                    | Adds id-attribute to select/input tags if given                                                                                                                                                                                                                                                                                                                                                                |
| field_order        | MDY                | The order in which to display the fields                                                                                                                                                                                                                                                                                                                                                                       |
| field_separator    | \\n                | String printed between different fields                                                                                                                                                                                                                                                                                                                                                                        |
| month_value_format | \%m                | strftime() format of the month values, default is %m for month numbers.                                                                                                                                                                                                                                                                                                                                        |
| all_empty          |                    | If supplied then the first element of any select-box has this value as it's label and "" as it's value. This is useful to make the select-boxes read "Please select" for example.                                                                                                                                                                                                                              |
| year_empty         |                    | If supplied then the first element of the year's select-box has this value as it's label and "" as it's value. This is useful to make the select-box read "Please select a year" for example. Note that you can use values like "-MM-DD" as time-attribute to indicate an unselected year.                                                                                                                     |
| month_empty        |                    | If supplied then the first element of the month's select-box has this value as it's label and "" as it's value. . Note that you can use values like "YYYY\--DD" as time-attribute to indicate an unselected month.                                                                                                                                                                                             |
| day_empty          |                    | If supplied then the first element of the day's select-box has this value as it's label and "" as it's value. Note that you can use values like "YYYY-MM-" as time-attribute to indicate an unselected day.                                                                                                                                                                                                    |

> **Note**
>
> There is an useful php function on the [date tips page](../../appendixes/tips.md)
> for converting `{html_select_date}` form values to a timestamp.

## Exaples

Template code

```smarty
{html_select_date}
```

This will output:

```html
<select name="Date_Month">
    <option value="1">January</option>
    <option value="2">February</option>
    <option value="3">March</option>
      ..... snipped .....
    <option value="10">October</option>
    <option value="11">November</option>
    <option value="12" selected="selected">December</option>
</select>
<select name="Date_Day">
    <option value="1">01</option>
    <option value="2">02</option>
    <option value="3">03</option>
      ..... snipped .....
    <option value="11">11</option>
    <option value="12">12</option>
    <option value="13" selected="selected">13</option>
    <option value="14">14</option>
    <option value="15">15</option>
      ..... snipped .....
    <option value="29">29</option>
    <option value="30">30</option>
    <option value="31">31</option>
</select>
<select name="Date_Year">
    <option value="2006" selected="selected">2006</option>
</select>
```
      
```smarty
{* start and end year can be relative to current year *}
{html_select_date prefix='StartDate' time=$time start_year='-5'
       end_year='+1' display_days=false}
```

With 2000 as the current year the output:

```html
<select name="StartDateMonth">
    <option value="1">January</option>
    <option value="2">February</option>
    .... snipped ....
    <option value="11">November</option>
    <option value="12" selected="selected">December</option>
</select>
<select name="StartDateYear">
    <option value="1995">1995</option>
    .... snipped ....
    <option value="1999">1999</option>
    <option value="2000" selected="selected">2000</option>
    <option value="2001">2001</option>
</select>
```
      
See also [`{html_select_time}`](language-function-html-select-time.md),
[`date_format`](../language-modifiers/language-modifier-date-format.md),
[`$smarty.now`](../language-variables/language-variables-smarty.md#smartynow-languagevariablessmartynow) and the [date tips
page](../../appendixes/tips.md#dates).
