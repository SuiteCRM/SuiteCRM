# date_format

This formats a date and time into the given
[`strftime()`](https://www.php.net/strftime) format. Dates can be passed to
Smarty as unix [timestamps](https://www.php.net/function.time), [DateTime
objects](https://www.php.net/class.DateTime), mysql timestamps or any string
made up of month day year, parsable by php\'s
[`strtotime()`](https://www.php.net/strtotime). Designers can then use
`date_format` to have complete control of the formatting of the date. If
the date passed to `date_format` is empty and a second parameter is
passed, that will be used as the date to format.

## Basic usage
```smarty
{$myVar|date_format:"%Y-%m-%d"}
```

## Parameters

| Parameter Position | Type   | Required | Default   | Description                                     |
|--------------------|--------|----------|-----------|-------------------------------------------------|
| 1                  | string | No       | %b %e, %Y | This is the format for the outputted date.      |
| 2                  | string | No       | n/a       | This is the default date if the input is empty. |

> **Note**
>
> Since Smarty-2.6.10 numeric values passed to `date_format` are
> *always* (except for mysql timestamps, see below) interpreted as a
> unix timestamp.
>
> Before Smarty-2.6.10 numeric strings that where also parsable by
> `strtotime()` in php (like `YYYYMMDD`) where sometimes (depending on
> the underlying implementation of `strtotime()`) interpreted as date
> strings and NOT as timestamps.
>
> The only exception are mysql timestamps: They are also numeric only
> and 14 characters long (`YYYYMMDDHHMMSS`), mysql timestamps have
> precedence over unix timestamps.

> **Note**
>
> `date_format` is essentially a wrapper to PHP's
> [`strftime()`](https://www.php.net/strftime) function. You may have more
> or less conversion specifiers available depending on your system's
> [`strftime()`](https://www.php.net/strftime) function where PHP was
> compiled. Check your system\'s manpage for a full list of valid
> specifiers. However, a few of the specifiers are emulated on Windows.
> These are: %D, %e, %h, %l, %n, %r, %R, %t, %T.

## Examples

```php
<?php

$config['date'] = '%I:%M %p';
$config['time'] = '%H:%M:%S';
$smarty->assign('config', $config);
$smarty->assign('yesterday', strtotime('-1 day'));

```

This template uses [`$smarty.now`](../language-variables/language-variables-smarty.md#smartynow-languagevariablessmartynow) to
get the current time:

```smarty
{$smarty.now|date_format}
{$smarty.now|date_format:"%D"}
{$smarty.now|date_format:$config.date}
{$yesterday|date_format}
{$yesterday|date_format:"%A, %B %e, %Y"}
{$yesterday|date_format:$config.time}
```

This above will output:

```
Jan 1, 2022
01/01/22
02:33 pm
Dec 31, 2021
Monday, December 1, 2021
14:33:00
```
       
## Conversion specifiers

`date_format` conversion specifiers:

-   %a - abbreviated weekday name according to the current locale
-   %A - full weekday name according to the current locale
-   %b - abbreviated month name according to the current locale
-   %B - full month name according to the current locale
-   %c - preferred date and time representation for the current locale
-   %C - century number (the year divided by 100 and truncated to an
    integer, range 00 to 99)
-   %d - day of the month as a decimal number (range 01 to 31)
-   %D - same as %m/%d/%y
-   %e - day of the month as a decimal number, a single digit is
    preceded by a space (range 1 to 31)
-   %g - Week-based year within century \[00,99\]
-   %G - Week-based year, including the century \[0000,9999\]
-   %h - same as %b
-   %H - hour as a decimal number using a 24-hour clock (range 00
    to 23)
-   %I - hour as a decimal number using a 12-hour clock (range 01
    to 12)
-   %j - day of the year as a decimal number (range 001 to 366)
-   %k - Hour (24-hour clock) single digits are preceded by a blank.
    (range 0 to 23)
-   %l - hour as a decimal number using a 12-hour clock, single digits
    preceded by a space (range 1 to 12)
-   %m - month as a decimal number (range 01 to 12)
-   %M - minute as a decimal number
-   %n - newline character
-   %p - either 'am' or 'pm' according to the given time value, or
    the corresponding strings for the current locale
-   %r - time in a.m. and p.m. notation
-   %R - time in 24 hour notation
-   %S - second as a decimal number
-   %t - tab character
-   %T - current time, equal to %H:%M:%S
-   %u - weekday as a decimal number \[1,7\], with 1 representing
    Monday
-   %U - week number of the current year as a decimal number, starting
    with the first Sunday as the first day of the first week
-   %V - The ISO 8601:1988 week number of the current year as a decimal
    number, range 01 to 53, where week 1 is the first week that has at
    least 4 days in the current year, and with Monday as the first day
    of the week.
-   %w - day of the week as a decimal, Sunday being 0
-   %W - week number of the current year as a decimal number, starting
    with the first Monday as the first day of the first week
-   %x - preferred date representation for the current locale without
    the time
-   %X - preferred time representation for the current locale without
    the date
-   %y - year as a decimal number without a century (range 00 to 99)
-   %Y - year as a decimal number including the century
-   %Z - time zone or name or abbreviation
-   %% - a literal '%' character

See also [`$smarty.now`](../language-variables/language-variables-smarty.md#smartynow-languagevariablessmartynow),
[`strftime()`](https://www.php.net/strftime),
[`{html_select_date}`](../language-custom-functions/language-function-html-select-date.md) and the
[date tips](../../appendixes/tips.md#dates) page.
