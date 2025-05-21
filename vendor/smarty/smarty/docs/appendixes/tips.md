# Tips & Tricks

## Blank Variable Handling

There may be times when you want to print a default value for an empty
variable instead of printing nothing, such as printing `&nbsp;` so that
html table backgrounds work properly. Many would use an
[`{if}`](../designers/language-builtin-functions/language-function-if.md) statement to handle this, but there is a
shorthand way with Smarty, using the
[`default`](../designers/language-modifiers/language-modifier-default.md) variable modifier.

> **Note**
>
> "Undefined variable" errors will show an E\_NOTICE if not disabled in
> PHP's [`error_reporting()`](https://www.php.net/error_reporting) level or
> Smarty's [`$error_reporting`](../programmers/api-variables/variable-error-reporting.md) property and
> a variable had not been assigned to Smarty.

```smarty

    {* the long way *}
    {if $title eq ''}
       &nbsp;
    {else}
       {$title}
    {/if}

    {* the short way *}
    {$title|default:'&nbsp;'}

```        

See also [`default`](../designers/language-modifiers/language-modifier-default.md) modifier and [default
variable handling](#default-variable-handling).

## Default Variable Handling

If a variable is used frequently throughout your templates, applying the
[`default`](../designers/language-modifiers/language-modifier-default.md) modifier every time it is
mentioned can get a bit ugly. You can remedy this by assigning the
variable its default value with the
[`{assign}`](../designers/language-builtin-functions/language-function-assign.md) function.


    {* do this somewhere at the top of your template *}
    {assign var='title' value=$title|default:'no title'}

    {* if $title was empty, it now contains the value "no title" when you use it *}
    {$title}

        

See also [`default`](../designers/language-modifiers/language-modifier-default.md) modifier and [blank
variable handling](#blank-variable-handling).

## Passing variable title to header template

When the majority of your templates use the same headers and footers, it
is common to split those out into their own templates and
[`{include}`](../designers/language-builtin-functions/language-function-include.md) them. But what if the header
needs to have a different title, depending on what page you are coming
from? You can pass the title to the header as an
[attribute](../designers/language-basic-syntax/language-syntax-attributes.md) when it is included.

`mainpage.tpl` - When the main page is drawn, the title of "Main Page"
is passed to the `header.tpl`, and will subsequently be used as the
title.

```smarty

{include file='header.tpl' title='Main Page'}
{* template body goes here *}
{include file='footer.tpl'}

```

`archives.tpl` - When the archives page is drawn, the title will be
"Archives". Notice in the archive example, we are using a variable from
the `archives_page.conf` file instead of a hard coded variable.

```smarty

{config_load file='archive_page.conf'}

{include file='header.tpl' title=#archivePageTitle#}
{* template body goes here *}
{include file='footer.tpl'}

```
        

`header.tpl` - Notice that "Smarty News" is printed if the `$title`
variable is not set, using the [`default`](../designers/language-modifiers/language-modifier-default.md)
variable modifier.

```smarty

<html>
    <head>
        <title>{$title|default:'Smarty News'}</title>
    </head>
<body>
    
```
        

`footer.tpl`

```smarty

    </body>
</html>

```
        

## Dates

As a rule of thumb, always pass dates to Smarty as
[timestamps](https://www.php.net/time). This allows template designers to
use the [`date_format`](../designers/language-modifiers/language-modifier-date-format.md) modifier for
full control over date formatting, and also makes it easy to compare
dates if necessary.

```smarty
{$startDate|date_format}
```
        

This will output:

```
Jan 4, 2009
```

```smarty

{$startDate|date_format:"%Y/%m/%d"}

```
        

This will output:

```
2009/01/04
```

Dates can be compared in the template by timestamps with:

```smarty

{if $order_date < $invoice_date}
   ...do something..
{/if}

```        

When using [`{html_select_date}`](../designers/language-custom-functions/language-function-html-select-date.md)
in a template, the programmer will most likely want to convert the
output from the form back into timestamp format. Here is a function to
help you with that.

```php

<?php

// this assumes your form elements are named
// startDate_Day, startDate_Month, startDate_Year

$startDate = makeTimeStamp($startDate_Year, $startDate_Month, $startDate_Day);

function makeTimeStamp($year='', $month='', $day='')
{
   if(empty($year)) {
       $year = strftime('%Y');
   }
   if(empty($month)) {
       $month = strftime('%m');
   }
   if(empty($day)) {
       $day = strftime('%d');
   }

   return mktime(0, 0, 0, $month, $day, $year);
}

```
    

See also [`{html_select_date}`](../designers/language-custom-functions/language-function-html-select-date.md),
[`{html_select_time}`](../designers/language-custom-functions/language-function-html-select-time.md),
[`date_format`](../designers/language-modifiers/language-modifier-date-format.md) and
[`$smarty.now`](../designers/language-variables/language-variables-smarty.md#smarty-now),

## Componentized Templates

Traditionally, programming templates into your applications goes as
follows: First, you accumulate your variables within your PHP
application, (maybe with database queries.) Then, you instantiate your
Smarty object, [`assign()`](../programmers/api-functions/api-assign.md) the variables and
[`display()`](../programmers/api-functions/api-display.md) the template. So lets say for example we
have a stock ticker on our template. We would collect the stock data in
our application, then assign these variables in the template and display
it. Now wouldn't it be nice if you could add this stock ticker to any
application by merely including the template, and not worry about
fetching the data up front?

You can do this by writing a custom plugin for fetching the content and
assigning it to a template variable.

`function.load_ticker.php` - drop file in
[`$plugins directory`](../programmers/api-variables/variable-plugins-dir.md)

```php

<?php

// setup our function for fetching stock data
function fetch_ticker($symbol)
{
   // put logic here that fetches $ticker_info
   // from some ticker resource
   return $ticker_info;
}

function smarty_function_load_ticker($params, $smarty)
{
   // call the function
   $ticker_info = fetch_ticker($params['symbol']);

   // assign template variable
   $smarty->assign($params['assign'], $ticker_info);
}

```

`index.tpl`

```smarty

{load_ticker symbol='SMARTY' assign='ticker'}

Stock Name: {$ticker.name} Stock Price: {$ticker.price}

``` 

See also: [`{include}`](../designers/language-builtin-functions/language-function-include.md).

## Obfuscating E-mail Addresses

Do you ever wonder how your email address gets on so many spam mailing
lists? One way spammers collect email addresses is from web pages. To
help combat this problem, you can make your email address show up in
scrambled javascript in the HTML source, yet it it will look and work
correctly in the browser. This is done with the
[`{mailto}`](../designers/language-custom-functions/language-function-mailto.md) plugin.

```smarty

<div id="contact">Send inquiries to
{mailto address=$EmailAddress encode='javascript' subject='Hello'}
</div>

```        

> **Note**
>
> This method isn\'t 100% foolproof. A spammer could conceivably program
> his e-mail collector to decode these values, but not likely\....
> hopefully..yet \... wheres that quantum computer :-?.

See also [`escape`](../designers/language-modifiers/language-modifier-escape.md) modifier and
[`{mailto}`](../designers/language-custom-functions/language-function-mailto.md).
