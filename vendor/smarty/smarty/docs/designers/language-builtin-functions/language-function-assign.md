# {assign}, {$var=...}

`{assign}` or `{$var=...}` is used for assigning template variables **during the
execution of a template**.

## Attributes of the {assign} syntax
| Attribute Name | Required   | Description                                                           |
|----------------|------------|-----------------------------------------------------------------------|
| var            |            | The name of the variable being assigned                               | 
| value          |            | The value being assigned                                              |                             
| scope          | (optional) | The scope of the assigned variable: \'parent\',\'root\' or \'global\' | 

## Attributes of the {$var=...} syntax
| Attribute Name | Required   | Description                                                           |
|----------------|------------|-----------------------------------------------------------------------|
| scope          | (optional) | The scope of the assigned variable: \'parent\',\'root\' or \'global\' | 

## Option Flags
| Name    | Description                                       |
|---------|---------------------------------------------------|
| nocache | Assigns the variable with the 'nocache' attribute |


> **Note**
>
> Assignment of variables in-template is essentially placing application
> logic into the presentation that may be better handled in PHP. Use at
> your own discretion.

## Examples

```smarty
{assign var="name" value="Bob"}  {* or *}
{assign "name" "Bob"} {* short-hand, or *}
{$name='Bob'}

The value of $name is {$name}.
```
      

The above example will output:

```
The value of $name is Bob.
```


```smarty
{assign var="name" value="Bob" nocache}  {* or *}
{assign "name" "Bob" nocache} {* short-hand, or *}
{$name='Bob' nocache}

The value of $name is {$name}.
```
The above example will output:
```
The value of $name is Bob.
```
      

```smarty
{assign var=running_total value=$running_total+$some_array[$row].some_value}  {* or *}
{$running_total=$running_total+$some_array[row].some_value}
```

Variables assigned in the included template will be seen in the
including template.

```smarty
{include file="sub_template.tpl"}

{* display variable assigned in sub_template *}
{$foo}<br>
```

The template above includes the example `sub_template.tpl` below:

```smarty

{* foo will be known also in the including template *}
{assign var="foo" value="something" scope=parent}
{$foo="something" scope=parent}

{* bar is assigned only local in the including template *}
{assign var="bar" value="value"} {* or *}
{$var="value"}

```

You can assign a variable to root of the current root tree. The variable
is seen by all templates using the same root tree.

```smarty
{assign var=foo value="bar" scope="root"}
```
      
A global variable is seen by all templates.

```smarty
{assign var=foo value="bar" scope="global"} {* or *}
{assign "foo" "bar" scope="global"} {* short-hand, or *}
{$foo="bar" scope="global"}
```
 
To access `{assign}` variables from a php script use
[`getTemplateVars()`](../../programmers/api-functions/api-get-template-vars.md). 
Here's the template that creates the variable `$foo`.

```smarty
{assign var="foo" value="Smarty"} {* or *}
{$foo="Smarty"}
```

The template variables are only available after/during template
execution as in the following script.

```php
<?php

// this will output nothing as the template has not been executed
echo $smarty->getTemplateVars('foo');

// fetch the template to a variable
$whole_page = $smarty->fetch('index.tpl');

// this will output 'smarty' as the template has been executed
echo $smarty->getTemplateVars('foo');

$smarty->assign('foo','Even smarter');

// this will output 'Even smarter'
echo $smarty->getTemplateVars('foo');
```

The following functions can also *optionally* assign template variables: [`{capture}`](#language.function.capture),
[`{include}`](#language.function.include),
[`{insert}`](#language.function.insert),
[`{counter}`](#language.function.counter),
[`{cycle}`](#language.function.cycle),
[`{eval}`](#language.function.eval),
[`{fetch}`](#language.function.fetch),
[`{math}`](#language.function.math) and
[`{textformat}`](#language.function.textformat).

See also [`{append}`](./language-function-append.md),
[`assign()`](#api.assign) and
[`getTemplateVars()`](#api.get.template.vars).
