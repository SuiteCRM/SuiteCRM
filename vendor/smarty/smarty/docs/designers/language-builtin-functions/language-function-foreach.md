# {foreach},{foreachelse}

`{foreach}` is used for looping over arrays of data. `{foreach}` has a
simpler and cleaner syntax than the
[`{section}`](./language-function-section.md) loop, and can also loop over
associative arrays.

## Option Flags

| Name    | Description                              |
|---------|------------------------------------------|
| nocache | Disables caching of the `{foreach}` loop |


## Examples

```smarty

{foreach $arrayvar as $itemvar}
  {$itemvar|escape}
{/foreach}

{foreach $arrayvar as $keyvar=>$itemvar}
  {$keyvar}: {$itemvar|escape}
{/foreach}

```
> **Note**
>
> This foreach syntax does not accept any named attributes. This syntax
> is new to Smarty 3, however the Smarty 2.x syntax
> `{foreach from=$myarray key="mykey" item="myitem"}` is still
> supported.

-   `{foreach}` loops can be nested.

-   The `array` variable, usually an array of values, determines the
    number of times `{foreach}` will loop. You can also pass an integer
    for arbitrary loops.

-   `{foreachelse}` is executed when there are no values in the `array`
    variable.

-   `{foreach}` properties are [`@index`](#index),
    [`@iteration`](#iteration),
    [`@first`](#first),
    [`@last`](#last),
    [`@show`](#show),
    [`@total`](#total).

-   `{foreach}` constructs are [`{break}`](#break),
    [`{continue}`](#continue).

-   Instead of specifying the `key` variable you can access the current
    key of the loop item by `{$item@key}` (see examples below).

> **Note**
>
> The `$var@property` syntax is new to Smarty 3, however when using the
> Smarty 2 `{foreach from=$myarray key="mykey" item="myitem"}` style
> syntax, the `$smarty.foreach.name.property` syntax is still supported.

> **Note**
>
> Although you can retrieve the array key with the syntax
> `{foreach $myArray as $myKey => $myValue}`, the key is always
> available as `$myValue@key` within the foreach loop.

```php
<?php
$arr = array('red', 'green', 'blue');
$smarty->assign('myColors', $arr);
```
    
Template to output `$myColors` in an un-ordered list

```smarty
<ul>
    {foreach $myColors as $color}
        <li>{$color}</li>
    {/foreach}
</ul>
```
      
The above example will output:

```html
<ul>
    <li>red</li>
    <li>green</li>
    <li>blue</li>
</ul>
```

```php      
<?php
$people = array('fname' => 'John', 'lname' => 'Doe', 'email' => 'j.doe@example.com');
$smarty->assign('myPeople', $people);
```

Template to output `$myArray` as key/value pairs.

```smarty
<ul>
    {foreach $myPeople as $value}
       <li>{$value@key}: {$value}</li>
    {/foreach}
</ul>
```
     
The above example will output:

```html
<ul>
    <li>fname: John</li>
    <li>lname: Doe</li>
    <li>email: j.doe@example.com</li>
</ul>
```
      
Assign an array to Smarty, the key contains the key for each looped
value.

```php
<?php
 $smarty->assign(
    'contacts', 
    [
         ['phone' => '555-555-1234', 'fax' => '555-555-5678', 'cell' => '555-555-0357'],
         ['phone' => '800-555-4444', 'fax' => '800-555-3333', 'cell' => '800-555-2222'],
     ]
 );
```

The template to output `$contact`.

```smarty
{* key always available as a property *}
{foreach $contacts as $contact}
  {foreach $contact as $value}
    {$value@key}: {$value}
  {/foreach}
{/foreach}

{* accessing key the PHP syntax alternate *}
{foreach $contacts as $contact}
  {foreach $contact as $key => $value}
    {$key}: {$value}
  {/foreach}
{/foreach}
```
      
Either of the above examples will output:

```
  phone: 555-555-1234
  fax: 555-555-5678
  cell: 555-555-0357
  phone: 800-555-4444
  fax: 800-555-3333
  cell: 800-555-2222
```
      
A database (PDO) example of looping over search results. This example is
looping over a PHP iterator instead of an array().

```php
<?php 
  include('Smarty.class.php'); 

  $smarty = new Smarty; 

  $dsn = 'mysql:host=localhost;dbname=test'; 
  $login = 'test'; 
  $passwd = 'test'; 

  // setting PDO to use buffered queries in mysql is 
  // important if you plan on using multiple result cursors 
  // in the template. 

  $db = new PDO($dsn, $login, $passwd, array( 
     PDO::MYSQL_ATTR_USE_BUFFERED_QUERY => true)); 

  $res = $db->prepare("select * from users"); 
  $res->execute(); 
  $res->setFetchMode(PDO::FETCH_LAZY); 

  // assign to smarty 
  $smarty->assign('res',$res); 

  $smarty->display('index.tpl');?>
```

```smarty
{foreach $res as $r} 
  {$r.id} 
  {$r.name}
{foreachelse}
  .. no results .. 
{/foreach}
```

The above is assuming the results contain the columns named `id` and
`name`.

What is the advantage of an iterator vs. looping over a plain old array?
With an array, all the results are accumulated into memory before being
looped. With an iterator, each result is loaded/released within the
loop. This saves processing time and memory, especially for very large
result sets.

## @index

`index` contains the current array index, starting with zero.

```smarty
{* output empty row on the 4th iteration (when index is 3) *}
<table>
    {foreach $items as $i}
      {if $i@index eq 3}
         {* put empty table row *}
         <tr><td>nbsp;</td></tr>
      {/if}
      <tr><td>{$i.label}</td></tr>
    {/foreach}
</table>
```
      

## @iteration

`iteration` contains the current loop iteration and always starts at
one, unlike [`index`](#index). It is incremented by one
on each iteration.

The *"is div by"* operator can be used to detect a specific iteration.
Here we bold-face the name every 4th iteration.

```smarty
{foreach $myNames as $name}
  {if $name@iteration is div by 4}
    <b>{$name}</b>
  {/if}
  {$name}
{/foreach}
```

The *"is even by"* and *"is odd by"* operators can be used to
alternate something every so many iterations. Choosing between even or
odd rotates which one starts. Here we switch the font color every 3rd
iteration.

```smarty
 {foreach $myNames as $name}
   {if $name@iteration is even by 3}
     <span style="color: #000">{$name}</span>
   {else}
     <span style="color: #eee">{$name}</span>
   {/if}
 {/foreach}
```

This will output something similar to this:

```html
<span style="color: #000">...</span>
<span style="color: #000">...</span>
<span style="color: #000">...</span>
<span style="color: #eee">...</span>
<span style="color: #eee">...</span>
<span style="color: #eee">...</span>
<span style="color: #000">...</span>
<span style="color: #000">...</span>
<span style="color: #000">...</span>
<span style="color: #eee">...</span>
<span style="color: #eee">...</span>
<span style="color: #eee">...</span>
...
```
       
## @first

`first` is TRUE if the current `{foreach}` iteration is the initial one.
Here we display a table header row on the first iteration.

```smarty
{* show table header at first iteration *}
<table>
    {foreach $items as $i}
      {if $i@first}
        <tr>
          <th>key</td>
          <th>name</td>
        </tr>
      {/if}
      <tr>
        <td>{$i@key}</td>
        <td>{$i.name}</td>
      </tr>
    {/foreach}
</table>
```
      
## @last

`last` is set to TRUE if the current `{foreach}` iteration is the final
one. Here we display a horizontal rule on the last iteration.

```smarty
{* Add horizontal rule at end of list *}
{foreach $items as $item}
  <a href="#{$item.id}">{$item.name}</a>{if $item@last}<hr>{else},{/if}
{foreachelse}
  ... no items to loop ...
{/foreach}
```
      
## @show

The show `show` property can be used after the execution of a
`{foreach}` loop to detect if data has been displayed or not. `show` is
a boolean value.

```smarty
<ul>
    {foreach $myArray as $name}
        <li>{$name}</li>
    {/foreach}
</ul>
{if $name@show} do something here if the array contained data {/if}
```

## @total

`total` contains the number of iterations that this `{foreach}` will
loop. This can be used inside or after the `{foreach}`.

```smarty
{* show number of rows at end *}
{foreach $items as $item}
  {$item.name}<hr/>
  {if $item@last}
    <div id="total">{$item@total} items</div>
  {/if}
{foreachelse}
 ... no items to loop ...
{/foreach}
```

See also [`{section}`](./language-function-section.md),
[`{for}`](./language-function-for.md) and
[`{while}`](./language-function-while.md)

## {break}

`{break}` aborts the iteration of the array

```smarty     
  {$data = [1,2,3,4,5]}
  {foreach $data as $value}
    {if $value == 3}
      {* abort iterating the array *}
      {break}
    {/if}
    {$value}
  {/foreach}
  {*
    prints: 1 2
  *}
``` 
       
## {continue}

`{continue}` leaves the current iteration and begins with the next
iteration.

```smarty
  {$data = [1,2,3,4,5]}
  {foreach $data as $value}
    {if $value == 3}
      {* skip this iteration *}
      {continue}
    {/if}
    {$value}
  {/foreach}
  {*
    prints: 1 2 4 5
  *}
```
