# {for}

The `{for}{forelse}` tag is used to create simple loops. The following different formats are supported:

-   `{for $var=$start to $end}` simple loop with step size of 1.

-   `{for $var=$start to $end step $step}` loop with individual step
    size.

`{forelse}` is executed when the loop is not iterated.

## Attributes

| Attribute | Required | Description                    |
|-----------|----------|--------------------------------|
| max       | No       | Limit the number of iterations |

## Option Flags

| Name    | Description                          |
|---------|--------------------------------------|
| nocache | Disables caching of the `{for}` loop |

## Examples

```smarty
<ul>
    {for $foo=1 to 3}
        <li>{$foo}</li>
    {/for}
</ul>
```
      
The above example will output:

```html
<ul>
    <li>1</li>
    <li>2</li>
    <li>3</li>
</ul>
```
      
```php
<?php
$smarty->assign('to',10);
```
    
```smarty
<ul>
    {for $foo=3 to $to max=3}
        <li>{$foo}</li>
    {/for}
</ul>
```

The above example will output:

```html
<ul>
    <li>3</li>
    <li>4</li>
    <li>5</li>
</ul>
```

```php
<?php
$smarty->assign('start',10);
$smarty->assign('to',5);
```

```smarty
<ul>
    {for $foo=$start to $to}
        <li>{$foo}</li>
    {forelse}
      no iteration
    {/for}
</ul>
```

The above example will output:

```
   no iteration
```
      
See also [`{foreach}`](./language-function-foreach.md),
[`{section}`](./language-function-section.md) and
[`{while}`](./language-function-while.md)
