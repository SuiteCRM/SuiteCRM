# {function}

`{function}` is used to create functions within a template and call them
just like a plugin function. Instead of writing a plugin that generates
presentational content, keeping it in the template is often a more
manageable choice. It also simplifies data traversal, such as deeply
nested menus.

> **Note**
>
> Template functions are defined global. Since the Smarty compiler is a
> single-pass compiler, The [`{call}`](#language.function.call) tag must
> be used to call a template function defined externally from the given
> template. Otherwise, you can directly use the function as
> `{funcname ...}` in the template.

## Attributes

| Attribute Name | Required | Description                                                   |
|----------------|----------|---------------------------------------------------------------|
| name           | Yes      | The name of the template function                             |
| \[var \...\]   | No       | default variable value to pass local to the template function |

-   The `{function}` tag must have the `name` attribute which contains
    the name of the template function. A tag with this name can be
    used to call the template function.

-   Default values for variables can be passed to the template function
    as [attributes](../language-basic-syntax/language-syntax-attributes.md). Like in PHP function
    declarations you can only use scalar values as default. The default
    values can be overwritten when the template function is being
    called.

-   You can use all variables from the calling template inside the
    template function. Changes to variables or new created variables
    inside the template function have local scope and are not visible
    inside the calling template after the template function is executed.



> **Note**
>
> You can pass any number of parameter to the template function when it
> is called. The parameter variables must not be declared in the
> `{funcname ...}` tag unless you what to use default values. Default
> values must be scalar and can not be variable. Variables must be
> passed when the template is called.

## Examples

```smarty
{* define the function *}
{function name=menu level=0}
{function menu level=0}          {* short-hand *}
  <ul class="level{$level}">
  {foreach $data as $entry}
    {if is_array($entry)}
      <li>{$entry@key}</li>
      {menu data=$entry level=$level+1}
    {else}
      <li>{$entry}</li>
    {/if}
  {/foreach}
  </ul>
{/function}

{* create an array to demonstrate *}
{$menu = ['item1','item2','item3' => ['item3-1','item3-2','item3-3' =>
['item3-3-1','item3-3-2']],'item4']}

{* run the array through the function *}
{menu data=$menu}
```

Will generate the following output

```
* item1
* item2
* item3
      o item3-1
      o item3-2
      o item3-3
            + item3-3-1
            + item3-3-2
* item4
```
      
See also [`{call}`](./language-function-call.md)
