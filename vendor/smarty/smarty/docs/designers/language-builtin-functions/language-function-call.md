# {call}

`{call}` is used to call a template function defined by the
[`{function}`](./language-function-function.md) tag just like a plugin
function.

> **Note**
>
> Template functions are defined global. Since the Smarty compiler is a
> single-pass compiler, The `{call}` tag must
> be used to call a template function defined externally from the given
> template. Otherwise you can directly use the function as
> `{funcname ...}` in the template.

-   The `{call}` tag must have the `name` attribute which contains the
    name of the template function.

-   Values for variables can be passed to the template function as
    [attributes](../language-basic-syntax/language-syntax-attributes.md).

## Attributes

| Attribute Name | Required | Description                                                                              |
|----------------|----------|------------------------------------------------------------------------------------------|
| name           | Yes      | The name of the template function                                                        |
| assign         | No       | The name of the variable that the output of called template function will be assigned to |
| [var ...]      | No       | variable to pass local to template function                                              |

## Option Flags

| Name    | Description                                |
|---------|--------------------------------------------|
| nocache | Call the template function in nocache mode |


## Examples

```smarty
    {* define the function *}
    {function name=menu level=0}
      <ul class="level{$level}">
      {foreach $data as $entry}
        {if is_array($entry)}
          <li>{$entry@key}</li>
          {call name=menu data=$entry level=$level+1}
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
    {call name=menu data=$menu}
    {call menu data=$menu} {* short-hand *}
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

See also [`{function}`](./language-function-function.md).
