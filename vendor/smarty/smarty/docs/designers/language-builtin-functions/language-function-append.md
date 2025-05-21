# {append}

`{append}` is used for creating or appending template variable arrays
**during the execution of a template**.

## Attributes

| Attribute | Required   | Description                                                                                        |
|-----------|------------|----------------------------------------------------------------------------------------------------|
| var       |            | The name of the variable being assigned                                                            |
| value     |            | The value being assigned                                                                           |
| index     | (optional) | The index for the new array element. If not specified the value is append to the end of the array. |
| scope     | (optional) | The scope of the assigned variable: parent, root or global. Defaults to local if omitted.          |

## Option Flags

| Name    | Description                                         |
|---------|-----------------------------------------------------|
| nocache | Assigns the variable with the 'nocache' attribute   | 

> **Note**
>
> Assignment of variables in-template is essentially placing application
> logic into the presentation that may be better handled in PHP. Use at
> your own discretion.

## Examples

```smarty
{append var='name' value='Bob' index='first'}
{append var='name' value='Meyer' index='last'}
// or 
{append 'name' 'Bob' index='first'} {* short-hand *}
{append 'name' 'Meyer' index='last'} {* short-hand *}

The first name is {$name.first}.<br>
The last name is {$name.last}.
```
      
The above example will output:


    The first name is Bob.
    The last name is Meyer.

      

See also [`append()`](#api.append) and
[`getTemplateVars()`](#api.get.template.vars).
