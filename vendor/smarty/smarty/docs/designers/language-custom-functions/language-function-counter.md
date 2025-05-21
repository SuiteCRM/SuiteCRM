# {counter}

`{counter}` is used to print out a count. `{counter}` will remember the
count on each iteration. You can adjust the number, the interval and the
direction of the count, as well as determine whether to print the
value. You can run multiple counters concurrently by supplying a unique
name for each one. If you do not supply a name, the name "default" will
be used.

## Attributes

| Attribute Name | Required | Description                                               |
|----------------|----------|-----------------------------------------------------------|
| name           | No       | The name of the counter                                   |
| start          | No       | The initial number to start counting from (defaults to 1) |
| skip           | No       | The interval to count by (defaults to 1)                  |
| direction      | No       | The direction to count (up/down) (defaults to 'up')       |
| print          | No       | Whether or not to print the value (defaults to true)      |
| assign         | No       | the template variable the output will be assigned to      |

If you supply the `assign` attribute, the output of the `{counter}`
function will be assigned to this template variable instead of being
output to the template.

## Examples

```smarty

{* initialize the count *}
{counter start=0 skip=2}<br />
{counter}<br />
{counter}<br />
{counter}<br />

```

this will output:

```html
0<br />
2<br />
4<br />
6<br />
```
      
