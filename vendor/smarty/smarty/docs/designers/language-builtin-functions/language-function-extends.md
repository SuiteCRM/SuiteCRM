# {extends}

`{extends}` tags are used in child templates in template inheritance for
extending parent templates. For details see section of [Template
Inheritance](../../programmers/advanced-features/advanced-features-template-inheritance.md).

-   The `{extends}` tag must be on the first line of the template.

-   If a child template extends a parent template with the `{extends}`
    tag it may contain only `{block}` tags. Any other template content
    is ignored.

-   Use the syntax for [template resources](../../programmers/resources.md) to extend files
    outside the [`$template_dir`](../../programmers/api-variables/variable-template-dir.md) directory.

## Attributes

| Attribute | Required | Description                                     |
|-----------|----------|-------------------------------------------------|
| file      | Yes      | The name of the template file which is extended |

> **Note**
>
> When extending a variable parent like `{extends file=$parent_file}`,
> make sure you include `$parent_file` in the
> [`$compile_id`](../../programmers/api-variables/variable-compile-id.md). Otherwise, Smarty cannot
> distinguish between different `$parent_file`s.

## Examples

```smarty
{extends file='parent.tpl'}
{extends 'parent.tpl'}  {* short-hand *}
```

See also [Template Inheritance](../../programmers/advanced-features/advanced-features-template-inheritance.md)
and [`{block}`](./language-function-block.md).
