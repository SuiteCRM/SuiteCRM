# {debug}

`{debug}` dumps the debug console to the page. This works regardless of
the [debug](../chapter-debugging-console.md) settings in the php script.
Since this gets executed at runtime, this is only able to show the
[assigned](../../programmers/api-functions/api-assign.md) variables; not the templates that are in use.
However, you can see all the currently available variables within the
scope of a template.

| Attribute Name | Required | Description                                                |
|----------------|----------|------------------------------------------------------------|
| output         | No       | output type, html or javascript (defaults to 'javascript') |

See also the [debugging console page](../chapter-debugging-console.md).
