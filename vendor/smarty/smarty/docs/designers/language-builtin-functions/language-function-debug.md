# {debug}

`{debug}` dumps the debug console to the page. This works regardless of
the [debug](../chapter-debugging-console.md) settings in the php script.
Since this gets executed at runtime, this is only able to show the
[assigned](../../programmers/api-functions/api-assign.md) variables; not the templates that are in use.
However, you can see all the currently available variables within the
scope of a template.

If caching is enabled and a page is loaded from cache `{debug}` does
show only the variables which assigned for the cached page.

In order to see also the variables which have been locally assigned
within the template it does make sense to place the `{debug}` tag at the
end of the template.

See also the [debugging console page](../chapter-debugging-console.md).
