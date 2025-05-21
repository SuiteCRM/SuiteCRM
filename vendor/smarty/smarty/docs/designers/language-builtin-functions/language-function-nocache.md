# {nocache}

`{nocache}` is used to disable caching of a template section. Every
`{nocache}` must be paired with a matching `{/nocache}`.

> **Note**
>
> Be sure any variables used within a non-cached section are also
> assigned from PHP when the page is loaded from the cache.

```smarty
Today's date is
{nocache}
{$smarty.now|date_format}
{/nocache}
```
   
The above code will output the current date on a cached page.

See also the [caching section](../../programmers/caching.md).
