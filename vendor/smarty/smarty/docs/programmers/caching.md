Caching
=======

Caching is used to speed up a call to [`display()`](./api-functions/api-display.md) or
[`fetch()`](./api-functions/api-fetch.md) by saving its output to a file. If a cached
version of the call is available, that is displayed instead of
regenerating the output. Caching can speed things up tremendously,
especially templates with longer computation times. Since the output of
[`display()`](./api-functions/api-display.md) or [`fetch()`](./api-functions/api-fetch.md) is cached, one
cache file could conceivably be made up of several template files,
config files, etc.

Since templates are dynamic, it is important to be careful what you are
caching and for how long. For instance, if you are displaying the front
page of your website that does not change its content very often, it
might work well to cache this page for an hour or more. On the other
hand, if you are displaying a page with a timetable containing new
information by the minute, it would not make sense to cache this page.

## Table of contents
- [Setting Up Caching](./caching/caching-setting-up.md)
- [Multiple Caches Per Page](./caching/caching-multiple-caches.md)
- [Controlling Cacheability of Output](./caching/caching-groups.md)
- [Custom Cache Implementation](./caching/caching-custom.md)
