\$force\_compile {#variable.force.compile}
================

This forces Smarty to (re)compile templates on every invocation. This
setting overrides [`$compile_check`](#variable.compile.check). By
default this is FALSE. This is handy for development and
[debugging](#chapter.debugging.console). It should never be used in a
production environment. If [`$caching`](#variable.caching) is enabled,
the cache file(s) will be regenerated every time.
