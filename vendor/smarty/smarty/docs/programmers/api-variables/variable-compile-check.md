\$compile\_check {#variable.compile.check}
================

Upon each invocation of the PHP application, Smarty tests to see if the
current template has changed (different timestamp) since the last time
it was compiled. If it has changed, it recompiles that template. If the
template has yet not been compiled at all, it will compile regardless of
this setting. By default this variable is set to TRUE.

Once an application is put into production (ie the templates won\'t be
changing), the compile check step is no longer needed. Be sure to set
`$compile_check` to FALSE for maximum performance. Note that if you
change this to FALSE and a template file is changed, you will \*not\*
see the change since the template will not get recompiled.

Note that up to Smarty 4.x, Smarty will check for the existence of 
the source template even if `$compile_check` is disabled.

If [`$caching`](#variable.caching) is enabled and `$compile_check` is
enabled, then the cache files will get regenerated if an involved
template file or config file was updated.

As of Smarty 3.1 `$compile_check` can be set to the value
`Smarty::COMPILECHECK_CACHEMISS`. This enables Smarty to revalidate the
compiled template, once a cache file is regenerated. So if there was a
cached template, but it\'s expired, Smarty will run a single
compile\_check before regenerating the cache.

See [`$force_compile`](#variable.force.compile) and
[`clearCompiledTemplate()`](#api.clear.compiled.tpl).
