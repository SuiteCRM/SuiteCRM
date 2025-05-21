\$default\_config\_type {#variable.default.config.type}
=======================

This tells smarty what resource type to use for config files. The
default value is `file`, meaning that `$smarty->configLoad('test.conf')`
and `$smarty->configLoad('file:test.conf')` are identical in meaning.
See the [resource](#resources) chapter for more details.
