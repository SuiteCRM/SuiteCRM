\$escape\_html {#variable.escape.html}
==============

Setting `$escape_html` to TRUE will escape all template variable output
by wrapping it in
`htmlspecialchars({$output}, ENT_QUOTES, $char_set);`,
which is the same as `{$variable|escape:"html"}`.

Template designers can choose to selectively disable this feature by
adding the `nofilter` flag: `{$variable nofilter}`.

Modifiers and Filters are run in the following order: modifier,
default\_modifier, \$escape\_html, registered variable filters,
autoloaded variable filters, template instance\'s variable filters.
Everything except the individual modifier can be disabled with the
`nofilter` flag.

> **Note**
>
> This is a compile time option. If you change the setting you must make
> sure that the templates get recompiled.
