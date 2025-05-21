\$default\_modifiers {#variable.default.modifiers}
====================

This is an array of modifiers to implicitly apply to every variable in a
template. For example, to HTML-escape every variable by default, use
`array('escape:"htmlall"')`. To make a variable exempt from default
modifiers, add the \'nofilter\' attribute to the output tag such as
`{$var nofilter}`.
