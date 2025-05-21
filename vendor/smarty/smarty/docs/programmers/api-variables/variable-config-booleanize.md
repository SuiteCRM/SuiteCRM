\$config\_booleanize {#variable.config.booleanize}
====================

If set to TRUE, [config files](#config.files) values of `on/true/yes`
and `off/false/no` get converted to boolean values automatically. This
way you can use the values in the template like so:
`{if #foobar#}...{/if}`. If foobar was `on`, `true` or `yes`, the `{if}`
statement will execute. Defaults to TRUE.
