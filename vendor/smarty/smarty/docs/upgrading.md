# Upgrading from an older version

## Upgrading from v3 to v4

Smarty 4 is mostly identical to Smarty 3. Most notably, it adds support for PHP8 and drops support for PHP7.0 and below.
Additionally, some deprecated features that have long been discouraged have been dropped from the language.

### Muting PHP8 warnings
If you simultaneously upgrade Smarty to v4 van PHP to v8, you may notice your error logs filling up with warnings about undefined or null template vars
due to a change in how PHP handles these. This may be helpful to spot errors, but if you find this annoying, you can use
`$smarty->muteUndefinedOrNullWarnings()` to make Smarty convert these warnings into notices.

### ASP tags
You can no longer user ASP-style tags like `<% %>` and `<%= %>` in your templates.
Replace them with `{...}` tags.

### SmartyBC
Check your codebase for `SmartyBC`.
We have dropped deprecated API calls that where only accessible through the SmartyBC class.

### No more embedded PHP
We have completely dropped support for `{php}` and `{include_php}` tags and embedded PHP in templates.
Check your templates for this, and rewrite any embedded PHP blocks, by moving logic to your PHP files or by
creating a [plugin function](./programmers/plugins/plugins-functions.md).

### Other changes

Search your code for the following changes:

- `SMARTY_RESOURCE_CHAR_SET` and `SMARTY_RESOURCE_DATE_FORMAT` constants have been removed
- `Smarty::muteExpectedErrors` and `Smarty::unmuteExpectedErrors` API methods have been removed
- `Smarty::getVariable` method has been removed. Use [Smarty::getTemplateVars](programmers/api-functions/api-get-template-vars.md) instead.
- [Smarty::registerResource](programmers/api-functions/api-register-resource.md) no longer accepts an array of callback functions





