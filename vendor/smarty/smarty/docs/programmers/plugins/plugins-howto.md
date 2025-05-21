How Plugins Work {#plugins.howto}
================

Plugins are always loaded on demand. Only the specific modifiers,
functions, resources, etc invoked in the templates scripts will be
loaded. Moreover, each plugin is loaded only once, even if you have
several different instances of Smarty running within the same request.

Pre/postfilters and output filters are a bit of a special case. Since
they are not mentioned in the templates, they must be registered or
loaded explicitly via API functions before the template is processed.
The order in which multiple filters of the same type are executed
depends on the order in which they are registered or loaded.

The [plugins directory](#variable.plugins.dir) can be a string
containing a path or an array containing multiple paths. To install a
plugin, simply place it in one of the directories and Smarty will use it
automatically.
