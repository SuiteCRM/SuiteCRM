Extending Smarty With Plugins {#plugins}
=============================

## Table of contents

- [How Plugins Work](./plugins/plugins-howto.md)
- [Naming Conventions](./plugins/plugins-naming-conventions.md) 
- [Writing Plugins](./plugins/plugins-writing.md) 
- [Template Functions](./plugins/plugins-functions.md) 
- [Modifiers](./plugins/plugins-modifiers.md)
- [Block Functions](./plugins/plugins-block-functions.md) 
- [Compiler Functions](./plugins/plugins-compiler-functions.md) 
- [Prefilters/Postfilters](./plugins/plugins-prefilters-postfilters.md)
- [Output Filters](./plugins/plugins-outputfilters.md) 
- [Resources](./plugins/plugins-resources.md)
- [Inserts](./plugins/plugins-inserts.md)

Version 2.0 introduced the plugin architecture that is used for almost
all the customizable functionality of Smarty. This includes:

-   functions

-   modifiers

-   block functions

-   compiler functions

-   prefilters

-   postfilters

-   outputfilters

-   resources

-   inserts

With the exception of resources, backwards compatibility with the old
way of registering handler functions via register\_\* API is preserved.
If you did not use the API but instead modified the class variables
`$custom_funcs`, `$custom_mods`, and other ones directly, then you will
need to adjust your scripts to either use the API or convert your custom
functionality into plugins.
