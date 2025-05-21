Naming Conventions {#plugins.naming.conventions}
==================

Plugin files and functions must follow a very specific naming convention
in order to be located by Smarty.

**plugin files** must be named as follows:

> `
>         type.name.php
>        `

-   Where `type` is one of these plugin types:

    -   function

    -   modifier

    -   block

    -   compiler

    -   prefilter

    -   postfilter

    -   outputfilter

    -   resource

    -   insert

-   And `name` should be a valid identifier; letters, numbers, and
    underscores only, see [php
    variables](https://www.php.net/language.variables).

-   Some examples: `function.html_select_date.php`, `resource.db.php`,
    `modifier.spacify.php`.

**plugin functions** inside the PHP files must be named as follows:

> `smarty_type_name`

-   The meanings of `type` and `name` are the same as above.

-   An example modifier name `foo` would be
    `function smarty_modifier_foo()`.

Smarty will output appropriate error messages if the plugin file it
needs is not found, or if the file or the plugin function are named
improperly.
