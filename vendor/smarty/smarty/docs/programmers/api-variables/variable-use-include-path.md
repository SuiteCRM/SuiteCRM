\$use\_include\_path {#variable.use.include.path}
====================

This tells smarty to respect the
[include\_path](https://www.php.net/ini.core.php#ini.include-path) within
the [`File Template Resource`](#resources.file) handler and the plugin
loader to resolve the directories known to
[`$template_dir`](#variable.template.dir). The flag also makes the
plugin loader check the include\_path for
[`$plugins_dir`](#variable.plugins.dir).

> **Note**
>
> You should not design your applications to rely on the include\_path,
> as this may - depending on your implementation - slow down your system
> (and Smarty) considerably.

If use\_include\_path is enabled, file discovery for
[`$template_dir`](#variable.template.dir) and
[`$plugins_dir`](#variable.plugins.dir) work as follows.

-   For each element `$directory` in array (\$template\_dir or
    \$plugins\_dir) do

-   Test if requested file is in `$directory` relative to the [current
    working directory](https://www.php.net/function.getcwd.php). If file
    found, return it.

-   For each `$path` in include\_path do

-   Test if requested file is in `$directory` relative to the `$path`
    (possibly relative to the [current working
    directory](https://www.php.net/function.getcwd.php)). If file found,
    return it.

-   Try default\_handler or fail.

This means that whenever a directory/file relative to the current
working directory is encountered, it is preferred over anything
potentially accessible through the include\_path.

> **Note**
>
> Smarty does not filter elements of the include\_path. That means a
> \".:\" within your include path will trigger the current working
> directory lookup twice.

See also [`Template Resources`](#resources) and
[`$template_dir`](#variable.template.dir)
