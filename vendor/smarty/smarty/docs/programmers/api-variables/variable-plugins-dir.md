\$plugins\_dir {#variable.plugins.dir}
==============

This is the directory or directories where Smarty will look for the
plugins that it needs. Default is `plugins/` under the
[`SMARTY_DIR`](#constant.smarty.dir). If you supply a relative path,
Smarty will first look under the [`SMARTY_DIR`](#constant.smarty.dir),
then relative to the current working directory, then relative to the PHP
include\_path. If `$plugins_dir` is an array of directories, Smarty will
search for your plugin in each plugin directory **in the order they are
given**.

> **Note**
>
> For best performance, do not setup your `$plugins_dir` to have to use
> the PHP include path. Use an absolute pathname, or a path relative to
> `SMARTY_DIR` or the current working directory.

> **Note**
>
> As of Smarty 3.1 the attribute \$plugins\_dir is no longer accessible
> directly. Use [`getPluginsDir()`](#api.get.plugins.dir),
> [`setPluginsDir()`](#api.set.plugins.dir) and
> [`addPluginsDir()`](#api.add.plugins.dir) instead.

See also [`getPluginsDir()`](#api.get.plugins.dir),
[`setPluginsDir()`](#api.set.plugins.dir) and
[`addPluginsDir()`](#api.add.plugins.dir).
