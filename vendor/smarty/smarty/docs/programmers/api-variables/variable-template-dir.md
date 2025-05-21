\$template\_dir {#variable.template.dir}
===============

This is the name of the default template directory. If you do not supply
a resource type when including files, they will be found here. By
default this is `./templates`, meaning that Smarty will look for the
`templates/` directory in the same directory as the executing php
script. \$template\_dir can also be an array of directory paths: Smarty
will traverse the directories and stop on the first matching template
found.

> **Note**
>
> It is not recommended to put this directory under the web server
> document root.

> **Note**
>
> If the directories known to `$template_dir` are relative to
> directories known to the
> [include\_path](https://www.php.net/ini.core.php#ini.include-path) you
> need to activate the [`$use_include_path`](#variable.use.include.path)
> option.

> **Note**
>
> As of Smarty 3.1 the attribute \$template\_dir is no longer accessible
> directly. Use [`getTemplateDir()`](#api.get.template.dir),
> [`setTemplateDir()`](#api.set.template.dir) and
> [`addTemplateDir()`](#api.add.template.dir) instead.

See also [`Template Resources`](#resources),
[`$use_include_path`](#variable.use.include.path),
[`getTemplateDir()`](#api.get.template.dir),
[`setTemplateDir()`](#api.set.template.dir) and
[`addTemplateDir()`](#api.add.template.dir).
