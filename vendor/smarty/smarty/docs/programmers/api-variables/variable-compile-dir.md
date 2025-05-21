\$compile\_dir {#variable.compile.dir}
==============

This is the name of the directory where compiled templates are located.
By default this is `./templates_c`, meaning that Smarty will look for
the `templates_c/` directory in the same directory as the executing php
script. **This directory must be writeable by the web server**, [see
install](#installing.smarty.basic) for more info.

> **Note**
>
> This setting must be either a relative or absolute path. include\_path
> is not used for writing files.

> **Note**
>
> It is not recommended to put this directory under the web server
> document root.

> **Note**
>
> As of Smarty 3.1 the attribute \$compile\_dir is no longer accessible
> directly. Use [`getCompileDir()`](#api.get.compile.dir) and
> [`setCompileDir()`](#api.set.compile.dir) instead.

See also [`getCompileDir()`](#api.get.compile.dir),
[`setCompileDir()`](#api.set.compile.dir),
[`$compile_id`](#variable.compile.id) and
[`$use_sub_dirs`](#variable.use.sub.dirs).
