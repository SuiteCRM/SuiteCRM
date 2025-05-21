\$config\_dir {#variable.config.dir}
=============

This is the directory used to store [config files](#config.files) used
in the templates. Default is `./configs`, meaning that Smarty will look
for the `configs/` directory in the same directory as the executing php
script.

> **Note**
>
> It is not recommended to put this directory under the web server
> document root.

> **Note**
>
> As of Smarty 3.1 the attribute \$config\_dir is no longer accessible
> directly. Use [`getConfigDir()`](#api.get.config.dir),
> [`setConfigDir()`](#api.set.config.dir) and
> [`addConfigDir()`](#api.add.config.dir) instead.

See also [`getConfigDir()`](#api.get.config.dir),
[`setConfigDir()`](#api.set.config.dir) and
[`addConfigDir()`](#api.add.config.dir).
