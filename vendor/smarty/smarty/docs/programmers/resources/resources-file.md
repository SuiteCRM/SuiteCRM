File Template Resources {#resources.file}
=======================

Smarty ships with a built-in template resource for the filesystem. The
`file:` is the default resource. The resource key `file:` must only be
specified, if the
[`$default_resource_type`](#variable.default.resource.type) has been
changed.

If the file resource cannot find the requested template, the
[`$default_template_handler_func`](#variable.default.template.handler.func)
is invoked.

> **Note**
>
> As of Smarty 3.1 the file resource no longer walks through the
> [include\_path](https://www.php.net/ini.core.php#ini.include-path) unless
> [`$use_include_path` is activated](#variable.use.include.path)

Templates from \$template\_dir {#templates.from.template.dir}
------------------------------

The file resource pulls templates source files from the directories
specified in [`$template_dir`](#variable.template.dir). The list of
directories is traversed in the order they appear in the array. The
first template found is the one to process.


    <?php
    $smarty->display('index.tpl');
    $smarty->display('file:index.tpl'); // same as above
    ?>

       

From within a Smarty template


    {include file='index.tpl'}
    {include file='file:index.tpl'} {* same as above *}

       

Templates from a specific \$template\_dir {#templates.from.specified.template.dir}
-----------------------------------------

Smarty 3.1 introduced the bracket-syntax for specifying an element from
[`$template_dir`](#variable.template.dir). This allows websites
employing multiple sets of templates better control over which template
to access.

The bracket-syntax can be used from anywhere you can specify the `file:`
resource type.


    <?php

    // setup template directories
    $smarty->setTemplateDir(array(
        './templates',            // element: 0, index: 0
        './templates_2',          // element: 1, index: 1
        '10' => 'templates_10',   // element: 2, index: '10'
        'foo' => 'templates_foo', // element: 3, index: 'foo'
    ));

    /*
      assume the template structure
      ./templates/foo.tpl
      ./templates_2/foo.tpl
      ./templates_2/bar.tpl
      ./templates_10/foo.tpl
      ./templates_10/bar.tpl
      ./templates_foo/foo.tpl
    */

    // regular access
    $smarty->display('file:foo.tpl'); 
    // will load ./templates/foo.tpl

    // using numeric index
    $smarty->display('file:[1]foo.tpl'); 
    // will load ./templates_2/foo.tpl

    // using numeric string index
    $smarty->display('file:[10]foo.tpl'); 
    // will load ./templates_10/foo.tpl

    // using string index
    $smarty->display('file:[foo]foo.tpl'); 
    // will load ./templates_foo/foo.tpl

    // using "unknown" numeric index (using element number)
    $smarty->display('file:[2]foo.tpl'); 
    // will load ./templates_10/foo.tpl

    ?>

       

From within a Smarty template


    {include file="file:foo.tpl"}
    {* will load ./templates/foo.tpl *}

    {include file="file:[1]foo.tpl"}
    {* will load ./templates_2/foo.tpl *}

    {include file="file:[foo]foo.tpl"}
    {* will load ./templates_foo/foo.tpl *}

       

Templates from any directory {#templates.from.any.dir}
----------------------------

Templates outside of the [`$template_dir`](#variable.template.dir)
require the `file:` template resource type, followed by the absolute
path to the template (with leading slash.)

> **Note**
>
> With [`Security`](#advanced.features.security) enabled, access to
> templates outside of the [`$template_dir`](#variable.template.dir) is
> not allowed unless you list those directories in `$secure_dir`.


    <?php
    $smarty->display('file:/export/templates/index.tpl');
    $smarty->display('file:/path/to/my/templates/menu.tpl');
    ?>

       

And from within a Smarty template:


    {include file='file:/usr/local/share/templates/navigation.tpl'}

       

Windows Filepaths {#templates.windows.filepath}
-----------------

If you are using a Windows machine, filepaths usually include a drive
letter (C:) at the beginning of the pathname. Be sure to use `file:` in
the path to avoid namespace conflicts and get the desired results.


    <?php
    $smarty->display('file:C:/export/templates/index.tpl');
    $smarty->display('file:F:/path/to/my/templates/menu.tpl');
    ?>

      

And from within Smarty template:


    {include file='file:D:/usr/local/share/templates/navigation.tpl'}
