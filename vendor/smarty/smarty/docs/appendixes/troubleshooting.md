# Troubleshooting

## Smarty/PHP errors

Smarty can catch many errors such as missing tag attributes or malformed
variable names. If this happens, you will see an error similar to the
following:

```
Warning: Smarty: [in index.tpl line 4]: syntax error: unknown tag - '%blah'
       in /path/to/smarty/Smarty.class.php on line 1041

Fatal error: Smarty: [in index.tpl line 28]: syntax error: missing section name
       in /path/to/smarty/Smarty.class.php on line 1041
```
        

Smarty shows you the template name, the line number and the error. After
that, the error consists of the actual line number in the Smarty class
that the error occurred.

There are certain errors that Smarty cannot catch, such as missing close
tags. These types of errors usually end up in PHP compile-time parsing
errors.

`Parse error: parse error in /path/to/smarty/templates_c/index.tpl.php on line 75`
    
When you encounter a PHP parsing error, the error line number will
correspond to the compiled PHP script, NOT the template itself. Usually
you can look at the template and spot the syntax error. Here are some
common things to look for: missing close tags for
[`{if}{/if}`](../designers/language-builtin-functions/language-function-if.md) or
[`{section}{/section}`](../designers/language-builtin-functions/language-function-section.md), 
or syntax of logic within an `{if}` tag. If you can\'t find the error, you might have to
open the compiled PHP file and go to the line number to figure out where
the corresponding error is in the template.

```
Warning: Smarty error: unable to read resource: "index.tpl" in...
```
or
```
Warning: Smarty error: unable to read resource: "site.conf" in...
```

-   The [`$template_dir`](../programmers/api-variables/variable-template-dir.md) is incorrect, doesn't
    exist or the file `index.tpl` is not in the `templates/` directory

-   A [`{config_load}`](../designers/language-builtin-functions/language-function-config-load.md) function is
    within a template (or [`configLoad()`](../programmers/api-functions/api-config-load.md) has been
    called) and either [`$config_dir`](../programmers/api-variables/variable-config-dir.md) is
    incorrect, does not exist or `site.conf` is not in the directory.

```
Fatal error: Smarty error: the $compile_dir 'templates_c' does not exist,
or is not a directory...
```

-   Either the [`$compile_dir`](../programmers/api-variables/variable-compile-dir.md)is incorrectly
    set, the directory does not exist, or `templates_c` is a file and
    not a directory.

```
Fatal error: Smarty error: unable to write to $compile_dir '....
```
        

-   The [`$compile_dir`](../programmers/api-variables/variable-compile-dir.md) is not writable by the
    web server. See the bottom of the [installing
    smarty](../getting-started.md#installation) page for more about permissions.

```
Fatal error: Smarty error: the $cache_dir 'cache' does not exist,
or is not a directory. in /..
```
        
-   This means that [`$caching`](../programmers/api-variables/variable-caching.md) is enabled and
    either; the [`$cache_dir`](../programmers/api-variables/variable-cache-dir.md) is incorrectly set,
    the directory does not exist, or `cache/` is a file and not a
    directory.

```
Fatal error: Smarty error: unable to write to $cache_dir '/...
```

-   This means that [`$caching`](../programmers/api-variables/variable-caching.md) is enabled and the
    [`$cache_dir`](../programmers/api-variables/variable-cache-dir.md) is not writable by the web
    server. See the bottom of the [installing
    smarty](../getting-started.md#installation) page for permissions.

```
Warning: filemtime(): stat failed for /path/to/smarty/cache/3ab50a623e65185c49bf17c63c90cc56070ea85c.one.tpl.php 
in /path/to/smarty/libs/sysplugins/smarty_resource.php
```

-   This means that your application registered a custom error handler
    (using [set_error_handler()](https://www.php.net/set_error_handler))
    which is not respecting the given `$errno` as it should. If, for
    whatever reason, this is the desired behaviour of your custom error
    handler, please call
    [`muteExpectedErrors()`](../programmers/api-functions/api-mute-expected-errors.md) after you've
    registered your custom error handler.

See also [debugging](../designers/chapter-debugging-console.md).
