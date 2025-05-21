Smarty Class Variables {#api.variables}
======================

These are all of the available Smarty class variables. You can access
them directly, or use the corresponding setter/getter methods.

- [$allow_php_templates](./api-variables/variable-allow-php-templates.md)
- [$auto_literal](./api-variables/variable-auto-literal.md)
- [$autoload_filters](./api-variables/variable-autoload-filters.md)
- [$cache_dir](./api-variables/variable-cache-dir.md)
- [$cache_id](./api-variables/variable-cache-id.md)
- [$cache_lifetime](./api-variables/variable-cache-lifetime.md)
- [$cache_locking](./api-variables/variable-cache-locking.md)
- [$cache_modified_check](./api-variables/variable-cache-modified-check.md)
- [$caching](./api-variables/variable-caching.md)
- [$caching_type](./api-variables/variable-caching-type.md)
- [$compile_check](./api-variables/variable-compile-check.md)
- [$compile_dir](./api-variables/variable-compile-dir.md)
- [$compile_id](./api-variables/variable-compile-id.md)
- [$compile_locking](./api-variables/variable-compile-locking.md)
- [$compiler_class](./api-variables/variable-compiler-class.md)
- [$config_booleanize](./api-variables/variable-config-booleanize.md)
- [$config_dir](./api-variables/variable-config-dir.md)
- [$config_overwrite](./api-variables/variable-config-overwrite.md)
- [$config_read_hidden](./api-variables/variable-config-read-hidden.md)
- [$debug_tpl](./api-variables/variable-debug-template.md)
- [$debugging](./api-variables/variable-debugging.md)
- [$debugging_ctrl](./api-variables/variable-debugging-ctrl.md)
- [$default_config_type](./api-variables/variable-default-config-type.md)
- [$default_modifiers](./api-variables/variable-default-modifiers.md)
- [$default_resource_type](./api-variables/variable-default-resource-type.md)
- [$default_config_handler_func](./api-variables/variable-default-config-handler-func.md)
- [$default_template_handler_func](./api-variables/variable-default-template-handler-func.md)
- [$direct_access_security](./api-variables/variable-direct-access-security.md)
- [$error_reporting](./api-variables/variable-error-reporting.md)
- [$escape_html](./api-variables/variable-escape-html.md)
- [$force_cache](./api-variables/variable-force-cache.md)
- [$force_compile](./api-variables/variable-force-compile.md)
- [$left_delimiter](./api-variables/variable-left-delimiter.md)
- [$locking_timeout](./api-variables/variable-locking-timeout.md)
- [$merge_compiled_includes](./api-variables/variable-merge-compiled-includes.md)
- [$plugins_dir](./api-variables/variable-plugins-dir.md)
- [$right_delimiter](./api-variables/variable-right-delimiter.md)
- [$smarty_debug_id](./api-variables/variable-smarty-debug-id.md)
- [$template_dir](./api-variables/variable-template-dir.md)
- [$trusted_dir](./api-variables/variable-trusted-dir.md)
- [$use_include_path](./api-variables/variable-use-include-path.md)
- [$use_sub_dirs](./api-variables/variable-use-sub-dirs.md)

> **Note**
>
> All class variables have magic setter/getter methods available.
> setter/getter methods are camelCaseFormat, unlike the variable itself.
> So for example, you can set and get the \$smarty-\>template\_dir
> variable with \$smarty-\>setTemplateDir(\$dir) and \$dir =
> \$smarty-\>getTemplateDir() respectively.

> **Note**
>
> See
> [`Changing settings by template`](./advanced-features/advanced-features-template-settings.md)
> section for how to change Smarty class variables for individual
> templates.
