\$merge\_compiled\_includes {#variable.merge.compiled.includes}
===========================

By setting `$merge_compiled_includes` to TRUE Smarty will merge the
compiled template code of subtemplates into the compiled code of the
main template. This increases rendering speed of templates using a many
different sub-templates.

Individual sub-templates can be merged by setting the `inline` option
flag within the `{include}` tag. `$merge_compiled_includes` does not
have to be enabled for the `inline` merge.

::: {.informalexample}

    <?php
    $smarty->merge_compiled_includes = true;
    ?>

            
:::

> **Note**
>
> This is a compile time option. If you change the setting you must make
> sure that the templates get recompiled.

See also [`{include}`](#language.function.include) tag
