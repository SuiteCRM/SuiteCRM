\$autoload\_filters {#variable.autoload.filters}
===================

If there are some filters that you wish to load on every template
invocation, you can specify them using this variable and Smarty will
automatically load them for you. The variable is an associative array
where keys are filter types and values are arrays of the filter names.
For example:

::: {.informalexample}

    <?php
    $smarty->autoload_filters = array('pre' => array('trim', 'stamp'),
                                      'output' => array('convert'));
    ?>

            
:::

See also [`registerFilter()`](#api.register.filter) and
[`loadFilter()`](#api.load.filter)
