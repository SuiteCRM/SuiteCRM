\$auto\_literal {#variable.auto.literal}
===============

The Smarty delimiter tags { and } will be ignored so long as they are
surrounded by white space. This behavior can be disabled by setting
auto\_literal to false.

::: {.informalexample}

    <?php
    $smarty->auto_literal = false;
    ?>

            
:::

See also [Escaping Smarty parsing](#language.escaping),
