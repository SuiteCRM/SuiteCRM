getTags()

return tags used by template

Description
===========

string

getTags

object

template

This function returns an array of tagname/attribute pairs for all tags
used by the template. It uses the following parameters:

-   `template` is the template object.

> **Note**
>
> This function is experimental.


    <?php
    include('Smarty.class.php');
    $smarty = new Smarty;

    // create template object
    $tpl = $smarty->createTemplate('index.tpl');

    // get tags
    $tags = $smarty->getTags($tpl);

    print_r($tags);

    ?>

        
