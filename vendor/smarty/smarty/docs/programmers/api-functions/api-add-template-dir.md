addTemplateDir()

add a directory to the list of directories where templates are stored

Description
===========

Smarty

addTemplateDir

string\|array

template\_dir

string

key


    <?php

    // add directory where templates are stored
    $smarty->addTemplateDir('./templates_1');

    // add directory where templates are stored and specify array-key
    $smarty->addTemplateDir('./templates_1', 'one');

    // add multiple directories where templates are stored and specify array-keys
    $smarty->addTemplateDir(array(
        'two' => './templates_2',
        'three' => './templates_3',
    ));

    // view the template dir chain
    var_dump($smarty->getTemplateDir());

    // chaining of method calls
    $smarty->setTemplateDir('./templates')
           ->addTemplateDir('./templates_1', 'one')
           ->addTemplateDir('./templates_2', 'two');

    ?>

       

See also [`getTemplateDir()`](#api.get.template.dir),
[`setTemplateDir()`](#api.set.template.dir) and
[`$template_dir`](#variable.template.dir).
