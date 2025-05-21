setTemplateDir()

set the directories where templates are stored

Description
===========

Smarty

setTemplateDir

string\|array

template\_dir


    <?php

    // set a single directory where the templates are stored
    $smarty->setTemplateDir('./cache');

    // view the template dir chain
    var_dump($smarty->getTemplateDir());

    // set multiple directoríes where templates are stored
    $smarty->setTemplateDir(array(
        'one' => './templates',
        'two' => './templates_2',
        'three' => './templates_3',
    ));

    // view the template dir chain
    var_dump($smarty->getTemplateDir());

    // chaining of method calls
    $smarty->setTemplateDir('./templates')
           ->setCompileDir('./templates_c')
           ->setCacheDir('./cache');

    ?>

       

See also [`getTemplateDir()`](#api.get.template.dir),
[`addTemplateDir()`](#api.add.template.dir) and
[`$template_dir`](#variable.template.dir).
