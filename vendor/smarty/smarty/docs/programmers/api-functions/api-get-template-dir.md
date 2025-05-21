getTemplateDir()

return the directory where templates are stored

Description
===========

string\|array

getTemplateDir

string

key


    <?php

    // set some template directories
    $smarty->setTemplateDir(array(
        'one' => './templates',
        'two' => './templates_2',
        'three' => './templates_3',
    ));

    // get all directories where templates are stored
    $template_dir = $smarty->getTemplateDir();
    var_dump($template_dir); // array

    // get directory identified by key
    $template_dir = $smarty->getTemplateDir('one');
    var_dump($template_dir); // string

    ?>

       

See also [`setTemplateDir()`](#api.set.template.dir),
[`addTemplateDir()`](#api.add.template.dir) and
[`$template_dir`](#variable.template.dir).
