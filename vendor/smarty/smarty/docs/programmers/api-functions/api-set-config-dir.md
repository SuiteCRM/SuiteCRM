setConfigDir()

set the directories where config files are stored

Description
===========

Smarty

setConfigDir

string\|array

config\_dir


    <?php

    // set a single directory where the config files are stored
    $smarty->setConfigDir('./config');

    // view the config dir chain
    var_dump($smarty->getConfigDir());

    // set multiple directoríes where config files are stored
    $smarty->setConfigDir(array(
        'one' => './config',
        'two' => './config_2',
        'three' => './config_3',
    ));

    // view the config dir chain
    var_dump($smarty->getConfigDir());

    // chaining of method calls
    $smarty->setTemplateDir('./templates')
           ->setConfigDir('./config')
           ->setCompileDir('./templates_c')
           ->setCacheDir('./cache');

    ?>

       

See also [`getConfigDir()`](#api.get.config.dir),
[`addConfigDir()`](#api.add.config.dir) and
[`$config_dir`](#variable.config.dir).
