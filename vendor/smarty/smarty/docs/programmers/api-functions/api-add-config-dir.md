addConfigDir()

add a directory to the list of directories where config files are stored

Description
===========

Smarty

addConfigDir

string\|array

config\_dir

string

key


    <?php

    // add directory where config files are stored
    $smarty->addConfigDir('./config_1');

    // add directory where config files are stored and specify array-key
    $smarty->addConfigDir('./config_1', 'one');

    // add multiple directories where config files are stored and specify array-keys
    $smarty->addTemplateDir(array(
        'two' => './config_2',
        'three' => './config_3',
    ));

    // view the template dir chain
    var_dump($smarty->getConfigDir());

    // chaining of method calls
    $smarty->setConfigDir('./config')
           ->addConfigDir('./config_1', 'one')
           ->addConfigDir('./config_2', 'two');

    ?>

       

See also [`getConfigDir()`](#api.get.config.dir),
[`setConfigDir()`](#api.set.config.dir) and
[`$config_dir`](#variable.config.dir).
