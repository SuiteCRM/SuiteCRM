getConfigDir()

return the directory where config files are stored

Description
===========

string\|array

getConfigDir

string

key


    <?php

    // set some config directories
    $smarty->setConfigDir(array(
        'one' => './config',
        'two' => './config_2',
        'three' => './config_3',
    ));

    // get all directories where config files are stored
    $config_dir = $smarty->getConfigDir();
    var_dump($config_dir); // array

    // get directory identified by key
    $config_dir = $smarty->getConfigDir('one');
    var_dump($config_dir); // string

    ?>

       

See also [`setConfigDir()`](#api.set.config.dir),
[`addConfigDir()`](#api.add.config.dir) and
[`$config_dir`](#variable.config.dir).
