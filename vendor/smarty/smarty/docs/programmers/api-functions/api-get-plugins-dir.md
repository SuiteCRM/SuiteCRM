getPluginsDir()

return the directory where plugins are stored

Description
===========

array

getPluginsDir


    <?php

    // set some plugins directories
    $smarty->setPluginsDir(array(
        './plugins',
        './plugins_2',
    ));

    // get all directories where plugins are stored
    $config_dir = $smarty->getPluginsDir();
    var_dump($config_dir); // array

    ?>

       

See also [`setPluginsDir()`](#api.set.plugins.dir),
[`addPluginsDir()`](#api.add.plugins.dir) and
[`$plugins_dir`](#variable.plugins.dir).
