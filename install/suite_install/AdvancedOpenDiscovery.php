<?php
function install_aod()
{
    require_once('modules/Administration/Administration.php');

    global $sugar_config;

    $sugar_config['aod']['enable_aod'] = false;

    ksort($sugar_config);
    write_array_to_file('sugar_config', $sugar_config, 'config.php');

    installAODHooks();
}

function installAODHooks()
{
    require_once('ModuleInstall/ModuleInstaller.php');

    $hooks = array(
        array(
            'module' => '',
            'hook' => 'after_save',
            'order' => 1,
            'description' => 'AOD Index Changes',
            'file' => 'modules/AOD_Index/AOD_LogicHooks.php',
            'class' => 'AOD_LogicHooks',
            'function' => 'saveModuleChanges',
        ),
        array(
            'module' => '',
            'hook' => 'after_delete',
            'order' => 1,
            'description' => 'AOD Index changes',
            'file' => 'modules/AOD_Index/AOD_LogicHooks.php',
            'class' => 'AOD_LogicHooks',
            'function' => 'saveModuleDelete',
        ),
        array(
            'module' => '',
            'hook' => 'after_restore',
            'order' => 1,
            'description' => 'AOD Index changes',
            'file' => 'modules/AOD_Index/AOD_LogicHooks.php',
            'class' => 'AOD_LogicHooks',
            'function' => 'saveModuleRestore',
        ),
    );

    foreach ($hooks as $hook) {
        check_logic_hook_file($hook['module'], $hook['hook'], array($hook['order'], $hook['description'], $hook['file'], $hook['class'], $hook['function']));
    }
}
