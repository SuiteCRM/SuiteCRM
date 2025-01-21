<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once 'custom/modules/Administration/SticAdvancedMenuController.php';
require_once 'modules/Configurator/Configurator.php';
$configurator = new Configurator();
$configurator->config['stic_advanced_menu_enabled'] = true;
$configurator->config['stic_advanced_menu_icons'] = '0';
$configurator->saveConfig();
SticAdvancedMenu::ConvertSuiteCRMMenuToAdvancedMenu();