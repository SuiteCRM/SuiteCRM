<?php
if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');


// Repairing and rebuilding
global $current_user;
$current_user = new User();
$current_user->getSystemUser();

// Reparación de roles para garantizar que los usuarios no administradores pueden acceder a los módulos
echo '<h3>Repairing roles</h3>';
include 'modules/ACL/install_actions.php';
$GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ':  Repairing roles');
