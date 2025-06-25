<?php
if(!defined('sugarEntry')) define('sugarEntry', true);
require_once 'include/entryPoint.php';

$status = (BeanFactory::getBean('Accounts')) ? "SticMonitorOk" : "offline";

echo $status; die;
