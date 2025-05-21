<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

function installStatus($msg, $cmd = null, $overwrite = false, $before = '[ok]<br>')
{
    $fname = 'install/status.json';
    if (!$overwrite && file_exists($fname)) {
        $stat = json_decode(file_get_contents($fname), null, 512, JSON_THROW_ON_ERROR);
        //$msg = json_encode($stat);
        $msg = $stat->message . $before . $msg;
    }
    file_put_contents($fname, json_encode(array(
        'message' => $msg,
        'command' => $cmd,
    ), JSON_THROW_ON_ERROR));
}
installStatus($mod_strings['LBL_START'], null, true, '');

// This file will load the configuration settings from session data,
// write to the config file, and execute any necessary database steps.
$GLOBALS['installing'] = true;
if (!isset($install_script) || !$install_script) {
    die($mod_strings['ERR_NO_DIRECT_SCRIPT']);
}
ini_set("output_buffering", "0");
set_time_limit(3600);
// flush after each output so the user can see the progress in real-time
ob_implicit_flush();


require_once('install/install_utils.php');

require_once('modules/TableDictionary.php');


$trackerManager = TrackerManager::getInstance();
$trackerManager->pause();


$cache_dir                          = sugar_cached("");
$line_entry_format                  = "&nbsp&nbsp&nbsp&nbsp&nbsp<b>";
$line_exit_format                   = "... &nbsp&nbsp</b>";
$rel_dictionary                 = $dictionary; // sourced by modules/TableDictionary.php
$render_table_close             = "";
$render_table_open                  = "";
$setup_db_admin_password            = $_SESSION['setup_db_admin_password'];
$setup_db_admin_user_name           = $_SESSION['setup_db_admin_user_name'];
$setup_db_create_database           = $_SESSION['setup_db_create_database'];
$setup_db_create_sugarsales_user    = $_SESSION['setup_db_create_sugarsales_user'];
$setup_db_database_name             = $_SESSION['setup_db_database_name'];
$setup_db_drop_tables               = $_SESSION['setup_db_drop_tables'];
$setup_db_host_instance             = $_SESSION['setup_db_host_instance'];
$setup_db_port_num                  = $_SESSION['setup_db_port_num'];
$setup_db_host_name                 = $_SESSION['setup_db_host_name'];
$demoData                           = $_SESSION['demoData'];
$setup_db_sugarsales_password       = $_SESSION['setup_db_sugarsales_password'];
$setup_db_sugarsales_user           = $_SESSION['setup_db_sugarsales_user'];
$setup_site_admin_user_name         = $_SESSION['setup_site_admin_user_name'];
$setup_site_admin_password          = $_SESSION['setup_site_admin_password'];
$setup_site_guid                    = (isset($_SESSION['setup_site_specify_guid']) && $_SESSION['setup_site_specify_guid'] != '') ? $_SESSION['setup_site_guid'] : '';
$setup_site_url                     = $_SESSION['setup_site_url'];
$parsed_url                         = parse_url((string) $setup_site_url);
$setup_site_host_name               = $parsed_url['host'];
$setup_site_log_dir                 = isset($_SESSION['setup_site_custom_log_dir']) ? $_SESSION['setup_site_log_dir'] : '.';
$setup_site_log_file                = 'suitecrm.log';  // may be an option later
$setup_site_session_path            = isset($_SESSION['setup_site_custom_session_path']) ? $_SESSION['setup_site_session_path'] : '';
$setup_site_log_level				='fatal';

/*sugar_cache_clear('TeamSetsCache');
if ( file_exists($cache_dir .'modules/Teams/TeamSetCache.php') ) {
    unlink($cache_dir.'modules/Teams/TeamSetCache.php');
}

sugar_cache_clear('TeamSetsMD5Cache');
if ( file_exists($cache_dir.'modules/Teams/TeamSetMD5Cache.php') ) {
    unlink($cache_dir.'modules/Teams/TeamSetMD5Cache.php');
}*/
$langHeader = get_language_header();
$out =<<<EOQ
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<!DOCTYPE HTML>
<html {$langHeader}>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
   <meta http-equiv="Content-Script-Type" content="text/javascript">
   <meta http-equiv="Content-Style-Type" content="text/css">
    <title>{$mod_strings['LBL_WIZARD_TITLE']} {$mod_strings['LBL_PERFORM_TITLE']}</title>
   <link REL="SHORTCUT ICON" HREF="$icon">
   <!-- <link rel="stylesheet" href="$css" type="text/css" /> -->
   <script type="text/javascript" src="$common"></script>
   <link rel="stylesheet" href="install/install2.css" type="text/css" />
   <script type="text/javascript" src="install/installCommon.js"></script>
   <script type="text/javascript" src="install/siteConfig.js"></script>
<link rel='stylesheet' type='text/css' href='include/javascript/yui/build/container/assets/container.css' />
<link rel="stylesheet" href="themes/SuiteP/css/fontello.css">
    <link rel="stylesheet" href="themes/SuiteP/css/animation.css"><!--[if IE 7]><link rel="stylesheet" href="css/fontello-ie7.css"><![endif]-->
</head>
<body onload="javascript:document.getElementById('button_next2').focus();">
<!--SuiteCRM installer-->
<div id="install_container">
<div id="install_box">
<header id="install_header">
                    <div id="steps">
                        <p>{$mod_strings['LBL_STEP2']}</p>
                        <i class="icon-progress-0" id="complete"></i>
                        <i class="icon-progress-1" id="complete"></i>
                        <i class="icon-progress-2"></i>
                    </div>
            <div class="install_img"><a href="https://suitecrm.com" target="_blank"><img src="{$sugar_md}" alt="SuiteCRM"></a></div>
</header>
EOQ;
echo $out;
installStatus($mod_strings['STAT_CONFIGURATION'], null, false, '');
installLog("calling handleSugarConfig()");
$bottle = handleSugarConfig();
//installLog("calling handleLog4Php()");
//handleLog4Php();

$server_software = $_SERVER["SERVER_SOFTWARE"];
if (strpos((string) $server_software, 'Microsoft-IIS') !== false) {
    installLog("calling handleWebConfig()");
    handleWebConfig();
} else {
    installLog("calling handleHtaccess()");
    handleHtaccess();
}

///////////////////////////////////////////////////////////////////////////////
////    START TABLE STUFF
echo "<br>";
echo "<b>{$mod_strings['LBL_PERFORM_TABLES']}</b>";
echo "<br>";

// create the SugarCRM database
if ($setup_db_create_database) {
    installLog("calling handleDbCreateDatabase()");
    installerHook('pre_handleDbCreateDatabase');
    handleDbCreateDatabase();
    installerHook('post_handleDbCreateDatabase');
} else {

// ensure the charset and collation are utf8
    installLog("calling handleDbCharsetCollation()");
    installerHook('pre_handleDbCharsetCollation');
    handleDbCharsetCollation();
    installerHook('post_handleDbCharsetCollation');
}

//Suite rebuild exts
/*require_once('ModuleInstall/ModuleInstaller.php');
$ModuleInstaller = new ModuleInstaller();
$ModuleInstaller->silent=true;
$ModuleInstaller->rebuild_modules();
$ModuleInstaller->rebuild_languages(  array ('en_us' => 'English (US)',));
$ModuleInstaller->rebuild_extensions();
$ModuleInstaller->rebuild_tabledictionary();*/

// create the SugarCRM database user
if ($setup_db_create_sugarsales_user) {
    installerHook('pre_handleDbCreateSugarUser');
    handleDbCreateSugarUser();
    installerHook('post_handleDbCreateSugarUser');
}

foreach ($beanFiles as $bean => $file) {
    require_once($file);
}
echo "<br>";
// load up the config_override.php file.
// This is used to provide default user settings
if (is_file("config_override.php")) {
    require_once("config_override.php");
}

$db                 = DBManagerFactory::getInstance();
$startTime          = microtime(true);
$focus              = 0;
$processed_tables   = array(); // for keeping track of the tables we have worked on
$empty              = array();
$new_tables     = 1; // is there ever a scenario where we DON'T create the admin user?
$new_config         = 1;
$new_report     = 1;

// add non-module Beans to this array to keep the installer from erroring.
$nonStandardModules = array(
    //'Tracker',
);


/**
 * loop through all the Beans and create their tables
 */
installStatus($mod_strings['STAT_CREATE_DB']);
 installLog("looping through all the Beans and create their tables");
 //start by clearing out the vardefs
 VardefManager::clearVardef();
installerHook('pre_createAllModuleTables');


foreach ($beanFiles as $bean => $file) {
    $doNotInit = array('Scheduler', 'SchedulersJob', 'ProjectTask','jjwg_Maps','jjwg_Address_Cache','jjwg_Areas','jjwg_Markers');

    if (in_array($bean, $doNotInit)) {
        $focus = new $bean(false);
    } else {
        $focus = new $bean();
    }

    if ($bean == 'Configurator') {
        continue;
    }

    $table_name = $focus->table_name;
    //installStatus(sprintf($mod_strings['STAT_CREATE_DB_TABLE'], $focus->table_name ));
    installLog("processing table ".$focus->table_name);
    // check to see if we have already setup this table
    if (!in_array($table_name, $processed_tables)) {
        if (!file_exists("modules/".$focus->module_dir."/vardefs.php")) {
            continue;
        }
        if (!in_array($bean, $nonStandardModules)) {
            require_once("modules/".$focus->module_dir."/vardefs.php"); // load up $dictionary
            if (isset($dictionary[$focus->object_name]['table']) && $dictionary[$focus->object_name]['table'] == 'does_not_exist') {
                continue; // support new vardef definitions
            }
        } else {
            continue; //no further processing needed for ignored beans.
        }

        // table has not been setup...we will do it now and remember that
        $processed_tables[] = $table_name;

        $focus->db->database = $db->database; // set db connection so we do not need to reconnect

        if ($setup_db_drop_tables) {
            drop_table_install($focus);
            installLog("dropping table ".$focus->table_name);
        }

        if (create_table_if_not_exist($focus)) {
            installLog("creating table ".$focus->table_name);
            if ($bean == "User") {
                $new_tables = 1;
            }
            if ($bean == "Administration") {
                $new_config = 1;
            }
        }

        installLog("creating Relationship Meta for ".$focus->getObjectName());
        installerHook('pre_createModuleTable', array('module' => $focus->getObjectName()));
        SugarBean::createRelationshipMeta($focus->getObjectName(), $db, $table_name, $empty, $focus->module_dir);
        installerHook('post_createModuleTable', array('module' => $focus->getObjectName()));
        echo ".";
    } // end if()
}


installerHook('post_createAllModuleTables');

echo "<br>";
////    END TABLE STUFF

///////////////////////////////////////////////////////////////////////////////
////    START RELATIONSHIP CREATION

    ksort($rel_dictionary);
    foreach ($rel_dictionary as $rel_name => $rel_data) {
        $table = $rel_data['table'];

        if ($setup_db_drop_tables) {
            if ($db->tableExists($table)) {
                $db->dropTableName($table);
            }
        }

        if (!$db->tableExists($table)) {
            $db->createTableParams($table, $rel_data['fields'], $rel_data['indices']);
        }

        SugarBean::createRelationshipMeta($rel_name, $db, $table, $rel_dictionary, '');
    }

///////////////////////////////////////////////////////////////////////////////
////    START CREATE DEFAULTS
    echo "<br>";
    echo "<b>{$mod_strings['LBL_PERFORM_CREATE_DEFAULT']}</b><br>";
    echo "<br>";
installStatus($mod_strings['STAT_CREATE_DEFAULT_SETTINGS']);
    installLog("Begin creating Defaults");
    installerHook('pre_createDefaultSettings');
    if ($new_config) {
        installLog("insert defaults into config table");
        insert_default_settings();
    }
    installerHook('post_createDefaultSettings');





    installerHook('pre_createUsers');
    if ($new_tables) {
        echo $line_entry_format.$mod_strings['LBL_PERFORM_DEFAULT_USERS'].$line_exit_format;
        installLog($mod_strings['LBL_PERFORM_DEFAULT_USERS']);
        create_default_users();
        echo $mod_strings['LBL_PERFORM_DONE'];
    } else {
        echo $line_entry_format.$mod_strings['LBL_PERFORM_ADMIN_PASSWORD'].$line_exit_format;
        installLog($mod_strings['LBL_PERFORM_ADMIN_PASSWORD']);
        $db->setUserName($setup_db_sugarsales_user);
        $db->setUserPassword($setup_db_sugarsales_password);
        set_admin_password($setup_site_admin_password);
        echo $mod_strings['LBL_PERFORM_DONE'];
    }
    installerHook('post_createUsers');




    // default OOB schedulers

    echo $line_entry_format.$mod_strings['LBL_PERFORM_DEFAULT_SCHEDULER'].$line_exit_format;
    installLog($mod_strings['LBL_PERFORM_DEFAULT_SCHEDULER']);
    $scheduler = BeanFactory::newBean('Schedulers');
    installerHook('pre_createDefaultSchedulers');
    $scheduler->rebuildDefaultSchedulers();
    installerHook('post_createDefaultSchedulers');


    echo $mod_strings['LBL_PERFORM_DONE'];



// Enable Sugar Feeds and add all feeds by default
installLog("Enable SugarFeeds");
enableSugarFeeds();

///////////////////////////////////////////////////////////////////////////
////    FINALIZE LANG PACK INSTALL
    if (isset($_SESSION['INSTALLED_LANG_PACKS']) && is_array($_SESSION['INSTALLED_LANG_PACKS']) && !empty($_SESSION['INSTALLED_LANG_PACKS'])) {
        updateUpgradeHistory();
    }


    //require_once('modules/Connectors/InstallDefaultConnectors.php');

    ///////////////////////////////////////////////////////////////////////////////
    ////    INSTALL PASSWORD TEMPLATES
    include('install/seed_data/Advanced_Password_SeedData.php');

///////////////////////////////////////////////////////////////////////////////
////    SETUP DONE
installLog("Installation has completed *********");

    $memoryUsed = '';
    if (function_exists('memory_get_usage')) {
        $memoryUsed = $mod_strings['LBL_PERFORM_OUTRO_5'] . memory_get_usage() . $mod_strings['LBL_PERFORM_OUTRO_6'];
    }


    $errTcpip = '';
    $fp = @fsockopen("www.suitecrm.com", 80, $errno, $errstr, 3);
    if (!$fp) {
        $errTcpip = "<p>{$mod_strings['ERR_PERFORM_NO_TCPIP']}</p>";
    }
    if ($fp && (!isset($_SESSION['oc_install']) || $_SESSION['oc_install'] == false)) {
        @fclose($fp);
        if ($next_step == 9999) {
            $next_step = 8;
        }
        $fpResult = <<<FP
     <form action="install.php" method="post" name="form" id="form">
     <input type="hidden" name="current_step" value="{$next_step}">
     <input class="button" type="submit" name="goto" value="{$mod_strings['LBL_NEXT']}" id="button_next2"/>
     </form>
FP;
    } else {
        $fpResult = <<<FP
            <form action="index.php" method="post" name="formFinish" id="formFinish">
                <input type="hidden" name="default_user_name" value="admin" />
                <input class="button" type="submit" name="next" value="{$mod_strings['LBL_PERFORM_FINISH']}" id="button_next2"/>
            </form>
FP;
    }

    if (isset($_SESSION['setup_site_sugarbeet_automatic_checks']) && $_SESSION['setup_site_sugarbeet_automatic_checks'] == true) {
        set_CheckUpdates_config_setting('automatic');
    } else {
        set_CheckUpdates_config_setting('manual');
    }
    if (!empty($_SESSION['setup_system_name'])) {
        $admin=BeanFactory::newBean('Administration');
        $admin->saveSetting('system', 'name', $_SESSION['setup_system_name']);
    }

    // Bug 28601 - Set the default list of tabs to show
    $enabled_tabs = array();
    $enabled_tabs[] = 'Home';
    $enabled_tabs[] = 'Accounts';
    $enabled_tabs[] = 'Contacts';
    $enabled_tabs[] = 'Opportunities';
    $enabled_tabs[] = 'Leads';
    $enabled_tabs[] = 'AOS_Quotes';
    $enabled_tabs[] = 'Calendar';
    $enabled_tabs[] = 'Documents';
    $enabled_tabs[] = 'Emails';
    $enabled_tabs[] = 'Spots';
    $enabled_tabs[] = 'Campaigns';
    $enabled_tabs[] = 'Calls';
    $enabled_tabs[] = 'Meetings';
    $enabled_tabs[] = 'Tasks';
    $enabled_tabs[] = 'Notes';
    $enabled_tabs[] = 'AOS_Invoices';
    $enabled_tabs[] = 'AOS_Contracts';
    $enabled_tabs[] = 'Cases';
    $enabled_tabs[] = 'Prospects';
    $enabled_tabs[] = 'ProspectLists';
    $enabled_tabs[] = 'Project';
    $enabled_tabs[] = 'AM_ProjectTemplates';
    $enabled_tabs[] = 'AM_TaskTemplates';
    $enabled_tabs[] = 'FP_events';
    $enabled_tabs[] = 'FP_Event_Locations';
    $enabled_tabs[] = 'AOS_Products';
    $enabled_tabs[] = 'AOS_Product_Categories';
    $enabled_tabs[] = 'AOS_PDF_Templates';
    $enabled_tabs[] = 'AOR_Reports';
    $enabled_tabs[] = 'AOK_KnowledgeBase';
    $enabled_tabs[] = 'AOK_Knowledge_Base_Categories';
    $enabled_tabs[] = 'EmailTemplates';
    $enabled_tabs[] = 'Surveys';

//Beginning of the scenario implementations
//We need to load the tabs so that we can remove those which are scenario based and un-selected
//Remove the custom tabConfig as this overwrites the complete list containined in the include/tabConfig.php
if (file_exists('custom/include/tabConfig.php')) {
    unlink('custom/include/tabConfig.php');
}
require_once('include/tabConfig.php');

//Remove the custom dashlet so that we can use the complete list of defaults to filter by category
if (file_exists('custom/modules/Home/dashlets.php')) {
    unlink('custom/modules/Home/dashlets.php');
}
//Check if the folder is in place
if (!file_exists('custom/modules/Home')) {
    sugar_mkdir('custom/modules/Home', 0775);
}
//Check if the folder is in place
if (!file_exists('custom/include')) {
    sugar_mkdir('custom/include', 0775);
}


require_once('modules/Home/dashlets.php');

if (isset($_SESSION['installation_scenarios'])) {
    foreach ($_SESSION['installation_scenarios'] as $scenario) {
        //If the item is not in $_SESSION['scenarios'], then unset them as they are not required
        if (!in_array($scenario['key'], $_SESSION['scenarios'])) {
            foreach ($scenario['modules'] as $module) {
                if (($removeKey = array_search($module, $enabled_tabs, true)) !== false) {
                    unset($enabled_tabs[$removeKey]);
                }
            }

            //Loop through the dashlets to remove from the default home page based on this scenario
            foreach ($scenario['dashlets'] as $dashlet) {
                //if (($removeKey = array_search($dashlet, $defaultDashlets)) !== false) {
                //    unset($defaultDashlets[$removeKey]);
                // }
                if (isset($defaultDashlets[$dashlet])) {
                    unset($defaultDashlets[$dashlet]);
                }
            }

            //If the scenario has an associated group tab, remove accordingly (by not adding to the custom tabconfig.php
            if (isset($scenario['groupedTabs'])) {
                unset($GLOBALS['tabStructure'][$scenario['groupedTabs']]);
            }
        }
    }
}

//Have a 'core' options, with accounts / contacts if no other scenario is selected
if (!is_null($_SESSION['scenarios'])) {
    unset($GLOBALS['tabStructure']['LBL_TABGROUP_DEFAULT']);
}


//Write the tabstructure to custom so that the grouping are not shown for the un-selected scenarios
$fileContents = "<?php \n" .'$GLOBALS["tabStructure"] ='.var_export($GLOBALS['tabStructure'], true).';';
sugar_file_put_contents('custom/include/tabConfig.php', $fileContents);

//Write the dashlets to custom so that the dashlets are not shown for the un-selected scenarios
$fileContents = "<?php \n" .'$defaultDashlets ='.var_export($defaultDashlets, true).';';
sugar_file_put_contents('custom/modules/Home/dashlets.php', $fileContents);


// End of the scenario implementations


installerHook('pre_setSystemTabs');
require_once('modules/MySettings/TabController.php');
$tabs = new TabController();
$tabs->set_system_tabs($enabled_tabs);
installerHook('post_setSystemTabs');
include_once('install/suite_install/suite_install.php');

post_install_modules();

//Call rebuildSprites
/*if(function_exists('imagecreatetruecolor'))
{
    require_once('modules/UpgradeWizard/uw_utils.php');
    rebuildSprites(true);
}*/

///////////////////////////////////////////////////////////////////////////////
////    START DEMO DATA

// populating the db with seed data
installLog("populating the db with seed data");
if ($_SESSION['demoData'] != 'no') {
    installerHook('pre_installDemoData');
    set_time_limit(301);

    echo "<br>";
    echo "<b>{$mod_strings['LBL_PERFORM_DEMO_DATA']}</b>";
    echo "<br><br>";

    print($render_table_close);
    print($render_table_open);

    global $current_user;
    $current_user = BeanFactory::newBean('Users');
    $current_user->retrieve(1);
    include("install/populateSeedData.php");
    installerHook('post_installDemoData');
}

/////////////////////////////////////////////////////////////
//// Store information by installConfig.php form

// save current superglobals and vars
$varStack['GLOBALS'] = $GLOBALS;
$varStack['defined_vars'] = get_defined_vars();

// restore previously posted form
$_REQUEST = array_merge($_REQUEST, $_SESSION);
$_POST = array_merge($_POST, $_SESSION);


installStatus($mod_strings['STAT_INSTALL_FINISH']);
installLog('Save configuration settings..');

//      <--------------------------------------------------------
//          from ConfigurationConroller->action_saveadminwizard()
//          ---------------------------------------------------------->

installLog('save locale');




//global $current_user;
installLog('new Administration');
$focus = BeanFactory::newBean('Administration');
installLog('retrieveSettings');
//$focus->retrieveSettings();
// switch off the adminwizard (mark that we have got past this point)
installLog('AdminWizard OFF');
$focus->saveSetting('system', 'adminwizard', 1);

installLog('saveConfig');
$focus->saveConfig();

installLog('new Configurator');
$configurator = new Configurator();
installLog('populateFromPost');
$configurator->populateFromPost();




installLog('handleOverride');
// add local settings to config overrides
if (!empty($_SESSION['default_date_format'])) {
    $sugar_config['default_date_format'] = $_SESSION['default_date_format'];
}
if (!empty($_SESSION['default_time_format'])) {
    $sugar_config['default_time_format'] = $_SESSION['default_time_format'];
}
if (!empty($_SESSION['default_language'])) {
    $sugar_config['default_language'] = $_SESSION['default_language'];
}
if (!empty($_SESSION['default_locale_name_format'])) {
    $sugar_config['default_locale_name_format'] = $_SESSION['default_locale_name_format'];
}
//$configurator->handleOverride();



// save current web-server user for the cron user check mechanism:
installLog('addCronAllowedUser');
addCronAllowedUser(getRunningUser());



installLog('saveConfig');
$configurator->saveConfig();








// Bug 37310 - Delete any existing currency that matches the one we've just set the default to during the admin wizard
installLog('new Currency');
$currency = new Currency;
installLog('retrieve');
$currency->retrieve($currency->retrieve_id_by_name($_REQUEST['default_currency_name']));
if (!empty($currency->id)
    && $currency->symbol == $_REQUEST['default_currency_symbol']
    && $currency->iso4217 == $_REQUEST['default_currency_iso4217']) {
    $currency->deleted = 1;
    installLog('DBG: save currency');
    $currency->save();
}


installLog('Save user settings..');

//      <------------------------------------------------
//          from UsersController->action_saveuserwizard()
//          ---------------------------------------------------------->


// set all of these default parameters since the Users save action will undo the defaults otherwise

// load admin
$current_user = BeanFactory::newBean('Users');
$current_user->retrieve(1);
$current_user->is_admin = '1';
$sugar_config = get_sugar_config_defaults();

// set local settings -  if neccessary you can set here more fields as named in User module / EditView form...
if (isset($_REQUEST['timezone']) && $_REQUEST['timezone']) {
    $current_user->setPreference('timezone', $_REQUEST['timezone']);
}

if (file_exists(__DIR__ . '/../modules/ACL/install_actions.php')) {
    require_once(__DIR__ . '/../modules/ACL/install_actions.php');
}

$_POST['dateformat'] = $_REQUEST['default_date_format'];
$_POST['record'] = $current_user->id;
$_POST['is_admin'] = ($current_user->is_admin ? 'on' : '');
$_POST['use_real_names'] = true;
$_POST['reminder_checked'] = '1';
$_POST['reminder_time'] = 1800;
$_POST['email_reminder_time'] = 3600;
$_POST['mailmerge_on'] = 'on';
$_POST['receive_notifications'] = $current_user->receive_notifications;
installLog('DBG: SugarThemeRegistry::getDefault');
$_POST['user_theme'] = (string) SugarThemeRegistry::getDefault();

// save and redirect to new view
$_REQUEST['do_not_redirect'] = true;

// restore superglobals and vars
foreach ($varStack['GLOBALS'] ?? [] as $index => $item) {
    $GLOBALS[$index] = $item;
}
foreach ($varStack['defined_vars'] as $__key => $__value) {
    $$__key = $__value;
}



$endTime = microtime(true);
$deltaTime = $endTime - $startTime;

if (!is_array($bottle) || !is_object($bottle)) {
    $bottle = $bottle;
    LoggerManager::getLogger()->warn('Bottle needs to be an array to perform setup');
}


if (is_countable($bottle) && count($bottle) > 0) {
    foreach ($bottle as $bottle_message) {
        $bottleMsg .= "{$bottle_message}\n";
    }
} else {
    $bottleMsg = $mod_strings['LBL_PERFORM_SUCCESS'];
}
installerHook('post_installModules');

$out =<<<EOQ
<br><p><b>{$mod_strings['LBL_PERFORM_OUTRO_1']} {$setup_sugar_version} {$mod_strings['LBL_PERFORM_OUTRO_2']}</b></p>

{$mod_strings['LBL_PERFORM_OUTRO_3']} {$deltaTime} {$mod_strings['LBL_PERFORM_OUTRO_4']}<br />
<p><b>{$memoryUsed}</b></p>
<p><b>{$errTcpip}</b></p>
<p><b>{$fpResult}</b></p>
</div>
<footer id="install_footer">
    <p id="footer_links"><a href="https://suitecrm.com" target="_blank">Visit suitecrm.com</a> | <a href="https://suitecrm.com/suitecrm/forum" target="_blank">Support Forums</a> | <a href="https://docs.suitecrm.com/admin/installation-guide/" target="_blank">Installation Guide</a> | <a href="LICENSE.txt" target="_blank">License</a>
</footer>
</div>
</body>
</html>
<!--
<bottle>{$bottleMsg}</bottle>
-->
EOQ;

echo $out;

$loginURL = str_replace('install.php', 'index.php', "//$_SERVER[HTTP_HOST]$_SERVER[REQUEST_URI]");
installStatus(sprintf($mod_strings['STAT_INSTALL_FINISH_LOGIN'], $loginURL), array('function' => 'redirect', 'arguments' => $loginURL));
