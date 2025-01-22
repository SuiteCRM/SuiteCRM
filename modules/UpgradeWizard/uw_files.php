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




global $sugar_version;
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

///////////////////////////////////////////////////////////////////////////////
////	DYNAMICALLY GENERATE UPGRADEWIZARD MODULE FILE LIST
$uwFilesCurrent = findAllFiles('modules/UpgradeWizard/', array());

// handle 4.x to 4.5.x+ (no UpgradeWizard module)
if ((is_countable($uwFilesCurrent) ? count($uwFilesCurrent) : 0) < 5) {
    $uwFiles = array(
        'modules/UpgradeWizard/language/en_us.lang.php',
        'modules/UpgradeWizard/cancel.php',
        'modules/UpgradeWizard/commit.php',
        'modules/UpgradeWizard/commitJson.php',
        'modules/UpgradeWizard/end.php',
        'modules/UpgradeWizard/Forms.php',
        'modules/UpgradeWizard/index.php',
        'modules/UpgradeWizard/Menu.php',
        'modules/UpgradeWizard/preflight.php',
        'modules/UpgradeWizard/preflightJson.php',
        'modules/UpgradeWizard/start.php',
        'modules/UpgradeWizard/su_utils.php',
        'modules/UpgradeWizard/su.php',
        'modules/UpgradeWizard/systemCheck.php',
        'modules/UpgradeWizard/systemCheckJson.php',
        'modules/UpgradeWizard/upgradeWizard.js',
        'modules/UpgradeWizard/upload.php',
        'modules/UpgradeWizard/uw_ajax.php',
        'modules/UpgradeWizard/uw_files.php',
        'modules/UpgradeWizard/uw_main.tpl',
        'modules/UpgradeWizard/uw_utils.php',
    );
} else {
    $uwFilesCurrent = findAllFiles('ModuleInstall', $uwFilesCurrent);
    $uwFilesCurrent = findAllFiles('include/javascript/yui', $uwFilesCurrent);
    $uwFilesCurrent[] = 'HandleAjaxCall.php';

    $uwFiles = array();
    foreach ($uwFilesCurrent as $file) {
        $uwFiles[] = str_replace("./", "", (string) clean_path($file));
    }
}
////	END DYNAMICALLY GENERATE UPGRADEWIZARD MODULE FILE LIST
///////////////////////////////////////////////////////////////////////////////

$uw_files = array(
    // standard files we steamroll with no warning
    'log4php.properties',
    'include/utils/encryption_utils.php',
    'include/Pear/Crypt_Blowfish/Blowfish.php',
    'include/Pear/Crypt_Blowfish/Blowfish/DefaultKey.php',
    'include/utils.php',
    'include/language/en_us.lang.php',
    'include/modules.php',
    'include/Localization/Localization.php',
    'install/language/en_us.lang.php',
    'XTemplate/xtpl.php',
    'include/database/DBManager.php',
    'include/database/DBManagerFactory.php',
    'include/database/MssqlManager.php',
    'include/database/MysqlManager.php',
    'include/database/DBManagerFactory.php',
);

$uw_files = array_merge($uw_files, $uwFiles);
