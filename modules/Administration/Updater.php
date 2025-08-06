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

global $app_strings;
global $app_list_strings;
global $mod_strings;
global $current_user;
global $sugar_config;

$xtpl=new XTemplate('modules/Administration/Updater.html');
$xtpl->assign("MOD", $mod_strings);
$xtpl->assign("APP", $app_strings);

if (isset($_REQUEST['useraction']) && ($_REQUEST['useraction']=='Save' || $_REQUEST['useraction']=='CheckNow')) {
    if (!empty($_REQUEST['type']) && $_REQUEST['type'] == 'automatic') {
        set_CheckUpdates_config_setting('automatic');
    } else {
        set_CheckUpdates_config_setting('manual');
    }

    $beat=false;
    if (!empty($_REQUEST['beat'])) {
        $beat=true;
    }
    if ($beat != get_sugarbeat()) {
        set_sugarbeat($beat);
    }
}

echo getClassicModuleTitle(
    "Administration",
    array(
            "<a href='index.php?module=Administration&action=index'>".translate('LBL_MODULE_NAME', 'Administration')."</a>",
           $mod_strings['LBL_SUITE_UPDATE_TITLE'],
           ),
    false
        );

if (get_sugarbeat()) {
    $xtpl->assign("SEND_STAT_CHECKED", "checked");
}

if (get_CheckUpdates_config_setting()=='automatic') {
    $xtpl->assign("AUTOMATIC_CHECKED", "checked");
}


/*if (isset($_REQUEST['useraction']) && $_REQUEST['useraction']=='CheckNow') {
    check_now(get_sugarbeat());
    loadLicense();

}*/

$xtpl->parse('main.stats');

$has_updates= false;
if (!empty($license->settings['license_latest_versions'])) {
    $encodedVersions = $license->settings['license_latest_versions'];

    $versions = unserialize(base64_decode($encodedVersions), ['allowed_classes' => false]);
    include('sugar_version.php');
    if (!empty($versions)) {
        foreach ($versions as $version) {
            if (compareVersions($version['version'], $sugar_version)) {
                $has_updates = true;
                $xtpl->assign("VERSION", $version);
                $xtpl->parse('main.updates.version');
            }
        }
    }
    if (!$has_updates) {
        $xtpl->parse('main.noupdates');
    } else {
        $xtpl->parse('main.updates');
    }
}

//return module and index.
$xtpl->assign("RETURN_MODULE", "Administration");
$xtpl->assign("RETURN_ACTION", "index");

$xtpl->parse("main");
$xtpl->out("main");
