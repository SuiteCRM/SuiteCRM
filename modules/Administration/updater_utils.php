<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

/**

********************************************************************************/
require_once('include/utils/encryption_utils.php');

function getSystemInfo($send_usage_info=true)
{
    global $sugar_config;
    global $db, $administration, $timedate;
    $info=array();
    $info = getBaseSystemInfo($send_usage_info);
    if ($send_usage_info) {
        $info['application_key']=$sugar_config['unique_key'];
        $info['php_version']=phpversion();
        if (isset($_SERVER['SERVER_SOFTWARE'])) {
            $info['server_software'] = $_SERVER['SERVER_SOFTWARE'];
        } // if

        //get user count.

        $query = "SELECT count(*) as total from users WHERE " . User::getLicensedUsersWhere();
        $result = $db->getOne($query, false, 'fetching active users count');
        if ($result !== false) {
            $info['users'] = $result;
        }

        if (empty($administration)) {
            $administration = BeanFactory::newBean('Administration');
        }
        $administration->retrieveSettings('system');
        $info['system_name'] = (!empty($administration->settings['system_name']))?substr((string) $administration->settings['system_name'], 0, 255):'';


        $result=$db->getOne("select count(*) count from users where status='Active' and deleted=0 and is_admin='1'", false, 'fetching admin count');
        if ($result !== false) {
            $info['admin_users'] = $result;
        }


        $result=$db->getOne("select count(*) count from users", false, 'fetching all users count');
        if ($result !== false) {
            $info['registered_users'] = $result;
        }

        $lastMonth = $db->convert("'". $timedate->getNow()->modify("-30 days")->asDb(false) . "'", 'datetime');
        if (!$send_usage_info) {
            $info['users_active_30_days'] = -1;
        } else {
            $info['users_active_30_days'] = $db->getOne("SELECT count( DISTINCT users.id ) user_count FROM tracker, users WHERE users.id = tracker.user_id AND  tracker.date_modified >= $lastMonth", false, 'fetching last 30 users count');
        }




        if (!$send_usage_info) {
            $info['latest_tracker_id'] = -1;
        } else {
            $id=$db->getOne("select id from tracker order by date_modified desc", false, 'fetching most recent tracker entry');
            if ($id !== false) {
                $info['latest_tracker_id'] = $id;
            }
        }

        $info['db_type']=$sugar_config['dbconfig']['db_type'];
        $info['db_version']=$db->version();
    }
    if (file_exists('distro.php')) {
        include('distro.php');
        if (!empty($distro_name)) {
            $info['distro_name'] = $distro_name;
        }
    }
    $info['os'] = php_uname('s');
    $info['os_version'] = php_uname('r');
    $info['timezone_u'] = $GLOBALS['current_user']->getPreference('timezone');
    $info['timezone'] = date('e');
    if ($info['timezone'] == 'e') {
        $info['timezone'] = date('T');
    }
    return $info;
}

function getBaseSystemInfo($send_usage_info=true)
{
    include('sugar_version.php');
    $info=array();

    if ($send_usage_info) {
        $info['sugar_db_version']=$sugar_db_version;
    }
    $info['sugar_version']=$sugar_version;
    $info['sugar_flavor']=$sugar_flavor;
    $info['auth_level'] = 0;



    return $info;
}

function check_now($send_usage_info=true, $get_request_data=false, $response_data = false, $from_install=false)
{
    global $sugar_config, $timedate, $sugar_version;
    global $license;
    $db = DBManagerFactory::getInstance();

    include('sugar_version.php');


    $return_array=array();
    if (!$from_install && empty($license)) {
        loadLicense(true);
    }

    if (!$response_data) {
        if ($from_install) {
            $info = getBaseSystemInfo(false);
        } else {
            $info = getSystemInfo($send_usage_info);
        }

        require_once('include/nusoap/nusoap.php');

        $GLOBALS['log']->debug('USING HTTPS TO CONNECT TO HEARTBEAT');
        $sclient = new nusoapclient('https://updates.sugarcrm.com/heartbeat/soap.php', false, false, false, false, false, 15, 15);
        $ping = $sclient->call('sugarPing', array());
        if (empty($ping) || $sclient->getError()) {
            if (!$get_request_data) {
                return array(
                    array(
                        'version' => $sugar_version,
                        'description' => "You have the latest version."
                    )
                );
            }
        }


        $key = '4829482749329';



        $encoded = sugarEncode($key, serialize($info));

        if ($get_request_data) {
            $request_data = array('key'=>$key, 'data'=>$encoded);
            return serialize($request_data);
        }
        $encodedResult = $sclient->call('sugarHome', array('key'=>$key, 'data'=>$encoded));
    } else {
        $encodedResult = 	$response_data['data'];
        $key = $response_data['key'];
    }

    if ($response_data || !$sclient->getError()) {
        $serializedResultData = sugarDecode($key, $encodedResult);
        $resultData = unserialize($serializedResultData);
        if ($response_data && empty($resultData)) {
            $resultData = array();
            $resultData['validation'] = 'invalid validation key';
        }
    } else {
        $resultData = array();
        $resultData['versions'] = array();
    }

    if ($response_data || !$sclient->getError()) {
        if (!empty($resultData['msg'])) {
            if (!empty($resultData['msg']['admin'])) {
                $license->saveSetting('license', 'msg_admin', base64_encode($resultData['msg']['admin']));
            } else {
                $license->saveSetting('license', 'msg_admin', '');
            }
            if (!empty($resultData['msg']['all'])) {
                $license->saveSetting('license', 'msg_all', base64_encode($resultData['msg']['all']));
            } else {
                $license->saveSetting('license', 'msg_all', '');
            }
        } else {
            $license->saveSetting('license', 'msg_admin', '');
            $license->saveSetting('license', 'msg_all', '');
        }
        $license->saveSetting('license', 'last_validation', 'success');
        unset($_SESSION['COULD_NOT_CONNECT']);
    } else {
        $resultData = array();
        $resultData['versions'] = array();

        $license->saveSetting('license', 'last_connection_fail', TimeDate::getInstance()->nowDb());
        $license->saveSetting('license', 'last_validation', 'no_connection');

        if (empty($license->settings['license_last_validation_success']) && empty($license->settings['license_last_validation_fail']) && empty($license->settings['license_vk_end_date'])) {
            $license->saveSetting('license', 'vk_end_date', TimeDate::getInstance()->nowDb());

            $license->saveSetting('license', 'validation_key', base64_encode(serialize(array('verified'=>false))));
        }
        $_SESSION['COULD_NOT_CONNECT'] =TimeDate::getInstance()->nowDb();
    }
    if (!empty($resultData['versions'])) {
        $license->saveSetting('license', 'latest_versions', base64_encode(serialize($resultData['versions'])));
    } else {
        $resultData['versions'] = array();
        $license->saveSetting('license', 'latest_versions', '')	;
    }

    if ((is_countable($resultData) ? count($resultData) : 0) == 1 && !empty($resultData['versions'][0]['version'])
        && compareVersions($sugar_version, $resultData['versions'][0]['version'])) {
        $resultData['versions'][0]['version'] = $sugar_version;
        $resultData['versions'][0]['description'] = "You have the latest version.";
    }


    return $resultData['versions'];
}
/*
 * returns true if $ver1 > $ver2
 */
function compareVersions($ver1, $ver2)
{
    return (version_compare($ver1, $ver2) === 1);
}
function set_CheckUpdates_config_setting($value)
{
    $admin=BeanFactory::newBean('Administration');
    $admin->saveSetting('Update', 'CheckUpdates', $value);
}
/* return's value for the 'CheckUpdates' config setting
* if the setting does not exist one gets created with a default value of automatic.
*/
function get_CheckUpdates_config_setting()
{
    $checkupdates='automatic';


    $admin=BeanFactory::newBean('Administration');
    $admin=$admin->retrieveSettings('Update', true);
    if (empty($admin->settings) || empty($admin->settings['Update_CheckUpdates'])) {
        $admin->saveSetting('Update', 'CheckUpdates', 'automatic');
    } else {
        $checkupdates=$admin->settings['Update_CheckUpdates'];
    }
    return $checkupdates;
}

function set_last_check_version_config_setting($value)
{
    $admin=BeanFactory::newBean('Administration');
    $admin->saveSetting('Update', 'last_check_version', $value);
}
function get_last_check_version_config_setting()
{
    $admin=BeanFactory::newBean('Administration');
    $admin=$admin->retrieveSettings('Update');
    if (empty($admin->settings) || empty($admin->settings['Update_last_check_version'])) {
        return null;
    } else {
        return $admin->settings['Update_last_check_version'];
    }
}


function set_last_check_date_config_setting($value)
{
    $admin=BeanFactory::newBean('Administration');
    $admin->saveSetting('Update', 'last_check_date', $value);
}
function get_last_check_date_config_setting()
{
    $admin=BeanFactory::newBean('Administration');
    $admin=$admin->retrieveSettings('Update');
    if (empty($admin->settings) || empty($admin->settings['Update_last_check_date'])) {
        return 0;
    } else {
        return $admin->settings['Update_last_check_date'];
    }
}

function set_sugarbeat($value)
{
    global $sugar_config;
    $_SUGARBEAT="sugarbeet";
    $sugar_config[$_SUGARBEAT] = $value;
    write_array_to_file("sugar_config", $sugar_config, "config.php");
}
function get_sugarbeat()
{
    global $sugar_config;
    $_SUGARBEAT="sugarbeet";

    if (isset($sugar_config[$_SUGARBEAT]) && $sugar_config[$_SUGARBEAT] == false) {
        return false;
    }
    return true;
}



function shouldCheckSugar()
{
    global $license, $timedate;
    if (

    get_CheckUpdates_config_setting() == 'automatic') {
        return true;
    }

    return false;
}



function loadLicense($firstLogin=false)
{
    $GLOBALS['license']=BeanFactory::newBean('Administration');
    $GLOBALS['license']=$GLOBALS['license']->retrieveSettings('license', $firstLogin);
}

function loginLicense()
{
    global $current_user, $license, $sugar_version;
    loadLicense(true);


    if (shouldCheckSugar()) {
        $last_check_date=get_last_check_date_config_setting();
        $current_date_time=time();
        $time_period=3*23*3600 ;
        if (($current_date_time - $last_check_date) > $time_period
        ) {
            $version = check_now(get_sugarbeat());

            unset($_SESSION['license_seats_needed']);
            loadLicense();
            set_last_check_date_config_setting((string)$current_date_time);
            include('sugar_version.php');

            $newVersion = '';
            if (!empty($version) && (is_countable($version) ? count($version) : 0) == 1) {
                $newVersion = $version[0]['version'];
            }

            if (version_compare($newVersion, $sugar_version, '>') && is_admin($current_user)) {
                //set session variables.
                $_SESSION['available_version']=$version[0]['version'];
                $_SESSION['available_version_description']=$version[0]['description'];
                set_last_check_version_config_setting($version[0]['version']);
            }
        }
    }
}
