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

/*********************************************************************************

 * Description:  This class is used to include the json server config inline. Previous method
 * of using <script src=json_server.php></script> causes multiple server hits per page load
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

global $app_strings, $json;
$json = getJSONobj();

#[\AllowDynamicProperties]
class json_config
{
    public $global_registry_var_name = 'GLOBAL_REGISTRY';

    public function get_static_json_server($configOnly = true, $getStrings = false, $module = null, $record = null, $scheduler = false)
    {
        global $current_user;
        $str = '';
        $str .= $this->getAppMetaJSON($scheduler);
        if (!$configOnly) {
            $str .= $this->getFocusData($module, $record);
            if ($getStrings) {
                $str .= $this->getStringsJSON($module);
            }
        }
        $str .= $this->getUserConfigJSON();

        return $str;
    }

    public function getAppMetaJSON($scheduler = false)
    {
        global $json, $sugar_config;

        $str = "\nvar ". $this->global_registry_var_name." = new Object();\n";
        $str .= "\n".$this->global_registry_var_name.".config = {\"site_url\":\"".getJavascriptSiteURL()."\"};\n";

        $str .= $this->global_registry_var_name.".meta = new Object();\n";
        $str .= $this->global_registry_var_name.".meta.modules = new Object();\n";

        /*
        $modules_arr = array('Meetings','Calls');
        $meta_modules = array();

        global $beanFiles,$beanList;
        //header('Content-type: text/xml');
        foreach($modules_arr as $module) {
        	require_once($beanFiles[$beanList[$module]]);
        	$focus = new $beanList[$module];
        	$meta_modules[$module] = array();
        	$meta_modules[$module]['field_defs'] = $focus->field_defs;
        }

        $str .= $this->global_registry_var_name.".meta.modules.Meetings = ". $json->encode($meta_modules['Meetings'])."\n";
        $str .= $this->global_registry_var_name.".meta.modules.Calls = ". $json->encode($meta_modules['Calls'])."\n";
        */
        return $str;
    }

    public function getUserConfigJSON()
    {
        global $timedate;
        global $current_user, $sugar_config;
        $json = getJSONobj();
        if (isset($_SESSION['authenticated_user_theme']) && $_SESSION['authenticated_user_theme'] != '') {
            $theme = $_SESSION['authenticated_user_theme'];
        } else {
            $theme = $sugar_config['default_theme'];
        }
        $user_arr = array();
        $user_arr['theme'] = $theme;
        $user_arr['fields'] = array();
        $user_arr['module'] = 'User';
        $user_arr['fields']['id'] = $current_user->id;
        $user_arr['fields']['user_name'] = $current_user->user_name;
        $user_arr['fields']['first_name'] = $current_user->first_name;
        $user_arr['fields']['last_name'] = $current_user->last_name;
        $user_arr['fields']['full_name'] = $current_user->full_name;
        $user_arr['fields']['email'] = $current_user->email1;
        $user_arr['fields']['gmt_offset'] = $timedate->getUserUTCOffset();
        $user_arr['fields']['date_time_format'] = $current_user->getUserDateTimePreferences();
        $str = "\n".$this->global_registry_var_name.".current_user = ".$json->encode($user_arr).";\n";
        return $str;
    }

    public function getFocusData($module, $record)
    {
        global $json;
        if (empty($module)) {
            return '';
        } else {
            if (empty($record)) {
                return "\n".$this->global_registry_var_name.'["focus"] = {"module":"'.$module.'",users_arr:[],fields:{"id":"-1"}}'."\n";
            }
        }

        $module_arr = $this->meeting_retrieve($module, $record);
        return "\n".$this->global_registry_var_name."['focus'] = ". $json->encode($module_arr).";\n";
    }

    /*	multiple project module related changes added by haris raheem*/
    public function meeting_retrieve($module, $record)
    {
        global $json, $response;
        global $beanFiles, $beanList;
        require_once($beanFiles[$beanList[$module]]);
        $focus = new $beanList[$module];

        if (empty($module) || empty($record)) {
            return '';
        }

        $focus->retrieve($record);
        $module_arr = $this->populateBean($focus);

        if ($module == 'Meetings') {
            $users = $focus->get_meeting_users();
        } else {
            if ($module == 'Calls') {
                $users = $focus->get_call_users();
            } else {
                if ($module == 'Project') {
                    $focus->load_relationships('users');
                    $users=$focus->get_linked_beans('project_users_1', 'User');
                } else {
                    if ($module == 'AM_ProjectTemplates') {
                        $focus->load_relationships('users');
                        $users=$focus->get_linked_beans('am_projecttemplates_users_1', 'User');
                    }
                }
            }
        }


        $module_arr['users_arr'] = array();

        foreach ($users as $user) {
            array_push($module_arr['users_arr'], $this->populateBean($user));
        }

        $module_arr['orig_users_arr_hash'] = array();

        foreach ($users as $user) {
            $module_arr['orig_users_arr_hash'][$user->id] = '1';
        }

        $module_arr['contacts_arr'] = array();

        $focus->load_relationships('contacts');

        if ($module == 'Project') {
            $contacts=$focus->get_linked_beans('project_contacts_1', 'Contact');
        } else {
            if ($module == 'AM_ProjectTemplates') {
                $contacts=$focus->get_linked_beans('am_projecttemplates_contacts_1', 'Contact');
            } else {
                $contacts=$focus->get_linked_beans('contacts', 'Contact');
            }
        }

        foreach ($contacts as $contact) {
            array_push($module_arr['users_arr'], $this->populateBean($contact));
        }

        $module_arr['leads_arr'] = array();

        if ($module != 'Project' && $module != 'AM_ProjectTemplates') {
            $focus->load_relationships('leads');
            $leads=$focus->get_linked_beans('leads', 'Lead');
            foreach ($leads as $lead) {
                array_push($module_arr['users_arr'], $this->populateBean($lead));
            }
        }
        return $module_arr;
    }

    public function getStringsJSON($module)
    {
        global $current_language;
        $currentModule = 'Calendar';
        $mod_list_strings = return_mod_list_strings_language($current_language, $currentModule);

        global $json;
        $str = "\n".$this->global_registry_var_name."['calendar_strings'] =  {\"dom_cal_month_long\":". $json->encode($mod_list_strings['dom_cal_month_long']).",\"dom_cal_weekdays_long\":". $json->encode($mod_list_strings['dom_cal_weekdays_long'])."}\n";
        if (empty($module)) {
            $module = 'Home';
        }
        $currentModule = $module;
        $mod_strings = return_module_language($current_language, $currentModule);
        return  $str . "\n".$this->global_registry_var_name."['meeting_strings'] =  ". $json->encode($mod_strings)."\n";
    }

    // HAS MEETING SPECIFIC CODE:
    public function populateBean(&$focus)
    {
        require_once('include/utils/db_utils.php');
        $all_fields = $focus->column_fields;
        // MEETING SPECIFIC
        $all_fields = array_merge($all_fields, array('required','accept_status','name')); // need name field for contacts and users
        $all_fields = $this->listFilter($focus->module_dir, $all_fields);
        //$all_fields = array_merge($focus->column_fields,$focus->additional_column_fields);

        $module_arr = array();

        $module_arr['module'] = $focus->object_name;

        $module_arr['fields'] = array();

        foreach ($all_fields as $field) {
            if (isset($focus->$field) && !is_object($focus->$field)) {
                $focus->$field =  from_html($focus->$field);
                $focus->$field =  preg_replace("/\r\n/", "<BR>", (string) $focus->$field);
                $focus->$field =  preg_replace("/\n/", "<BR>", $focus->$field);
                $module_arr['fields'][$field] = $focus->$field;
            }
        }
        $GLOBALS['log']->debug("JSON_SERVER:populate bean:");
        return $module_arr;
    }

    /**
     * @param string $module
     * @param array $fields
     *
     * @return array
     */
    public function listFilter($module, $fields)
    {
        $currentUser = $this->getCurrentUser();

        // admin users can access any field
        if ($currentUser && $currentUser->isAdminForModule($module)) {
            return $fields;
        }

        $noAccessFields = array(
            'Users' => array(
                'show_on_employees' => true,
                'portal_only' => true,
                'is_group' => true,
                'system_generated_password' => true,
                'external_auth_only' => true,
                'sugar_login' => true,
                'authenticate_id' => true,
                'pwd_last_changed' => true,
                'is_admin' => true,
                'user_name' => true,
                'user_hash' => true,
                'password' => true,
                'last_login' => true,
            ),
        );
        if (!empty($noAccessFields[$module])) {
            $fields = array_diff($fields, array_keys($noAccessFields[$module]));
        }
        return $fields;
    }

    /**
     * Get current user
     * @return User|null
     */
    protected function getCurrentUser()
    {
        return isset($GLOBALS['current_user']) ? $GLOBALS['current_user'] : null;
    }
}
