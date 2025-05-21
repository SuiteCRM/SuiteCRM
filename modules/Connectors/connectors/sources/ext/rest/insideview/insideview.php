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


require_once('include/connectors/sources/ext/rest/rest.php');
#[\AllowDynamicProperties]
class ext_rest_insideview extends ext_rest
{
    protected $_enable_in_wizard = false;
    protected $_enable_in_hover = false;
    protected $_enable_in_admin_properties = false;
    protected $_enable_in_admin_mapping = false;
    protected $_enable_in_admin_search = false;
    protected $_has_testing_enabled = false;

    protected $orgId;
    protected $orgName;
    protected $userId;
    public $allowedModuleList;
    
    public function __construct()
    {
        global $app_list_strings;
        $this->allowedModuleList = array('Accounts' => $app_list_strings['moduleList']['Accounts'],
                                         'Contacts' => $app_list_strings['moduleList']['Contacts'],
                                         'Opportunities' => $app_list_strings['moduleList']['Opportunities'],
                                         'Leads' => $app_list_strings['moduleList']['Leads']);

        parent::__construct();
    }

    public function filterAllowedModules($moduleList)
    {
        // InsideView currently has no ability to talk to modules other than these four
        $outModuleList = array();
        foreach ($moduleList as $module) {
            if (!in_array($module, $this->allowedModuleList)) {
                continue;
            } else {
                $outModuleList[$module] = $module;
            }
        }
        return $outModuleList;
    }

    public function saveMappingHook($mapping)
    {
        $removeList = array();
        foreach ($this->allowedModuleList as $module_name=>$display_name) {
            $removeList[$module_name] = $module_name;
        }

        if (is_array($mapping['beans'])) {
            foreach ($mapping['beans'] as $module => $ignore) {
                unset($removeList[$module]);
                
                check_logic_hook_file($module, 'after_ui_frame', array(1, $module. ' InsideView frame', 'modules/Connectors/connectors/sources/ext/rest/insideview/InsideViewLogicHook.php', 'InsideViewLogicHook', 'showFrame'));
            }
        }

        foreach ($removeList as $module) {
            remove_logic_hook($module, 'after_ui_frame', array(1, $module. ' InsideView frame', 'modules/Connectors/connectors/sources/ext/rest/insideview/InsideViewLogicHook.php', 'InsideViewLogicHook', 'showFrame'));
        }

        return parent::saveMappingHook($mapping);
    }

    

    public function getItem($args=array(), $module=null)
    {
    }
    public function getList($args=array(), $module=null)
    {
    }


    public function ext_allowInsideView($request)
    {
        $GLOBALS['current_user']->setPreference('allowInsideView', 1, 0, 'Connectors');
        return true;
    }
}
