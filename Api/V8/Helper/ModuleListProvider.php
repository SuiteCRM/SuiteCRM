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

namespace Api\V8\Helper;

/**
 * Class ModuleListProvider
 * @package Api\V8\Helper
 */
#[\AllowDynamicProperties]
class ModuleListProvider
{
    /**
     * @return array
     */
    public function getModuleList()
    {
        global $current_user;

        $modules = query_module_access_list($current_user);
        \ACLController::filterModuleList($modules, false);
        $modules = $this->removeInvisibleModules($modules);
        $modules = $this->markACLAccess($modules);
        $modules = $this->addModuleLabels($modules);

        return $modules;
    }

    /**
     * @param $modules
     * @return mixed
     */
    private function addModuleLabels($modules)
    {
        global $app_list_strings;

        foreach ($modules as $moduleName => &$moduleData) {
            $moduleData['label'] = $app_list_strings['moduleList'][$moduleName];
        }
        return $modules;
    }

    /**
     * @param $modules
     * @return array
     */
    private function removeInvisibleModules($modules)
    {
        global $modInvisList;

        foreach ($modInvisList as $invis) {
            unset($modules[$invis]);
        }

        return $modules;
    }

    /**
     * @param $modules
     * @return mixed
     */
    private function markACLAccess($modules)
    {
        global $current_user;

        $modulesWithAccess = [];
        $moduleActions = \ACLAction::getUserActions($current_user->id, true);

        foreach ($moduleActions as $moduleName => $value) {
            if (!in_array($moduleName, $modules, true)) {
                continue;
            }
            $access = $this->buildAccessArray($moduleName, $value['module']);
            if (!count($access)) {
                continue;
            }
            $modulesWithAccess[$moduleName] = [
                'label' => '',
                'access' => array_unique($access)
            ];
        }

        return $modulesWithAccess;
    }

    /**
     * @param mixed $moduleName
     * @param mixed $actions
     * @return array
     */
    private function buildAccessArray($moduleName, $actions)
    {
        $access = [];
        foreach ($actions as $actionName => $record) {
            if (!$this->hasACL($record['aclaccess'], $moduleName)) {
                continue;
            }
            $access[] = $actionName;
        }
        return $access;
    }

    /**
     * @param mixed $level
     * @param mixed $module
     * @return bool
     */
    private function hasACL($level, $module)
    {
        global $current_user;

        if (is_admin(is_admin($current_user))) {
            return true;
        }

        return $level >= ACL_ALLOW_ENABLED;
    }
}
