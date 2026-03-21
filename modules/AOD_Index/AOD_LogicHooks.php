<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2014 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

#[\AllowDynamicProperties]
class AOD_LogicHooks
{
    /**
     * @deprecated since v7.12.0
     * @param SugarBean $bean
     * @param $event
     * @param $arguments
     */
    public function saveModuleChanges(SugarBean $bean, $event, $arguments)
    {
        if ($bean->module_name == 'AOD_Index') {
            return;
        }
        if (defined('sugarEntry') && defined('SUGARCRM_IS_INSTALLING')) {
            return;
        }
        try {
            $index = BeanFactory::getBean("AOD_Index")->getIndex();
            $index->index($bean->module_name, $bean->id);
        } catch (Exception $ex) {
            $GLOBALS['log']->error($ex->getMessage());
        }
    }

    /**
     * @deprecated since v7.12.0
     * @param SugarBean $bean
     * @param $event
     * @param $arguments
     */
    public function saveModuleDelete(SugarBean $bean, $event, $arguments)
    {
        if ($bean->module_name == 'AOD_Index') {
            return;
        }
        if (defined('sugarEntry') && defined('SUGARCRM_IS_INSTALLING')) {
            return;
        }
        try {
            $index = BeanFactory::getBean("AOD_Index")->getIndex();
            $index->remove($bean->module_name, $bean->id);
        } catch (Exception $ex) {
            $GLOBALS['log']->error($ex->getMessage());
        }
    }

    /**
     * @deprecated since v7.12.0
     * @param SugarBean $bean
     * @param $event
     * @param $arguments
     */
    public function saveModuleRestore(SugarBean $bean, $event, $arguments)
    {
        if ($bean->module_name == 'AOD_Index') {
            return;
        }
        if (defined('sugarEntry') && defined('SUGARCRM_IS_INSTALLING')) {
            return;
        }
        try {
            $index = BeanFactory::getBean("AOD_Index")->getIndex();
            $index->index($bean->module_name, $bean->id);
        } catch (Exception $ex) {
            $GLOBALS['log']->error($ex->getMessage());
        }
    }
}
