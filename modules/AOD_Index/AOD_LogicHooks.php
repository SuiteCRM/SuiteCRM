<?php
 /**
 *
 *
 * @package
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility Ltd <support@salesagility.com>
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
