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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once('include/MVC/View/SugarView.php');

/**
 * @deprecated since v7.12.0
 * Class AOD_IndexViewIndexData
 */
#[\AllowDynamicProperties]
class AOD_IndexViewIndexData extends SugarView
{

    /**
     * display the form
     * @deprecated since v7.12.0
     */
    public function display()
    {

        global $timedate, $current_language;
        $db = DBManagerFactory::getInstance();

        parent::display();

        $index = BeanFactory::getBean("AOD_Index");
        $index = $index->getIndex();
        $listViewDefs = [];


        $beanList = $index->getIndexableModules();

        $moduleCounts = array();

        foreach ($beanList as $beanModule => $beanName) {
            $bean = BeanFactory::getBean($beanModule);
            if (!$bean || !method_exists($bean, "getTableName") || !$bean->getTableName()) {
                continue;
            }
            $query = "SELECT COUNT(DISTINCT b.id) FROM ".$bean->getTableName()." b WHERE b.deleted = 0";
            $moduleCounts[$beanModule] = $db->getOne($query);
        }


        $revisionCount = array_sum($moduleCounts);
        $indexedCount = $db->getOne("SELECT COUNT(*) FROM aod_indexevent WHERE deleted = 0 AND success = 1");
        $failedCount = $db->getOne("SELECT COUNT(*) FROM aod_indexevent WHERE deleted = 0 AND success = 0");

        $indexFiles = is_countable(glob($index->location."/*.cfs")) ? count(glob($index->location."/*.cfs")) : 0;

        $this->ss->assign("revisionCount", $revisionCount);
        $this->ss->assign("indexedCount", $indexedCount);
        $this->ss->assign("failedCount", $failedCount);
        $this->ss->assign("index", $index);
        $this->ss->assign("indexFiles", $indexFiles);
        echo $this->ss->fetch('modules/AOD_Index/tpls/indexdata.tpl');




        if ($failedCount) {
            $seed = BeanFactory::newBean("AOD_IndexEvent");

            $lv = new ListViewSmarty();
            $lv->lvd->additionalDetails = false;
            $mod_strings = return_module_language($current_language, $seed->module_dir);

            require('modules/'.$seed->module_dir.'/metadata/listviewdefs.php');

            if (file_exists('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php')) {
                require('custom/modules/'.$seed->module_dir.'/metadata/listviewdefs.php');
            }

            $lv->displayColumns = $listViewDefs[$seed->module_dir];

            $lv->quickViewLinks = false;
            $lv->export = false;
            $lv->mergeduplicates = false;
            $lv->multiSelect = false;
            $lv->delete = false;
            $lv->select = false;
            $lv->showMassupdateFields = false;
            $lv->email = false;

            $lv->setup($seed, 'include/ListView/ListViewNoMassUpdate.tpl', 'success = 0', '', 0, 10);

            echo '<br /><br />' . get_form_header($GLOBALS['mod_strings']['LBL_FAILED_RECORDS'] . ' (' . $lv->data['pageData']['offsets']['total'] . ')', '', false);
            if ($lv->data['pageData']['offsets']['total'] == 0) {
                echo "No data";
            } else {
                echo $lv->display();
            }
        }
    }
}
