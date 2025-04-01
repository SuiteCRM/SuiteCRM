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

require_once('modules/Import/views/ImportView.php');
require_once('modules/Import/ImportCacheFiles.php');
require_once('modules/Import/sources/ImportFile.php');
require_once('modules/Import/views/ImportListView.php');
require_once('include/ListView/ListViewFacade.php');

#[\AllowDynamicProperties]
class ImportViewLast extends ImportView
{
    protected $pageTitleKey = 'LBL_STEP_5_TITLE';

    public $lvf;

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_strings, $current_user, $sugar_config, $current_language;



        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);
        $this->ss->assign("TYPE", $_REQUEST['type']);
        $this->ss->assign("HEADER", $app_strings['LBL_IMPORT']." ". $mod_strings['LBL_MODULE_NAME']);
        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false));
        // lookup this module's $mod_strings to get the correct module name
        $module_mod_strings =
            return_module_language($current_language, $_REQUEST['import_module']);
        $this->ss->assign("MODULENAME", $module_mod_strings['LBL_MODULE_NAME']);

        // read status file to get totals for records imported, errors, and duplicates
        $count        = 0;
        $errorCount   = 0;
        $dupeCount    = 0;
        $createdCount = 0;
        $updatedCount = 0;
        $fp = sugar_fopen(ImportCacheFiles::getStatusFileName(), 'r');
        
        // Read the data if we successfully opened file
        if ($fp !== false) {
            // Read rows 1 by 1 and add the info
            while ($row = fgetcsv($fp, 8192)) {
                $count         += (int) $row[0];
                $errorCount    += (int) $row[1];
                $dupeCount     += (int) $row[2];
                $createdCount  += (int) $row[3];
                $updatedCount  += (int) $row[4];
            }
            fclose($fp);
        }
        
        $this->ss->assign("showUndoButton", false);
        if ($createdCount > 0) {
            $this->ss->assign("showUndoButton", true);
        }

        if ($errorCount > 0 &&  ($createdCount <= 0 && $updatedCount <= 0)) {
            $activeTab = 2;
        } else {
            if ($dupeCount > 0 &&  ($createdCount <= 0 && $updatedCount <= 0)) {
                $activeTab = 1;
            } else {
                $activeTab = 0;
            }
        }

        $this->ss->assign("JAVASCRIPT", $this->_getJS($activeTab));

        $this->ss->assign("errorCount", $errorCount);
        $this->ss->assign("dupeCount", $dupeCount);
        $this->ss->assign("createdCount", $createdCount);
        $this->ss->assign("updatedCount", $updatedCount);
        $this->ss->assign("errorFile", ImportCacheFiles::convertFileNameToUrl(ImportCacheFiles::getErrorFileName()));
        $this->ss->assign("errorrecordsFile", ImportCacheFiles::convertFileNameToUrl(ImportCacheFiles::getErrorRecordsWithoutErrorFileName()));
        $this->ss->assign("dupeFile", ImportCacheFiles::convertFileNameToUrl(ImportCacheFiles::getDuplicateFileName()));

        if ($this->bean->object_name == "Prospect") {
            $this->ss->assign("PROSPECTLISTBUTTON", $this->_addToProspectListButton());
        } else {
            $this->ss->assign("PROSPECTLISTBUTTON", "");
        }

        $resultsTable = "";
        foreach (UsersLastImport::getBeansByImport($_REQUEST['import_module']) as $beanname) {
            // load bean
            if (!($this->bean instanceof $beanname)) {
                $this->bean = new $beanname;
            }
            $resultsTable .= $this->getListViewResults();
        }
        if (empty($resultsTable)) {
            $resultsTable = $this->getListViewResults();
        }

        $this->ss->assign("RESULTS_TABLE", $resultsTable);
        $this->ss->assign("ERROR_TABLE", $this->getListViewTableFromFile(ImportCacheFiles::getErrorRecordsFileName(), 'errors'));
        $this->ss->assign("DUP_TABLE", $this->getListViewTableFromFile(ImportCacheFiles::getDuplicateFileDisplayName(), 'dup'));
        $content = $this->ss->fetch('modules/Import/tpls/last.tpl');
        $this->ss->assign("CONTENT", $content);
        $this->ss->display('modules/Import/tpls/wizardWrapper.tpl');
    }

    protected function getListViewResults()
    {
        global $mod_strings, $current_language;
        // build listview to show imported records
        $lvf = !empty($this->lvf) ? $this->lvf : new ListViewFacade($this->bean, $this->bean->module_dir, 0);

        $params = array();
        if (!empty($_REQUEST['orderBy'])) {
            $params['orderBy'] = $_REQUEST['orderBy'];
            $params['overrideOrder'] = true;
            if (!empty($_REQUEST['sortOrder'])) {
                $params['sortOrder'] = $_REQUEST['sortOrder'];
            }
        }
        $beanname = ($this->bean->object_name == 'Case' ? 'aCase' : $this->bean->object_name);
        // add users_last_import joins so we only show records done in this import
        $params['custom_from']  = ', users_last_import';
        $params['custom_where'] = " AND users_last_import.assigned_user_id = '{$GLOBALS['current_user']->id}'
                AND users_last_import.bean_type = '{$beanname}'
                AND users_last_import.bean_id = {$this->bean->table_name}.id
                AND users_last_import.deleted = 0
                AND {$this->bean->table_name}.deleted = 0";

        $lvf->lv->mergeduplicates = false;
        $lvf->lv->showMassupdateFields = false;
        if ($lvf->type == 2) {
            $lvf->template = 'include/ListView/ListViewNoMassUpdate.tpl';
        }

        $module_mod_strings = return_module_language($current_language, $this->bean->module_dir);
        $lvf->setup('', '', $params, $module_mod_strings, 0, -1, '', strtoupper($beanname), array(), 'id');
        global $app_list_strings;
        return $lvf->display($app_list_strings['moduleList'][$this->bean->module_dir], 'main', true);
    }

    protected function getListViewTableFromFile($fileName, $tableName)
    {
        $has_header = $_REQUEST['has_header'] == 'on' ? true : false;
        $if = new ImportFile($fileName, ",", '"', false, false);
        $if->setHeaderRow($has_header);
        $lv = new ImportListView($if, array('offset'=> 0), $tableName);
        return $lv->display(true);
    }

    /**
     * Returns JS used in this view
     */
    private function _getJS($activeTab)
    {
        return <<<EOJAVASCRIPT

document.getElementById('importmore').onclick = function(){
    document.getElementById('importlast').action.value = 'Step1';
    return true;
}

document.getElementById('finished').onclick = function(){
    document.getElementById('importlast').module.value = document.getElementById('importlast').import_module.value;
    document.getElementById('importlast').action.value = 'index';
	return true;

}

if ( typeof(SUGAR) == 'undefined' )
    SUGAR = {};
if ( typeof(SUGAR.IV) == 'undefined' )
    SUGAR.IV = {};

SUGAR.IV = {

    getTable : function(tableID, offset) {
        var callback = {
            success: function(o)
            {
                var tableKey = tableID + '_table';
                document.getElementById(tableKey).innerHTML = o.responseText;
            },
            failure: function(o) {},
        };
        var has_header = document.getElementById('importlast').has_header.value
        var url = 'index.php?action=RefreshTable&module=Import&offset=' + offset + '&tableID=' + tableID + '&has_header=' + has_header;
        YAHOO.util.Connect.asyncRequest('GET', url, callback);
    },
    togglePages : function(activePage)
    {
        var num_tabs = 3;
        var pageId = 'pageNumIW_' + activePage;
        activeDashboardPage = activePage;
        activeTab = activePage;

        //hide all pages first for display purposes
        for(var i=0; i < num_tabs; i++)
        {
            var pageDivId = 'pageNumIW_'+i+'_div';
            var pageDivElem = document.getElementById(pageDivId);
            pageDivElem.style.display = 'none';
        }

        for(var i=0; i < num_tabs; i++)
        {
            var tabId = 'pageNumIW_'+i;
            var anchorId = 'pageNumIW_'+i+'_anchor';
            var pageDivId = 'pageNumIW_'+i+'_div';

            var tabElem = document.getElementById(tabId);
            var anchorElem = document.getElementById(anchorId);
            var pageDivElem = document.getElementById(pageDivId);

            if(tabId == pageId)
            {
                tabElem.className = 'active';
                anchorElem.className = 'current';
                pageDivElem.style.display = '';
            }
            else
            {
                tabElem.className = '';
                anchorElem.className = '';
            }
        }
    }
}

SUGAR.IV.togglePages('$activeTab');


EOJAVASCRIPT;
    }
    /**
     * Returns a button to add this list of prospects to a Target List
     *
     * @return string html code to display button
     */
    private function _addToProspectListButton()
    {
        global $app_strings, $sugar_version, $sugar_config, $current_user;

        $query = "SELECT distinct prospects.id, prospects.assigned_user_id, prospects.first_name, prospects.last_name, prospects.phone_work, prospects.title,
				email_addresses.email_address email1, users.user_name as assigned_user_name
				FROM users_last_import,prospects
                LEFT JOIN users ON prospects.assigned_user_id=users.id
				LEFT JOIN email_addr_bean_rel on prospects.id = email_addr_bean_rel.bean_id and email_addr_bean_rel.bean_module='Prospect' and email_addr_bean_rel.primary_address=1 and email_addr_bean_rel.deleted=0
				LEFT JOIN email_addresses on email_addresses.id = email_addr_bean_rel.email_address_id
				WHERE users_last_import.assigned_user_id = '{$current_user->id}' AND users_last_import.bean_type='Prospect' AND users_last_import.bean_id=prospects.id
				AND users_last_import.deleted=0 AND prospects.deleted=0";

        $prospect_id = array();
        if (!empty($query)) {
            $res=DBManagerFactory::getInstance()->query($query);
            while ($row = DBManagerFactory::getInstance()->fetchByAssoc($res)) {
                $prospect_id[]=$row['id'];
            }
        }
        $popup_request_data = array(
            'call_back_function' => 'set_return_and_save_background',
            'form_name' => 'DetailView',
            'field_to_name_array' => array(
                'id' => 'prospect_list_id',
            ),
            'passthru_data' => array(
                'child_field' => 'notused',
                'return_url' => 'notused',
                'link_field_name' => 'notused',
                'module_name' => 'notused',
                'refresh_page'=>'1',
                'return_type'=>'addtoprospectlist',
                'parent_module'=>'ProspectLists',
                'parent_type'=>'ProspectList',
                'child_id'=>'id',
                'link_attribute'=>'prospects',
                'link_type'=>'default',	 //polymorphic or default
                'prospect_ids'=>$prospect_id,
            )
        );

        $json = getJSONobj();
        $encoded_popup_request_data = $json->encode($popup_request_data);

        return <<<EOHTML
<script type="text/javascript" src="include/SubPanel/SubPanelTiles.js?s={$sugar_version}&c={$sugar_config['js_custom_version']}"></script>
<input align=right" type="button" name="select_button" id="select_button" class="button"
     title="{$app_strings['LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL']}"
     value="{$app_strings['LBL_ADD_TO_PROSPECT_LIST_BUTTON_LABEL']}"
     onclick='open_popup("ProspectLists",600,400,"",true,true,$encoded_popup_request_data,"Single","true");' />
EOHTML;
    }
}
