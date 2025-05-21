<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

/**

 * Description: Controller for the Import module
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */

require_once("modules/Import/Forms.php");
require_once("include/MVC/Controller/SugarController.php");
require_once('modules/Import/sources/ImportFile.php');
require_once('modules/Import/views/ImportListView.php');

#[\AllowDynamicProperties]
class ImportController extends SugarController
{
    /**
     * @see SugarController::loadBean()
     */
    public function loadBean()
    {
        global $mod_strings;

        if (!isset($_REQUEST['import_module'])) {
            $_REQUEST['message'] = $mod_strings['LBL_ERROR_IMPORTS_NOT_SET_UP'];
            $this->view = 'error';
            $this->_processed = true;
            return; // there is no module to load
        }

        $this->importModule = $_REQUEST['import_module'];

        $this->bean = loadBean($this->importModule);
        if ($this->bean) {
            if (!$this->bean->importable) {
                $this->bean = false;
            } elseif ($_REQUEST['import_module'] == 'Users' && !is_admin($GLOBALS['current_user'])) {
                $this->bean = false;
            } elseif ($this->bean->bean_implements('ACL')) {
                if (!ACLController::checkAccess($this->bean->module_dir, 'import', true)) {
                    ACLController::displayNoAccess();
                    sugar_die('');
                }
            }
        }

        if (!$this->bean && $this->importModule != "Administration") {
            $_REQUEST['message'] = $mod_strings['LBL_ERROR_IMPORTS_NOT_SET_UP'];
            $this->view = 'error';
            $this->_processed = true;
        } else {
            $GLOBALS['FOCUS'] = $this->bean;
        }
    }

    public function action_index()
    {
        $this->action_Step1();
    }

    public function action_mapping()
    {
        global $mod_strings, $current_user;
        $results = array('message' => '');
        // handle publishing and deleting import maps
        if (isset($_REQUEST['delete_map_id'])) {
            $import_map = BeanFactory::newBean('Import_1');
            $import_map->mark_deleted($_REQUEST['delete_map_id']);
        }

        if (isset($_REQUEST['publish'])) {
            $import_map = BeanFactory::newBean('Import_1');

            $import_map = $import_map->retrieve($_REQUEST['import_map_id'], false);

            if ($_REQUEST['publish'] == 'yes') {
                $result = $import_map->mark_published($current_user->id, true);
                if (!$result) {
                    $results['message'] = $mod_strings['LBL_ERROR_UNABLE_TO_PUBLISH'];
                }
            } elseif ($_REQUEST['publish'] == 'no') {
                // if you don't own this importmap, you do now, unless you have a map by the same name
                $result = $import_map->mark_published($current_user->id, false);
                if (!$result) {
                    $results['message'] = $mod_strings['LBL_ERROR_UNABLE_TO_UNPUBLISH'];
                }
            }
        }

        echo json_encode($results);
        sugar_cleanup(true);
    }

    public function action_RefreshMapping()
    {
        global $mod_strings;
        require_once __DIR__ . '/../../modules/Import/sources/ImportFile.php';
        require_once __DIR__ . '/../../modules/Import/views/view.confirm.php';
        $v = new ImportViewConfirm();
        $fileName = $_REQUEST['importFile'];

        if (isset($fileName) && strpos((string) $fileName, '..') !== false) {
            LoggerManager::getLogger()->security('Directory navigation attack denied');
            return;
        }

        if (isset($fileName) && !hasValidFileName('import_refresh_mapping_file_name', str_replace('upload://', '', (string) $fileName))) {
            LoggerManager::getLogger()->fatal('Invalid importFile file name');
            return;
        }

        if (strpos((string) $fileName, 'phar://') !== false) {
            LoggerManager::getLogger()->fatal('Invalid importFile file path');
            return;
        }

        $delim = $_REQUEST['delim'];

        if ($delim === '\t') {
            $delim = "\t";
        }

        $enclosure = $_REQUEST['qualif'];
        $enclosure = html_entity_decode((string) $enclosure, ENT_QUOTES);
        $hasHeader = !empty($_REQUEST['header']);

        $importFile = new ImportFile($fileName, $delim, $enclosure, false);
        $importFile->setHeaderRow($hasHeader);
        $rows = $v->getSampleSet($importFile);

        $ss = new Sugar_Smarty();
        $ss->assign("SAMPLE_ROWS", $rows);
        $ss->assign("HAS_HEADER", $hasHeader);
        $ss->assign("column_count", $v->getMaxColumnsInSampleSet($rows));
        $ss->assign("MOD", $mod_strings);
        $ss->display('modules/Import/tpls/confirm_table.tpl');
        sugar_cleanup(true);
    }

    public function action_RefreshTable()
    {
        $offset = isset($_REQUEST['offset']) ? $_REQUEST['offset'] : 0;
        $tableID = isset($_REQUEST['tableID']) ? $_REQUEST['tableID'] : 'errors';
        $has_header = $_REQUEST['has_header'] == 'on' ? true : false;
        if ($tableID == 'dup') {
            $tableFilename = ImportCacheFiles::getDuplicateFileName();
        } else {
            $tableFilename = ImportCacheFiles::getErrorRecordsFileName();
        }

        $if = new ImportFile($tableFilename, ",", '"', false, false);
        $if->setHeaderRow($has_header);
        $lv = new ImportListView($if, array('offset'=> $offset), $tableID);
        $lv->display(false);

        sugar_cleanup(true);
    }

    public function action_Step1()
    {
        $fromAdminView = isset($_REQUEST['from_admin_wizard']) ? $_REQUEST['from_admin_wizard'] : false;
        if ($this->importModule == 'Administration' || $fromAdminView
        ) {
            $this->view = 'step1';
        } else {
            $this->view = 'step2';
        }
    }

    public function action_Step2()
    {
        $this->view = 'step2';
    }

    public function action_Confirm()
    {
        $this->view = 'confirm';
    }

    public function action_Step3()
    {
        $this->view = 'step3';
    }

    public function action_DupCheck()
    {
        $this->view = 'dupcheck';
    }

    public function action_Step4()
    {
        $this->view = 'step4';
    }

    public function action_Last()
    {
        $this->view = 'last';
    }

    public function action_Undo()
    {
        $this->view = 'undo';
    }

    public function action_Error()
    {
        $this->view = 'error';
    }

    public function action_ExtStep1()
    {
        $this->view = 'extStep1';
    }

    public function action_Extdupcheck()
    {
        $this->view = 'extdupcheck';
    }

    public function action_Extimport()
    {
        $this->view = 'extimport';
    }

    public function action_GetControl()
    {
        echo getControl($_REQUEST['import_module'], $_REQUEST['field_name']);
        exit;
    }

    public function action_AuthenticatedSources()
    {
        $this->view = 'authenticatedsources';
    }

    public function action_RevokeAccess()
    {
        $this->view = 'revokeaccess';
    }
}
