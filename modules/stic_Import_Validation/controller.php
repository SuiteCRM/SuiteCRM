<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once("modules/stic_Import_Validation/Forms.php");
require_once("include/MVC/Controller/SugarController.php");
require_once('modules/stic_Import_Validation/sources/ImportFile.php');
require_once('modules/stic_Import_Validation/views/ImportListView.php');

#[\AllowDynamicProperties]
class stic_Import_ValidationController extends SugarController
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
        require_once __DIR__ . '/../../modules/stic_Import_Validation/sources/ImportFile.php';
        require_once __DIR__ . '/../../modules/stic_Import_Validation/views/view.confirm.php';
        $v = new stic_Import_ValidationViewConfirm();
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
        $ss->display('modules/stic_Import_Validation/tpls/confirm_table.tpl');
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
        $lv = new stic_Import_ValidationListView($if, array('offset'=> $offset), $tableID);
        $lv->display(false);

        sugar_cleanup(true);
    }

    public function action_Step1()
    {
        // STIC-Code MHP 
        if (!isset($_REQUEST["multimodule"]) || !$_REQUEST["multimodule"]) {
            // If we are not in a multimodule import, we eliminate the information in the session
            $_SESSION["stic_ImporValidation"] = array();
        } else {
            // If we are in a multimodule import, add the _REQUEST to the SESSION
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);          
        }

        // Init the statistics of the module that we are going to validate
        $_SESSION["stic_ImporValidation"]['modules'][$this->importModule] = array (
            "translatedName" => translate('LBL_MODULE_NAME', $_REQUEST["import_module"]),
            "errorCount" => 0,
            "createdCount" => 0,
        );
        // END STIC-Code MHP 
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
        // STIC-Code MHP - If we are not in a multimodule import, add the _REQUEST to the SESSION
        if (!isset($_SESSION["stic_ImporValidation"]["multimodule"]) || !$_SESSION["stic_ImporValidation"]["multimodule"]) {
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);
        }
        $this->view = 'step2';
    }

    public function action_Confirm()
    {
        // STIC-Code MHP - If we are not in a multimodule import, add the _REQUEST to the SESSION
        if (!isset($_SESSION["stic_ImporValidation"]["multimodule"]) || !$_SESSION["stic_ImporValidation"]["multimodule"]) {
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);
        } else {
            // Delete the error file 
            unlink(ImportCacheFiles::getErrorRecordsWithoutErrorFileName());  
            // Set the _REQUEST with values stored in SESSION during the validation of the first module
            $_REQUEST['type'] = 'import';          
            $_REQUEST["import_module"] = $_SESSION["stic_ImporValidation"]["import_module"];
            $_REQUEST["import_type"] = $_SESSION["stic_ImporValidation"]["import_type"];
            $_REQUEST["file_name"] = $_SESSION["stic_ImporValidation"]["file_name"];
            $_REQUEST["importlocale_charset"] = $_SESSION["stic_ImporValidation"]["importlocale_charset"];
            $_REQUEST["importlocale_dateformat"] = $_SESSION["stic_ImporValidation"]["importlocale_dateformat"];
            $_REQUEST["importlocale_timeformat"] = $_SESSION["stic_ImporValidation"]["importlocale_timeformat"];
            $_REQUEST["importlocale_timezone"] = $_SESSION["stic_ImporValidation"]["importlocale_timezone"];
            $_REQUEST["importlocale_currency"] = $_SESSION["stic_ImporValidation"]["importlocale_currency"];
            $_REQUEST["importlocale_default_currency_significant_digits"] = $_SESSION["stic_ImporValidation"]["importlocale_default_currency_significant_digits"];
            $_REQUEST["importlocale_num_grp_sep"] = $_SESSION["stic_ImporValidation"]["importlocale_num_grp_sep"];
            $_REQUEST["importlocale_dec_sep"] = $_SESSION["stic_ImporValidation"]["importlocale_dec_sep"];
            $_REQUEST["importlocale_default_locale_name_format"] = $_SESSION["stic_ImporValidation"]["importlocale_default_locale_name_format"];
            $_REQUEST["custom_delimiter"] = $_SESSION["stic_ImporValidation"]["custom_delimiter"];
            $_REQUEST["custom_delimiter_other"] = $_SESSION["stic_ImporValidation"]["custom_delimiter_other"];
            $_REQUEST["custom_enclosure"] = $_SESSION["stic_ImporValidation"]["custom_enclosure"];
            $_REQUEST["custom_enclosure_other"] = $_SESSION["stic_ImporValidation"]["custom_enclosure_other"];
            $_REQUEST["button"] = $_SESSION["stic_ImporValidation"]["button"];
        }
        // Remove from session the error file name and the search filters
        unset($_SESSION["stic_ImporValidation"]['errorFileName']);
        unset($_SESSION["stic_ImporValidation"]['enabled_dupes']);        
        // END STIC-Code MHP
        $this->view = 'confirm';              
    }

    public function action_Step3()
    {
        // STIC-Code MHP - If we are not in a multimodule import, add the _REQUEST to the SESSION
        if (!isset($_SESSION["stic_ImporValidation"]["multimodule"]) || !$_SESSION["stic_ImporValidation"]["multimodule"]) {
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);
        } else {
            // If we are in a multimodule import, get the delimiter and the enclousure from the session
            $_REQUEST["custom_delimiter"] = $_SESSION["stic_ImporValidation"]["custom_delimiter"];
            $_REQUEST["custom_enclosure"] = $_SESSION["stic_ImporValidation"]["custom_enclosure"];
        }
        // END STIC-Code MHP
        $this->view = 'step3';
    }

    public function action_DupCheck()
    {
        // STIC-Code MHP - If we are not in a multimodule import, add the _REQUEST to the SESSION
        if (!isset($_SESSION["stic_ImporValidation"]["multimodule"]) || !$_SESSION["stic_ImporValidation"]["multimodule"]) {
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);
        }
        // END STIC-Code MHP
        $this->view = 'dupcheck';
    }

    public function action_Step4()
    {
        // STIC-Code MHP - If we are not in a multimodule import, add the _REQUEST to the SESSION
        if (!isset($_SESSION["stic_ImporValidation"]["multimodule"]) || !$_SESSION["stic_ImporValidation"]["multimodule"]) {
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);
        } else {
            // If we are in a multimodule import, set in the REQUEST the values of advanced options stored in the SESSION
            $_REQUEST["custom_delimiter"] = $_SESSION["stic_ImporValidation"]["custom_delimiter"];
            $_REQUEST["custom_delimiter_other"] = $_SESSION["stic_ImporValidation"]["custom_delimiter_other"];
            $_REQUEST["custom_enclosure"] = $_SESSION["stic_ImporValidation"]["custom_enclosure"];
            $_REQUEST["custom_enclosure_other"] = $_SESSION["stic_ImporValidation"]["custom_enclosure_other"];
            $_REQUEST["importlocale_charset"] = $_SESSION["stic_ImporValidation"]["importlocale_charset"];
            $_REQUEST["importlocale_dateformat"] = $_SESSION["stic_ImporValidation"]["importlocale_dateformat"];
            $_REQUEST["importlocale_timeformat"] = $_SESSION["stic_ImporValidation"]["importlocale_timeformat"];
            $_REQUEST["importlocale_timezone"] = $_SESSION["stic_ImporValidation"]["importlocale_timezone"];
            $_REQUEST["importlocale_currency"] = $_SESSION["stic_ImporValidation"]["importlocale_currency"];
            $_REQUEST["importlocale_default_currency_significant_digits"] = $_SESSION["stic_ImporValidation"]["importlocale_default_currency_significant_digits"];
            $_REQUEST["importlocale_num_grp_sep"] = $_SESSION["stic_ImporValidation"]["importlocale_num_grp_sep"];
            $_REQUEST["importlocale_dec_sep"] = $_SESSION["stic_ImporValidation"]["importlocale_dec_sep"];
            $_REQUEST["importlocale_default_locale_name_format"] = $_SESSION["stic_ImporValidation"]["importlocale_default_locale_name_format"];
        }
        // Reset the duplicate filters
        $_SESSION["stic_ImporValidation"]['duplicateFilters'] = array();
        // END STIC-Code MHP
        $this->view = 'step4';
    }

    public function action_Last()
    {
        // STIC-Code MHP - If we are not in a multimodule import, add the _REQUEST to the SESSION
        if (!isset($_SESSION["stic_ImporValidation"]["multimodule"]) || !$_SESSION["stic_ImporValidation"]["multimodule"]) {
            $_SESSION["stic_ImporValidation"] = array_merge($_SESSION["stic_ImporValidation"], $_REQUEST);
        }
        // END STIC-Code MHP
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

    public function action_stic_getControl()
    {
        // STIC-Code MHP - Change the function name
        // echo getControl($_REQUEST['import_module'], $_REQUEST['field_name']);
        echo stic_getControl($_REQUEST['import_module'], $_REQUEST['field_name']);
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
