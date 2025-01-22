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

require_once('include/MVC/View/SugarView.php');


#[\AllowDynamicProperties]
class ImportView extends SugarView
{
    protected $currentStep;
    protected $pageTitleKey;
    protected $instruction;

    public function __construct($bean = null, $view_object_map = array())
    {
        global $mod_strings;

        parent::__construct($bean, $view_object_map);

        if (isset($_REQUEST['button']) && trim($_REQUEST['button']) === htmlentities((string) $mod_strings['LBL_BACK'])) {
            // if the request comes from the "Back" button, decrease the step count
            $this->currentStep = isset($_REQUEST['current_step']) ? ($_REQUEST['current_step'] - 1) : 1;
        } else {
            $this->currentStep = isset($_REQUEST['current_step']) ? ($_REQUEST['current_step'] + 1) : 1;
        }
        $this->importModule = isset($_REQUEST['import_module']) ? $_REQUEST['import_module'] : '';
    }

    public function preDisplay()
    {
        if (!is_file('cache/jsLanguage/Import/' . $GLOBALS['current_language'] . '.js')) {
            require_once('include/language/jsLanguage.php');
            jsLanguage::createModuleStringsCache('Import', $GLOBALS['current_language']);
        }
        echo '<script src="cache/jsLanguage/Import/'. $GLOBALS['current_language'] . '.js"></script>';
        parent::preDisplay();
    }

    /**
     * @see SugarView::getMenu()
     */
    public function getMenu($module = null)
    {
        global $mod_strings, $current_language;

        if (empty($module)) {
            $module = $this->importModule;
        }

        $old_mod_strings = $mod_strings;
        $mod_strings = return_module_language($current_language, $module);
        $returnMenu = parent::getMenu($module);
        $mod_strings = $old_mod_strings;

        return $returnMenu;
    }

    /**
     * @see SugarView::_getModuleTab()
     */
    protected function _getModuleTab()
    {
        global $app_list_strings, $moduleTabMap;

        // Need to figure out what tab this module belongs to, most modules have their own tabs, but there are exceptions.
        if (!empty($_REQUEST['module_tab'])) {
            return $_REQUEST['module_tab'];
        } elseif (isset($moduleTabMap[$this->importModule])) {
            return $moduleTabMap[$this->importModule];
        }
        // Default anonymous pages to be under Home
        elseif (!isset($app_list_strings['moduleList'][$this->importModule])) {
            return 'Home';
        } else {
            return $this->importModule;
        }
    }

    /**
     * Send our output to the importer controller.
     *
     * @param string $html
     * @param string $submitContent
     * @param string $script
     * @param bool $encode
     * @return void
     */
    protected function sendJsonOutput($html = "", $submitContent = "", $script = "", $encode = false)
    {
        $title = $this->getModuleTitle(false);
        $out = array(
            'html'          => $html,
            'submitContent' => $submitContent,
            'title'         => $title,
            'script'        => $script);

        if ($encode) {
            $function = function (&$val) {
                $val = htmlspecialchars($val, ENT_NOQUOTES);
            };

            array_walk($out, $function);
        }
        echo json_encode($out);
    }

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings, $app_list_strings;
        $returnArray = array(string_format($mod_strings[$this->pageTitleKey], array($this->currentStep)));

        return $returnArray;
    }

    protected function getInstruction()
    {
        global $mod_strings;

        $ins = '';

        if ($this->instruction) {
            $ins_string = $mod_strings[$this->instruction];
            $ins = '<div class="import_instruction">' . $ins_string . '</div>';
        }

        return $ins;
    }

    /**
    * Displays the Smarty template for an error
    *
    * @param string $message error message to show
    * @param string $module what module we were importing into
    * @param string $action what page we should go back to
    */
    protected function _showImportError($message, $module, $action = 'Step1')
    {
        $ss = new Sugar_Smarty();

        $ss->assign("MESSAGE", $message);
        $ss->assign("ACTION", $action);
        $ss->assign("IMPORT_MODULE", $module);
        $ss->assign("MOD", $GLOBALS['mod_strings']);
        $ss->assign("SOURCE", "");
        if (isset($_REQUEST['source'])) {
            $ss->assign("SOURCE", $_REQUEST['source']);
        }

        echo $ss->fetch('modules/Import/tpls/error.tpl');
    }
}
