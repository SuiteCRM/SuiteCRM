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

/**

 * Description: view handler for step 1 of the import process
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 */
require_once('modules/Import/views/ImportView.php');
require_once('include/externalAPI/ExternalAPIFactory.php');
require_once('modules/Import/Importer.php');


#[\AllowDynamicProperties]
class ImportViewStep1 extends ImportView
{
    protected $pageTitleKey = 'LBL_STEP_1_TITLE';

    public function __construct($bean = null, $view_object_map = array())
    {
        parent::__construct($bean, $view_object_map);
        $this->currentStep = isset($_REQUEST['current_step']) ? ($_REQUEST['current_step'] + 1) : 1;
        $this->importModule = isset($_REQUEST['import_module']) ? $_REQUEST['import_module'] : '';
        if (isset($_REQUEST['from_admin_wizard']) &&  $_REQUEST['from_admin_wizard']) {
            $this->importModule = 'Administration';
        }
    }

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings, $app_list_strings;

        $iconPath = $this->getModuleTitleIconPath($this->module);
        $returnArray = array();
        if (!empty($iconPath) && !$browserTitle) {
            $returnArray[] = "<a href='index.php?module={$_REQUEST['import_module']}&action=index'><!--not_in_theme!--><img src='{$iconPath}' alt='{$app_list_strings['moduleList'][$_REQUEST['import_module']]}' title='{$app_list_strings['moduleList'][$_REQUEST['import_module']]}' align='absmiddle'></a>";
        } else {
            $returnArray[] = $app_list_strings['moduleList'][$_REQUEST['import_module']];
        }
        $returnArray[] = "<a href='index.php?module=Import&action=Step1&import_module={$_REQUEST['import_module']}'>".$mod_strings['LBL_MODULE_NAME']."</a>";
        $returnArray[] = $mod_strings['LBL_STEP_1_TITLE'];

        return $returnArray;
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_strings, $current_user;
        global $sugar_config;

        $this->ss->assign("MODULE_TITLE", $this->getModuleTitle(false));
        $this->ss->assign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LNK_DELETE']));
        $this->ss->assign("PUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('publish_inline', 'align="absmiddle" border="0"', null, null, '.gif', $mod_strings['LBL_PUBLISH']));
        $this->ss->assign("UNPUBLISH_INLINE_PNG", SugarThemeRegistry::current()->getImage('unpublish_inline', 'align="absmiddle" border="0"', null, null, '.gif', $mod_strings['LBL_UNPUBLISH']));
        $this->ss->assign("IMPORT_MODULE", $_REQUEST['import_module']);

        $showModuleSelection = ($this->importModule == 'Administration');
        $importableModulesOptions = array();
        $importablePersonModules = array();
        //If we are coming from the admin link, get the module list.
        if ($showModuleSelection) {
            $tmpImportable = Importer::getImportableModules();
            $importableModulesOptions = get_select_options_with_id($tmpImportable, '');
            $importablePersonModules = $this->getImportablePersonModulesJS();
            $this->ss->assign("IMPORT_MODULE", key($tmpImportable));
        } else {
            $this->instruction = 'LBL_SELECT_DS_INSTRUCTION';
            $this->ss->assign('INSTRUCTION', $this->getInstruction());
        }
        $this->ss->assign("FROM_ADMIN", $showModuleSelection);
        $this->ss->assign("PERSON_MODULE_LIST", json_encode($importablePersonModules));
        $this->ss->assign("showModuleSelection", $showModuleSelection);
        $this->ss->assign("IMPORTABLE_MODULES_OPTIONS", $importableModulesOptions);

        $this->ss->assign("EXTERNAL_SOURCES", $this->getAllImportableExternalEAPMs());
        $this->ss->assign("EXTERNAL_AUTHENTICATED_SOURCES", json_encode($this->getAuthenticatedImportableExternalEAPMs()));
        $selectExternal = !empty($_REQUEST['application']) ? $_REQUEST['application'] : '';
        $this->ss->assign("selectExternalSource", $selectExternal);

        $content = $this->ss->fetch('modules/Import/tpls/step1.tpl');

        $submitContent = "<table width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\"><tr><td align=\"right\">";
        $submitContent .= "<input title=\"".$mod_strings['LBL_IMPORT_COMPLETE']."\" onclick=\"SUGAR.importWizard.closeDialog();\" class=\"button\" type=\"submit\" name=\"finished\" value=\"  ".$mod_strings['LBL_IMPORT_COMPLETE']."  \" id=\"finished\">";
        $submitContent .= "<input title=\"".$mod_strings['LBL_NEXT']."\" class=\"button primary\" type=\"submit\" name=\"button\" value=\"  ".$mod_strings['LBL_NEXT']."  \"  id=\"gonext\"></td></tr></table>";

        $this->ss->assign("JAVASCRIPT", $this->_getJS());
        $this->ss->assign("CONTENT", $content);
        $this->ss->display('modules/Import/tpls/wizardWrapper.tpl');
    }

    private function getImportablePersonModulesJS()
    {
        global $beanList;
        $results = array();
        foreach ($beanList as $moduleName => $beanName) {
            if (class_exists($beanName)) {
                $tmp = new $beanName();
                if (isset($tmp->importable) && $tmp->importable && ($tmp instanceof Person)) {
                    $results[$moduleName] = $moduleName;
                }
            }
        }

        return $results;
    }

    private function getAllImportableExternalEAPMs()
    {
        ExternalAPIFactory::clearCache();
        return ExternalAPIFactory::getModuleDropDown('Import', true, false);
    }

    private function getAuthenticatedImportableExternalEAPMs()
    {
        return ExternalAPIFactory::getModuleDropDown('Import', false, false);
    }
    /**
     * Returns JS used in this view
     */
    private function _getJS($sourceType = false)
    {
        global $mod_strings;
        $EXTERNAL_AUTHENTICATED_SOURCES = json_encode($this->getAuthenticatedImportableExternalEAPMs());
        $selectExternalSource = !empty($_REQUEST['application']) ? $_REQUEST['application'] : '';

        $showModuleSelection = ($this->importModule == 'Administration');
        $importableModulesOptions = array();
        $importablePersonModules = array();
        //If we are coming from the admin link, get the module list.
        if ($showModuleSelection) {
            $importablePersonModules = $this->getImportablePersonModulesJS();
        }


        $PERSON_MODULE_LIST = json_encode($importablePersonModules);

        return <<<EOJAVASCRIPT


document.getElementById('gonext').onclick = function()
{
    clear_all_errors();
    var csvSourceEl = document.getElementById('csv_source');
    var isCsvSource = csvSourceEl ? csvSourceEl.checked : true;
    if( isCsvSource )
    {
        document.getElementById('importstep1').action.value = 'Step2';
        return true;
    }
    else
    {
        if(selectedExternalSource == '')
        {
            add_error_style('importstep1','external_source',"{$mod_strings['ERR_MISSING_REQUIRED_FIELDS']} {$mod_strings['LBL_EXTERNAL_SOURCE']}");
            return false;
        }

        document.getElementById('importstep1').action.value = 'ExtStep1';
        document.getElementById('importstep1').external_source.value = selectedExternalSource;

        return true;
    }
}

YAHOO.util.Event.onDOMReady(function(){

    var oButtonGroup = new YAHOO.widget.ButtonGroup("smtpButtonGroup");

    function toggleExternalSource(el)
    {
        var trEl = document.getElementById('external_sources_tr');
        var externalSourceBttns = oButtonGroup.getButtons();

        if(this.value == 'csv')
        {
            trEl.style.display = 'none';
            document.getElementById('gonext').disabled = false;
            document.getElementById('ext_source_sign_in_bttn').style.display = 'none';

            //Turn off ext source selection
            oButtonGroup.set("checkedButton", null, true);
            for(i=0;i<externalSourceBttns.length;i++)
            {
                externalSourceBttns[i].set("checked", true, true);
            }
            selectedExternalSource = '';
        }
        else
        {
            trEl.style.display = '';
            document.getElementById('gonext').disabled = true;
            
            //Highlight the first selection by default
            if(externalSourceBttns.length >= 1)
            {
                if(selectedExternalSource == '')
                    oButtonGroup.check(0);
            }
        }
    }

    YAHOO.util.Event.addListener(['ext_source','csv_source'], "click", toggleExternalSource);

    function isExtSourceAuthenticated(source)
    {
        if( typeof(auth_sources[source]) != 'undefined')
            return true;
        else
            return false;
    }

    function isExtSourceValid(v)
    {
        if(v == '')
        {
            document.getElementById('ext_source_sign_in_bttn').style.display = 'none';
            return '';
        }
        if( !isExtSourceAuthenticated(v) )
        {
            document.getElementById('ext_source_sign_in_bttn').style.display = '';
            document.getElementById('ext_source_sign_out_bttn').style.display = 'none';
            document.getElementById('gonext').disabled = true;
        }
        else
        {
            document.getElementById('ext_source_sign_in_bttn').style.display = 'none';
            document.getElementById('gonext').disabled = false;
        }
    }

    function openExtAuthWindow()
    {
        var api = externalApis[selectedExternalSource];
        if (!api) {
            return;
        }

        if (api.authMethod === "oauth2") {
            openOauth2Window(api.loginUrl);
        } else {
            var import_module = document.getElementById('importstep1').import_module.value;
            var url = api.loginUrl + "&return_module=Import&return_action=" + import_module;
            document.location = url;
        }
    }

    function openOauth2Window(url)
    {
        window.open(url, '_blank', "width=600,height=400,centerscreen=1,resizable=1");
    }

    function setImportModule()
    {
        var selectedModuleEl = document.getElementById('admin_import_module');
        if(!selectedModuleEl)
        {
            return;
        }

        //Check if the module selected by the admin is a person type module, if not hide
        //the external source.
        var selectedModule = selectedModuleEl.value;
        document.getElementById('importstep1').import_module.value = selectedModule;
    }

    YAHOO.util.Event.addListener('ext_source_sign_in_bttn', "click", openExtAuthWindow);
    YAHOO.util.Event.addListener('admin_import_module', "change", setImportModule);


    function initExtSourceSelection()
    {
        var el1 = YAHOO.util.Dom.get('ext_source');
        if(selectedExternalSource == '')
            return;

        el1.checked = true;
        toggleExternalSource();
        isExtSourceValid(selectedExternalSource);
    }
    initExtSourceSelection();

    setImportModule();
});


var auth_sources = {$EXTERNAL_AUTHENTICATED_SOURCES}
var selectedExternalSource = '{$selectExternalSource}';
var personModules = {$PERSON_MODULE_LIST};

EOJAVASCRIPT;
    }
}
