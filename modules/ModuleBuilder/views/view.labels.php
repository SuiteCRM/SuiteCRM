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

/*
 * Created on Jul 24, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
require_once('modules/ModuleBuilder/MB/AjaxCompose.php');
require_once('modules/ModuleBuilder/views/view.modulefields.php');
#[\AllowDynamicProperties]
class ViewLabels extends ViewModulefields
{
    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
           translate('LBL_MODULE_NAME', 'Administration'),
           ModuleBuilderController::getModuleTitle(),
           );
    }

    //STUDIO LABELS ONLY//
    //TODO Bundle Studio and ModuleBuilder label handling to increase maintainability.
    public function display()
    {
        $translatedEditModule = '';
        $editModule = $_REQUEST['view_module'];
        $allLabels = (!empty($_REQUEST['labels']) && $_REQUEST['labels']== 'all');

        if (!isset($_REQUEST['MB'])) {
            global $app_list_strings;
            $moduleNames = array_change_key_case($app_list_strings['moduleList']);
            $translatedEditModule = $moduleNames[strtolower($editModule)];
        }
        $selected_lang = (!empty($_REQUEST['selected_lang'])? $_REQUEST['selected_lang']:$_SESSION['authenticated_user_language']);
        if (empty($selected_lang)) {
            $selected_lang = $GLOBALS['sugar_config']['default_language'];
        }

        $smarty = new Sugar_Smarty();
        global $mod_strings;
        $smarty->assign('mod_strings', $mod_strings);
        $smarty->assign('available_languages', get_languages());


        $objectName = BeanFactory::getObjectName($editModule);
        VardefManager::loadVardef($editModule, $objectName);
        global $dictionary;
        $vnames = array();
        //jchi 24557 . We should list all the lables in viewdefs(list,detail,edit,quickcreate) that the user can edit them.
        require_once 'modules/ModuleBuilder/parsers/views/ListLayoutMetaDataParser.php' ;
        $parser = new ListLayoutMetaDataParser(MB_LISTVIEW, $editModule) ;
        foreach ($parser->getLayout() as $key => $def) {
            if (isset($def['label'])) {
                $vnames[$def['label']] = $def['label'];
            }
        }

        require_once 'modules/ModuleBuilder/parsers/views/GridLayoutMetaDataParser.php' ;
        $variableMap = $this->getVariableMap($editModule);
        foreach ($variableMap as $key => $value) {
            $gridLayoutMetaDataParserTemp = new GridLayoutMetaDataParser($value, $editModule) ;
            foreach ($gridLayoutMetaDataParserTemp->getLayout() as $panel) {
                foreach ($panel as $row) {
                    foreach ($row as $fieldArray) { // fieldArray is an array('name'=>name,'label'=>label)
                        if (isset($fieldArray [ 'label' ])) {
                            $vnames[$fieldArray [ 'label' ] ] = $fieldArray [ 'label' ] ;
                        }
                    }
                }
            }
        }
        //end

        //Get Subpanel Labels:
        require_once('include/SubPanel/SubPanel.php') ;
        $subList =  SubPanel::getModuleSubpanels($editModule);
        foreach ($subList as $subpanel => $titleLabel) {
            $vnames[$titleLabel] = $titleLabel;
        }

        foreach ($dictionary[$objectName]['fields'] as $name=>$def) {
            if (isset($def['vname'])) {
                $vnames[$def['vname']] = $def['vname'];
            }
        }
        $formatted_mod_strings = array();

        //we shouldn't set the $refresh=true here, or will lost template language mod_strings.
        //return_module_language($selected_lang, $editModule,false) : the mod_strings will be included from cache files here.
        foreach (return_module_language($selected_lang, $editModule, false) as $name=>$label) {
            //#25294
            if ($allLabels || isset($vnames[$name]) || preg_match('/lbl_city|lbl_country|lbl_billing_address|lbl_alt_address|lbl_shipping_address|lbl_postal_code|lbl_state$/si', (string) $name)) {
                $formatted_mod_strings[$name] = htmlentities((string) $label, ENT_QUOTES, 'UTF-8');
            }
        }
        //Grab everything from the custom files
        $mod_bak = $mod_strings;
        $files = array(
            "custom/modules/$editModule/language/$selected_lang.lang.php",
            "custom/modules/$editModule/Ext/Language/$selected_lang.lang.ext.php"
        );
        foreach ($files as $langfile) {
            $mod_strings = array();
            if (is_file($langfile)) {
                include($langfile);
                foreach ($mod_strings as $key => $label) {
                    $formatted_mod_strings[$key] = htmlentities($label, ENT_QUOTES, 'UTF-8');
                }
            }
        }
        $mod_strings = $mod_bak;
        ksort($formatted_mod_strings);
        $smarty->assign('MOD_LABELS', $formatted_mod_strings);
        $smarty->assign('view_module', $editModule);
        $smarty->assign('APP', $GLOBALS['app_strings']);
        $smarty->assign('selected_lang', $selected_lang);
        $smarty->assign('defaultHelp', 'labelsBtn');
        $smarty->assign('assistant', array('key'=>'labels', 'group'=>'module'));
        $smarty->assign('labels_choice', $mod_strings['labelTypes']);
        $smarty->assign('labels_current', $allLabels?"all":"");

        $ajax = new AjaxCompose();
        $ajax->addCrumb($mod_strings['LBL_STUDIO'], 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard")');
        $ajax->addCrumb($translatedEditModule, 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view_module='.$editModule.'")');
        $ajax->addCrumb($mod_strings['LBL_LABELS'], '');

        $html = $smarty->fetch('modules/ModuleBuilder/tpls/labels.tpl');
        $ajax->addSection('center', $GLOBALS['mod_strings']['LBL_SECTION_EDLABELS'], $html);
        echo $ajax->getJavascript();
    }

    // fixing bug #39749: Quick Create in Studio
    public function getVariableMap($module)
    {
        $variableMap = array(MB_EDITVIEW => 'EditView',
                             MB_DETAILVIEW => 'DetailView',
                             MB_QUICKCREATE => 'QuickCreate');

        $hideQuickCreateForModules = array('KBDocuments',
                                           'ProjectTask',
                                           'Campaigns',
                                           'Quotes',
                                           'ProductTemplates');

        if (in_array($module, $hideQuickCreateForModules)) {
            if (isset($variableMap['quickcreate'])) {
                unset($variableMap['quickcreate']);
            }
        }

        if ($module == 'KBDocuments') {
            $variableMap  = array();
        }

        return $variableMap;
    }
}
