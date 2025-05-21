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

require_once('modules/ModuleBuilder/MB/AjaxCompose.php');
require_once('modules/ModuleBuilder/views/view.modulefield.php');
 
#[\AllowDynamicProperties]
class ViewModulefields extends SugarView
{
    public $mbModule;
    
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

    public function display()
    {
        $titleLBL = [];
        $smarty = new Sugar_Smarty();
        global $mod_strings;
        $bak_mod_strings=$mod_strings;
        $smarty->assign('mod_strings', $mod_strings);

        $module_name = $_REQUEST['view_module'];

        global $current_language;
        $module_strings = return_module_language($current_language, $module_name);

        $fieldsData = array();
        $customFieldsData = array();

        //use fieldTypes variable to map field type to displayed field type
        $fieldTypes = $mod_strings['fieldTypes'];
        //add datetimecombo type field from the vardef overrides to point to Datetime type
        $fieldTypes['datetime'] = $fieldTypes['datetimecombo'];

        if (!isset($_REQUEST['view_package']) || $_REQUEST['view_package'] == 'studio') {
            //$this->loadPackageHelp($module_name);
            $studioClass = new stdClass;
            $studioClass->name = $module_name;

            $objectName = BeanFactory::getObjectName($module_name);

            VardefManager::loadVardef($module_name, $objectName, true);
            global $dictionary;
            $f = array($mod_strings['LBL_HCUSTOM']=>array(), $mod_strings['LBL_HDEFAULT']=>array());

            foreach ($dictionary[$objectName]['fields'] as $def) {
                if ($this->isValidStudioField($def)) {
                    $def['label'] = translate($def['vname'], $module_name);
                    //Custom relate fields will have a non-db source, but custom_module set
                    if (isset($def['source']) && $def['source'] == 'custom_fields' || isset($def['custom_module'])) {
                        $f[$mod_strings['LBL_HCUSTOM']][$def['name']] = $def;
                        $def['custom'] = true;
                    } else {
                        $f[$mod_strings['LBL_HDEFAULT']][$def['name']] = $def;
                        $def['custom'] = false;
                    }

                    $def['type'] = isset($fieldTypes[$def['type']]) ? $fieldTypes[$def['type']] : ucfirst($def['type']);
                    $fieldsData[] = $def;
                    $customFieldsData[$def['name']] = $def['custom'];
                }
            }
            $smarty->assign('module', $studioClass);

            $package = new stdClass;
            $package->name = '';
            $smarty->assign('package', $package);
            global $current_user;
            $sortPreferences = $current_user->getPreference('fieldsTableColumn', 'ModuleBuilder');
            $smarty->assign('sortPreferences', $sortPreferences);
            $smarty->assign('fieldsData', getJSONobj()->encode($fieldsData));
            $smarty->assign('customFieldsData', getJSONobj()->encode($customFieldsData));
            $smarty->assign('studio', true);
            $ajax = new AjaxCompose();
            $ajax->addCrumb($mod_strings['LBL_STUDIO'], 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard")');
            $ajax->addCrumb(translate($module_name), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view_module='.$module_name.'")');
            $ajax->addCrumb($mod_strings['LBL_FIELDS'], '');
            $ajax->addSection('center', $mod_strings['LBL_EDIT_FIELDS'], $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/fields.tpl'));
            $_REQUEST['field'] = '';

            echo $ajax->getJavascript();
        } else {
            require_once('modules/ModuleBuilder/MB/ModuleBuilder.php');
            $mb = new ModuleBuilder();
            $mb->getPackage($_REQUEST['view_package']);
            $package = $mb->packages[$_REQUEST['view_package']];

            $package->getModule($module_name);
            $this->mbModule = $package->modules[$module_name];
            $this->loadPackageHelp($module_name);
            $this->mbModule->getVardefs(true);
            $this->mbModule->mbvardefs->vardefs['fields'] = array_reverse($this->mbModule->mbvardefs->vardefs['fields'], true);
            $loadedFields = array();

            if (file_exists($this->mbModule->path. '/language/'.$current_language.'.lang.php')) {
                include($this->mbModule->path .'/language/'.$current_language.'.lang.php');
                $this->mbModule->setModStrings($current_language, $mod_strings);
            } elseif (file_exists($this->mbModule->path. '/language/en_us.lang.php')) {
                include($this->mbModule->path .'/language/en_us.lang.php');
                $this->mbModule->setModStrings('en_us', $mod_strings);
            }

            foreach ($this->mbModule->mbvardefs->vardefs['fields'] as $k=>$v) {
                if ($k != $this->mbModule->name) {
                    foreach ($v as $field => $def) {
                        if (array_key_exists($field, $this->mbModule->mbvardefs->vardefs['fields'][$this->mbModule->name])) {
                            $this->mbModule->mbvardefs->vardefs['fields'][$k][$field] = $this->mbModule->mbvardefs->vardefs['fields'][$this->mbModule->name][$field];

                            unset($this->mbModule->mbvardefs->vardefs['fields'][$this->mbModule->name][$field]);
                        }
                    }
                }
            }

            foreach ($this->mbModule->mbvardefs->vardefs['fields'] as $k=>$v) {
                if ($k != $module_name) {
                    $titleLBL[$k]=translate("LBL_".strtoupper($k), 'ModuleBuilder');
                } else {
                    $titleLBL[$k]=$k;
                }
                foreach ($v as $field => $def) {
                    /**
                     * https://github.com/salesagility/SuiteCRM/issues/879
                     *
                     * Added check for to see if field is a valid studio field
                     */
                    if ($this->isValidStudioField($def)) {
                        if (isset($loadedFields[$field])) {
                            unset($this->mbModule->mbvardefs->vardefs['fields'][$k][$field]);
                        } else {
                            $this->mbModule->mbvardefs->vardefs['fields'][$k][$field]['label'] = isset($def['vname']) && isset($this->mbModule->mblanguage->strings[$current_language.'.lang.php'][$def['vname']]) ? $this->mbModule->mblanguage->strings[$current_language.'.lang.php'][$def['vname']] : $field;
                            $customFieldsData[$field] = ($k == $this->mbModule->name) ? true : false;
                            $loadedFields[$field] = true;

                            $type = $this->mbModule->mbvardefs->vardefs['fields'][$k][$field]['type'];
                            $this->mbModule->mbvardefs->vardefs['fields'][$k][$field]['type'] = isset($fieldTypes[$type]) ? $fieldTypes[$type] : ucfirst($type);
                            $fieldsData[] = $this->mbModule->mbvardefs->vardefs['fields'][$k][$field];
                        }
                    }
                }
            }

            $this->mbModule->mbvardefs->vardefs['fields'][$module_name] = $this->cullFields($this->mbModule->mbvardefs->vardefs['fields'][$module_name]);

            $smarty->assign('fieldsData', getJSONobj()->encode($fieldsData));
            $smarty->assign('customFieldsData', getJSONobj()->encode($customFieldsData));
            global $current_user;
            $sortPreferences = $current_user->getPreference('fieldsTableColumn', 'ModuleBuilder');
            $smarty->assign('sortPreferences', $sortPreferences);
            $smarty->assign('title', $titleLBL);
            $smarty->assign('package', $package);
            $smarty->assign('module', $this->mbModule);
            $smarty->assign('editLabelsMb', '1');
            $smarty->assign('studio', false);

            $ajax = new AjaxCompose();
            $ajax->addCrumb($bak_mod_strings['LBL_MODULEBUILDER'], 'ModuleBuilder.main("mb")');
            $ajax->addCrumb($package->name, 'ModuleBuilder.getContent("module=ModuleBuilder&action=package&package='.$package->name.'")');
            $ajax->addCrumb($module_name, 'ModuleBuilder.getContent("module=ModuleBuilder&action=module&view_package='.$package->name.'&view_module='. $module_name . '")');
            $ajax->addCrumb($bak_mod_strings['LBL_FIELDS'], '');
            $ajax->addSection('center', $bak_mod_strings["LBL_FIELDS"], $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/fields.tpl'));
            $_REQUEST['field'] = '';

            echo $ajax->getJavascript();
        }
    }

    public function loadPackageHelp(
        $name
        ) {
        $this->mbModule->help['default'] = (empty($name))?'create':'modify';
        $this->mbModule->help['group'] = 'module';
        $this->mbModule->help['group'] = 'module';
    }

    public function cullFields(
        $def
        ) {
        if (!empty($def['parent_id'])) {
            unset($def['parent_id']);
        }
        if (!empty($def['parent_type'])) {
            unset($def['parent_type']);
        }
        if (!empty($def['currency_id'])) {
            unset($def['currency_id']);
        }
        return $def;
    }
    
    public function isValidStudioField(
        $def
        ) {
        if (isset($def['studio'])) {
            if (is_array($def [ 'studio' ])) {
                if (isset($def['studio']['editField']) && $def['studio']['editField'] == true) {
                    return true;
                }
                if (isset($def['studio']['required']) && $def['studio']['required']) {
                    return true;
                }
            } else {
                if ($def['studio'] == 'visible') {
                    return true;
                }
                if ($def['studio'] == 'hidden' || $def['studio'] == 'false' || !$def['studio']) {
                    return false;
                }
            }
        }
        if (empty($def ['source']) || $def ['source'] == 'db' || $def ['source'] == 'custom_fields') {
            if ($def ['type'] != 'id' && (empty($def ['dbType']) || $def ['dbType'] != 'id')) {
                return true;
            }
        }
        
        return false;
    }
}
