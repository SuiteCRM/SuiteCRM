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

define('MB_TEMPLATES', 'include/SugarObjects/templates') ;
define('MB_IMPLEMENTS', 'include/SugarObjects/implements') ;
require_once('modules/ModuleBuilder/MB/MBVardefs.php') ;
require_once('modules/ModuleBuilder/MB/MBRelationship.php') ;
require_once('modules/ModuleBuilder/MB/MBLanguage.php') ;

#[\AllowDynamicProperties]
class MBModule
{
    public $name = '' ;
    public $config = array(
    'assignable' => 1 , 'security_groups' => 1, 'acl' => 1 , 'has_tab' => 1 , 'studio' => 1 , 'audit' => 1 ) ;
    public $mbpublicdefs ;
    public $errors = array( ) ;
    public $path = '' ;
    public $implementable = array(
    'has_tab' => 'Navigation Tab' ) ;
    public $always_implement = array( 'assignable' => 'Assignable', 'security_groups' => 'Security Groups' , 'acl' => 'Access Controls' , 'studio' => 'Studio Support' , 'audit' => 'Audit Table' ) ;
    public $iTemplate = array(
    'assignable', 'security_groups' ) ;

    public $config_md5 = null ;

    public function __construct($name, $path, $package, $package_key)
    {
        global $mod_strings;
        $this->config [ 'templates' ] = array( 'basic' => 1 ) ;

        $this->name = MBModule::getDBName($name) ;
        $this->key_name = $package_key . '_' . $name ;
        $this->package = $package ;
        $this->package_key = $package_key ;
        $this->package_path = $path ;

        $this->implementable = array(
        'has_tab' => !empty($mod_strings[ 'LBL_NAV_TAB' ]) ? $mod_strings[ 'LBL_NAV_TAB' ] : false) ;
        $this->path = $this->getModuleDir() ;
        //		$this->mbrelationship = new MBRelationship($this->name, $this->path, $this->key_name);
        $this->relationships = new UndeployedRelationships($this->path) ;
        $this->mbvardefs = new MBVardefs($this->name, $this->path, $this->key_name) ;

        $this->load() ;
    }

    public function getDBName($name)
    {
        return preg_replace("/[^\w]+/", "_", (string) $name) ;
    }

    public function getModuleName()
    {
        return $this->name;
    }

    public function getPackageName()
    {
        return $this->package;
    }

    /**
     * @return UndeployedRelationships
     */
    public function getRelationships()
    {
        return $this->relationships;
    }

    /**
     * Loads the module based on the module name
     *
     */
    public function load()
    {
        if (file_exists($this->path . '/config.php')) {
            include($this->path . '/config.php') ;
            $this->config = $config ;
        }
        $label = (! empty($this->config [ 'label' ])) ? $this->config [ 'label' ] : $this->name ;
        $this->mblanguage = new MBLanguage($this->name, $this->path, $label, $this->key_name) ;
        foreach ($this->iTemplate as $temp) {
            if (! empty($this->config [ $temp ])) {
                $this->mbvardefs->iTemplates [ $temp ] = 1 ;
                $this->mblanguage->iTemplates [ $temp ] = $temp ;
            }
        }
        $this->mbvardefs->templates = $this->config [ 'templates' ] ;
        $this->mblanguage->templates = $this->config [ 'templates' ] ;
        $this->mbvardefs->load() ;
        $this->mblanguage->load() ;
    }

    public function addTemplate($template)
    {
        $this->config [ 'templates' ] [ $template ] = 1 ;
    }

    public function getModuleDir()
    {
        return $this->package_path . '/modules/' . $this->name ;
    }

    public function removeTemplate($template)
    {
        unset($this->config [ 'templates' ] [ $template ]) ;
    }

    public function getVardefs($by_group = false)
    {
        $this->mbvardefs->updateVardefs($by_group) ;
        return $this->mbvardefs->getVardefs() ;
    }

    public function addField($vardef)
    {
        $this->mbvardefs->addFieldVardef($vardef) ;
    }

    public function addFieldObject($field)
    {
        $vardef = $field->get_field_def() ;
        $this->mbvardefs->mergeVardefs();
        $existingVardefs = $this->mbvardefs->getVardefs() ;
        //Merge with the existing vardef if it already exists
        if (!empty($existingVardefs['fields'][$vardef['name']])) {
            $vardef = array_merge($existingVardefs['fields'][$vardef['name']], $vardef);
        }
        if (! empty($vardef [ 'source' ]) && $vardef [ 'source' ] == 'custom_fields') {
            unset($vardef [ 'source' ]) ;
        }

        $this->mbvardefs->load();
        $this->addField($vardef) ;
        $this->mbvardefs->save();
    }

    public function deleteField($name)
    {
        $this->mbvardefs->deleteField($name) ;
    }

    public function fieldExists($name = '', $type = '')
    {
        $vardefs = $this->getVardefs();
        if (! empty($vardefs)) {
            if (empty($type) && empty($name)) {
                return false ;
            } else {
                if (empty($type)) {
                    return ! empty($vardefs [ 'fields' ] [ $name ]) ;
                } else {
                    if (empty($name)) {
                        foreach ($vardefs [ 'fields' ] as $def) {
                            if ($def [ 'type' ] == $type) {
                                return true ;
                            }
                        }
                        return false ;
                    } else {
                        return (! empty($vardefs [ 'fields' ] [ $name ]) && ($vardefs [ 'fields' ] [ $name ] [ 'type' ] == $type)) ;
                    }
                }
            }
        } else {
            return false ;
        }
    }

    public function getModStrings($language = 'en_us')
    {
        return $this->mblanguage->getModStrings($language) ;
    }

    public function setModStrings($language, $mod_strings= array())
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }

        $language .= '.lang.php';
        $this->mblanguage->strings [$language] = $mod_strings;
    }

    public function setLabel($language, $key, $value= null)
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }

        $language .= '.lang.php' ;
        $this->mblanguage->strings [ $language ] [ $key ] = $value ;
        //Ensure this key exists in all languages
        foreach ($this->mblanguage->strings as $lang => $values) {
            if (empty($values[$key])) {
                $this->mblanguage->strings[$lang][$key] = $value;
            }
        }
    }

    public function deleteLabel($language, $key= null)
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }

        foreach ($this->mblanguage->strings as $lang => $values) {
            if (!empty($values[$key])) {
                unset($this->mblanguage->strings[$lang][$key]);
            }
        }
    }

    /**
     * Required for an MB module to work with Dynamic fields
     */
    public function addLabel($displayLabel)
    {
        $this->setLabel('en_us', $this->getDBName($displayLabel, false), translate($displayLabel));
        $this->save();
    }

    public function getLabel($language, $key= null)
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }

        $language .= '.lang.php' ;
        if (empty($this->mblanguage->strings [ $language ] [ $key ])) {
            return '' ;
        }
        return $this->mblanguage->strings [ $language ] [ $key ] ;
    }

    public function getAppListStrings($language = 'en_us')
    {
        return $this->mblanguage->getAppListStrings($language) ;
    }

    public function setAppListStrings($language, $app_list_strings= array())
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }
        $language .= '.lang.php' ;
        $this->mblanguage->appListStrings [ $language ] = $app_list_strings ;
    }

    public function setDropDown($language, $key = null, $value= null)
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }
        $language .= '.lang.php' ;
        $this->mblanguage->appListStrings [ $language ] [ $key ] = $value ;
    }

    public function deleteDropDown($language, $key= null)
    {
        // set $language = 'en_us' as default
        if (!$language) {
            $language = 'en_us';
        }

        $language .= '.lang.php' ;
        unset($this->mblanguage->appListStrings [ $language ] [ $key ]) ;
    }

    public function save()
    {
        $this->path = $this->getModuleDir() ;
        if (mkdir_recursive($this->path)) {
            $this->setConfigMD5() ;
            $old_config_md5 = $this->config_md5 ;
            $this->saveConfig() ;
            $this->getVardefs() ;
            $this->mbvardefs->save($this->key_name) ;
            //           $this->mbrelationship->save ( $this->key_name ) ;
            $this->relationships->save() ;
            $this->copyMetaData() ;
            $this->copyDashlet() ;
            $this->copyViews() ;
            if (0 != strcmp($old_config_md5, $this->config_md5)) {
                $this->mblanguage->reload() ;
            }
            $this->mblanguage->label = $this->config [ 'label' ] ;
            //pass in the key_name incase it has changed mblanguage will check if it is different and handle it accordingly
            $this->mblanguage->save($this->key_name) ;

            if (! file_exists($this->package_path . "/icons/icon_" . ucfirst($this->key_name) . ".gif")) {
                $this->createIcon() ;
            }

            $this->errors = array_merge($this->errors, $this->mbvardefs->errors) ;
        }
    }

    public function copyDashlet()
    {
        $templates = array_reverse($this->config [ 'templates' ], true) ;
        foreach ($templates as $template => $a) {
            if (file_exists(MB_TEMPLATES . '/' . $template . '/Dashlets/Dashlet')) {
                $this->copyMetaRecursive(MB_TEMPLATES . '/' . $template . '/Dashlets/Dashlet', $this->path . '/Dashlets/' . $this->key_name . 'Dashlet/') ;
            }
        }
    }

    public function copyViews()
    {
        $templates = array_reverse($this->config [ 'templates' ], true) ;
        foreach ($templates as $template => $a) {
            if (file_exists(MB_TEMPLATES . '/' . $template . '/views')) {
                $this->copyMetaRecursive(MB_TEMPLATES . '/' . $template . '/views', $this->path . '/views/') ;
            }
        }
    }

    public function copyCustomFiles($from, $to)
    {
        $d = dir($from) ;
        while ($filename = $d->read()) {
            if (substr($filename, 0, 1) == '.') {
                continue ;
            }
            if ($filename != 'metadata' && $filename != 'Dashlets' && $filename != 'relationships' && $filename != 'language' && $filename != 'config.php' && $filename != 'relationships.php' && $filename != 'vardefs.php') {
                copy_recursive("$from/$filename", "$to/$filename") ;
            }
        }
    }

    public function copyMetaData()
    {
        $templates = array_reverse($this->config [ 'templates' ], true) ;
        foreach ($templates as $template => $a) {
            if (file_exists(MB_TEMPLATES . '/' . $template . '/metadata')) {
                $this->copyMetaRecursive(MB_TEMPLATES . '/' . $template . '/metadata', $this->path . '/metadata/') ;
            }
        }
    }

    public function copyMetaRecursive($from, $to, $overwrite = false)
    {
        if (! file_exists($from)) {
            return ;
        }
        if (is_dir($from)) {
            $findArray = array( '<module_name>' , '<_module_name>' , '<MODULE_NAME>' , '<object_name>' , '<_object_name>' , '<OBJECT_NAME>' );
            $replaceArray = array( $this->key_name , strtolower($this->key_name) , strtoupper($this->key_name) ,
                                    $this->key_name , strtolower($this->key_name) , strtoupper($this->key_name) );
            mkdir_recursive($to) ;
            $d = dir($from) ;
            while ($e = $d->read()) {
                if (substr($e, 0, 1) == '.') {
                    continue ;
                }
                $nfrom = $from . '/' . $e ;
                $nto = $to . '/' . str_replace('m-n-', $this->key_name, $e) ;
                if (is_dir($nfrom)) {
                    $this->copyMetaRecursive($nfrom, $nto, $overwrite) ;
                } else {
                    if ($overwrite || ! file_exists($nto)) {
                        $contents = file_get_contents($nfrom) ;
                        $contents = str_replace($findArray, $replaceArray, $contents) ;
                        sugar_file_put_contents($nto, $contents) ;
                    }
                }
            }
        }
    }

    public function saveConfig()
    {
        $header = file_get_contents('modules/ModuleBuilder/MB/header.php') ;
        if (! write_array_to_file('config', $this->config, $this->path . '/config.php', 'w', $header)) {
            $this->errors [] = 'Could not save config to ' . $this->path . '/config.php' ;
        }
        $this->setConfigMD5() ;
    }

    public function setConfigMD5()
    {
        if (file_exists($this->path . '/config.php')) {
            $this->config_md5 = md5(base64_encode(serialize($this->config))) ;
        }
    }

    public function build($basepath)
    {
        global $app_list_strings;
        $path = $basepath . '/modules/' . $this->key_name ;
        if (mkdir_recursive($path)) {
            $this->createClasses($path) ;
            $this->createMenu($path);
            $this->copyCustomFiles($this->path, $path) ;
            $this->copyMetaRecursive($this->path . '/metadata/', $path . '/metadata/', true) ;
            $this->copyMetaRecursive(
                $this->path . '/Dashlets/' . $this->key_name . 'Dashlet/',
                $path . '/Dashlets/' . $this->key_name . 'Dashlet/',
                true
            ) ;
            $app_list_strings['moduleList'][$this->key_name] = $this->mblanguage->label;
            $this->relationships->build($basepath) ;
            $this->mblanguage->build($path) ;
        }
    }

    public function createClasses($path)
    {
        $class = array( ) ;
        $class [ 'name' ] = $this->key_name ;
        $class [ 'table_name' ] = strtolower($class [ 'name' ]) ;
        $class [ 'extends' ] = 'Basic' ;
        $class [ 'requires' ] [] = MB_TEMPLATES . '/basic/Basic.php' ;
        $class [ 'requires' ] = array( ) ;
        $class [ 'audited' ] = (! empty($this->config [ 'audit' ])) ? 'true' : 'false' ;
        $class [ 'acl' ] = ! empty($this->config [ 'acl' ]) ;
        $class [ 'templates' ] = "'basic'" ;
        foreach ($this->iTemplate as $template) {
            if (! empty($this->config [ $template ])) {
                $class [ 'templates' ] .= ",'$template'" ;
            }
        }
        foreach ($this->config [ 'templates' ] as $template => $a) {
            if ($template == 'basic') {
                continue ;
            }
            $class [ 'templates' ] .= ",'$template'" ;
            $class [ 'extends' ] = ucFirst($template) ;
            $class [ 'requires' ] [] = MB_TEMPLATES . '/' . $template . '/' . ucfirst($template) . '.php' ;
        }
        $class [ 'importable' ] = $this->config [ 'importable' ] ;
        $class [ 'inline_edit' ] = isset($this->config [ 'inline_edit' ]) ? $this->config [ 'inline_edit' ] : null;
        $this->mbvardefs->updateVardefs() ;
        $class [ 'fields' ] = $this->mbvardefs->vardefs [ 'fields' ] ;
        $class [ 'fields_string' ] = var_export_helper($this->mbvardefs->vardef [ 'fields' ]) ;
        $relationship = array( ) ;
        $class [ 'relationships' ] = var_export_helper($this->mbvardefs->vardef [ 'relationships' ]) ;
        $smarty = new Sugar_Smarty() ;
        $smarty->left_delimiter = '{{' ;
        $smarty->right_delimiter = '}}' ;
        $smarty->assign('class', $class) ;

        if (! file_exists($path . '/' . $class [ 'name' ] . '.php')) {
            sugar_file_put_contents(
                $path . '/' . $class ['name'] . '.php',
                $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/Class.tpl')
            );
        }
        //write vardefs
        sugar_file_put_contents(
            $path . '/vardefs.php',
            $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/vardef.tpl')
        );

        if (! file_exists($path . '/metadata')) {
            mkdir_recursive($path . '/metadata') ;
        }
        if (! empty($this->config [ 'studio' ])) {
            sugar_file_put_contents(
                $path . '/metadata/studio.php',
                $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/Studio.tpl')
            );
        } else {
            if (file_exists($path . '/metadata/studio.php')) {
                unlink($path . '/metadata/studio.php') ;
            }
        }
    }

    public function createMenu($path)
    {
        $smarty = new Sugar_Smarty() ;
        $smarty->assign('moduleName', $this->key_name) ;
        $smarty->assign('showvCard', array_key_exists('person', $this->config[ 'templates' ])) ;
        $smarty->assign('showimport', $this->config['importable']);
        //write sugar generated class
        sugar_file_put_contents(
            $path . '/' . 'Menu.php',
            $smarty->fetch('modules/ModuleBuilder/tpls/MBModule/Menu.tpl')
        );
    }

    public function addInstallDefs(&$installDefs)
    {
        $name = $this->key_name ;
        $installDefs [ 'copy' ] [] = array( 'from' => '<basepath>/SugarModules/modules/' . $name , 'to' => 'modules/' . $name ) ;
        $installDefs [ 'beans' ] [] = array( 'module' => $name , 'class' => $name , 'path' => 'modules/' . $name . '/' . $name . '.php' , 'tab' => $this->config [ 'has_tab' ] ) ;
        $this->relationships->addInstallDefs($installDefs) ;
    }

    public function getNodes()
    {
        $lSubs = array( ) ;
        $psubs = $this->getProvidedSubpanels() ;
        foreach ($psubs as $sub) {
            $subLabel = $sub ;
            if ($subLabel == 'default') {
                $subLabel = $GLOBALS [ 'mod_strings' ] [ 'LBL_DEFAULT' ] ;
            }
            $lSubs [] = array( 'name' => $subLabel , 'type' => 'list' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view=ListView&view_module=' . $this->name . '&view_package=' . $this->package . '&subpanel=' . $sub . '&subpanelLabel=' . $subLabel . '&local=1' ) ;
        }

        $searchSubs = array( ) ;
        $searchSubs [] = array( 'name' => translate('LBL_BASIC_SEARCH') , 'type' => 'list' , 'action' => "module=ModuleBuilder&MB=true&action=editLayout&view=basic_search&view_module={$this->name}&view_package={$this->package}"  ) ;
        $searchSubs [] = array( 'name' => translate('LBL_ADVANCED_SEARCH') , 'type' => 'list' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view=advanced_search&view_module=' . $this->name . '&view_package=' . $this->package  ) ;
        $dashlets = array( );
        $dashlets [] = array('name' => translate('LBL_DASHLETLISTVIEW') , 'type' => 'dashlet' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view=dashlet&view_module=' . $this->name . '&view_package=' . $this->package );
        $dashlets [] = array('name' => translate('LBL_DASHLETSEARCHVIEW') , 'type' => 'dashletsearch' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view=dashletsearch&view_module=' . $this->name . '&view_package=' . $this->package );

        $popups = array( );
        $popups [] = array('name' => translate('LBL_POPUPLISTVIEW') , 'type' => 'popuplistview' , 'action' => 'module=ModuleBuilder&action=editLayout&view=popuplist&view_module=' . $this->name . '&view_package=' . $this->package );
        $popups [] = array('name' => translate('LBL_POPUPSEARCH') , 'type' => 'popupsearch' , 'action' => 'module=ModuleBuilder&action=editLayout&view=popupsearch&view_module=' . $this->name . '&view_package=' . $this->package );

        $layouts = array(
            array( 'name' => translate('LBL_EDITVIEW') , 'type' => 'edit' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view='.MB_EDITVIEW.'&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_DETAILVIEW') , 'type' => 'detail' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view='.MB_DETAILVIEW.'&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_LISTVIEW') , 'type' => 'list' , 'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view='.MB_LISTVIEW.'&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_QUICKCREATE') , 'type' => MB_QUICKCREATE,  'action' => 'module=ModuleBuilder&MB=true&action=editLayout&view='.MB_QUICKCREATE.'&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_DASHLET') , 'type' => 'Folder', 'children' => $dashlets, 'action' => 'module=ModuleBuilder&MB=true&action=wizard&view=dashlet&view_module=' . $this->name . '&view_package=' . $this->package  ),
            array( 'name' => translate('LBL_POPUP') , 'type' => 'Folder', 'children' => $popups, 'action' => 'module=ModuleBuilder&MB=true&action=wizard&view=popup&view_module=' . $this->name . '&view_package=' . $this->package  ),
            array( 'name' => translate('LBL_SEARCH_FORMS') , 'action' => 'module=ModuleBuilder&MB=true&action=wizard&view=search&view_module=' . $this->name . '&view_package=' . $this->package , 'type' => 'folder' , 'children' => $searchSubs )
            ) ;

        $children = array(
            array( 'name' => translate('LBL_FIELDS') , 'action' => 'module=ModuleBuilder&action=modulefields&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_LABELS') , 'action' => 'module=ModuleBuilder&action=modulelabels&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_RELATIONSHIPS') , 'action' => 'module=ModuleBuilder&action=relationships&view_module=' . $this->name . '&view_package=' . $this->package ) ,
            array( 'name' => translate('LBL_LAYOUTS') , 'type' => 'Folder' , 'action' => "module=ModuleBuilder&action=wizard&view_module={$this->name}&view_package={$this->package}&MB=1" , 'children' => $layouts ) ,
            ) ;

        if (count($lSubs) > 0) {
            $children [] = array( 'name' => translate('LBL_AVAILABLE_SUBPANELS') , 'type' => 'folder' , 'children' => $lSubs ) ;
        }

        $nodes = array( 'name' => $this->name , 'children' => $children , 'action' => 'module=ModuleBuilder&action=module&view_module=' . $this->name . '&view_package=' . $this->package ) ;

        return $nodes ;
    }


    public function getProvidedSubpanels()
    {
        $this->providedSubpanels = array() ;

        $subpanelDir = $this->getModuleDir() . '/metadata/subpanels/' ;
        if (file_exists($subpanelDir)) {
            $f = dir($subpanelDir) ;
            require_once 'modules/ModuleBuilder/parsers/relationships/AbstractRelationships.php' ;

            while ($g = $f->read()) {
                // sanity check to confirm that this is a usable subpanel...
                if (substr($g, 0, 1) != '.' && AbstractRelationships::validSubpanel($subpanelDir . $g)) {
                    $subname = str_replace('.php', '', $g) ;
                    $this->providedSubpanels [ $subname ] = $subname ;
                }
            }
        }

        return $this->providedSubpanels;
    }

    public static function getTypes()
    {
        $types = array( ) ;
        $d = dir(MB_TEMPLATES) ;
        while ($e = $d->read()) {
            if (substr($e, 0, 1) != '.') {
                $types [ $e ] = $e ;
            }
        }

        return $types ;
    }

    public function rename($new_name)
    {
        $old = $this->getModuleDir() ;
        $old_name = $this->key_name;
        $this->name = $new_name ;
        $this->key_name = $this->package_key . '_' . $this->name ;
        $new = $this->getModuleDir() ;
        if (file_exists($new)) {
            return false ;
        }
        $renamed = rename($old, $new) ;
        if ($renamed) {
            $this->renameMetaData($new, $old_name) ;
            $this->renameLanguageFiles($new) ;
        }
        return $renamed ;
    }

    public function renameLanguageFiles($new_dir, $duplicate = false)
    {
        $this->mblanguage->name = $this->name ;
        $this->mblanguage->path = $new_dir ;
        $this->mblanguage->generateAppStrings() ;
        $this->mblanguage->save($this->key_name, $duplicate, true) ;
    }

    /**
     * Rename module name in metadata
     * @param string $new_dir
     * @param string $old_name
     */
    public function renameMetaData($new_dir, $old_name)
    {
        $GLOBALS [ 'log' ]->debug('MBModule.php->renameMetaData: new_dir=' . $new_dir) ;
        if (! file_exists($new_dir)) {
            return ;
        }
        $dir = dir($new_dir) ;
        while ($e = $dir->read()) {
            if (substr($e, 0, 1) != '.') {
                if (is_dir($new_dir . '/' . $e)) {
                    $this->renameMetaData($new_dir . '/' . $e, $old_name) ;
                }
                if (is_file($new_dir . '/' . $e)) {
                    $contents = file_get_contents($new_dir . '/' . $e) ;
                    $search_array = array(
                        '/(\$module_name[ ]*=[ ]*\').*(\'[ ]*;)/',
                        '/(\$_module_name[ ]*=[ ]*\').*(\'[ ]*;)/',
                        '/(\$MODULE_NAME[ ]*=[ ]*\').*(\'[ ]*;)/',
                        '/(\$object_name[ ]*=[ ]*\').*(\'[ ]*;)/',
                        '/(\$_object_name[ ]*=[ ]*\').*(\'[ ]*;)/',
                        '/(\$OBJECT_NAME[ ]*=[ ]*\').*(\'[ ]*;)/'
                    );
                    $replace_array = array(
                        '$1' . $this->key_name . '$2',
                        '$1' . strtolower($this->key_name) . '$2',
                        '$1' . strtoupper($this->key_name) . '$2',
                        '$1' . $this->key_name . '$2',
                        '$1' . strtolower($this->key_name) . '$2',
                        '$1' . strtoupper($this->key_name) . '$2',
                    );
                    $contents = preg_replace($search_array, $replace_array, $contents);
                    $search_array = array(
                        "{$old_name}_",
                        "{$old_name}Dashlet"
                    );
                    $replace_array = array(
                        $this->key_name . '_',
                        $this->key_name . 'Dashlet'
                    );
                    $contents = str_replace($search_array, $replace_array, $contents);


                    if ("relationships.php" == $e) {
                        //bug 39598 Relationship Name Is Not Updated If Module Name Is Changed In Module Builder
                        $contents = str_replace("'{$old_name}'", "'{$this->key_name}'", $contents) ;
                    }

                    sugar_file_put_contents($new_dir . '/' . $e, $contents) ;
                }
            }
        }
    }

    public function copy($new_name)
    {
        $old = $this->getModuleDir() ;

        $count = 0 ;
        $old_name = $this->key_name;
        $this->name = $new_name ;
        $this->key_name = $this->package_key . '_' . $this->name ;
        $new = $this->getModuleDir() ;
        while (file_exists($new)) {
            $count ++ ;
            $this->name = $new_name . $count ;
            $this->key_name = $this->package_key . '_' . $this->name ;
            $new = $this->getModuleDir() ;
        }

        $new = $this->getModuleDir() ;
        $copied = copy_recursive($old, $new) ;

        if ($copied) {
            $this->renameMetaData($new, $old_name) ;
            $this->renameLanguageFiles($new, true) ;
        }
        return $copied ;
    }

    public function delete()
    {
        return rmdir_recursive($this->getModuleDir()) ;
    }

    public function populateFromPost()
    {
        foreach ($this->implementable as $key => $value) {
            $this->config [ $key ] = ! empty($_REQUEST [ $key ]) ;
        }
        foreach ($this->always_implement as $key => $value) {
            $this->config [ $key ] = true ;
        }
        if (! empty($_REQUEST [ 'type' ])) {
            $this->addTemplate($_REQUEST [ 'type' ]) ;
        }

        if (! empty($_REQUEST [ 'label' ])) {
            $this->config [ 'label' ] = $_REQUEST [ 'label' ] ;
        }

        $this->config [ 'importable' ] = ! empty($_REQUEST[ 'importable' ]) ;
    }

    public function getAvailibleSubpanelDef($panelName)
    {
        $filepath = $this->getModuleDir() . "/metadata/subpanels/{$panelName}.php" ;
        if (file_exists($filepath)) {
            $subpanel_layout = [];
            include($filepath) ;
            return $subpanel_layout ;
        }
        return array( ) ;
    }

    public function saveAvailibleSubpanelDef($panelName, $layout)
    {
        $dir = $this->getModuleDir() . "/metadata/subpanels" ;
        $filepath = "$dir/{$panelName}.php" ;
        if (mkdir_recursive($dir)) {
            // preserve any $module_name entry if one exists
            if (file_exists($filepath)) {
                include($filepath) ;
            }
            $module_name = (isset($module_name)) ? $module_name : $this->key_name ;
            $layout = "<?php\n" . '$module_name=\'' . $module_name . "';\n" . '$subpanel_layout = ' . var_export_helper($layout) . ";" ;
            $GLOBALS [ 'log' ]->debug("About to save this file to $filepath") ;
            $GLOBALS [ 'log' ]->debug($layout) ;
            sugar_file_put_contents($filepath, $layout) ;
        }
    }

    public function getLocalSubpanelDef($panelName)
    {
    }

    public function createIcon()
    {
        $icondir = $this->package_path . "/icons";
        mkdir_recursive($icondir);
        mkdir_recursive($icondir . "/sub_panel/modules");
        mkdir_recursive($icondir . "/sidebar/modules");

        $template = "";
        foreach ($this->config ['templates'] as $temp => $val) {
            $template = $temp;
        }

        // GIF Version
        copy("include/SugarObjects/templates/$template/icons/$template.gif", "$icondir/icon_" . ucfirst($this->key_name) . ".gif");
        copy("include/SugarObjects/templates/$template/icons/$template.gif", "$icondir/" . $this->key_name . ".gif");
        // SVG Version
        if (file_exists("include/SugarObjects/templates/$template/icons/$template.svg")) {
            copy("include/SugarObjects/templates/$template/icons/$template.svg", "$icondir/" . $this->key_name . ".svg");
        }
        // GIF Version
        if (file_exists("include/SugarObjects/templates/$template/icons/Create$template.gif")) {
            copy("include/SugarObjects/templates/$template/icons/Create$template.gif", "$icondir/Create" . $this->key_name . ".gif");
        }
        // SVG Version
        if (file_exists("include/SugarObjects/templates/$template/icons/Create$template.svg")) {
            copy("include/SugarObjects/templates/$template/icons/Create$template.svg", "$icondir/Create" . $this->key_name . ".svg");
        }
        // GIF Version
        if (file_exists("include/SugarObjects/templates/$template/icons/{$template}_32.gif")) {
            copy("include/SugarObjects/templates/$template/icons/{$template}_32.gif", "$icondir/icon_" . $this->key_name . "_32.gif");
        }
        // SVG Version
        if (file_exists("include/SugarObjects/templates/$template/icons/{$template}_32.svg")) {
            copy("include/SugarObjects/templates/$template/icons/{$template}_32.svg", "$icondir/icon_" . $this->key_name . "_32.svg");
        }

        // SuiteP Support
        if (file_exists("include/SugarObjects/templates/$template/icons/sidebar/modules/{$template}.svg")) {
            copy("include/SugarObjects/templates/$template/icons/sidebar/modules/{$template}.svg", "$icondir/sidebar/modules/" . $this->key_name . ".svg");
        }
        if (file_exists("include/SugarObjects/templates/$template/icons/sub_panel/{$template}.svg")) {
            copy("include/SugarObjects/templates/$template/icons/sub_panel/{$template}.svg", "$icondir/sub_panel/" . $this->key_name . ".svg");
        }
        if (file_exists("include/SugarObjects/templates/$template/icons/sub_panel/modules/{$template}.svg")) {
            copy("include/SugarObjects/templates/$template/icons/sub_panel/modules/{$template}.svg", "$icondir/sub_panel/modules/" . $this->key_name . ".svg");
        }
    }

    public function removeFieldFromLayouts($fieldName)
    {
        // hardcoded list of types for now, as also hardcoded in a different form in getNodes
        // TODO: replace by similar mechanism to StudioModule to determine the list of available views for this module
        $views = array( 'editview' , 'detailview' , 'listview' , 'basic_search' , 'advanced_search' , 'dashlet' , 'popuplist');

        foreach ($views as $type) {
            $parser = ParserFactory::getParser($type, $this->name, $this->package) ;
            if ($parser->removeField($fieldName)) {
                $parser->handleSave(false) ;
            } // don't populate from $_REQUEST, just save as is...
        }
        //Remove the fields in subpanel
        $psubs = $this->getProvidedSubpanels() ;
        foreach ($psubs as $sub) {
            $parser = ParserFactory::getParser(MB_LISTVIEW, $this->name, $this->package, $sub) ;
            if ($parser->removeField($fieldName)) {
                $parser->handleSave(false) ;
            }
        }
    }

    /**
     * Returns an array of fields defs with all the link fields for this module.
     * @return array
     */
    public function getLinkFields()
    {
        $list = $this->relationships->getRelationshipList();
        $field_defs = array();
        foreach ($list as $name) {
            $rel = $this->relationships->get($name);
            $relFields = $rel->buildVardefs();
            $relDef = $rel->getDefinition();
            $relLabels = $rel->getLabels();
            $relatedModule = $this->key_name == $relDef['rhs_module'] ? $relDef['lhs_module'] : $relDef['rhs_module'];
            if (!empty($relFields[$this->key_name])) {
                //Massage the result of getVardefs to look like field_defs
                foreach ($relFields[$this->key_name] as $def) {
                    $def['module'] = $relatedModule;
                    $def['translated_label'] = empty($relLabels[$this->key_name][$def['vname']]) ?
                        $name : $relLabels[$this->key_name][$def['vname']];
                    $field_defs[$def['name']] = $def;
                }
            }
        }

        return $field_defs;
    }

    /**
     * Returns a TemplateField object by name
     * Returns a TemplateField object by name or null if field not exists. If type not set use text type as default
     *
     * @param string $name
     * @return TemplateField|null
     *
     */
    public function getField($name)
    {
        $field = null;
        $varDefs = $this->getVardefs();
        if (isset($varDefs['fields'][$name])) {
            $fieldVarDefs = $varDefs['fields'][$name];
            if (!isset($fieldVarDefs['type'])) {
                $fieldVarDefs['type'] = 'varchar';
            }
            $field = get_widget($fieldVarDefs['type']);
            foreach ($fieldVarDefs as $key => $opt) {
                $field->$key = $opt;
            }
        }
        return $field;
    }
}
