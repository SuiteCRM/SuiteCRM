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



require_once('modules/ModuleBuilder/views/view.listview.php') ;
require_once 'modules/ModuleBuilder/parsers/constants.php' ;

#[\AllowDynamicProperties]
class ViewPopupview extends ViewListView
{
    public function __construct()
    {
        $this->editModule = $_REQUEST [ 'view_module' ] ;
        $this->editLayout = $_REQUEST [ 'view' ] ;
        $this->editPackage = (isset($_REQUEST [ 'view_package' ]) && ! empty($_REQUEST [ 'view_package' ])) ? $_REQUEST [ 'view_package' ] : null ;

        $this->fromModuleBuilder = isset($_REQUEST [ 'MB' ]) || (isset($_REQUEST['view_package']) && $_REQUEST['view_package'] && $_REQUEST['view_package'] != 'studio') ;
        if (!$this->fromModuleBuilder) {
            global $app_list_strings ;
            $moduleNames = array_change_key_case($app_list_strings [ 'moduleList' ]) ;
            $this->translatedEditModule = $moduleNames [ strtolower($this->editModule) ] ;
        }
    }

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

//    /*
//     * Pseudo-constructor to enable subclasses to call a parent's constructor without knowing the parent in PHP4
//     */
//    function init()
//    {
//
//    }

    public function preDisplay()
    {
    }

    public function display(
        $preview = false
        ) {
        require_once 'modules/ModuleBuilder/parsers/ParserFactory.php' ;
        $parser = ParserFactory::getParser($this->editLayout, $this->editModule, $this->editPackage) ;

        $smarty = $this->constructSmarty($parser) ;

        if (isset($this->view_object_map['new_parser'])) {
            $smarty = $this->constructSmarty($this->view_object_map['new_parser']);
        }

        if ($preview) {
            echo $smarty->fetch("modules/ModuleBuilder/tpls/Preview/listView.tpl") ;
        } else {
            $ajax = $this->constructAjax() ;
            $ajax->addSection('center', translate('LBL_POPUP'), $smarty->fetch("modules/ModuleBuilder/tpls/listView.tpl")) ;
            echo $ajax->getJavascript() ;
        }
    }

    public function constructAjax()
    {
        require_once('modules/ModuleBuilder/MB/AjaxCompose.php') ;
        $ajax = new AjaxCompose() ;

        if ($this->fromModuleBuilder) {
            $ajax->addCrumb(translate('LBL_MODULEBUILDER', 'ModuleBuilder'), 'ModuleBuilder.main("mb")') ;
            $ajax->addCrumb($_REQUEST [ 'view_package' ], 'ModuleBuilder.getContent("module=ModuleBuilder&action=package&package=' . $this->editPackage . '")') ;
            $ajax->addCrumb($this->editModule, 'ModuleBuilder.getContent("module=ModuleBuilder&action=module&view_package=' . $this->editPackage . '&view_module=' . $this->editModule . '")') ;
            $ajax->addCrumb(translate('LBL_LAYOUTS', 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&layouts=1&MB=1&view_package='.$this->editPackage.'&view_module=' . $this->editModule . '")');
            $ajax->addCrumb(translate('LBL_POPUP', 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view=popup&MB=1&view_package='.$this->editPackage.'&view_module=' . $this->editModule . '")');

            $ViewLabel = ($this->editLayout == MB_POPUPLIST) ? 'LBL_POPUPLISTVIEW' : 'LBL_POPUPSEARCH';
            $ajax->addCrumb(translate($ViewLabel, 'ModuleBuilder'), '') ;
        } else {
            $ajax->addCrumb(translate($this->editModule), 'ModuleBuilder.getContent("module=ModuleBuilder&action=module&view_module=' . $this->editModule . '")') ;
            $ajax->addCrumb(translate('LBL_LAYOUTS', 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&layouts=1&view_module=' . $this->editModule . '")');
            $ajax->addCrumb(translate('LBL_POPUP', 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view=popup&view_module=' . $this->editModule . '")');
            $ViewLabel = ($this->editLayout == MB_POPUPLIST) ? 'LBL_POPUPLISTVIEW' : 'LBL_POPUPSEARCH';
            $ajax->addCrumb(translate($ViewLabel, 'ModuleBuilder'), '') ;
        }
        return $ajax ;
    }

    public function constructSmarty(
        $parser
        ) {
        $smarty = new Sugar_Smarty() ;
        $smarty->assign('translate', true) ;
        $smarty->assign('language', $parser->getLanguage()) ;

        $smarty->assign('view', $this->editLayout) ;
        $smarty->assign('action', 'popupSave') ;
        $smarty->assign('module', 'ModuleBuilder');
        $smarty->assign('view_module', $this->editModule) ;
        $smarty->assign('field_defs', $parser->getFieldDefs()) ;
        $helpName = (isset($_REQUEST['view']) && $_REQUEST['view']==MB_POPUPSEARCH) ? 'searchViewEditor' : 'popupListViewEditor';
        $smarty->assign('helpName', $helpName) ;
        $smarty->assign('helpDefault', 'modify') ;
        if ($this->fromModuleBuilder) {
            $mb = new ModuleBuilder() ;
            $module = & $mb->getPackageModule($this->editPackage, $this->editModule) ;
            $smarty->assign('current_mod_strings', $module->getModStrings());
        }

        $smarty->assign('title', $this->_constructTitle()) ;
        $groups = array( ) ;
        foreach ($parser->columns as $column => $function) {
            $groups [ $GLOBALS [ 'mod_strings' ] [ $column ] ] = $parser->$function() ;
        }
        foreach ($groups as $groupKey => $group) {
            foreach ($group as $fieldKey => $field) {
                if (isset($field [ 'width' ])) {
                    if (substr((string) $field [ 'width' ], - 1, 1) == '%') {
                        $groups [ $groupKey ] [ $fieldKey ] [ 'width' ] = substr((string) $field [ 'width' ], 0, strlen((string) $field [ 'width' ]) - 1) ;
                    }
                }
            }
        }

        $smarty->assign('groups', $groups) ;

        global $image_path, $mod_strings;
        $imageSave = SugarThemeRegistry::current()->getImage('studio_save', '', null, null, '.gif', $mod_strings['LBL_BTN_SAVE']) ;


        $histaction = "ModuleBuilder.history.browse(\"{$this->editModule}\", \"{$this->editLayout}\")" ;
        if (isset($this->searchlayout)) {
            $histaction = "ModuleBuilder.history.browse(\"{$this->editModule}\", \"{$this->editLayout}\", \"{$this->searchlayout}\")" ;
        }

        $buttons = array( ) ;
        if (! $this->fromModuleBuilder) {
            $buttons [] = array( 'name' => 'savebtn' , 'image' => $imageSave , 'text' => $GLOBALS [ 'mod_strings' ] [ 'LBL_BTN_SAVEPUBLISH' ] , 'actionScript' => "onclick='studiotabs.generateGroupForm(\"edittabs\");ModuleBuilder.state.isDirty=false;ModuleBuilder.submitForm(\"edittabs\" )'" ) ;
        } else {
            $buttons [] = array( 'name' => 'mbsavebtn' , 'image' => $imageSave , 'text' => $GLOBALS [ 'mod_strings' ] [ 'LBL_BTN_SAVE' ] , 'actionScript' => "onclick='studiotabs.generateGroupForm(\"edittabs\");ModuleBuilder.state.isDirty=false;ModuleBuilder.submitForm(\"edittabs\" )'" ) ;
        }
        $buttons [] = array( 'name' => 'historyBtn' , 'text' => translate('LBL_HISTORY') , 'actionScript' => "onclick='$histaction'" ) ;
        $smarty->assign('buttons', $this->_buildImageButtons($buttons)) ;
        $editImage = SugarThemeRegistry::current()->getImage('edit_inline', '', null, null, '.gif', $mod_strings['LBL_EDIT']) ;

        $smarty->assign('editImage', $editImage) ;
        $deleteImage = SugarThemeRegistry::current()->getImage('delete_inline', '', null, null, '.gif', $mod_strings['LBL_MB_DELETE']) ;

        $smarty->assign('deleteImage', $deleteImage) ;
        $smarty->assign('MOD', $GLOBALS [ 'mod_strings' ]) ;

        if ($this->fromModuleBuilder) {
            $smarty->assign('MB', true) ;
            $smarty->assign('view_package', $this->editPackage) ;
            $smarty->assign('description', $GLOBALS [ 'mod_strings' ] [ 'LBL_LISTVIEW_DESCRIPTION' ]) ;
        } else {
            $smarty->assign('description', $GLOBALS [ 'mod_strings' ] [ 'LBL_LISTVIEW_DESCRIPTION' ]) ;
        }

        return $smarty ;
    }

    public function _constructTitle()
    {
        global $app_list_strings ;

        if ($this->fromModuleBuilder) {
            $title = $this->editModule ;
        } else {
            $title =  $app_list_strings [ 'moduleList' ] [ $this->editModule ] ;
        }
        return $GLOBALS [ 'mod_strings' ] [ 'LBL_LISTVIEW_EDIT' ] . ':&nbsp;' . $title ;
    }
}
