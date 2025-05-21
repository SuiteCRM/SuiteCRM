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
class ViewSearchView extends ViewListView
{
    public function __construct()
    {
        parent::__construct();
        if (!empty($_REQUEST['searchlayout'])) {
            $this->editLayout = $_REQUEST['searchlayout'];
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

    // DO NOT REMOVE - overrides parent ViewEdit preDisplay() which attempts to load a bean for a non-existent module
    public function preDisplay()
    {
    }

    public function display(
        $preview = false
        ) {
        $packageName = (isset($_REQUEST [ 'view_package' ])) ? $_REQUEST [ 'view_package' ] : '' ;
        require_once 'modules/ModuleBuilder/parsers/ParserFactory.php' ;
        $parser = ParserFactory::getParser($this->editLayout, $this->editModule, $packageName) ;

        $smarty = parent::constructSmarty($parser) ;
        $smarty->assign('action', 'searchViewSave') ;
        $smarty->assign('view', $this->editLayout) ;
        $smarty->assign('helpName', 'searchViewEditor') ;
        $smarty->assign('helpDefault', 'modify') ;

        if ($preview) {
            echo $smarty->fetch("modules/ModuleBuilder/tpls/Preview/listView.tpl") ;
        } else {
            $ajax = $this->constructAjax() ;
            $ajax->addSection('center', translate($this->title), $smarty->fetch("modules/ModuleBuilder/tpls/listView.tpl")) ;
            echo $ajax->getJavascript() ;
        }
    }

    public function constructAjax()
    {
        require_once('modules/ModuleBuilder/MB/AjaxCompose.php') ;
        $ajax = new AjaxCompose() ;
        switch ($this->editLayout) {
            default:
                $searchLabel = 'LBL_' . strtoupper($this->editLayout) ;
        }

        $layoutLabel = 'LBL_LAYOUTS' ;
        $layoutView = 'layouts' ;


        if ($this->fromModuleBuilder) {
            $ajax->addCrumb(translate('LBL_MODULEBUILDER', 'ModuleBuilder'), 'ModuleBuilder.main("mb")') ;
            $ajax->addCrumb($_REQUEST [ 'view_package' ], 'ModuleBuilder.getContent("module=ModuleBuilder&action=package&package=' . $_REQUEST [ 'view_package' ] . '")') ;
            $ajax->addCrumb($this->editModule, 'ModuleBuilder.getContent("module=ModuleBuilder&action=module&view_package=' . $_REQUEST [ 'view_package' ] . "&view_module={$this->editModule}" . '")') ;
            $ajax->addCrumb(translate($layoutLabel, 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&MB=true&action=wizard&view_module=' . $this->editModule. '&view_package=' . $_REQUEST['view_package'] . '")') ;
            if ($layoutLabel === 'LBL_LAYOUTS') {
                $ajax->addCrumb(translate('LBL_SEARCH_FORMS', 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&MB=true&action=wizard&view=search&view_module=' .$this->editModule . '&view_package=' . $_REQUEST [ 'view_package' ] . '")') ;
            }
            $ajax->addCrumb(translate($searchLabel, 'ModuleBuilder'), '') ;
        } else {
            $ajax->addCrumb(translate('LBL_STUDIO', 'ModuleBuilder'), 'ModuleBuilder.main("studio")') ;
            $ajax->addCrumb($this->translatedEditModule, 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view_module=' . $this->editModule . '")') ;
            $ajax->addCrumb(translate($layoutLabel, 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view='.$layoutView.'&view_module=' . $this->editModule . '")') ;
            if ($layoutLabel === 'LBL_LAYOUTS') {
                $ajax->addCrumb(translate('LBL_SEARCH_FORMS', 'ModuleBuilder'), 'ModuleBuilder.getContent("module=ModuleBuilder&action=wizard&view=search&view_module=' .$this->editModule . '")') ;
            }
            $ajax->addCrumb(translate($searchLabel, 'ModuleBuilder'), '') ;
        }
        $this->title = $searchLabel;
        return $ajax ;
    }
}
