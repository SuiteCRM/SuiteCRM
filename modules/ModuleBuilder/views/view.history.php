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


require_once('modules/ModuleBuilder/MB/AjaxCompose.php') ;
require_once('modules/ModuleBuilder/parsers/views/History.php') ;
require_once('modules/ModuleBuilder/parsers/ParserFactory.php') ;

#[\AllowDynamicProperties]
class ViewHistory extends SugarView
{
    public $pageSize = 10 ;

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
        $this->layout = strtolower($_REQUEST [ 'view' ]) ;
        
        $subpanelName = null ;
        if ((strtolower($this->layout) == 'listview') && (!empty($_REQUEST [ 'subpanel' ]))) {
            $subpanelName = $_REQUEST [ 'subpanel' ] ;
        }
        
        $packageName = (isset($_REQUEST [ 'view_package' ]) && (strtolower($_REQUEST [ 'view_package' ]) != 'studio')) ? $_REQUEST [ 'view_package' ] : null ;
        $this->module = $_REQUEST [ 'view_module' ] ;
        
        $this->parser = ParserFactory::getParser($this->layout, $this->module, $packageName, $subpanelName) ;
        $this->history = $this->parser->getHistory() ;
        $action = ! empty($_REQUEST [ 'histAction' ]) ? $_REQUEST [ 'histAction' ] : 'browse' ;
        $GLOBALS['log']->debug(get_class($this)."->display(): performing History action {$action}") ;
        $this->$action() ;
    }

    public function browse()
    {
        $smarty = new Sugar_Smarty() ;
        global $mod_strings ;
        $smarty->assign('mod_strings', $mod_strings) ;
        $smarty->assign('view_module', $this->module) ;
        $smarty->assign('view', $this->layout) ;
        
        if (! empty($_REQUEST [ 'subpanel' ])) {
            $smarty->assign('subpanel', $_REQUEST [ 'subpanel' ]) ;
        }
        $stamps = array( ) ;
        global $timedate ;
        $userFormat = $timedate->get_date_time_format() ;
        $page = ! empty($_REQUEST [ 'page' ]) ? $_REQUEST [ 'page' ] : 0 ;
        $count = $this->history->getCount();
        $ts = $this->history->getNth($page * $this->pageSize) ;
        $snapshots = array( ) ;
        for ($i = 0 ; $i <= $this->pageSize && $ts > 0 ; $i ++) {
            $dbDate = $timedate->fromTimestamp($ts)->asDb();
            $displayTS = $timedate->to_display_date_time($dbDate) ;
            if ($page * $this->pageSize + $i + 1 == $count) {
                $displayTS = translate("LBL_MB_DEFAULT_LAYOUT");
            }
            $snapshots [ $ts ] = $displayTS ;
            $ts = $this->history->getNext() ;
        }
        if (count($snapshots) > $this->pageSize) {
            $smarty->assign('nextPage', true) ;
        }
        $snapshots = array_slice($snapshots, 0, $this->pageSize, true) ;
        $smarty->assign('currentPage', $page) ;
        $smarty->assign('snapshots', $snapshots) ;
        
        $html = $smarty->fetch('modules/ModuleBuilder/tpls/history.tpl') ;
        echo $html ;
    }

    public function preview()
    {
        $view = null;
        global $mod_strings ;
        if (! isset($_REQUEST [ 'sid' ])) {
            die('SID Required') ;
        }
        $sid = $_REQUEST [ 'sid' ] ;
        $subpanel = '';
        if (! empty($_REQUEST [ 'subpanel' ])) {
            $subpanel = ',"' . $_REQUEST [ 'subpanel' ] . '"' ;
        }
        echo "<input type='button' name='close$sid' value='". translate('LBL_BTN_CLOSE')."' " .
                "class='button' onclick='ModuleBuilder.tabPanel.removeTab(ModuleBuilder.tabPanel.get(\"activeTab\"));' style='margin:5px;'>" .
             "<input type='button' name='restore$sid' value='" . translate('LBL_MB_RESTORE') . "' " .
                "class='button' onclick='ModuleBuilder.history.revert(\"$this->module\",\"{$this->layout}\",\"$sid\"$subpanel);' style='margin:5px;'>" ;
        $this->history->restoreByTimestamp($sid) ;
        $view ;
        if ($this->layout == 'listview') {
            require_once("modules/ModuleBuilder/views/view.listview.php") ;
            $view = new ViewListView() ;
        } else {
            if ($this->layout == 'basic_search' || $this->layout == 'advanced_search') {
                require_once("modules/ModuleBuilder/views/view.searchview.php") ;
                $view = new ViewSearchView() ;
            } else {
                if ($this->layout == 'dashlet' || $this->layout == 'dashletsearch') {
                    require_once("modules/ModuleBuilder/views/view.dashlet.php") ;
                    $view = new ViewDashlet() ;
                } else {
                    if ($this->layout == 'popuplist' || $this->layout == 'popupsearch') {
                        require_once("modules/ModuleBuilder/views/view.popupview.php") ;
                        $view = new ViewPopupview() ;
                    } else {
                        require_once("modules/ModuleBuilder/views/view.layoutview.php") ;
                        $view = new ViewLayoutView() ;
                    }
                }
            }
        }
        
        $view->display(true) ;
        $this->history->undoRestore() ;
    }

    public function restore()
    {
        if (! isset($_REQUEST [ 'sid' ])) {
            die('SID Required') ;
        }
        $sid = $_REQUEST [ 'sid' ] ;
        $this->history->restoreByTimestamp($sid) ;
    }

    /**
     * Restores a layout to its current customized state.
     * Called when leaving a restored layout without saving.
     */
    public function unrestore()
    {
        $this->history->undoRestore() ;
    }
}
