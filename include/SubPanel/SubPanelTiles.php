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

require_once('include/SubPanel/SubPanel.php');
require_once('include/SubPanel/SubPanelTilesTabs.php');
require_once('include/SubPanel/SubPanelDefinitions.php');

/**
 * Subpanel tiles
 * @api
 */
#[\AllowDynamicProperties]
class SubPanelTiles
{
    public $id;
    public $module;
    public $focus;
    public $start_on_field;
    public $layout_manager;
    public $layout_def_key;
    public $show_tabs = false;

    /**
     * @var \SuiteCRM\SubPanel\SubPanelRowCounter
     */
    protected $rowCounter;

    public $subpanel_definitions;

    public $hidden_tabs=array(); //consumer of this class can array of tabs that should be hidden. the tab name
    //should be the array.

    public function __construct(&$focus, $layout_def_key='', $layout_def_override = '')
    {
        $this->focus = $focus;
        $this->id = $focus->id;
        $this->module = $focus->module_dir;
        $this->layout_def_key = $layout_def_key;
        $this->subpanel_definitions=new SubPanelDefinitions($focus, $layout_def_key, $layout_def_override);
        $this->rowCounter = new \SuiteCRM\SubPanel\SubPanelRowCounter($focus);
    }

    /*
     * Return the current selected or requested subpanel tab
     * @return	string	The identifier for the selected subpanel tab (e.g., 'Other')
     */
    public function getSelectedGroup()
    {
        global $current_user;

        if (isset($_REQUEST['subpanelTabs'])) {
            $_SESSION['subpanelTabs'] = $_REQUEST['subpanelTabs'];
        }

        // include/tabConfig.php in turn includes the customized file at custom/include/tabConfig.php
        require 'include/tabConfig.php';

        $subpanelTabsPref = $current_user->getPreference('subpanel_tabs');
        if (!isset($subpanelTabsPref)) {
            $subpanelTabsPref = $GLOBALS['sugar_config']['default_subpanel_tabs'];
        }
        if (!empty($GLOBALS['tabStructure']) && (!empty($_SESSION['subpanelTabs']) || !empty($sugar_config['subpanelTabs']) || !empty($subpanelTabsPref))) {
            // Determine selected group
            if (!empty($_REQUEST['subpanel'])) {
                $selected_group = $_REQUEST['subpanel'];
            } elseif (!empty($_COOKIE[$this->module.'_sp_tab'])) {
                $selected_group = $_COOKIE[$this->module.'_sp_tab'];
            } elseif (!empty($_SESSION['parentTab']) && !empty($GLOBALS['tabStructure'][$_SESSION['parentTab']]) && in_array($this->module, $GLOBALS['tabStructure'][$_SESSION['parentTab']]['modules'])) {
                $selected_group = $_SESSION['parentTab'];
            } else {
                $selected_group = '';
                foreach ($GLOBALS['tabStructure'] as $mainTab => $group) {
                    if (in_array($this->module, $group['modules'])) {
                        $selected_group = $mainTab;
                        break;
                    }
                }
                if (!$selected_group) {
                    $selected_group = 'All';
                }
            }
        } else {
            $selected_group = '';
        }
        return $selected_group;
    }

    /*
     * Determine which subpanels should be shown within the selected tab group (e.g., 'Other');
     * @param boolean $showTabs		True if we should call the code to render each visible tab
     * @param string $selectedGroup	The requested tab group
     * @return array Visible tabs
     */
    public function getTabs($showTabs = true, $selectedGroup='')
    {
        global $current_user;

        //get all the "tabs" - this actually means all the subpanels available for display within a tab
        $tabs = $this->subpanel_definitions->get_available_tabs();

        if (!empty($selectedGroup)) {
            // Bug #44344 : Custom relationships under same module only show once in subpanel tabs
            // use object property instead new object to have ability run unit test (can override subpanel_definitions)
            $objSubPanelTilesTabs = new SubPanelTilesTabs($this->focus);
            $tabs = $objSubPanelTilesTabs->getTabs($tabs, $showTabs, $selectedGroup);
            unset($objSubPanelTilesTabs);
            return $tabs;
        } else {
            // see if user current user has custom subpanel layout
            $objSubPanelTilesTabs = new SubPanelTilesTabs($this->focus);
            $tabs = $objSubPanelTilesTabs->applyUserCustomLayoutToTabs($tabs);

            /* Check if the preference is set now,
             * because there's no point in executing this code if
             * we aren't going to render anything.
             */
            $subpanelLinksPref = $current_user->getPreference('subpanel_links');
            if (!isset($subpanelLinksPref)) {
                $subpanelLinksPref = $GLOBALS['sugar_config']['default_subpanel_links'];
            }

            if ($showTabs && $subpanelLinksPref) {
                require_once('include/SubPanel/SugarTab.php');
                $sugarTab = new SugarTab();

                $displayTabs = array();

                foreach ($tabs as $tab) {
                    $displayTabs []= array('key'=>$tab, 'label'=>translate($this->subpanel_definitions->layout_defs['subpanel_setup'][$tab]['title_key']));
                }
                $sugarTab->setup(array(), array(), $displayTabs);
                $sugarTab->display();
            }
        }
        return $tabs;
    }
    public function display($showContainer = true, $forceTabless = false)
    {
        global $layout_edit_mode, $sugar_version, $sugar_config, $current_user, $app_strings, $modListHeader;

        if (isset($layout_edit_mode) && $layout_edit_mode) {
            return;
        }

        $template = new Sugar_Smarty();
        $template_header = "";
        $template_body = "";
        $template_footer = "";

        $tabs = array();
        $tabs_properties = array();
        $tab_names = array();

        $module_sub_panels = [];

        $default_div_display = 'inline';
        if (!empty($sugar_config['hide_subpanels_on_login'])) {
            if (!isset($_SESSION['visited_details'][$this->focus->module_dir])) {
                SugarApplication::setCookie($this->focus->module_dir . '_divs', '', 0, null, null, isSSL(), true);
                unset($_COOKIE[$this->focus->module_dir . '_divs']);
                $_SESSION['visited_details'][$this->focus->module_dir] = true;
            }
            $default_div_display = 'none';
        }
        $div_cookies = get_sub_cookies($this->focus->module_dir . '_divs');


        if (empty($_REQUEST['subpanels'])) {
            $selected_group = $forceTabless?'':$this->getSelectedGroup();
            $usersLayout = $current_user->getPreference('subpanelLayout', $this->focus->module_dir);

            // we need to use some intelligence here when restoring the user's layout, as new modules with new subpanels might have been installed since the user's layout was recorded
            // this means that we can't just restore the old layout verbatim as the new subpanels would then go walkabout
            // so we need to do a merge while attempting as best we can to preserve the sense of the specified order
            // this is complicated by the different ordering schemes used in the two sources for the panels: the user's layout uses an ordinal layout, the panels from getTabs have an explicit ordering driven by the 'order' parameter
            // it's not clear how to best reconcile these two schemes; so we punt on it, and add all new panels to the end of the user's layout. At least this will give them a clue that something has changed...
            // we also now check for tabs that have been removed since the user saved their preferences.

            $tabs = $this->getTabs($showContainer, $selected_group) ;

            if (!empty($usersLayout)) {
                $availableTabs = $tabs ;
                $tabs = array_intersect($usersLayout, $availableTabs) ; // remove any tabs that have been removed since the user's layout was saved
                foreach (array_diff($availableTabs, $usersLayout) as $tab) {
                    $tabs [] = $tab;
                }
            }
        } else {
            $tabs = explode(',', $_REQUEST['subpanels']);
        }

        // Display the group header. this section is executed only if the tabbed interface is being used.
        $current_key = '';
        if (! empty($this->show_tabs)) {
            require_once('include/tabs.php');
            $tab_panel = new SugarWidgetTabs($tabs, $current_key, 'showSubPanel');
            $template_header .= get_form_header('Related', '', false);
            $template_header .= $tab_panel->display();
        }

        if (empty($GLOBALS['relationships'])) {
            if (!class_exists('Relationship')) {
                require('modules/Relationships/Relationship.php');
            }
            $rel= BeanFactory::newBean('Relationships');
            $rel->load_relationship_meta();
        }

        foreach ($tabs as $t => $tab) {
            // load meta definition of the sub-panel.
            $thisPanel = $this->subpanel_definitions->load_subpanel($tab);
            if ($thisPanel === false) {
                continue;
            }
            // this if-block will try to skip over ophaned subpanels. Studio/MB are being delete unloaded modules completely.
            // this check will ignore subpanels that are collections (activities, history, etc)
            if (!isset($thisPanel->_instance_properties['collection_list']) and isset($thisPanel->_instance_properties['get_subpanel_data'])) {
                // ignore when data source is a function

                if (!isset($this->focus->field_defs[$thisPanel->_instance_properties['get_subpanel_data']])) {
                    if (stripos($thisPanel->_instance_properties['get_subpanel_data'], 'function:') === false) {
                        $GLOBALS['log']->fatal("Bad subpanel definition, it has incorrect value for get_subpanel_data property " .$tab);
                        continue;
                    }
                } else {
                    $rel_name='';
                    if (isset($this->focus->field_defs[$thisPanel->_instance_properties['get_subpanel_data']]['relationship'])) {
                        $rel_name=$this->focus->field_defs[$thisPanel->_instance_properties['get_subpanel_data']]['relationship'];
                    }

                    if (empty($rel_name) or !isset($GLOBALS['relationships'][$rel_name])) {
                        $GLOBALS['log']->fatal("Missing relationship definition " .$rel_name. ". skipping " .$tab ." subpanel");
                        continue;
                    }
                }
            }

            if ($thisPanel->isCollection()) {
                // collect names of sub-panels that may contain items of each module
                $collection_list = $thisPanel->get_inst_prop_value('collection_list');
                if (is_array($collection_list)) {
                    foreach ($collection_list as $data) {
                        if (!empty($data['module'])) {
                            $module_sub_panels[$data['module']][$tab] = true;
                        }
                    }
                }
            } else {
                $module = $thisPanel->get_module_name();
                if (!empty($module)) {
                    $module_sub_panels[$module][$tab] = true;
                }
            }

            $div_display = $default_div_display;

            if (isset($thisPanel->_instance_properties['collapsed'])
                && $thisPanel->_instance_properties['collapsed']) {
                $div_display = 'none';
            }

            if (!empty($sugar_config['hide_subpanels']) || $thisPanel->isDefaultHidden()) {
                $div_display = 'none';
            }

            $cookie_name = $this->module . '_' . $tab . '_v';
            if (isset($div_cookies[$cookie_name])) {
                $div_display = $div_cookies[$cookie_name] === 'false' ? 'none' : '';
            }

            if ($div_display == 'none') {
                $opp_display = 'inline';
                $tabs_properties[$t]['expanded_subpanels'] = false;
            } else {
                $opp_display = 'none';
                $tabs_properties[$t]['expanded_subpanels'] = true;
            }

            if (!empty($this->layout_def_key)) {
                $layout_def_key = $this->layout_def_key;
            } else {
                $layout_def_key = '';
            }

            if (empty($this->show_tabs)) {
                ///
                /// Legacy Support for subpanels
                $show_icon_html = SugarThemeRegistry::current()->getImage('advanced_search', 'border="0" align="absmiddle"', null, null, '.gif', translate('LBL_SHOW'));
                $hide_icon_html = SugarThemeRegistry::current()->getImage('basic_search', 'border="0" align="absmiddle"', null, null, '.gif', translate('LBL_HIDE'));

                $tabs_properties[$t]['show_icon_html'] = $show_icon_html;
                $tabs_properties[$t]['hide_icon_html'] = $hide_icon_html;

                $max_min = "<a name=\"$tab\"> </a><span id=\"show_link_".$tab."\" style=\"display: $opp_display\"><a href='#' class='utilsLink' onclick=\"current_child_field = '".$tab."';showSubPanel('".$tab."',null,true,'".$layout_def_key."');document.getElementById('show_link_".$tab."').style.display='none';document.getElementById('hide_link_".$tab."').style.display='';return false;\">"
                    . "" . $show_icon_html . "</a></span>";
                $max_min .= "<span id=\"hide_link_".$tab."\" style=\"display: $div_display\"><a href='#' class='utilsLink' onclick=\"hideSubPanel('".$tab."');document.getElementById('hide_link_".$tab."').style.display='none';document.getElementById('show_link_".$tab."').style.display='';return false;\">"
                    . "" . $hide_icon_html . "</a></span>";
                $tabs_properties[$t]['title'] = $thisPanel->get_title();
                $tabs_properties[$t]['module_name'] = $thisPanel->get_module_name();
                $tabs_properties[$t]['get_form_header']  = get_form_header($thisPanel->get_title(), $max_min, false, false);
            }

            $tabs_properties[$t]['cookie_name'] = $cookie_name;
            $tabs_properties[$t]['div_display'] = $div_display;
            $tabs_properties[$t]['opp_display'] = $opp_display;

            $tabs_properties[$t]['subpanel_body'] = '';
            $tabs_properties[$t]['buttons'] = '';

            // We only preload this subpanel's contents if it's expanded
            if ($tabs_properties[$t]['expanded_subpanels']) {
                // Get Subpanel
                include_once('include/SubPanel/SubPanel.php');
                $subpanel_object = new SubPanel($this->module, $_REQUEST['record'], $tab, $thisPanel, $layout_def_key);

                $arr = array();
                // TODO: Remove x-template:
                $tabs_properties[$t]['subpanel_body'] = $subpanel_object->ProcessSubPanelListView(
                    'include/SubPanel/tpls/SubPanelDynamic.tpl',
                    $arr
                );

                // Get subpanel buttons
                $tabs_properties[$t]['buttons'] = $this->get_buttons($thisPanel, $subpanel_object->subpanel_query);
            } elseif ($current_user->getPreference('count_collapsed_subpanels')) {
                $subPanelDef = $this->subpanel_definitions->layout_defs['subpanel_setup'][$tab];
                $count = (int)$this->rowCounter->getSubPanelRowCount($subPanelDef);

                $extraClass = '';
                if ($count === 0) {
                    $countStr = $count.'';
                } elseif ($count > 0) {
                    $countStr = $count.'';
                    $tabs_properties[$t]['collapsed_override'] = 1;
                } else {
                    $countStr = '...';
                    $extraClass = ' incomplete';
                }

                $tabs_properties[$t]['title'] .= ' (<span class="subPanelCountHint' . $extraClass . '" data-subpanel="' . $tab . '" data-module="' . $layout_def_key . '" data-record="' . $_REQUEST['record'] . '">' . $countStr . '</span>)';
            }


            array_push($tab_names, $tab);
        }

        $tab_names = '["' . implode('","', $tab_names) . '"]';

        if (!isset($module_sub_panels)) {
            $module_sub_panels = [];
        }
        $module_sub_panels = array_map('array_keys', $module_sub_panels);
        $module_sub_panels = json_encode($module_sub_panels);

        $template->assign('layout_def_key', $this->layout_def_key);
        $template->assign('show_subpanel_tabs', $this->show_tabs);
        $template->assign('subpanel_tabs', $tabs);
        $template->assign('subpanel_tabs_properties', $tabs_properties);
        $template->assign('module_sub_panels', $module_sub_panels);
        $template->assign('sugar_config', $sugar_config);
        $template->assign('REQUEST', $_REQUEST);
        $template->assign('GLOBALS', $GLOBALS);
        $template->assign('selected_group', $selected_group);
        $template->assign('tab_names', $tab_names);
        $template->assign('module_sub_panels', $module_sub_panels);
        $template->assign('module', $this->module);
        $template->assign('APP', $app_strings);

        $template_body = $template->fetch('include/SubPanel/tpls/SubPanelTiles.tpl');

        return $template_header . $template_body . $template_footer;
    }

    public function getLayoutManager()
    {
        require_once('include/generic/LayoutManager.php');
        if ($this->layout_manager == null) {
            $this->layout_manager = new LayoutManager();
        }
        return $this->layout_manager;
    }

    /**
     * @param aSubPanel $thisPanel
     * @param $panel_query
     * @return string
     */
    public function get_buttons($thisPanel, $panel_query=null)
    {
        $subpanel_def = $thisPanel->get_buttons();
        $layout_manager = $this->getLayoutManager();

        //for action button at the top of each subpanel
        // bug#51275: smarty widget to help provide the action menu functionality as it is currently sprinkled throughout the app with html
        $buttons = array();
        $widget_contents = '';
        foreach ($subpanel_def as $widget_data) {
            $widget_data['action'] = $_REQUEST['action'];
            $widget_data['module'] =  $thisPanel->get_inst_prop_value('module');
            $widget_data['focus'] = $this->focus;
            $widget_data['subpanel_definition'] = $thisPanel;
            $widget_contents .= '<td class="buttons">' . "\n";

            if (empty($widget_data['widget_class'])) {
                $buttons[] = "widget_class not defined for top subpanel buttons";
            } else {
                $button = $layout_manager->widgetDisplay($widget_data);
                if ($button) {
                    $buttons[] = $button;
                }
            }
        }
        require_once('include/Smarty/plugins/function.sugar_action_menu.php');
        $widget_contents = smarty_function_sugar_action_menu(
            [
                'buttons' => $buttons,
                'class' => 'clickMenu fancymenu',
                'flat' => $thisPanel->get_inst_prop_value('flat')
            ],
            $this->xTemplate
        );
        return $widget_contents;
    }

    /**
     * @param string $html_text
     * @param aSubPanel $thisPanel
     * @return string
     */
    public function getCheckbox($html_text, $thisPanel)
    {
        $template = new Sugar_Smarty();

        if ($thisPanel->get_inst_prop_value('select_link_top')) {
            $html_text .= $template->fetch('include/SubPanel/tpls/SubPanelCheckbox.tpl');
        }

        return $html_text;
    }
}
