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

require_once('include/ListView/ListViewSubPanel.php');
require_once('include/SubPanel/registered_layout_defs.php');
/**
 * Subpanel
 * @api
 */
#[\AllowDynamicProperties]
class SubPanel
{
    public $hideNewButton = false;
    public $subpanel_id;
    public $parent_record_id;
    public $parent_module;  // the name of the parent module
    public $parent_bean;  // the instantiated bean of the parent
    public $template_file;
    public $linked_fields;
    public $action = 'DetailView';
    public $show_select_button = true;
    public $subpanel_define = null;  // contains the layout_def.php
    public $subpanel_defs;
    public $subpanel_query=null;
    public $layout_def_key='';
    public $search_query='';
    public $collections = array();

    public function __construct($module, $record_id, $subpanel_id, $subpanelDef, $layout_def_key='', $collections = array())
    {
        global $beanList, $beanFiles, $focus, $app_strings;


        $this->subpanel_defs=$subpanelDef;
        $this->subpanel_id = $subpanel_id;
        $this->parent_record_id = $record_id;
        $this->parent_module = $module;
        $this->layout_def_key = $layout_def_key;
        $this->collections = $collections;

        $this->parent_bean = $focus;
        $result = $focus;

        if (empty($result)) {
            $parent_bean_name = $beanList[$module];
            $parent_bean_file = $beanFiles[$parent_bean_name];
            require_once($parent_bean_file);
            $this->parent_bean = new $parent_bean_name();
            $this->parent_bean->retrieve($this->parent_record_id);
            $result = $this->parent_bean;
        }

        if ($record_id!='fab4' && $result == null) {
            sugar_die($app_strings['ERROR_NO_RECORD']);
        }

        if (empty($subpanelDef)) {
            //load the subpanel by name.
            require_once 'include/SubPanel/SubPanelDefinitions.php' ;
            $panelsdef=new SubPanelDefinitions($result, $layout_def_key);
            $subpanelDef=$panelsdef->load_subpanel($subpanel_id, false, false, $this->search_query, $collections);
            $this->subpanel_defs=$subpanelDef;
        }

        $this->buildSearchQuery();
    }

    public function setTemplateFile($template_file)
    {
        $this->template_file = $template_file;
    }

    public function setBeanList(&$value)
    {
        $this->bean_list =$value;
    }

    public function setHideNewButton($value)
    {
        $this->hideNewButton = $value;
    }


    public function getHeaderText($currentModule)
    {
    }

    public function get_buttons($panel_query=null)
    {
        $thisPanel =& $this->subpanel_defs;
        $subpanel_def = $thisPanel->get_buttons();

        if (!isset($this->listview)) {
            $this->listview = new ListViewSubPanel();
        }
        $layout_manager = $this->listview->getLayoutManager();
        $widget_contents = '<div><table cellpadding="0" cellspacing="0"><tr>';
        foreach ($subpanel_def as $widget_data) {
            $widget_data['action'] = $_REQUEST['action'];
            $widget_data['module'] =  $thisPanel->get_inst_prop_value('module');
            $widget_data['focus'] = $this->parent_bean;
            $widget_data['subpanel_definition'] = $thisPanel;
            $widget_contents .= '<td style="padding-right: 2px; padding-bottom: 2px;">' . "\n";

            if (empty($widget_data['widget_class'])) {
                $widget_contents .= "widget_class not defined for top subpanel buttons";
            } else {
                $widget_contents .= $layout_manager->widgetDisplay($widget_data);
            }

            $widget_contents .= '</td>';
        }

        $widget_contents .= '</tr></table></div>';
        return $widget_contents;
    }


    public function ProcessSubPanelListView($xTemplatePath, &$mod_strings, $countOnly = false)
    {
        global $app_strings;
        global $current_user;
        global $sugar_config;
        global $app_strings;

        //		if(isset($this->listview)){
        //			$ListView =& $this->listview;
        //		}else{
        //			$this->listview = new ListViewSubPanel();
        //		}
        $this->listview = new ListViewSubPanel();
        $ListView =& $this->listview;
        $ListView->initNewSmartyTemplate($xTemplatePath, $this->subpanel_defs->mod_strings);
        $ListView->smartyTemplateAssign("RETURN_URL", "&return_module=".$this->parent_module."&return_action=DetailView&return_id=".$this->parent_bean->id);
        $ListView->smartyTemplateAssign("RELATED_MODULE", $this->parent_module);  // TODO: what about unions?
        $ListView->smartyTemplateAssign("RECORD_ID", $this->parent_bean->id);
        $ListView->smartyTemplateAssign("EDIT_INLINE_PNG", SugarThemeRegistry::current()->getImage('edit_inline', 'align="absmiddle"  border="0"', null, null, '.gif', $app_strings['LNK_EDIT']));
        $ListView->smartyTemplateAssign("DELETE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LBL_DELETE_INLINE']));
        $ListView->smartyTemplateAssign("REMOVE_INLINE_PNG", SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LBL_ID_FF_REMOVE']));
        $ListView->smartyTemplateAssign("APP", $app_strings);
        $header_text= '';

        $ListView->smartyTemplateAssign("SUBPANEL_ID", $this->subpanel_id);
        $ListView->smartyTemplateAssign("SUBPANEL_SEARCH", $this->getSearchForm());
        $display_sps = '';
        if ($this->search_query == '' && empty($this->collections)) {
            $display_sps = 'display:none';
        }
        $ListView->smartyTemplateAssign("DISPLAY_SPS", $display_sps);

        if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
            $exploded = explode('/', $xTemplatePath);
            $file_name = $exploded[count($exploded) - 1];
            $mod_name =  $exploded[count($exploded) - 2];
            $header_text= "&nbsp;<a href='index.php?action=index&module=DynamicLayout&from_action=$file_name&from_module=$mod_name&mod_lang="
                .$_REQUEST['module']."'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', 'Edit Layout')."</a>";
        }
        $ListView->setHeaderTitle('');
        $ListView->setHeaderText('');

        ob_start();

        $ListView->is_dynamic = true;
        $ListView->records_per_page = $sugar_config['list_max_entries_per_subpanel'] + 0;
        if (isset($this->subpanel_defs->_instance_properties['records_per_page'])) {
            $ListView->records_per_page = $this->subpanel_defs->_instance_properties['records_per_page'] + 0;
        }
        $ListView->start_link_wrapper = "javascript:showSubPanel('".$this->subpanel_id."','";
        $ListView->subpanel_id = $this->subpanel_id;
        $ListView->end_link_wrapper = "',true);";
        if (!empty($this->layout_def_key)) {
            $ListView->end_link_wrapper = '&layout_def_key='.$this->layout_def_key.$ListView->end_link_wrapper;
        }

        $where = '';
        $ListView->setQuery($where, '', '', '');
        $ListView->show_export_button = false;

        //function returns the query that was used to populate sub-panel data.

        $query=$ListView->process_dynamic_listview($this->parent_module, $this->parent_bean, $this->subpanel_defs, $countOnly);


        $this->subpanel_query=$query;
        $ob_contents = ob_get_contents();
        ob_end_clean();
        if ($countOnly) {
            return $query;
        }
        return $ob_contents;
    }

    public function display($countOnly = false)
    {
        $result_array = array();

        $return_string = $this->ProcessSubPanelListView($this->template_file, $result_array, $countOnly);

        if ($countOnly) {
            print $return_string['row_count'];
        } else {
            print $return_string;
        }
    }

    public function getModulesWithSubpanels()
    {
        global $beanList;
        $dir = dir('modules');
        $modules = array();
        while ($entry = $dir->read()) {
            if (file_exists('modules/' . $entry . '/layout_defs.php')) {
                $modules[$entry] = $entry;
            }
        }
        return $modules;
    }

    public static function getModuleSubpanels($module)
    {
        require_once('include/SubPanel/SubPanelDefinitions.php');
        global $beanList, $beanFiles;
        if (!isset($beanList[$module])) {
            return array();
        }

        $class = $beanList[$module];
        require_once($beanFiles[$class]);
        $mod = new $class();
        $spd = new SubPanelDefinitions($mod);
        $tabs = $spd->get_available_tabs(true);
        $ret_tabs = array();
        $reject_tabs = array('history'=>1, 'activities'=>1);
        foreach ($tabs as $key=>$tab) {
            foreach ($tab as $k=>$v) {
                if (! isset($reject_tabs [$k])) {
                    $ret_tabs [$k] = $v;
                }
            }
        }

        return $ret_tabs;
    }

    //saves overrides for defs
    public function saveSubPanelDefOverride($panel, $subsection, $override)
    {
        global $layout_defs, $beanList;

        //save the new subpanel
        $name = "subpanel_layout['list_fields']";

        //bugfix: load looks for moduleName/metadata/subpanels, not moduleName/subpanels
        $path = 'custom/modules/'. $panel->_instance_properties['module'] . '/metadata/subpanels';

        //bug# 40171: "Custom subpanels not working as expected"
        //each custom subpanel needs to have a unique custom def file
        $filename = $panel->parent_bean->object_name . "_subpanel_" . $panel->name; //bug 42262 (filename with $panel->_instance_properties['get_subpanel_data'] can create problem if had word "function" in it)
        $oldName1 = '_override' . $panel->parent_bean->object_name .$panel->_instance_properties['module'] . $panel->_instance_properties['subpanel_name'] ;
        $oldName2 = '_override' . $panel->parent_bean->object_name .$panel->_instance_properties['get_subpanel_data'] ;
        if (file_exists('custom/Extension/modules/'. $panel->parent_bean->module_dir . "/Ext/Layoutdefs/$oldName1.php")) {
            unlink('custom/Extension/modules/'. $panel->parent_bean->module_dir . "/Ext/Layoutdefs/$oldName1.php");
        }
        if (file_exists('custom/Extension/modules/'. $panel->parent_bean->module_dir . "/Ext/Layoutdefs/$oldName2.php")) {
            unlink('custom/Extension/modules/'. $panel->parent_bean->module_dir . "/Ext/Layoutdefs/$oldName2.php");
        }
        $extname = '_override'.$filename;
        //end of bug# 40171

        mkdir_recursive($path, true);
        write_array_to_file($name, $override, $path.'/' . $filename .'.php');

        //save the override for the layoutdef
        //tyoung 10.12.07 pushed panel->name to lowercase to match case in subpaneldefs.php files -
        //gave error on bad index 'module' as this override key didn't match the key in the subpaneldefs
        $name = "layout_defs['".  $panel->parent_bean->module_dir. "']['subpanel_setup']['" .strtolower($panel->name). "']";
        //  	$GLOBALS['log']->debug('SubPanel.php->saveSubPanelDefOverride(): '.$name);
        $newValue = override_value_to_string($name, 'override_subpanel_name', $filename);
        mkdir_recursive('custom/Extension/modules/'. $panel->parent_bean->module_dir . '/Ext/Layoutdefs', true);
        sugar_file_put_contents(
            'custom/Extension/modules/'. $panel->parent_bean->module_dir . "/Ext/Layoutdefs/$extname.php",
            "<?php\n//auto-generated file DO NOT EDIT\n$newValue\n?>"
        );
        require_once('ModuleInstall/ModuleInstaller.php');
        $moduleInstaller = new ModuleInstaller();
        $moduleInstaller->silent = true; // make sure that the ModuleInstaller->log() function doesn't echo while rebuilding the layoutdefs
        $moduleInstaller->rebuild_layoutdefs();
        if (file_exists('modules/'.  $panel->parent_bean->module_dir . '/layout_defs.php')) {
            include('modules/'.  $panel->parent_bean->module_dir . '/layout_defs.php');
        }
        if (file_exists('custom/modules/'.  $panel->parent_bean->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php')) {
            include('custom/modules/'.  $panel->parent_bean->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php');
        }
    }

    public function get_subpanel_setup($module)
    {
        $subpanel_setup = '';
        $layout_defs = get_layout_defs();

        if (!empty($layout_defs) && !empty($layout_defs[$module]['subpanel_setup'])) {
            $subpanel_setup = $layout_defs[$module]['subpanel_setup'];
        }

        return $subpanel_setup;
    }

    /**
     * Retrieve the subpanel definition from the registered layout_defs arrays.
     */
    public function getSubPanelDefine($module, $subpanel_id)
    {
        $default_subpanel_define = SubPanel::_get_default_subpanel_define($module, $subpanel_id);
        $custom_subpanel_define = SubPanel::_get_custom_subpanel_define($module, $subpanel_id);

        $subpanel_define = array_merge($default_subpanel_define, $custom_subpanel_define);

        if (empty($subpanel_define)) {
            print('Could not load subpanel definition for: ' . $subpanel_id);
        }

        return $subpanel_define;
    }

    public function _get_custom_subpanel_define($module, $subpanel_id)
    {
        $ret_val = array();

        if ($subpanel_id != '') {
            $layout_defs = get_layout_defs();

            if (!empty($layout_defs[$module]['custom_subpanel_defines'][$subpanel_id])) {
                $ret_val = $layout_defs[$module]['custom_subpanel_defines'][$subpanel_id];
            }
        }

        return $ret_val;
    }

    public function _get_default_subpanel_define($module, $subpanel_id)
    {
        $ret_val = array();

        if ($subpanel_id != '') {
            $layout_defs = get_layout_defs();

            if (!empty($layout_defs[$subpanel_id]['default_subpanel_define'])) {
                $ret_val = $layout_defs[$subpanel_id]['default_subpanel_define'];
            }
        }

        return $ret_val;
    }

    public function buildSearchQuery()
    {
        $thisPanel =& $this->subpanel_defs;
        $subpanel_defs = $thisPanel->_instance_properties;

        require_once('include/SubPanel/SubPanelSearchForm.php');

        if (isset($subpanel_defs['type']) && $subpanel_defs['type'] == 'collection') {
            $arrayValues = array_values($subpanel_defs['collection_list']);
            $collection = array_shift($arrayValues);
            $module = $collection['module'];
        } else {
            $module = $subpanel_defs['module'];
        }
        if ($module) {
            $seed = BeanFactory::getBean($module);
        } else {
            $seed = BeanFactory::newBean('Meetings');
        }

        $_REQUEST['searchFormTab'] = 'basic_search';
        $searchForm = new SubPanelSearchForm($seed, $module, $this);

        $searchMetaData = $searchForm->retrieveSearchDefs($module);
        $searchForm->setup($searchMetaData['searchdefs'], $searchMetaData['searchFields'], 'SubpanelSearchFormGeneric.tpl', 'basic_search');

        $searchForm->populateFromRequest();

        $where_clauses = $searchForm->generateSearchWhere(true, $seed->module_dir);

        if (count($where_clauses) > 0) {
            $this->search_query = '('. implode(' ) AND ( ', $where_clauses) . ')';
        }
        $GLOBALS['log']->info("Subpanel Where Clause: $this->search_query");
    }

    public function get_searchdefs($module)
    {
        $thisPanel =& $this->subpanel_defs;
        $subpanel_defs = $thisPanel->_instance_properties;

        if (isset($subpanel_defs['searchdefs'])) {
            $searchdefs[$module]['layout']['basic_search'] = $subpanel_defs['searchdefs'];
            $searchdefs[$module]['templateMeta'] = array('maxColumns' => 3, 'maxColumnsBasic' => 4, 'widths' => array( 'label' => 10, 'field' => 30 )) ;
            return $searchdefs;
        }

        return false;
    }

    public function getSearchForm()
    {
        $thisPanel =& $this->subpanel_defs;
        $subpanel_defs = $thisPanel->_instance_properties;
        require_once('include/SubPanel/SubPanelSearchForm.php');

        if (isset($subpanel_defs['type']) && $subpanel_defs['type'] == 'collection') {
            $arrayValues = array_values($subpanel_defs['collection_list']);
            $collection = array_shift($arrayValues);
            $module = $collection['module'];
        } else {
            $module = $subpanel_defs['module'];
        }
        $seed = BeanFactory::getBean($module);

        $searchForm = new SubPanelSearchForm($seed, $module, $this);

        $searchMetaData = $searchForm->retrieveSearchDefs($module);

        if ($subpanel_searchMetaData = $this->get_searchdefs($module)) {
            $searchForm->setup($subpanel_searchMetaData, $searchMetaData['searchFields'], 'SubpanelSearchFormGeneric.tpl', 'basic_search');

            if (!empty($this->collections)) {
                $searchForm->searchFields['collection'] = array();
            }

            $searchForm->populateFromRequest();

            return $searchForm->display();
        }

        return '';
    }
}
