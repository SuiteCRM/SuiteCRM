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



//input
//	module directory
//constructor
//	open the layout_definitions file.
//
/**
 * Subpanel implementation
 * @api
 */
class aSubPanel
{
    public $name ;
    public $_instance_properties ;

    public $mod_strings ;
    public $panel_definition ;
    public $sub_subpanels ;
    public $parent_bean ;

    /**
     * Can we display this subpanel?
     *
     * This is set after it loads the def's for the subpanel.  If there are no beans to display in the collection
     * we don't want to display this as it will just throw errors.
     *
     * @var bool
     */
    public $canDisplay = true;

    //module's table name and column fields.
    public $table_name ;
    public $db_fields ;
    public $bean_name ;
    public $template_instance ;

    public $search_query;
    public $base_collection_list = array();

    public function __construct(
        $name,
        $instance_properties,
        $parent_bean,
        $reload = false,
        $original_only = false,
        $search_query = '',
        $collections = array()
    ) {
        if (isset($instance_properties['collection_list'])) {
            $this->base_collection_list = $instance_properties['collection_list'];
        }

        if (!empty($collections) && isset($instance_properties['collection_list' ])) {
            foreach ($instance_properties['collection_list' ] as $cname => $value) {
                if (!in_array($value['module'], $collections)) {
                    unset($instance_properties['collection_list'][$cname]);
                }
            }
        }

        $this->_instance_properties = $instance_properties ;

        $this->search_query = '';
        if ((!isset($instance_properties['type']) || $instance_properties['type'] != 'collection')) {
            if (isset($this->_instance_properties['module'])) {
                $this->search_query = $this->buildSearchQuery($this->_instance_properties['module']);
            }
        }

        $this->name = $name ;
        $this->parent_bean = $parent_bean ;

        //set language
        global $current_language ;
        if (! isset($parent_bean->mbvardefs)) {
            $mod_strings = return_module_language($current_language, $parent_bean->module_dir) ;
        }
        $this->mod_strings = $mod_strings ;

        if ($this->isCollection()) {
            $this->canDisplay = $this->load_sub_subpanels() ; //load sub-panel definition.
        } else {
            if (!isset($this->_instance_properties [ 'module' ])) {
                $GLOBALS['log']->fatal('Undefined index: module');
                $instancePropertiesModule = null;
            } else {
                $instancePropertiesModule = $this->_instance_properties [ 'module' ];
            }
            if (!isset($this->_instance_properties [ 'subpanel_name' ])) {
                $GLOBALS['log']->fatal('Invalid or missing SubPanelDefinition property: subpanel_name');
                $def_path = null;
            } else {
                $subPanelName = $this->_instance_properties ['subpanel_name'];
                $def_path = 'modules/' . $this->_instance_properties [ 'module' ] . '/metadata/subpanels/' . $subPanelName . '.php' ;
            }

            $orig_exists = is_file($def_path);
            $loaded = false;
            if ($orig_exists) {
                require($def_path);
                $loaded = true;
            }
            if (is_file("custom/$def_path") && (!$original_only  || !$orig_exists)) {
                require("custom/$def_path");
                $loaded = true;
            }

            if (! $original_only && isset($this->_instance_properties [ 'override_subpanel_name' ]) && file_exists('custom/modules/' . $this->_instance_properties [ 'module' ] . '/metadata/subpanels/' . $this->_instance_properties [ 'override_subpanel_name' ] . '.php')) {
                $custom_def_path = 'custom/modules/' . $this->_instance_properties [ 'module' ] . '/metadata/subpanels/' . $this->_instance_properties [ 'override_subpanel_name' ] . '.php' ;
                require($custom_def_path) ;
                $loaded = true;
            }

            if (!$loaded) {
                $GLOBALS['log']->fatal("Failed to load original or custom subpanel data for $name in $def_path");
                $this->canDisplay = false;
            }

            // load module info from the module's bean file
            $this->load_module_info();

            // check that the loaded subpanel definition includes a $subpanel_layout section - some, such as
            // projecttasks/default do not...
            $this->panel_definition = array();
            if (isset($subpanel_layout) && is_array($subpanel_layout)) {
                $this->set_panel_definition($subpanel_layout);
            }
        }
    }

    /**
     * @return string
     */
    public function buildSearchQuery($module)
    {
        $seed = BeanFactory::getBean($module);
        if ($seed) {
            require_once('include/SubPanel/SubPanelSearchForm.php');

            $_REQUEST['searchFormTab'] = 'basic_search';
            $searchForm = new SubPanelSearchForm($seed, $module, $this);

            $searchMetaData = $searchForm->retrieveSearchDefs($module);
            $searchForm->setup(
                $searchMetaData['searchdefs'],
                $searchMetaData['searchFields'],
                'SubpanelSearchFormGeneric.tpl',
                'basic_search'
            );

            $searchForm->populateFromRequest();

            $where_clauses = $searchForm->generateSearchWhere(true, $seed->module_dir);

            $GLOBALS['log']->info("Subpanel Where Clause: $this->search_query");

            if (count($where_clauses) > 0) {
                return '(' . implode(' ) AND ( ', $where_clauses) . ')';
            }
        }
        return '';
    }

    /**
     * is the sub panel default hidden?
     *
     * @return bool
     */
    public function isDefaultHidden()
    {
        if (isset($this->_instance_properties['default_hidden']) && $this->_instance_properties['default_hidden'] == true) {
            return true;
        }

        return false;
    }


    public function distinct_query()
    {
        if (isset($this->_instance_properties [ 'get_distinct_data' ])) {
            return !empty($this->_instance_properties['get_distinct_data']) ? true : false;
        }
        return false ;
    }

    //return the translated header value.
    public function get_title()
    {
        if (empty($this->mod_strings [ $this->_instance_properties [ 'title_key' ] ])) {
            return translate($this->_instance_properties [ 'title_key' ], $this->_instance_properties [ 'module' ]) ;
        }
        return $this->mod_strings [ $this->_instance_properties [ 'title_key' ] ] ;
    }

    //return the definition of buttons. looks for buttons in 2 locations.
    public function get_buttons()
    {
        $buttons = array( ) ;
        if (isset($this->_instance_properties [ 'top_buttons' ])) {
            //this will happen only in the case of sub-panels with multiple sources(activities).
            $buttons = $this->_instance_properties [ 'top_buttons' ] ;
        } else {
            $buttons = $this->panel_definition [ 'top_buttons' ] ;
        }

        // permissions. hide SubPanelTopComposeEmailButton from activities if email module is disabled.
        //only email is  being tested because other submodules in activites/history such as notes, tasks, meetings
        // and calls cannot be disabled.
        //as of today these are the only 2 sub-panels that use the union clause.
        $mod_name = $this->get_module_name() ;
        if ($mod_name == 'Activities' || $mod_name == 'History') {
            global $modListHeader ;
            global $modules_exempt_from_availability_check ;
            if (isset($modListHeader) && (! (array_key_exists('Emails', $modListHeader) || array_key_exists('Emails', $modules_exempt_from_availability_check)))) {
                foreach ($buttons as $key => $button) {
                    foreach ($button as $property => $value) {
                        if ($value === 'SubPanelTopComposeEmailButton' || $value === 'SubPanelTopArchiveEmailButton') {
                            //remove this button from the array.
                            unset($buttons [ $key ]) ;
                        }
                    }
                }
            }
        }

        return $buttons ;
    }


    /**
     * Load the Sub-Panel objects if it can from the metadata files.
     *
     * call this function for sub-panels that have unions.
     *
     * @return bool         True by default if the subpanel was loaded.  Will return false if none in the collection are
     *                      allowed by the current user.
     */
    public function load_sub_subpanels()
    {
        global $modListHeader ;
        // added a check for security of tabs to see if an user has access to them
        // this prevents passing an "unseen" tab to the query string and pulling up its contents
        if (! isset($modListHeader)) {
            global $current_user ;
            if (isset($current_user)) {
                $modListHeader = query_module_access_list($current_user) ;
            }
        }

        //by default all the activities modules are exempt, so hiding them won't affect their appearance unless the 'activity' subpanel itself is hidden.
        //add email to the list temporarily so it is not affected in activities subpanel
        global $modules_exempt_from_availability_check ;
        $modules_exempt_from_availability_check['Emails'] = 'Emails';

        $listFieldMap = array();

        if (empty($this->sub_subpanels)) {
            $panels = $this->get_inst_prop_value('collection_list') ;
            if (null === $panels) {
                $GLOBALS['log']->fatal('Incorrect or missing SubPanelDefinition property: collection_list');
                $panels = array();
            }
            foreach ($panels as $panel => $properties) {
                if (array_key_exists($properties [ 'module' ], $modListHeader) || array_key_exists($properties [ 'module' ], $modules_exempt_from_availability_check)) {
                    $this->sub_subpanels [ $panel ] = new aSubPanel($panel, $properties, $this->parent_bean, false, false, $this->search_query, $this->base_collection_list) ;
                }
            }
            // if it's empty just dump out as there is nothing to process.
            if (empty($this->sub_subpanels)) {
                return false;
            }//Sync displayed list fields across the subpanels
            $display_fields = $this->getDisplayFieldsFromCollection($this->sub_subpanels);
            $query_fields = array();
            foreach ($this->sub_subpanels as $key => $subpanel) {
                $list_fields = $subpanel->get_list_fields();
                $listFieldMap[$key] = array();
                $index = 0;
                foreach ($list_fields as $field => $def) {
                    if (isset($def['vname']) && isset($def['width'])) {
                        $index++;
                        if (!empty($def['alias'])) {
                            $listFieldMap[$key][$def['alias']] = $field;
                        } else {
                            $listFieldMap[$key][$field] = $field;
                        }
                        if (!isset($display_fields[$def['vname']])) {
                            if (count($display_fields) > $index) {
                                //Try to insert the new field in an order that makes sense
                                $start = array_slice($display_fields, 0, $index);
                                $end = array_slice($display_fields, $index);
                                $display_fields = array_merge(
                                    $start,
                                    array($def['vname'] => array('name' => $field, 'vname' => $def['vname'], 'width' => $def['width'] )),
                                    $end
                                );
                            } else {
                                $display_fields[$def['vname']] = array(
                                    'name' => empty($def['alias']) ? $field : $def['alias'],
                                    'vname' => $def['vname'],
                                    'width' => $def['width'],
                                );
                            }
                        }
                    } else {
                        $query_fields[$field] = $def;
                    }
                }
            }
            foreach ($this->sub_subpanels as $key => $subpanel) {
                $list_fields = array();
                foreach ($display_fields as $vname => $def) {
                    $field = $def['name'];
                    $list_key = isset($listFieldMap[$key][$field]) ? $listFieldMap[$key][$field] : $field;

                    if (isset($subpanel->panel_definition['list_fields'][$field])) {
                        $list_fields[$field] = $subpanel->panel_definition['list_fields'][$field];
                    } else {
                        if ($list_key != $field && isset($subpanel->panel_definition['list_fields'][$list_key])) {
                            $list_fields[$list_key] = $subpanel->panel_definition['list_fields'][$list_key];
                        } else {
                            $list_fields[$field] = $display_fields[$vname];
                        }
                    }
                }
                foreach ($query_fields as $field => $def) {
                    if (isset($subpanel->panel_definition['list_fields'][$field])) {
                        $list_fields[$field] = $subpanel->panel_definition['list_fields'][$field];
                    } else {
                        $list_fields[$field] = $def;
                    }
                }
                $subpanel->panel_definition['list_fields'] = $list_fields;
            }
        }

        return true;
    }

    protected function getDisplayFieldsFromCollection($sub_subpanels)
    {
        $display_fields = array();
        foreach ($sub_subpanels as $key => $subpanel) {
            $list_fields = $subpanel->get_list_fields();
            $index = 0;
            foreach ($list_fields as $field => $def) {
                if (isset($def['vname']) && isset($def['width'])) {
                    $index++;
                    if (!isset($display_fields[$def['vname']])) {
                        if (count($display_fields) > $index) {
                            //Try to insert the new field in an order that makes sense
                            $start = array_slice($display_fields, 0, $index);
                            $end = array_slice($display_fields, $index);
                            $display_fields = array_merge(
                                $start,
                                array($def['vname'] => array('name' => $field, 'vname' => $def['vname'], 'width' => $def['width'] )),
                                $end
                            );
                        } else {
                            $display_fields[$def['vname']] = array(
                                'name' => $field,
                                'vname' => $def['vname'],
                                'width' => $def['width'],
                            );
                        }
                    }
                }
            }
        }
        return $display_fields;
    }

    public function isDatasourceFunction()
    {
        $subpanelData = $this->get_inst_prop_value('get_subpanel_data') ?? '';
        if (strpos($subpanelData, 'function') === false) {
            return false ;
        }
        return true ;
    }

    /**
     * Test to see if the sub panels defs contain a collection
     *
     * @return bool
     */
    public function isCollection()
    {
        return ($this->get_inst_prop_value('type') == 'collection');
    }

    //get value of a property defined at the panel instance level.
    public function get_inst_prop_value($name)
    {
        return isset($this->_instance_properties[$name]) ? $this->_instance_properties [ $name ] : null;
    }
    //get value of a property defined at the panel definition level.
    public function get_def_prop_value($name)
    {
        if (isset($this->panel_definition [ $name ])) {
            return $this->panel_definition [ $name ] ;
        } else {
            return null ;
        }
    }

    //if datasource is of the type function then return the function name
    //else return the value as is.
    public function get_function_parameters()
    {
        $parameters = array( ) ;
        if ($this->isDatasourceFunction()) {
            $parameters = $this->get_inst_prop_value('function_parameters') ;
        }
        return $parameters ;
    }

    public function get_data_source_name($check_set_subpanel_data = false)
    {
        $prop_value = null ;
        if ($check_set_subpanel_data) {
            $prop_value = $this->get_inst_prop_value('set_subpanel_data') ;
        }
        if (! empty($prop_value)) {
            return $prop_value ;
        } else {
            //fall back to default behavior.
        }
        if ($this->isDatasourceFunction()) {
            return (substr_replace($this->get_inst_prop_value('get_subpanel_data'), '', 0, 9)) ;
        } else {
            return $this->get_inst_prop_value('get_subpanel_data') ;
        }
    }

    //returns the where clause for the query.
    public function get_where()
    {
        if ($this->get_def_prop_value('where') != '' && $this->search_query != '') {
            return $this->get_def_prop_value('where').' AND '.$this->search_query;
        } else {
            if ($this->search_query != '') {
                return $this->search_query;
            }
        }
        return $this->get_def_prop_value('where') ;
    }

    public function is_fill_in_additional_fields()
    {
        // do both. inst_prop returns values from metadata/subpaneldefs.php and def_prop returns from subpanel/default.php
        $temp = $this->get_inst_prop_value('fill_in_additional_fields') || $this->get_def_prop_value('fill_in_additional_fields') ;
        return $temp ;
    }

    public function get_list_fields()
    {
        if (isset($this->panel_definition [ 'list_fields' ])) {
            return $this->panel_definition [ 'list_fields' ] ;
        } else {
            return array( ) ;
        }
    }

    public function get_module_name()
    {
        return $this->get_inst_prop_value('module') ;
    }

    public function get_name()
    {
        return $this->name ;
    }

    //load subpanel module's table name and column fields.
    public function load_module_info()
    {
        global $beanList ;
        global $beanFiles ;

        $module_name = $this->get_module_name() ;
        if (! empty($module_name)) {
            $bean_name = $beanList [ $this->get_module_name() ] ;

            $this->bean_name = $bean_name ;

            include_once($beanFiles [ $bean_name ]) ;
            $this->template_instance = new $bean_name() ;
            $this->template_instance->force_load_details = true ;
            $this->table_name = $this->template_instance->table_name ;
            //$this->db_fields=$this->template_instance->column_fields;
        }
    }
    //this function is to be used only with sub-panels that are based
    //on collections.
    public function get_header_panel_def()
    {
        if (! empty($this->sub_subpanels)) {
            if (! empty($this->_instance_properties [ 'header_definition_from_subpanel' ]) && ! empty($this->sub_subpanels [ $this->_instance_properties [ 'header_definition_from_subpanel' ] ])) {
                return $this->sub_subpanels [ $this->_instance_properties [ 'header_definition_from_subpanel' ] ] ;
            } else {
                $display_fields = array();
                //If we are not pulling from a specific subpanel, create a list of all list fields and use that.
                foreach ($this->sub_subpanels as $subpanel) {
                    $list_fields = $subpanel->get_list_fields();
                    foreach ($list_fields as $field => $def) {
                    }
                }

                reset($this->sub_subpanels) ;
                return current($this->sub_subpanels) ;
            }
        }
        return null ;
    }

    /**
     * Returns an array of current properties of the class.
     * It will simply give the class name for instances of classes.
     */
    public function _to_array()
    {
        return array( '_instance_properties' => $this->_instance_properties , 'db_fields' => $this->db_fields , 'mod_strings' => $this->mod_strings , 'name' => $this->name , 'panel_definition' => $this->panel_definition , 'parent_bean' => get_class($this->parent_bean) , 'sub_subpanels' => $this->sub_subpanels , 'table_name' => $this->table_name , 'template_instance' => get_class($this->template_instance) ) ;
    }

    /**
     * Sets definition of the subpanel
     *
     * @param array $definition
     */
    protected function set_panel_definition(array $definition)
    {
        $this->panel_definition = $definition;
    }
}

class SubPanelDefinitions
{
    public $_focus ;
    public $_visible_tabs_array ;
    public $panels ;
    public $layout_defs ;

    /**
     * Enter description here...
     *
     * @param SugarBean $focus - this is the bean you want to get the data from
     * @param string $layout_def_key - if you wish to use a layout_def defined in the default metadata/subpaneldefs.php
     * that is not keyed off of $bean->module_dir pass in the key here
     * @param array $layout_def_override - if you wish to override the default loaded layout defs you pass them in here.
     * @return SubPanelDefinitions
     */
    public function __construct($focus, $layout_def_key = '', $layout_def_override = '')
    {
        $this->_focus = $focus ;
        if (! empty($layout_def_override)) {
            $this->layout_defs = $layout_def_override ;
        } else {
            $this->open_layout_defs(false, $layout_def_key) ;
        }
    }

    /**
     * This function returns an ordered list of all "tabs", actually subpanels, for this module
     * The source list is obtained from the subpanel layout contained in the layout_defs for this module,
     * found either in the modules metadata/subpaneldefs.php file, or in the modules custom/.../Ext/Layoutdefs/layoutdefs.ext.php file
     * and filtered through an ACL check.
     * Note that the keys for the resulting array of tabs are in practice the name of the underlying source relationship for the subpanel
     * So for example, the key for a custom module's subpanel with Accounts might be 'one_one_accounts', as generated by the Studio Relationship Editor
     * Although OOB module subpanels have keys such as 'accounts', which might on the face of it appear to be a reference to the related module, in fact 'accounts' is still the relationship name
     * @param boolean 	Optional - include the subpanel title label in the return array (false)
     * @return array	All tabs that pass an ACL check
     */
    public function get_available_tabs($FromGetModuleSubpanels = false)
    {
        global $modListHeader ;
        global $modules_exempt_from_availability_check ;

        if (isset($this->_visible_tabs_array)) {
            return $this->_visible_tabs_array ;
        }

        if (empty($modListHeader)) {
            $modListHeader = query_module_access_list($GLOBALS['current_user']);
        }

        $this->_visible_tabs_array = array( ) ; // bug 16820 - make sure this is an array for the later ksort

        if (isset($this->layout_defs [ 'subpanel_setup' ])) { // bug 17434 - belts-and-braces - check that we have some subpanels first
            //retrieve list of hidden subpanels
            $hidden_panels = $this->get_hidden_subpanels();

            //activities is a special use case in that if it is hidden,
            //then the history tab should be hidden too.
            if (!empty($hidden_panels) && is_array($hidden_panels) && in_array('activities', $hidden_panels)) {
                //add history to list hidden_panels
                $hidden_panels['history'] = 'history';
            }

            foreach ($this->layout_defs [ 'subpanel_setup' ] as $key => $values_array) {
                //exclude if this subpanel is hidden from admin screens
                $module = $key;
                if (isset($values_array['module'])) {
                    $module = strtolower($values_array['module']);
                }
                if ($hidden_panels && is_array($hidden_panels) && (in_array($module, $hidden_panels) || array_key_exists($module, $hidden_panels))) {
                    //this panel is hidden, skip it
                    continue;
                }

                // make sure the module attribute is set, else none of this works...
                if (!isset($values_array [ 'module' ])) {
                    $GLOBALS['log']->debug("SubPanelDefinitions->get_available_tabs(): no module defined in subpaneldefs for '$key' =>" . var_export($values_array, true) . " - ingoring subpanel defintion") ;
                    continue;
                }

                //check permissions.
                $exempt = array_key_exists($values_array [ 'module' ], $modules_exempt_from_availability_check) ;
                $ok = $exempt || ((! ACLController::moduleSupportsACL($values_array [ 'module' ]) || ACLController::checkAccess($values_array [ 'module' ], 'list', true))) ;

                $GLOBALS [ 'log' ]->debug("SubPanelDefinitions->get_available_tabs(): " . $key . "= " . ($exempt ? "exempt " : "not exempt " .($ok ? " ACL OK" : ""))) ;

                if ($ok) {
                    while (! empty($this->_visible_tabs_array [ $values_array [ 'order' ] ])) {
                        $values_array [ 'order' ] ++ ;
                    }

                    $this->_visible_tabs_array [ $values_array ['order'] ] = ($FromGetModuleSubpanels) ? array($key=>$values_array['title_key']) : $key ;
                }
            }
        }

        ksort($this->_visible_tabs_array) ;
        return $this->_visible_tabs_array ;
    }

    /**
     * Load the definition of the a sub-panel.
     * Also the sub-panel is added to an array of sub-panels.
     * use of reload has been deprecated, since the subpanel is initialized every time.
     *
     * @param string $name              The name of the sub-panel to reload
     * @param boolean $reload           Reload the sub-panel (unused)
     * @param boolean $original_only    Only load the original sub-panel and no custom ones
     * @return boolean|aSubPanel        Returns aSubPanel object or boolean false if one is not found or it can't be
     *      displayed due to ACL reasons.
     */
    public function load_subpanel($name, $reload = false, $original_only = false, $search_query = '', $collections = array())
    {
        $panelName = strtolower($name);

        if (!array_key_exists($panelName, $this->layout_defs ['subpanel_setup'])) {
            LoggerManager::getLogger()->error(
                sprintf(
                    "Trying to load subpanel without definition: %s in module %s",
                    $panelName,
                    $this->_focus->module_dir
                )
            );
            return false;
        }

        if (!is_dir('modules/' . $this->layout_defs ['subpanel_setup'][$panelName] ['module'])) {
            return false;
        }

        $subpanel = new aSubPanel(
            $name,
            $this->layout_defs ['subpanel_setup'] [$panelName],
            $this->_focus,
            $reload,
            $original_only,
            $search_query,
            $collections
        );

        // only return the subpanel object if we can display it.
        if ($subpanel->canDisplay == true) {
            return $subpanel;
        }

        // by default return false so we don't show anything if it's not required.
        return false;
    }

    /**
     * Load the layout def file and associate the definition with a variable in the file.
     */
    public function open_layout_defs($reload = false, $layout_def_key = '', $original_only = false)
    {
        $layout_defs [ $this->_focus->module_dir ] = array( ) ;
        $layout_defs [ $layout_def_key ] = array( ) ;

        if (empty($this->layout_defs) || $reload || (! empty($layout_def_key) && ! isset($layout_defs [ $layout_def_key ]))) {
            if (file_exists(get_custom_file_if_exists('modules/' . $this->_focus->module_dir . '/metadata/subpaneldefs.php'))) {
                require get_custom_file_if_exists('modules/' . $this->_focus->module_dir . '/metadata/subpaneldefs.php');
            }

            if (! $original_only && file_exists('custom/modules/' . $this->_focus->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php')) {
                require('custom/modules/' . $this->_focus->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php') ;
            }

            if (! empty($layout_def_key)) {
                $this->layout_defs = $layout_defs [ $layout_def_key ] ;
            } else {
                $this->layout_defs = $layout_defs [ $this->_focus->module_dir ] ;
            }
        }
    }

    /**
     * Removes a tab from the list of loaded tabs.
     * Returns true if successful, false otherwise.
     * Hint: Used by Campaign's DetailView.
     */
    public function exclude_tab($tab_name)
    {
        $result = false ;
        //unset layout definition
        if (! empty($this->layout_defs [ 'subpanel_setup' ] [ $tab_name ])) {
            unset($this->layout_defs [ 'subpanel_setup' ] [ $tab_name ]) ;
        }
        //unset instance from _visible_tab_array
        if (! empty($this->_visible_tabs_array)) {
            $key = array_search($tab_name, $this->_visible_tabs_array) ;
            if ($key !== false) {
                unset($this->_visible_tabs_array [ $key ]) ;
            }
        }
        return $result ;
    }


    /**
     * return all available subpanels that belong to the list of tab modules.  You can optionally return all
     * available subpanels, and also optionally group by module (prepends the key with the bean class name).
     */
    public function get_all_subpanels($return_tab_modules_only = true, $group_by_module = false)
    {
        global $moduleList, $beanFiles, $beanList, $module;

        //use tab controller function to get module list with named keys
        require_once("modules/MySettings/TabController.php");
        $tabController = new TabController();
        $modules_to_check = $tabController->get_key_array($moduleList);

        //change case to match subpanel processing later on
        $modules_to_check = array_change_key_case($modules_to_check);
        // Append on the CampaignLog module, because that is where the subpanels point, not directly to Campaigns
        $modules_to_check['campaignlog'] = "CampaignLog";


        $spd = '';
        $spd_arr = array();
        //iterate through modules and build subpanel array
        foreach ($modules_to_check as $mod_name) {

            //skip if module name is not in bean list, otherwise get the bean class name
            if (!isset($beanList[$mod_name])) {
                continue;
            }
            $class = $beanList[$mod_name];

            //skip if class name is not in file list, otherwise require the bean file and create new class
            if (!isset($beanFiles[$class]) || !file_exists($beanFiles[$class])) {
                continue;
            }

            //retrieve subpanels for this bean
            require_once($beanFiles[$class]);
            $bean_class = new $class();

            //create new subpanel definition instance and get list of tabs
            $spd = new SubPanelDefinitions($bean_class) ;
            $sub_tabs = $spd->get_available_tabs();

            //add each subpanel to array of total subpanles
            foreach ($sub_tabs as $panel_key) {
                $panel_key = strtolower($panel_key);
                $panel_module = $panel_key;
                if (isset($spd->layout_defs['subpanel_setup'][$panel_key]['module'])) {
                    $panel_module = strtolower($spd->layout_defs['subpanel_setup'][$panel_key]['module']);
                }
                //if module_only flag is set, only if it is also in module array
                if ($return_tab_modules_only && !array_key_exists($panel_module, $modules_to_check)) {
                    continue;
                }
                $panel_key_name = $panel_module;

                //group_by_key_name is set to true, then array will hold an entry for each
                //subpanel, with the module name prepended in the key
                if ($group_by_module) {
                    $panel_key_name = $class.'_'.$panel_key_name;
                }
                //add panel name to subpanel array
                $spd_arr[$panel_key_name] = $panel_module;
            }
        }
        return 	$spd_arr;
    }

    /*
     * save array of hidden panels to mysettings category in config table
     */
    public function set_hidden_subpanels($panels)
    {
        $administration = BeanFactory::newBean('Administration');
        $serialized = base64_encode(serialize($panels));
        $administration->saveSetting('MySettings', 'hide_subpanels', $serialized);
    }

    /*
     * retrieve hidden subpanels
     */
    public function get_hidden_subpanels()
    {
        global $moduleList;

        //create variable as static to minimize queries
        static $hidden_subpanels = null;

        // if the static value is not already cached, then retrieve it.
        if (empty($hidden_subpanels)) {

            //create Administration object and retrieve any settings for panels
            $administration = BeanFactory::newBean('Administration');
            $administration->retrieveSettings('MySettings');

            if (isset($administration->settings) && isset($administration->settings['MySettings_hide_subpanels'])) {
                $hidden_subpanels = $administration->settings['MySettings_hide_subpanels'];
                $hidden_subpanels = trim($hidden_subpanels);

                //make sure serialized string is not empty
                if (!empty($hidden_subpanels)) {
                    //decode and unserialize to retrieve the array
                    $hidden_subpanels = base64_decode($hidden_subpanels);
                    $hidden_subpanels = unserialize($hidden_subpanels);

                    //Ensure modules saved in the preferences exist.
                    //get user preference
                    //unserialize and add to array if not empty
                    $pref_hidden = array();
                    foreach ($pref_hidden as $id => $pref_hidden_panel) {
                        $hidden_subpanels[] = $pref_hidden_panel;
                    }
                } else {
                    //no settings found, return empty
                    return $hidden_subpanels;
                }
            } else {	//no settings found, return empty
                return $hidden_subpanels;
            }
        }

        return $hidden_subpanels;
    }
}
