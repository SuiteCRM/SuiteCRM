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

    /**
     * ListView for the subpanel- list of many objects
     * @api
     */
    #[\AllowDynamicProperties]
    class ListViewSubPanel extends ListView
    {
        protected $smartyTemplate;
        protected $smartyTemplatePath;

        public function __construct()
        {
            $this->smartyTemplate = new Sugar_Smarty();
            parent::__construct();
        }

        public function initNewSmartyTemplate($templatePath, $modString, $imagePath = null)
        {

            // validate path is a .tpl file
            $path_parts = pathinfo($templatePath);
            if ($path_parts['extension'] != 'tpl') {
                $GLOBALS['log']->fatal('ListViewSubPanel::initNewSmartyTemplate path must have an tpl extension');

                // TODO: Remove after 7.8. This is just to see which subpanels need to be upgraded to smarty
                echo 'ListViewSubPanel::initNewSmartyTemplate path must have an tpl extension';

                return;
            }

            $this->smartyTemplate = new Sugar_Smarty();
            $this->smartyTemplatePath = $templatePath;

            if (isset($modString)) {
                $this->setModStrings($modString);
            }

            if (isset($imagePath)) {
                $this->setImagePath($imagePath);
            }
        }

        public function getSmartyTemplate()
        {
            return $this->smartyTemplate;
        }

        public function setSmartyTemplate($template)
        {
            $this->smartyTemplate = $template;
        }

        public function smartyTemplateAssign($name, $value)
        {
            if (!isset($this->smartyTemplate)) {
                $this->createSmartyTemplate();
            }
            $this->smartyTemplate->assign($name, $value);
        }

        public function createSmartyTemplate()
        {
            if (!isset($this->smartyTemplate)) {
                if (isset($this->smartyTemplatePath)) {
                    $this->smartyTemplate = new Sugar_Smarty($this->smartyTemplatePath);
                    $this->smartyTemplate->assign("APP", $this->local_app_strings);
                    if (isset($this->local_mod_strings)) {
                        $this->smartyTemplatePath->assign("MOD", $this->local_mod_strings);
                    }
                    $this->smartyTemplate->assign("THEME", $this->local_theme);
                    $this->smartyTemplate->assign("IMAGE_PATH", $this->local_image_path);
                    $this->smartyTemplate->assign("MODULE_NAME", $this->local_current_module);
                } else {
                    $GLOBALS['log']->error("NO SMARTY TEMPLATE PATH DEFINED CANNOT CREATE SMARTY TEMPLATE");
                }
            }
        }

        public function setCurrentModule($currentModule)
        {
            unset($this->local_current_module);
            $this->local_current_module = $currentModule;
            if (isset($this->smartyTemplate)) {
                $this->smartyTemplate->assign("MODULE_NAME", $this->local_current_module);
            }
        }


        public function process_dynamic_listview($source_module, $sugarbean, $subpanel_def, $countOnly = false)
        {
            $this->source_module = $source_module;
            $this->subpanel_module = $subpanel_def->name;
            if (!isset($this->smartyTemplate)) {
                $this->createSmartyTemplate();
            }

            $html_var = $this->subpanel_module . "_CELL";

            $list_data = $this->processUnionBeans($sugarbean, $subpanel_def, $html_var, $countOnly);

            if ($countOnly) {
                return $list_data;
            }

            $list = $list_data['list'];
            $parent_data = $list_data['parent_data'];

            if ($subpanel_def->isCollection()) {
                $thepanel = $subpanel_def->get_header_panel_def();
            } else {
                $thepanel = $subpanel_def;
            }


            $this->process_dynamic_listview_header($thepanel->get_module_name(), $thepanel, $html_var);
            $this->process_dynamic_listview_rows($list, $parent_data, 'dyn_list_view', $html_var, $subpanel_def);


            $this->smartyTemplate->display($this->smartyTemplatePath);

            if (isset($_SESSION['validation'])) {
                print base64_decode('PGEgaHJlZj0naHR0cDovL3d3dy5zdWdhcmNybS5jb20nPlBPV0VSRUQmbmJzcDtCWSZuYnNwO1NVR0FSQ1JNPC9hPg==');
            }
            if (isset($list_data['query'])) {
                return ($list_data['query']);
            }
        }

        public function process_dynamic_listview_header($source_module, $subpanel_def, $html_var = 'CELL')
        {
            $layout_manager = $this->getLayoutManager();
            $layout_manager->setAttribute('order_by_link', $this->processOrderBy($html_var));
            $layout_manager->setAttribute('context', 'HeaderCell');
            $layout_manager->setAttribute('image_path', $this->local_image_path);
            $layout_manager->setAttribute('html_varName', $html_var);
            $layout_manager->setAttribute('module_name', $source_module);
            list($orderBy, $desc) = $this->getOrderByInfo($html_var);

            if ($orderBy == 'amount*1') {
                $orderBy = 'amount';
            }
            $buttons = false;
            $col_count = 0;
            $widget_contents = array();
            $footable = array();

            foreach ($subpanel_def->get_list_fields() as $column_name => $widget_args) {
                $usage = empty($widget_args['usage']) ? '' : $widget_args['usage'];
                if ($usage != 'query_only' || !empty($widget_args['force_query_only_display'])) {
                    $imgArrow = '';

                    if ($orderBy == $column_name || (isset($widget_args['sort_by']) && str_replace('.', '_', $widget_args['sort_by']) == $orderBy)) {
                        $imgArrow = "_down";
                        if ($this->sort_order == 'asc') {
                            $imgArrow = "_up";
                        };
                    }

                    if (!preg_match("/_button/i", $column_name)) {
                        $widget_args['name'] = $column_name;
                        $widget_args['sort'] = $imgArrow;
                        $widget_args['start_link_wrapper'] = $this->start_link_wrapper;
                        $widget_args['end_link_wrapper'] = $this->end_link_wrapper;
                        $widget_args['subpanel_module'] = $this->subpanel_module;

                        $widget_contents[] = $layout_manager->widgetDisplay($widget_args);
                        $cell_width = empty($widget_args['width']) ? '' : $widget_args['width'];


                        static $count;

                        if (!isset($count)) {
                            $count = 0;
                        } else {
                            $count++;
                        }

                        if ($col_count == 0 || $column_name == 'name') {
                            $footable = 'data-toggle="true"';
                        } else {
                            $footable = 'data-hide="phone"';
                            if ($col_count > 2) {
                                $footable = 'data-hide="phone,phonelandscape"';
                            }
                            if ($col_count > 4) {
                                $footable = 'data-hide="phone,phonelandscape,tablet"';
                            }
                        }
                    } else {
                        $buttons = true;
                    }
                }
                ++$col_count;
            }

            $this->smartyTemplate->assign('HEADER_CELLS', $widget_contents);
        }

        public function process_dynamic_listview_rows($data, $parent_data, $smartyTemplateSection, $html_varName, $subpanel_def)
        {
            global $subpanel_item_count;
            global $odd_bg;
            global $even_bg;
            global $hilite_bg;
            global $click_bg;

            $widget_contents = array();
            $button_contents = array();

            $this->smartyTemplate->assign("BG_HILITE", $hilite_bg);
            $this->smartyTemplate->assign('CHECKALL', SugarThemeRegistry::current()->getImage('blank', '', 1, 1, ".gif", ''));
            //$this->smartyTemplate->assign("BG_CLICK", $click_bg);
            $subpanel_item_count = 0;
            $oddRow = true;
            $count = 0;
            reset($data);

            //GETTING OFFSET
            $offset = ($this->getOffset($html_varName)) === false ? 0 : $this->getOffset($html_varName);
            //$totaltime = 0;
            $processed_ids = array();

            $fill_additional_fields = array();
            //Either retrieve the is_fill_in_additional_fields property from the lone
            //subpanel or visit each subpanel's subpanels to retrieve the is_fill_in_addition_fields
            //property
            $subpanel_list = array();
            if ($subpanel_def->isCollection()) {
                $subpanel_list = $subpanel_def->sub_subpanels;
            } else {
                $subpanel_list[] = $subpanel_def;
            }

            foreach ($subpanel_list as $this_subpanel) {
                if ($this_subpanel->is_fill_in_additional_fields()) {
                    $fill_additional_fields[] = $this_subpanel->bean_name;
                    $fill_additional_fields[$this_subpanel->bean_name] = true;
                }
            }

            if (empty($data)) {
                $thepanel = $subpanel_def;
                if ($subpanel_def->isCollection()) {
                    $thepanel = $subpanel_def->get_header_panel_def();
                }
            }

            foreach ($data as $aVal => $aItem) {
                $widget_contents[$aVal] = array();
                $subpanel_item_count++;
                $aItem->check_date_relationships_load();

                if (!empty($fill_additional_fields[$aItem->object_name]) || ($aItem->object_name == 'Case' && !empty($fill_additional_fields['aCase']))) {
                    $aItem->fill_in_additional_list_fields();
                }
                $aItem->call_custom_logic("process_record");

                if (isset($parent_data[$aItem->id])) {
                    $aItem->parent_name = $parent_data[$aItem->id]['parent_name'];
                    if (!empty($parent_data[$aItem->id]['parent_name_owner'])) {
                        $aItem->parent_name_owner = $parent_data[$aItem->id]['parent_name_owner'];
                        $aItem->parent_name_mod = $parent_data[$aItem->id]['parent_name_mod'];
                    }
                }
                $fields = $aItem->get_list_view_data();
                if (isset($processed_ids[$aItem->id])) {
                    continue;
                } else {
                    $processed_ids[$aItem->id] = 1;
                }


                //ADD OFFSET TO ARRAY
                $fields['OFFSET'] = ((int)$offset + $count + 1);

                if ($this->shouldProcess) {
                    if ($aItem->ACLAccess('EditView')) {
                        $widget_contents[$aVal][0] = "<input type='checkbox' class='checkbox' name='mass[]' value='" . $fields['ID'] . "' />";
                    } else {
                        $widget_contents[$aVal][0] = '';
                    }
//                    if ($aItem->ACLAccess('DetailView')) {
//                        $this->smartyTemplate->assign('TAG_NAME', 'a');
//                    }
//                    else {
//                        $this->smartyTemplate->assign('TAG_NAME', 'span');
//                    }
                    $this->smartyTemplate->assign('CHECKALL', "<input type='checkbox'  title='" . $GLOBALS['app_strings']['LBL_SELECT_ALL_TITLE'] . "' class='checkbox' name='massall' id='massall' value='' onclick='sListView.check_all(document.MassUpdate, \"mass[]\", this.checked);' />");
                }
                $oddRow = !$oddRow;

                $layout_manager = $this->getLayoutManager();
                $layout_manager->setAttribute('context', 'List');
                $layout_manager->setAttribute('image_path', $this->local_image_path);
                $layout_manager->setAttribute('module_name', $subpanel_def->_instance_properties['module']);
                if (!empty($this->child_focus)) {
                    $layout_manager->setAttribute('related_module_name', $this->child_focus->module_dir);
                }
                //AG$subpanel_data = $this->list_field_defs;
                //$bla = array_pop($subpanel_data);
                //select which sub-panel to display here, the decision will be made based on the type of
                //the sub-panel and panel in the bean being processed.
                if ($subpanel_def->isCollection()) {
                    $thepanel = $subpanel_def->sub_subpanels[$aItem->panel_name];
                } else {
                    $thepanel = $subpanel_def;
                }

                /* BEGIN - SECURITY GROUPS */

                //This check is costly doing it field by field in the below foreach
                //instead pull up here and do once per record....
                $aclaccess_is_owner = false;
                $aclaccess_in_group = false;

                global $current_user;
                if (is_admin($current_user)) {
                    $aclaccess_is_owner = true;
                } else {
                    $aclaccess_is_owner = $aItem->isOwner($current_user->id);
                }

                require_once("modules/SecurityGroups/SecurityGroup.php");
                $aclaccess_in_group = SecurityGroup::groupHasAccess($aItem->module_dir, $aItem->id);

                /* END - SECURITY GROUPS */

                //get data source name
                $linked_field = $thepanel->get_data_source_name();
                $linked_field_set = $thepanel->get_data_source_name(true);
                static $count;
                if (!isset($count)) {
                    $count = 0;
                }
                /* BEGIN - SECURITY GROUPS */
                /**
                 * $field_acl['DetailView'] = $aItem->ACLAccess('DetailView');
                 * $field_acl['ListView'] = $aItem->ACLAccess('ListView');
                 * $field_acl['EditView'] = $aItem->ACLAccess('EditView');
                 */
                //pass is_owner, in_group...vars defined above
                $field_acl['DetailView'] = $aItem->ACLAccess('DetailView', $aclaccess_is_owner, $aclaccess_in_group);
                $field_acl['ListView'] = $aItem->ACLAccess('ListView', $aclaccess_is_owner, $aclaccess_in_group);
                $field_acl['EditView'] = $aItem->ACLAccess('EditView', $aclaccess_is_owner, $aclaccess_in_group);
                /* END - SECURITY GROUPS */
                foreach ($thepanel->get_list_fields() as $field_name => $list_field) {
                    //add linked field attribute to the array.
                    $list_field['linked_field'] = $linked_field;
                    $list_field['linked_field_set'] = $linked_field_set;

                    $usage = empty($list_field['usage']) ? '' : $list_field['usage'];
                    if ($usage == 'query_only' && !empty($list_field['force_query_only_display'])) {
                        //if you are here you have column that is query only but needs to be displayed as blank.  This is helpful
                        //for collections such as Activities where you have a field in only one object and wish to show it in the subpanel list
                        $count++;
                        $widget_contents[$aVal][$field_name] = '&nbsp;';
                    } else {
                        if ($usage != 'query_only') {
                            $list_field['name'] = $field_name;

                            $module_field = $field_name . '_mod';
                            $owner_field = $field_name . '_owner';
                            if (!empty($aItem->$module_field)) {
                                $list_field['owner_id'] = $aItem->$owner_field;
                                $list_field['owner_module'] = $aItem->$module_field;
                            } else {
                                $list_field['owner_id'] = false;
                                $list_field['owner_module'] = false;
                            }
                            if (isset($list_field['alias'])) {
                                $list_field['name'] = $list_field['alias'];
                                // Clone field def from origin field def to alias field def
                                $alias_field_def = $aItem->field_defs[$field_name];
                                $alias_field_def['name'] = $list_field['alias'];
                                // Add alias field def into bean to can render field in subpanel
                                $aItem->field_defs[$list_field['alias']] = $alias_field_def;
                                if (!isset($fields[strtoupper($list_field['alias'])]) || empty($fields[strtoupper($list_field['alias'])])) {
                                    global $timedate;
                                    $fields[strtoupper($list_field['alias'])] = (!empty($aItem->$field_name)) ? $aItem->$field_name : $timedate->to_display_date_time($aItem->{$list_field['alias']});
                                }
                            } else {
                                $list_field['name'] = $field_name;
                            }
                            $list_field['fields'] = $fields;
                            $list_field['module'] = $aItem->module_dir;
                            $list_field['start_link_wrapper'] = $this->start_link_wrapper;
                            $list_field['end_link_wrapper'] = $this->end_link_wrapper;
                            $list_field['subpanel_id'] = $this->subpanel_id;
                            $list_field += $field_acl;
                            if (isset($aItem->field_defs[strtolower($list_field['name'])])) {
                                require_once('include/SugarFields/SugarFieldHandler.php');
                                // We need to see if a sugar field exists for this field type first,
                                // if it doesn't, toss it at the old sugarWidgets. This is for
                                // backwards compatibility and will be removed in a future release
                                $vardef = $aItem->field_defs[strtolower($list_field['name'])];
                                if (isset($vardef['type'])) {
                                    $fieldType = isset($vardef['custom_type']) ? $vardef['custom_type'] : $vardef['type'];
                                    $tmpField = SugarFieldHandler::getSugarField($fieldType, true);
                                } else {
                                    $tmpField = null;
                                }

                                if ($tmpField != null) {
                                    $widget_contents[$aVal][$field_name] = SugarFieldHandler::displaySmarty($list_field['fields'], $vardef, 'ListView', $list_field);
                                } else {
                                    // No SugarField for this particular type
                                    // Use the old, icky, SugarWidget for now
                                    $widget_contents[$aVal][$field_name] = $layout_manager->widgetDisplay($list_field);
                                }

                                if (isset($list_field['widget_class']) && $list_field['widget_class'] == 'SubPanelDetailViewLink') {
                                    // We need to call into the old SugarWidgets for the time being, so it can generate a proper link with all the various corner-cases handled
                                    // So we'll populate the field data with the pre-rendered display for the field
                                    $list_field['fields'][$field_name] = $widget_contents[$aVal][$field_name];
                                    if ('full_name' == $field_name) {//bug #32465
                                        $list_field['fields'][strtoupper($field_name)] = $widget_contents[$aVal][$field_name];
                                    }

                                    //vardef source is non db, assign the field name to varname for processing of column.
                                    if (!empty($vardef['source']) && $vardef['source'] == 'non-db') {
                                        $list_field['varname'] = $field_name;
                                    }
                                    $widget_contents[$aVal][$field_name] = $layout_manager->widgetDisplay($list_field);
                                } else {
                                    if (isset($list_field['widget_class']) && $list_field['widget_class'] == 'SubPanelEmailLink') {
                                        if (isset($list_field['fields']['EMAIL1_LINK'])) {
                                            $widget_contents[$aVal][$field_name] = $list_field['fields']['EMAIL1_LINK'];
                                        } else {
                                            $widget_contents[$aVal][$field_name] = $layout_manager->widgetDisplay($list_field);
                                        }
                                    }
                                }

                                $count++;
                                if (empty($widget_contents)) {
                                    $widget_contents[$aVal][$field_name] = '&nbsp;';
                                }
                            } else {
                                // This handles the edit and remove buttons and icon widget
                                if (isset($list_field['widget_class']) && $list_field['widget_class'] == "SubPanelIcon") {
                                    $count++;
                                    $widget_contents[$aVal][$field_name] = $layout_manager->widgetDisplay($list_field);

                                    if (empty($widget_contents[$aVal][$field_name])) {
                                        $widget_contents[$aVal][$field_name] = '&nbsp;';
                                    }
                                } elseif (preg_match("/button/i", $list_field['name'])) {
                                    if ((($list_field['name'] === 'edit_button' && $field_acl['EditView']) || ($list_field['name'] === 'close_button' && $field_acl['EditView']) || ($list_field['name'] === 'remove_button' && $field_acl['EditView'])) && '' != ($_content = $layout_manager->widgetDisplay($list_field))) {
                                        $button_contents[$aVal][] = $_content;
                                        unset($_content);
                                    } else {
                                        $doNotProcessTheseActions = array("edit_button", "close_button","remove_button");
                                        if (!in_array($list_field['name'], $doNotProcessTheseActions) && '' != ($_content = $layout_manager->widgetDisplay($list_field))) {
                                            $button_contents[$aVal][] = $_content;
                                            unset($_content);
                                        } else {
                                            $button_contents[$aVal][] = '';
                                        }
                                    }
                                } else {
                                    $count++;
//                                    $this->smartyTemplate->assign('CLASS', "");
                                    $widget_contents[$aVal][$field_name] = $layout_manager->widgetDisplay($list_field);
//                                    $this->smartyTemplate->assign('CELL_COUNT', $count);
                                    if (empty($widget_contents[$aVal][$field_name])) {
                                        $widget_contents[$aVal][$field_name] = '&nbsp;';
                                    }
                                }
                            }
                        }
                    }
                }

                $aItem->setupCustomFields($aItem->module_dir);
                $aItem->custom_fields->populateAllXTPL($this->smartyTemplate, 'detail', $html_varName, $fields);

                $count++;
            }
            $this->smartyTemplate->assign('ROWS', $widget_contents);
            $this->smartyTemplate->assign('ROWS_BUTTONS', $button_contents);
        }

        public function processListNavigation($smartyTemplateSection, $html_varName, $current_offset, $next_offset, $previous_offset, $row_count, $sugarbean = null, $subpanel_def = null, $col_count = 20)
        {
            global $export_module;
            global $sugar_config;
            global $current_user;
            global $currentModule;
            global $app_strings;

            if (!isset($current_offset) || empty($current_offset)) {
                $current_offset=0;
            }
            $start_record = $current_offset + 1;

            if (!is_numeric($col_count)) {
                $col_count = 20;
            }

            if ($row_count == 0) {
                $start_record = 0;
            }

            $end_record = $start_record + $this->records_per_page;
            // back up the last page.
            if ($end_record > $row_count + 1) {
                $end_record = $row_count + 1;
            }
            // Determine the start location of the last page
            if ($row_count == 0) {
                $number_pages = 0;
            } else {
                $number_pages = floor(($row_count - 1) / $this->records_per_page);
            }

            $last_offset = $number_pages * $this->records_per_page;

            if (empty($this->query_limit) || $this->query_limit > $this->records_per_page) {
                $this->base_URL = $this->getBaseURL($html_varName);
                $dynamic_url = '';

                if ($this->is_dynamic) {
                    $dynamic_url .= '&' . $this->getSessionVariableName($html_varName, 'ORDER_BY') . '=' . $this->getSessionVariable($html_varName, 'ORDER_BY') . '&sort_order=' . $this->sort_order . '&to_pdf=true&action=SubPanelViewer&subpanel=' . $this->subpanel_module;
                }

                $current_URL = htmlentities($this->base_URL . $current_offset . $dynamic_url);
                $start_URL = htmlentities($this->base_URL . "0" . $dynamic_url);
                $previous_URL = htmlentities($this->base_URL . $previous_offset . $dynamic_url);
                $next_URL = htmlentities($this->base_URL . $next_offset . $dynamic_url);
                $end_URL = htmlentities($this->base_URL . 'end' . $dynamic_url);

                if (!empty($this->start_link_wrapper)) {
                    $current_URL = $this->start_link_wrapper . $current_URL . $this->end_link_wrapper;
                    $start_URL = $this->start_link_wrapper . $start_URL . $this->end_link_wrapper;
                    $previous_URL = $this->start_link_wrapper . $previous_URL . $this->end_link_wrapper;
                    $next_URL = $this->start_link_wrapper . $next_URL . $this->end_link_wrapper;
                    $end_URL = $this->start_link_wrapper . $end_URL . $this->end_link_wrapper;
                }

                $moduleString = htmlspecialchars("{$currentModule}_{$html_varName}_offset");
                $moduleStringOrder = htmlspecialchars("{$currentModule}_{$html_varName}_ORDER_BY");
                if ($this->shouldProcess && !$this->multi_select_popup) {
                    // check the checkboxes onload
                    echo "<script>YAHOO.util.Event.addListener(window, \"load\", sListView.check_boxes);</script>\n";

                    $massUpdateRun = isset($_REQUEST['massupdate']) && $_REQUEST['massupdate'] == 'true';
                    $uids = empty($_REQUEST['uid']) || $massUpdateRun ? '' : $_REQUEST['uid'];
                    $select_entire_list = ($massUpdateRun) ? 0 : (isset($_POST['select_entire_list']) ? $_POST['select_entire_list'] : (isset($_REQUEST['select_entire_list']) ? htmlspecialchars($_REQUEST['select_entire_list']) : 0));

                    echo "<textarea style='display: none' name='uid'>{$uids}</textarea>\n" . "<input type='hidden' name='select_entire_list' value='{$select_entire_list}'>\n" . "<input type='hidden' name='{$moduleString}' value='0'>\n" . "<input type='hidden' name='{$moduleStringOrder}' value='0'>\n";
                }


                $GLOBALS['log']->debug("Offsets: (start, previous, next, last)(0, $previous_offset, $next_offset, $last_offset)");

                if (0 == $current_offset) {
                    $start_link = "<button type='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' class='button' disabled><span class='suitepicon suitepicon-action-first'></span></button>";
                    $previous_link = "<button type='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' class='button' disabled><span class='suitepicon suitepicon-action-left'></span></button>";
                } else {
                    if ($this->multi_select_popup) {// nav links for multiselect popup, submit form to save checks.
                        $start_link = "<button type='button' class='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' onClick='javascript:save_checks(0, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-first'></span></button>";
                        $previous_link = "<button type='button' class='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick='javascript:save_checks($previous_offset, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-left'></span></button>";
                    } elseif ($this->shouldProcess) {
                        $start_link = "<button type='button' class='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' onClick='location.href=\"$start_URL\"; sListView.save_checks(0, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-first'></span></button>";
                        $previous_link = "<button type='button' class='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick='location.href=\"$previous_URL\"; sListView.save_checks($previous_offset, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-left'></span></button>";
                    } else {
                        $onClick = '';
                        if (0 != preg_match('/javascript.*/', $start_URL)) {
                            $onClick = "\"$start_URL;\"";
                        } else {
                            $onClick = "'location.href=\"$start_URL\";'";
                        }
                        $start_link = "<button type='button' class='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' onClick=" . $onClick . "><span class='suitepicon suitepicon-action-first'></span></button>";

                        $onClick = '';
                        if (0 != preg_match('/javascript.*/', $previous_URL)) {
                            $onClick = "\"$previous_URL;\"";
                        } else {
                            $onClick = "'location.href=\"$previous_URL\";'";
                        }
                        $previous_link = "<button type='button' class='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick=" . $onClick . "><span class='suitepicon suitepicon-action-left'></span></button>";
                    }
                }

                if ($last_offset <= $current_offset) {
                    $end_link = "<button type='button' name='listViewEndButton' title='{$this->local_app_strings['LNK_LIST_END']}' class='button' disabled><span class='suitepicon suitepicon-action-last'></span></button>";
                    $next_link = "<button type='button' name='listViewNextButton' title='{$this->local_app_strings['LNK_LIST_NEXT']}' class='button' disabled><span class='suitepicon suitepicon-action-right'></span></button>";
                } else {
                    if ($this->multi_select_popup) { // nav links for multiselect popup, submit form to save checks.
                        $end_link = "<button type='button' name='listViewEndButton' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick='javascript:save_checks($last_offset, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-last'></span></button>";
                        if (!empty($sugar_config['disable_count_query'])) {
                            $end_link = '';
                        }
                        $next_link = "<button type='button' name='listViewNextButton' title='{$this->local_app_strings['LNK_LIST_NEXT']}' class='button' onClick='javascript:save_checks($next_offset, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-right'></span></button>";
                    } elseif ($this->shouldProcess) {
                        $end_link = "<button type='button' name='listViewEndButton' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick='location.href=\"$end_URL\"; sListView.save_checks(\"end\", \"{$moduleString}\");'><span class='suitepicon suitepicon-action-last'></span></button>";
                        $next_link = "<button type='button' name='listViewNextButton' class='button' title='{$this->local_app_strings['LNK_LIST_NEXT']}' onClick='location.href=\"$next_URL\"; sListView.save_checks($next_offset, \"{$moduleString}\");'><span class='suitepicon suitepicon-action-right'></span></button>";
                    } else {
                        $onClick = '';
                        if (0 != preg_match('/javascript.*/', $next_URL)) {
                            $onClick = "\"$next_URL;\"";
                        } else {
                            $onClick = "'location.href=\"$next_URL\";'";
                        }
                        $next_link = "<button type='button' name='listViewNextButton' class='button' title='{$this->local_app_strings['LNK_LIST_NEXT']}' onClick=" . $onClick . "><span class='suitepicon suitepicon-action-right'></span></button>";

                        $onClick = '';
                        if (0 != preg_match('/javascript.*/', $end_URL)) {
                            $onClick = "\"$end_URL;\"";
                        } else {
                            $onClick = "'location.href=\"$end_URL\";'";
                        }
                        $end_link = "<button type='button' name='listViewEndButton' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick=" . $onClick . "><span class='suitepicon suitepicon-action-last'></span></button>";
                    }
                }

                $GLOBALS['log']->info("Offset (next, current, prev)($next_offset, $current_offset, $previous_offset)");
                $GLOBALS['log']->info("Start/end records ($start_record, $end_record)");

                $end_record = $end_record - 1;

                $script_href = "<a style=\'width: 150px\' name=\"thispage\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'if (document.MassUpdate.select_entire_list.value==1){document.MassUpdate.select_entire_list.value=0;sListView.check_all(document.MassUpdate, \"mass[]\", true, $this->records_per_page)}else {sListView.check_all(document.MassUpdate, \"mass[]\", true)};\' href=\'#\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_CURRENT']}&nbsp;&#x28;{$this->records_per_page}&#x29;&#x200E;</a>" . "<a style=\'width: 150px\' name=\"selectall\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.check_entire_list(document.MassUpdate, \"mass[]\",true,{$row_count});\' href=\'#\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}&nbsp;&#x28;{$row_count}&#x29;&#x200E;</a>" . "<a style=\'width: 150px\' name=\"deselect\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.clear_all(document.MassUpdate, \"mass[]\", false);\' href=\'#\'>{$this->local_app_strings['LBL_LISTVIEW_NONE']}</a>";

                $close_inline_img = SugarThemeRegistry::current()->getImage('close_inline', 'border=0', null, null, ".gif", $app_strings['LBL_CLOSEINLINE']);

                echo "<script>
                function select_dialog() {
                	var \$dialog = \$('<div></div>')
					.html('<a style=\'width: 150px\' name=\"thispage\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'if (document.MassUpdate.select_entire_list.value==1){document.MassUpdate.select_entire_list.value=0;sListView.check_all(document.MassUpdate, \"mass[]\", true, $this->records_per_page)}else {sListView.check_all(document.MassUpdate, \"mass[]\", true)};\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_CURRENT']}&nbsp;&#x28;{$this->records_per_page}&#x29;&#x200E;</a>" . "<a style=\'width: 150px\' name=\"selectall\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.check_entire_list(document.MassUpdate, \"mass[]\",true,{$row_count});\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}&nbsp;&#x28;{$row_count}&#x29;&#x200E;</a>" . "<a style=\'width: 150px\' name=\"deselect\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.clear_all(document.MassUpdate, \"mass[]\", false);\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_NONE']}</a>')
					.dialog({
						autoOpen: false,
						width: 150
					});
					\$dialog.dialog('open');

                }
                </script>";

                if ($this->show_select_menu) {
                    $total_label = "";
                    $total = $row_count;
                    $pageTotal = ($row_count > 0) ? $end_record - $start_record + 1 : 0;
                    if (!empty($GLOBALS['sugar_config']['disable_count_query']) && $GLOBALS['sugar_config']['disable_count_query'] === true && $total > $pageTotal) {
                        $this->show_plus = true;
                        $total = $pageTotal;
                        $total_label = $total . '+';
                    } else {
                        $this->show_plus = false;
                        $total_label = $total;
                    }
                    echo "<input type='hidden' name='show_plus' value='{$this->show_plus}'>\n";

                    //Bug#52931: Replace with actionMenu
                    //$select_link = "<a id='select_link' onclick='return select_dialog();' href=\"javascript:void(0)\">".$this->local_app_strings['LBL_LINK_SELECT']."&nbsp;".SugarThemeRegistry::current()->getImage('MoreDetail', 'border=0', 11, 7, '.png', $app_strings['LBL_MOREDETAIL'])."</a>";
                    $menuItems = array(
                        "<input title=\"" . $app_strings['LBL_SELECT_ALL_TITLE'] . "\" type='checkbox' class='checkbox massall' name='massall' id='massall' value='' onclick='sListView.check_all(document.MassUpdate, \"mass[]\", this.checked);' /><a href='javascript: void(0);'></a>", "<a  name='thispage' id='button_select_this_page' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick='if (document.MassUpdate.select_entire_list.value==1){document.MassUpdate.select_entire_list.value=0;sListView.check_all(document.MassUpdate, \"mass[]\", true, $pageTotal)}else {sListView.check_all(document.MassUpdate, \"mass[]\", true)};' href='#'>{$app_strings['LBL_LISTVIEW_OPTION_CURRENT']}&nbsp;&#x28;{$pageTotal}&#x29;&#x200E;</a>", "<a  name='selectall' id='button_select_all' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick='sListView.check_entire_list(document.MassUpdate, \"mass[]\",true,{$total});' href='#'>{$app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}&nbsp;&#x28;{$total_label}&#x29;&#x200E;</a>", "<a name='deselect' id='button_deselect' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick='sListView.clear_all(document.MassUpdate, \"mass[]\", false);' href='#'>{$app_strings['LBL_LISTVIEW_NONE']}</a>",
                    );
                    require_once('include/Smarty/plugins/function.sugar_action_menu.php');
                    $select_link = smarty_function_sugar_action_menu(array(
                        'class' => 'clickMenu selectmenu', 'id' => 'selectLink', 'buttons' => $menuItems, 'flat' => false,
                    ), $this->smartyTemplate);
                } else {
                    $select_link = "&nbsp;";
                }

                $export_link = '<input class="button" type="button" value="' . $this->local_app_strings['LBL_EXPORT'] . '" ' . 'onclick="return sListView.send_form(true, \'' . $_REQUEST['module'] . '\', \'index.php?entryPoint=export\',\'' . $this->local_app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\')">';

                if ($this->show_delete_button) {
                    $delete_link = '<input class="button" type="button" id="delete_button" name="Delete" value="' . $this->local_app_strings['LBL_DELETE_BUTTON_LABEL'] . '" onclick="return sListView.send_mass_update(\'selected\',\'' . $this->local_app_strings['LBL_LISTVIEW_NO_SELECTED'] . '\', 1)">';
                } else {
                    $delete_link = '&nbsp;';
                }

                $admin = BeanFactory::newBean('Administration');
                $admin->retrieveSettings('system');

                $user_merge = $current_user->getPreference('mailmerge_on');
                if ($user_merge == 'on' && isset($admin->settings['system_mailmerge_on']) && $admin->settings['system_mailmerge_on']) {
                    echo "<script>
                function mailmerge_dialog(el) {
                   	var \$dialog = \$('<div></div>')
					.html('<a  class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'return sListView.send_form(true, \"MailMerge\", \"index.php\", \"{$this->local_app_strings['LBL_LISTVIEW_NO_SELECTED']}\")\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_SELECTED']}</a>"
                        . "<a  class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' href=\'index.php?action=index&module=MailMerge\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_CURRENT']}</a>"
                        . "<a  class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' href=\'index.php?action=index&module=MailMerge&entire=true\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}</a>')
					.dialog({
						autoOpen: false,
						title: '". $this->local_app_strings['LBL_MAILMERGE']."',
						width: 150,
						position: {
						    my: myPos,
						    at: atPos,
						    of: \$(el)
					 	}
					});

                }
            </script>";
                    $merge_link = "&nbsp;|&nbsp;<a id='mailmerge_link' onclick='return mailmerge_dialog(this)'; href=\"javascript:void(0)\">".$this->local_app_strings['LBL_MAILMERGE']."</a>";
                } else {
                    $merge_link = "&nbsp;";
                }

                $selected_objects_span = "&nbsp;|&nbsp;{$this->local_app_strings['LBL_LISTVIEW_SELECTED_OBJECTS']}<input  style='border: 0px; background: transparent; font-size: inherit; color: inherit' type='text' readonly name='selectCount[]' value='" . ((isset($_POST['mass'])) ? count($_POST['mass']) : 0) . "' />";

                if ($_REQUEST['module'] == 'Home' || $this->local_current_module == 'Import' || $this->show_export_button == false || (!empty($sugar_config['disable_export'])) || (!empty($sugar_config['admin_export_only']) && !(is_admin($current_user) || (ACLController::moduleSupportsACL($_REQUEST['module']) && ACLAction::getUserAccessLevel($current_user->id, $_REQUEST['module'], 'access') == ACL_ALLOW_ENABLED && (ACLAction::getUserAccessLevel($current_user->id, $_REQUEST['module'], 'admin') == ACL_ALLOW_ADMIN || ACLAction::getUserAccessLevel($current_user->id, $_REQUEST['module'], 'admin') == ACL_ALLOW_ADMIN_DEV))))) {
                    if ($_REQUEST['module'] != 'InboundEmail' && $_REQUEST['module'] != 'EmailMan' && $_REQUEST['module'] != 'iFrames') {
                        $selected_objects_span = '';
                    }
                    $export_link = "&nbsp;";
                    $merge_link = "&nbsp;";
                } elseif ($_REQUEST['module'] != "Accounts" && $_REQUEST['module'] != "Cases" && $_REQUEST['module'] != "Contacts" && $_REQUEST['module'] != "Leads" && $_REQUEST['module'] != "Opportunities") {
                    $merge_link = "&nbsp;";
                }

                if ($this->show_paging == true) {
                    if (!empty($sugar_config['disable_count_query'])) {
                        if ($row_count > $end_record) {
                            $row_count .= '+';
                        }
                    }

                    $html_text = '';
                    $html_text .= "<tr class='pagination' role='presentation'>\n";
                    $html_text .= "<td COLSPAN=\"$col_count\" align=\"right\">\n";
                    //$html_text .= "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\"><tr><td align=\"left\"  >$export_link$merge_link$selected_objects_span</td>\n";
                    //$html_text .= "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\"><tr><td align=\"left\"  >";
                    if ($subpanel_def != null) {
                        include_once('include/SubPanel/SubPanelTiles.php');
                        $subpanelTiles = new SubPanelTiles($sugarbean);
                        $html_text .= "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\"><tr><td align=\"left\"  >";

                        $html_text = $subpanelTiles->getCheckbox($html_text, $subpanel_def);

                        //attempt to get the query to recreate this subpanel
                        if (!empty($this->response)) {
                            $response =& $this->response;
                        } else {
                            $response = SugarBean::get_union_related_list($sugarbean, $this->sortby, $this->sort_order, $this->query_where, $current_offset, -1, $this->records_per_page, $this->query_limit, $subpanel_def);
                            $this->response = $response;
                        }
                        //if query is present, then pass it in as parameter
                        if (isset($response['query']) && !empty($response['query'])) {
                            $html_text .= $subpanelTiles->get_buttons($subpanel_def, $response['query']);
                        } else {
                            $html_text .= $subpanelTiles->get_buttons($subpanel_def);
                        }
                    } else {
                        $html_text .= "<table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\"><tr><td align=\"left\"  nowrap>$select_link&nbsp;$export_link&nbsp;$delete_link&nbsp;$selected_objects_span";
                    }
                    $html_text .= "</td>\n<td nowrap align=\"right\">" . $start_link . "&nbsp;&nbsp;" . $previous_link . "&nbsp;&nbsp;<span class='pageNumbers'>(" . $start_record . " - " . $end_record . " " . $this->local_app_strings['LBL_LIST_OF'] . " " . $row_count . ")</span>&nbsp;&nbsp;" . $next_link . "&nbsp;&nbsp;" . $end_link . "</td></tr></table>\n";
                    $html_text .= "</td>\n";
                    $html_text .= "</tr>\n";
                    $this->smartyTemplate->assign("PAGINATION", $html_text);
                }

                //C.L. - Fix for 23461
                if (empty($_REQUEST['action']) || $_REQUEST['action'] != 'Popup') {
                    $_SESSION['export_where'] = $this->query_where;
                }
            }
        } // end processListNavigation

        public function processOrderBy($html_varName)
        {
            if (!isset($this->base_URL)) {
                $this->base_URL = $_SERVER['PHP_SELF'];

                if (isset($_SERVER['QUERY_STRING'])) {
                    $this->base_URL = preg_replace("/\&" . $this->getSessionVariableName($html_varName, "ORDER_BY") . "=[0-9a-zA-Z\_\.]*/", "", $this->base_URL . '?' . $_SERVER['QUERY_STRING']);
                    $this->base_URL = preg_replace("/\&" . $this->getSessionVariableName($html_varName, "offset") . "=[0-9]*/", "", $this->base_URL);
                }
                if ($_SERVER['REQUEST_METHOD'] == 'POST') {
                    $this->base_URL .= '?';
                    if (isset($_REQUEST['action'])) {
                        $this->base_URL .= '&action=' . $_REQUEST['action'];
                    }
                    if (isset($_REQUEST['record'])) {
                        $this->base_URL .= '&record=' . $_REQUEST['record'];
                    }
                    if (isset($_REQUEST['module'])) {
                        $this->base_URL .= '&module=' . $_REQUEST['module'];
                    }
                }
                $this->base_URL .= "&" . $this->getSessionVariableName($html_varName, "offset") . "=";
            }

            if ($this->is_dynamic) {
                $this->base_URL .= '&to_pdf=true&action=SubPanelViewer&subpanel=' . $this->source_module;
            }

            //bug43465 start
            if (isset($this->appendToBaseUrl) && is_array($this->appendToBaseUrl)) {
                foreach ($this->appendToBaseUrl as $key => $value) {
                    $fullRequestString = $key . '=' . $value;

                    if ($this->base_URL == "/index.php") {
                        $this->base_URL .= "?";
                    } else {
                        if ($fullRequestString == substr($this->baseURL, '-' . strlen($fullRequestString))) {
                            $this->base_URL = preg_replace("/&" . $key . "\=.*/", "", $this->base_URL);
                        } else {
                            $this->base_URL = preg_replace("/&" . $key . "\=.*?&/", "&", $this->base_URL);
                        }
                        $this->base_URL .= "&";
                    }
                    if (!empty($value)) {
                        $this->base_URL .= "{$key}={$value}";
                    }
                }
            }
            //bug43465 end

            $sort_URL_base = $this->base_URL . "&" . $this->getSessionVariableName($html_varName, "ORDER_BY") . "=";

            if ($sort_URL_base !== "") {
                $this->smartyTemplate->assign("ORDER_BY", $sort_URL_base);

                return $sort_URL_base;
            } else {
                return '';
            }
        }
    }
