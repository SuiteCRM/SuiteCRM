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
 * Copyright (C) 2011 - 2020 SalesAgility Ltd.
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

require_once('include/EditView/SugarVCR.php');
/**
 * ListView - list of many objects
 * @api
 */
class ListView
{
    public $local_theme= null;
    public $local_app_strings= null;
    public $local_image_path = null;
    public $local_current_module = null;
    public $local_mod_strings = null;
    public $records_per_page = 20;
    public $xTemplate = null;
    public $xTemplatePath = null;
    public $seed_data = null;
    public $query_where = null;
    public $query_limit = -1;
    public $query_orderby = null;
    public $header_title = '';
    public $header_text = '';
    public $initialized = false;
    public $show_export_button = true;
    public $show_delete_button = true;
    public $show_select_menu = true;
    public $show_paging = true;
    public $show_mass_update = true;
    public $show_mass_update_form = true;
    public $query_where_has_changed = false;
    public $display_header_and_footer = true;
    public $baseURL = '';
    public $is_dynamic = false;
    public $inline = false;
    public $start_link_wrapper = '';
    public $end_link_wrapper = '';
    public $list_field_defs = array();

    public $shouldProcess = false;
    public $data_array;
    public $related_field_name = '';
    public $child_focus = '';
    public $layout_manager = null;
    public $process_for_popups = false;
    public $multi_select_popup=false;
    public $_additionalDetails = false;
    public $additionalDetailsFunction = null;
    public $sort_order = '';
    public $force_mass_update=false;
    public $keep_mass_update_form_open=false;
    public $ignorePopulateOnly = false;

    public function setDataArray($value)
    {
        $this->data_array = $value;
    }

    public function processListViewMulti($seed, $xTemplateSection, $html_varName)
    {
        $this->shouldProcess = true;

        echo "<form name='MassUpdate' method='post' action='index.php'>";
        $this->processListViewTwo($seed, $xTemplateSection, $html_varName);

        echo "<a href='javascript:" . ((!$this->multi_select_popup) ? 'sListView.' : ''). "check_all(document.MassUpdate, \"mass[]\", true)'>".translate('LBL_CHECKALL')."</a> - <a href='javascript:sListView.check_all(document.MassUpdate, \"mass[]\", false);'>".translate('LBL_CLEARALL')."</a>";
        echo '<br><br>';
    }


    public function processListView($seed, $xTemplateSection, $html_varName)
    {
        global $sugar_config;

        $populateOnly = $this->ignorePopulateOnly ? false : (!empty($sugar_config['save_query']) && $sugar_config['save_query'] == 'populate_only');
        if (isset($seed->module_dir) && $populateOnly) {
            if (empty($GLOBALS['displayListView']) && strcmp(strtolower($_REQUEST['action']), 'popup') != 0 && (!empty($_REQUEST['clear_query']) || $_REQUEST['module'] == $seed->module_dir && ((empty($_REQUEST['query']) || $_REQUEST['query'] == 'MSI')&& (empty($_SESSION['last_search_mod']) || $_SESSION['last_search_mod'] != $seed->module_dir)))) {
                $_SESSION['last_search_mod'] = $_REQUEST['module'] ;
                return;
            }
        }
        if (strcmp(strtolower($_REQUEST['action']), 'popup') != 0) {
            $_SESSION['last_search_mod'] = $_REQUEST['module'] ;
        }
        //following session variable will track the detail view navigation history.
        //needs to the reset after each search.
        $this->setLocalSessionVariable($html_varName, "DETAIL_NAV_HISTORY", false);

        require_once('include/MassUpdate.php');
        $mass = new MassUpdate();
        $add_acl_javascript = false;
        if (!isset($_REQUEST['action'])) {
            $this->shouldProcess=false;
        } else {
            $this->shouldProcess = is_subclass_of($seed, "SugarBean")
        && (($_REQUEST['action'] == 'index') || ('ListView' == substr($_REQUEST['action'], 0, 8)) /* cn: to include all ListViewXXX.php type views */)
        && ($_REQUEST['module'] == $seed->module_dir);
        }

        //when processing a multi-select popup.
        if ($this->process_for_popups && $this->multi_select_popup) {
            $this->shouldProcess =true;
        }
        //mass update turned off?
        if (!$this->show_mass_update) {
            $this->shouldProcess = false;
        }
        if (is_subclass_of($seed, "SugarBean")) {
            if ($seed->bean_implements('ACL')) {
                if (!ACLController::checkAccess($seed->module_dir, 'list', true)) {
                    if ($_REQUEST['module'] != 'Home') {
                        ACLController::displayNoAccess();
                    }
                    return;
                }
                if (!ACLController::checkAccess($seed->module_dir, 'export', true)) {
                    $sugar_config['disable_export']= true;
                }
            }
        }

        //force mass update form if requested.
        if ($this->force_mass_update) {
            $this->shouldProcess = true;
        }

        if ($this->shouldProcess) {
            echo $mass->getDisplayMassUpdateForm(true, $this->multi_select_popup);
            echo $mass->getMassUpdateFormHeader($this->multi_select_popup);
            $mass->setSugarBean($seed);

            //C.L. Fix for 10048, do not process handleMassUpdate for multi select popups
            if (!$this->multi_select_popup) {
                $mass->handleMassUpdate();
            }
        }

        $this->processListViewTwo($seed, $xTemplateSection, $html_varName);

        if ($this->shouldProcess && empty($this->process_for_popups)) {
            //echo "<a href='javascript:sListView.clear_all(document.MassUpdate, \"mass[]\");'>".translate('LBL_CLEARALL')."</a>";
            // cn: preserves current functionality, exception is InboundEmail
            if ($this->show_mass_update_form) {
                echo $mass->getMassUpdateForm();
            }
            if (!$this->keep_mass_update_form_open) {
                echo $mass->endMassUpdateForm();
            }
        }
    }


    public function process_dynamic_listview($source_module, $sugarbean, $subpanel_def)
    {
        $this->source_module = $source_module;
        $this->subpanel_module = $subpanel_def->name;
        if (!isset($this->xTemplate)) {
            $this->createXTemplate();
        }

        $html_var = $this->subpanel_module . "_CELL";

        $list_data = $this->processUnionBeans($sugarbean, $subpanel_def, $html_var);

        $list = $list_data['list'];
        $parent_data = $list_data['parent_data'];

        if ($subpanel_def->isCollection()) {
            $thepanel=$subpanel_def->get_header_panel_def();
        } else {
            $thepanel=$subpanel_def;
        }



        $this->process_dynamic_listview_header($thepanel->get_module_name(), $thepanel, $html_var);
        $this->process_dynamic_listview_rows($list, $parent_data, 'dyn_list_view', $html_var, $subpanel_def);

        if ($this->display_header_and_footer) {
            $this->getAdditionalHeader();
            if (!empty($this->header_title)) {
                echo get_form_header($this->header_title, $this->header_text, false);
            }
        }

        $this->xTemplate->out('dyn_list_view');

        if (isset($_SESSION['validation'])) {
            print base64_decode('PGEgaHJlZj0naHR0cDovL3d3dy5zdWdhcmNybS5jb20nPlBPV0VSRUQmbmJzcDtCWSZuYnNwO1NVR0FSQ1JNPC9hPg==');
        }
        if (isset($list_data['query'])) {
            return ($list_data['query']);
        }
    }

    /**
     * @return void
     * @param unknown $data
     * @param unknown $xTemplateSection
     * @param unknown $html_varName
     * @desc INTERNAL FUNCTION handles the rows
     */
    public function process_dynamic_listview_rows($data, $parent_data, $xtemplateSection, $html_varName, $subpanel_def)
    {
        global $subpanel_item_count;
        global $odd_bg;
        global $even_bg;
        global $hilite_bg;
        global $click_bg;

        $this->xTemplate->assign("BG_HILITE", $hilite_bg);
        $this->xTemplate->assign('CHECKALL', SugarThemeRegistry::current()->getImage('blank', '', 1, 1, ".gif", ''));
        //$this->xTemplate->assign("BG_CLICK", $click_bg);
        $subpanel_item_count = 0;
        $oddRow = true;
        $count = 0;
        reset($data);

        //GETTING OFFSET
        $offset = $this->getOffset($html_varName);
        //$totaltime = 0;
        $processed_ids = array();

        $fill_additional_fields = array();
        //Either retrieve the is_fill_in_additional_fields property from the lone
        //subpanel or visit each subpanel's subpanels to retrieve the is_fill_in_addition_fields
        //property
        $subpanel_list=array();
        if ($subpanel_def->isCollection()) {
            $subpanel_list=$subpanel_def->sub_subpanels;
        } else {
            $subpanel_list[]= $subpanel_def;
        }

        foreach ($subpanel_list as $this_subpanel) {
            if ($this_subpanel->is_fill_in_additional_fields()) {
                $fill_additional_fields[] = $this_subpanel->bean_name;
                $fill_additional_fields[$this_subpanel->bean_name] = true;
            }
        }

        if (empty($data)) {
            $this->xTemplate->assign("ROW_COLOR", 'oddListRow');
            $thepanel=$subpanel_def;
            if ($subpanel_def->isCollection()) {
                $thepanel=$subpanel_def->get_header_panel_def();
            }
            $this->xTemplate->assign("COL_COUNT", count($thepanel->get_list_fields()));
            $this->xTemplate->parse($xtemplateSection.".nodata");
        }
        foreach ($data as $aVal => $aItem) {
            $subpanel_item_count++;
            $aItem->check_date_relationships_load();
            // TODO: expensive and needs to be removed and done better elsewhere

            if (!empty($fill_additional_fields[$aItem->object_name])
        || ($aItem->object_name == 'Case' && !empty($fill_additional_fields['aCase']))
        ) {
                $aItem->fill_in_additional_list_fields();
                //$aItem->fill_in_additional_detail_fields();
            }
            //rrs bug: 25343
            $aItem->call_custom_logic("process_record");

            if (isset($parent_data[$aItem->id])) {
                $aItem->parent_name = $parent_data[$aItem->id]['parent_name'];
                if (!empty($parent_data[$aItem->id]['parent_name_owner'])) {
                    $aItem->parent_name_owner =  $parent_data[$aItem->id]['parent_name_owner'];
                    $aItem->parent_name_mod =  $parent_data[$aItem->id]['parent_name_mod'];
                }
            }
            $fields = $aItem->get_list_view_data();
            if (isset($processed_ids[$aItem->id])) {
                continue;
            } else {
                $processed_ids[$aItem->id] = 1;
            }


            //ADD OFFSET TO ARRAY
            $fields['OFFSET'] = ($offset + $count + 1);

            if ($this->shouldProcess) {
                if ($aItem->ACLAccess('EditView')) {
                    $this->xTemplate->assign('PREROW', "<input type='checkbox' class='checkbox' name='mass[]' value='". $fields['ID']. "' />");
                } else {
                    $this->xTemplate->assign('PREROW', '');
                }
                if ($aItem->ACLAccess('DetailView')) {
                    $this->xTemplate->assign('TAG_NAME', 'a');
                } else {
                    $this->xTemplate->assign('TAG_NAME', 'span');
                }
                $this->xTemplate->assign('CHECKALL', "<label class=\"hidden glyphicon bootstrap-checkbox glyphicon-unchecked\"><span class='suitepicon suitepicon-action-caret'></span></label><input type='checkbox'  title='".$GLOBALS['app_strings']['LBL_SELECT_ALL_TITLE']."' class='bootstrap-checkbox-hidden checkbox' name='massall' id='massall' value='' onclick='sListView.check_all(document.MassUpdate, \"mass[]\", this.checked);' />");
            }

            if ($oddRow) {
                $ROW_COLOR = 'oddListRow';
                $BG_COLOR =  $odd_bg;
            } else {
                $ROW_COLOR = 'evenListRow';
                $BG_COLOR =  $even_bg;
            }
            $oddRow = !$oddRow;
            $button_contents = array();
            $this->xTemplate->assign("ROW_COLOR", $ROW_COLOR);
            $this->xTemplate->assign("BG_COLOR", $BG_COLOR);
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
                $thepanel=$subpanel_def->sub_subpanels[$aItem->panel_name];
            } else {
                $thepanel=$subpanel_def;
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
            $linked_field=$thepanel->get_data_source_name();
            $linked_field_set=$thepanel->get_data_source_name(true);
            static $count;
            if (!isset($count)) {
                $count = 0;
            }
            /* BEGIN - SECURITY GROUPS */
            /**
        $field_acl['DetailView'] = $aItem->ACLAccess('DetailView');
        $field_acl['ListView'] = $aItem->ACLAccess('ListView');
        $field_acl['EditView'] = $aItem->ACLAccess('EditView');
        $field_acl['Delete'] = $aItem->ACLAccess('Delete');
            */
            //pass is_owner, in_group...vars defined above
            $field_acl['DetailView'] = $aItem->ACLAccess('DetailView', $aclaccess_is_owner, $aclaccess_in_group);
            $field_acl['ListView'] = $aItem->ACLAccess('ListView', $aclaccess_is_owner, $aclaccess_in_group);
            $field_acl['EditView'] = $aItem->ACLAccess('EditView', $aclaccess_is_owner, $aclaccess_in_group);
            $field_acl['Delete'] = $aItem->ACLAccess('Delete', $aclaccess_is_owner, $aclaccess_in_group);
            /* END - SECURITY GROUPS */
            foreach ($thepanel->get_list_fields() as $field_name=>$list_field) {
                //add linked field attribute to the array.
                $list_field['linked_field']=$linked_field;
                $list_field['linked_field_set']=$linked_field_set;

                $usage = empty($list_field['usage']) ? '' : $list_field['usage'];
                if ($usage == 'query_only' && !empty($list_field['force_query_only_display'])) {
                    //if you are here you have column that is query only but needs to be displayed as blank.  This is helpful
                    //for collections such as Activities where you have a field in only one object and wish to show it in the subpanel list
                    $count++;
                    $widget_contents = '&nbsp;';
                    $this->xTemplate->assign('CLASS', "");
                    $this->xTemplate->assign('CELL_COUNT', $count);
                    $this->xTemplate->assign('CELL', $widget_contents);
                    $this->xTemplate->parse($xtemplateSection.".row.cell");
                } else {
                    if ($usage != 'query_only') {
                        $list_field['name']=$field_name;

                        $module_field = $field_name.'_mod';
                        $owner_field = $field_name.'_owner';
                        if (!empty($aItem->$module_field)) {
                            $list_field['owner_id'] = $aItem->$owner_field;
                            $list_field['owner_module'] = $aItem->$module_field;
                        } else {
                            $list_field['owner_id'] = false;
                            $list_field['owner_module'] = false;
                        }
                        if (isset($list_field['alias'])) {
                            $list_field['name'] = $list_field['alias'];
                        } else {
                            $list_field['name']=$field_name;
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
                                $fieldType = isset($vardef['custom_type'])?$vardef['custom_type']:$vardef['type'];
                                $tmpField = SugarFieldHandler::getSugarField($fieldType, true);
                            } else {
                                $tmpField = null;
                            }

                            if ($tmpField != null) {
                                $widget_contents = SugarFieldHandler::displaySmarty($list_field['fields'], $vardef, 'ListView', $list_field);
                            } else {
                                // No SugarField for this particular type
                                // Use the old, icky, SugarWidget for now
                                $widget_contents = $layout_manager->widgetDisplay($list_field);
                            }

                            if (isset($list_field['widget_class']) && $list_field['widget_class'] == 'SubPanelDetailViewLink') {
                                // We need to call into the old SugarWidgets for the time being, so it can generate a proper link with all the various corner-cases handled
                                // So we'll populate the field data with the pre-rendered display for the field
                                $list_field['fields'][$field_name] = $widget_contents;
                                if ('full_name' == $field_name) {//bug #32465
                                    $list_field['fields'][strtoupper($field_name)] = $widget_contents;
                                }

                                //vardef source is non db, assign the field name to varname for processing of column.
                                if (!empty($vardef['source']) && $vardef['source']=='non-db') {
                                    $list_field['varname'] = $field_name;
                                }
                                $widget_contents = $layout_manager->widgetDisplay($list_field);
                            } else {
                                if (isset($list_field['widget_class']) && $list_field['widget_class'] == 'SubPanelEmailLink') {
                                    $widget_contents = $layout_manager->widgetDisplay($list_field);
                                }
                            }

                            $count++;
                            $this->xTemplate->assign('CELL_COUNT', $count);
                            $this->xTemplate->assign('CLASS', "");
                            if (empty($widget_contents)) {
                                $widget_contents = '&nbsp;';
                            }
                            $this->xTemplate->assign('CELL', $widget_contents);
                            $this->xTemplate->parse($xtemplateSection.".row.cell");
                        } else {
                            // This handles the edit and remove buttons and icon widget
                            if (isset($list_field['widget_class']) && $list_field['widget_class'] == "SubPanelIcon") {
                                $count++;
                                $widget_contents = $layout_manager->widgetDisplay($list_field);
                                $this->xTemplate->assign('CELL_COUNT', $count);
                                $this->xTemplate->assign('CLASS', "");
                                if (empty($widget_contents)) {
                                    $widget_contents = '&nbsp;';
                                }
                                $this->xTemplate->assign('CELL', $widget_contents);
                                $this->xTemplate->parse($xtemplateSection.".row.cell");
                            } elseif (preg_match("/button/i", $list_field['name'])) {
                                if ((($list_field['name'] === 'edit_button' && $field_acl['EditView']) || ($list_field['name'] === 'close_button' && $field_acl['EditView']) || ($list_field['name'] === 'remove_button' && $field_acl['Delete'])) && '' != ($_content = $layout_manager->widgetDisplay($list_field))) {
                                    $button_contents[] = $_content;
                                    unset($_content);
                                } else {
                                    $button_contents[] = '';
                                }
                            } else {
                                $count++;
                                $this->xTemplate->assign('CLASS', "");
                                $widget_contents = $layout_manager->widgetDisplay($list_field);
                                $this->xTemplate->assign('CELL_COUNT', $count);
                                if (empty($widget_contents)) {
                                    $widget_contents = '&nbsp;';
                                }
                                $this->xTemplate->assign('CELL', $widget_contents);
                                $this->xTemplate->parse($xtemplateSection.".row.cell");
                            }
                        }
                    }
                }
            }


            // Make sure we have at least one button before rendering a column for
            // the action buttons in a list view. Relevant bugs: #51647 and #51640.
            if (!empty($button_contents)) {
                $button_contents = array_filter($button_contents);
                if (!empty($button_contents)) {
                    // this is for inline buttons on listviews
                    // bug#51275: smarty widget to help provide the action menu functionality as it is currently sprinkled throughout the app with html
                    require_once('include/Smarty/plugins/function.sugar_action_menu.php');
                    $tempid = create_guid();
                    array_unshift($button_contents, "<div style='display: inline' id='$tempid'>" . array_shift($button_contents) . "</div>");
                    $action_button = smarty_function_sugar_action_menu(array(
                    'id' => $tempid,
                    'buttons' => $button_contents,
                    'class' => 'clickMenu subpanel records fancymenu button',
                    'flat' => false //assign flat value as false to display dropdown menu at any other preferences.
                ), $this->xTemplate);
                } else {
                    $action_button = '';
                }
                $this->xTemplate->assign('CLASS', "inlineButtons");
                $this->xTemplate->assign('CELL_COUNT', ++$count);
                //Bug#51275 for beta3 pre_script is not required any more
                $this->xTemplate->assign('CELL', $action_button);
                $this->xTemplate->parse($xtemplateSection . ".row.cell");
            }


            $aItem->setupCustomFields($aItem->module_dir);
            $aItem->custom_fields->populateAllXTPL($this->xTemplate, 'detail', $html_varName, $fields);

            $count++;

            $this->xTemplate->parse($xtemplateSection.".row");
        }

        $this->xTemplate->parse($xtemplateSection);
    }

    /**sets whether or not to display the xtemplate header and footer
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setDisplayHeaderAndFooter($bool)
    {
        $this->display_header_and_footer = $bool;
    }

    /**initializes ListView
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function __construct()
    {
        if (!$this->initialized) {
            global $sugar_config;
            $this->records_per_page = $sugar_config['list_max_entries_per_page'] + 0;
            $this->initialized = true;
            global $app_strings, $currentModule;
            $this->local_theme = (string)SugarThemeRegistry::current();
            $this->local_app_strings =$app_strings;
            $this->local_image_path = SugarThemeRegistry::current()->getImagePath();
            $this->local_current_module = $currentModule;
        }
    }

    /**sets how many records should be displayed per page in the list view
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setRecordsPerPage($count)
    {
        $this->records_per_page = $count;
    }
    /**sets the header title */
    public function setHeaderTitle($value)
    {
        $this->header_title = $value;
    }
    /**sets the header text this is text that's appended to the header table and is usually used for the creation of buttons
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setHeaderText($value)
    {
        $this->header_text = $value;
    }
    /**sets the path for the XTemplate HTML file to be used this is only needed to be set if you are allowing ListView to create the XTemplate
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setXTemplatePath($value)
    {
        $this->xTemplatePath= $value;
    }

    /**this is a helper function for allowing ListView to create a new XTemplate it groups parameters that should be set into a single function
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function initNewXTemplate($XTemplatePath, $modString, $imagePath = null)
    {
        $this->setXTemplatePath($XTemplatePath);
        if (isset($modString)) {
            $this->setModStrings($modString);
        }
        if (isset($imagePath)) {
            $this->setImagePath($imagePath);
        }
    }


    public function getOrderBy($varName, $defaultOrderBy='', $force_sortorder='')
    {
        $sortBy = $this->getSessionVariable($varName, "ORDER_BY") ;

        $orderByDirection = $this->getSessionVariableName($varName, "order_by_direction");
        $orderByColumn = $this->getSessionVariableName($varName, "ORDER_BY");
        $lastEqualsSortBy = false;
        $defaultOrder = false; //ascending

        if (empty($sortBy)) {
            $this->setUserVariable($varName, "ORDER_BY", $defaultOrderBy);
            $sortBy = $defaultOrderBy;
        } else {
            $this->setUserVariable($varName, "ORDER_BY", $sortBy);
        }

        $desc = $this->getSessionVariable($varName, $sortBy."S");

        if (empty($desc)) {
            $desc = $defaultOrder;
        }
        $defaultOrder = $desc ? 'desc' : 'asc';
        $orderByValue = $defaultOrder;
        if (isset($_REQUEST[$orderByDirection])) {
            $possibleRequestOrderBy = $_REQUEST[$orderByDirection];
            if ($possibleRequestOrderBy == 'asc' || $possibleRequestOrderBy == 'desc') {
                $orderByValue = $possibleRequestOrderBy;
            }
        }

        if (isset($_REQUEST[$orderByColumn])) {
            $last = $this->getSessionVariable($varName, "OBL");
        }
        if (!empty($last) && $last == $sortBy) {
            $lastEqualsSortBy = true;
        } else {
            $orderByValue = $defaultOrder;
            $this->setSessionVariable($varName, "OBL", $sortBy);
        }
        $desc = $orderByValue == 'desc';
        $orderByDirectionValue = false;
        $this->setSessionVariable($varName, $sortBy . "S", $desc);
        if (!empty($sortBy)) {
            if (empty($force_sortorder)) {
                if (substr_count(strtolower($sortBy), ' desc') == 0 && substr_count(strtolower($sortBy), ' asc') == 0) {
                    if ($sortBy) {
                        $orderByDirectionValue = $desc ? 'asc' : 'desc';
                    }
                    $this->query_orderby = $sortBy . ' ' . $orderByValue;
                }
            } else {
                $this->query_orderby = $sortBy . ' ' . $force_sortorder;
            }
            if (!isset($this->appendToBaseUrl)) {
                $this->appendToBaseUrl = array();
            }
            if ($orderByDirectionValue) {
                $this->appendToBaseUrl[$orderByDirection] = $orderByDirectionValue;
            }
            $offsetVar = $this->getSessionVariableName($varName, "offset");
            if (isset($_REQUEST[$offsetVar])) {
                $this->appendToBaseUrl[$offsetVar] = $_REQUEST[$offsetVar];
            }
            //Just clear from url...
            $this->appendToBaseUrl[$orderByColumn] = false;
        } else {
            $this->query_orderby = "";
        }
        $this->sortby = $sortBy;
        return $this->query_orderby;
    }


    /**sets the parameters dealing with the db
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setQuery($where, $limit, $orderBy, $varName, $allowOrderByOveride=true)
    {
        $this->query_where = $where;
        if ($this->getSessionVariable("query", "where") != $where) {
            $this->query_where_has_changed = true;
            $this->setSessionVariable("query", "where", $where);
        }

        $this->query_limit = $limit;
        if (!$allowOrderByOveride) {
            $this->query_orderby = $orderBy;
            return;
        }
        $this->getOrderBy($varName, $orderBy);

        $this->setLocalSessionVariable($varName, "QUERY_WHERE", $where);

        //SETTING ORDER_BY FOR USE IN DETAILVIEW
        $this->setLocalSessionVariable($varName, "ORDER_BY_DETAIL", $this->query_orderby);
    }

    public function displayArrow()
    {
    }

    /**sets the theme used only use if it is different from the global
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setTheme($theme)
    {
        $this->local_theme = $theme;
        if (isset($this->xTemplate)) {
            $this->xTemplate->assign("THEME", $this->local_theme);
        }
    }

    /**sets the AppStrings used only use if it is different from the global
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setAppStrings($app_strings)
    {
        unset($this->local_app_strings);
        $this->local_app_strings = $app_strings;
        if (isset($this->xTemplate)) {
            $this->xTemplate->assign("APP", $this->local_app_strings);
        }
    }

    /**sets the ModStrings used
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setModStrings($mod_strings)
    {
        unset($this->local_module_strings);
        $this->local_mod_strings = $mod_strings;
        if (isset($this->xTemplate)) {
            $this->xTemplate->assign("MOD", $this->local_mod_strings);
        }
    }

    /**sets the ImagePath used
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setImagePath($image_path)
    {
        $this->local_image_path = $image_path;
        if (empty($this->local_image_path)) {
            $this->local_image_path = SugarThemeRegistry::get($this->local_theme)->getImagePath();
        }
        if (isset($this->xTemplate)) {
            $this->xTemplate->assign("IMAGE_PATH", $this->local_image_path);
        }
    }

    /**sets the currentModule only use if this is different from the global
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setCurrentModule($currentModule)
    {
        unset($this->local_current_module);
        $this->local_current_module = $currentModule;
        if (isset($this->xTemplate)) {
            $this->xTemplate->assign("MODULE_NAME", $this->local_current_module);
        }
    }

    /**INTERNAL FUNCTION creates an XTemplate DO NOT CALL THIS THIS IS AN INTERNAL FUNCTION
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function createXTemplate()
    {
        if (!isset($this->xTemplate)) {
            if (isset($this->xTemplatePath)) {
                $this->xTemplate = new XTemplate($this->xTemplatePath);
                $this->xTemplate->assign("APP", $this->local_app_strings);
                if (isset($this->local_mod_strings)) {
                    $this->xTemplate->assign("MOD", $this->local_mod_strings);
                }
                $this->xTemplate->assign("THEME", $this->local_theme);
                $this->xTemplate->assign("IMAGE_PATH", $this->local_image_path);
                $this->xTemplate->assign("MODULE_NAME", $this->local_current_module);
            } else {
                $GLOBALS['log']->error("NO XTEMPLATEPATH DEFINED CANNOT CREATE XTEMPLATE");
            }
        }
    }

    /**sets the XTemplate telling ListView to use newXTemplate as its current XTemplate
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setXTemplate($newXTemplate)
    {
        $this->xTemplate = $newXTemplate;
    }

    /**returns the XTemplate
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function getXTemplate()
    {
        return $this->xTemplate;
    }

    /**assigns a name value pair to the XTemplate
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function xTemplateAssign($name, $value)
    {
        if (!isset($this->xTemplate)) {
            $this->createXTemplate();
        }
        $this->xTemplate->assign($name, $value);
    }

    /**
     * INTERNAL FUNCTION returns the offset first checking the query then checking the session if the where clause has changed from the last time it returns 0
     * @param $localVarName
     * @return int
     */
    public function getOffset($localVarName)
    {
        if ($this->query_where_has_changed || isset($GLOBALS['record_has_changed'])) {
            $this->setSessionVariable($localVarName, 'offset', 0);
        }
        // this might return several kinds of values: 0, '', 'end', etc
        $offset = $this->getSessionVariable($localVarName, 'offset');
        if (isset($offset) && ($offset !== '')) {
            return $offset;
        }

        return 0;
    }

    /**INTERNAL FUNCTION sets the offset in the session
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setOffset($localVarName, $value)
    {
        $this->setSessionVariable($localVarName, "offset", $value);
    }

    /**INTERNAL FUNCTION sets a session variable
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function setSessionVariable($localVarName, $varName, $value)
    {
        $_SESSION[$this->local_current_module."_".$localVarName."_".$varName] = $value;
    }

    public function setUserVariable($localVarName, $varName, $value)
    {
        if ($this->is_dynamic ||  $localVarName == 'CELL') {
            return;
        }
        global $current_user;
        $current_user->setPreference($this->local_current_module."_".$localVarName."_".$varName, $value);
    }

    /**INTERNAL FUNCTION returns a session variable first checking the query for it then checking the session
     * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
     * All Rights Reserved.
     * Contributor(s): ______________________________________.
    */
    public function getSessionVariable($localVarName, $varName)
    {
        //Set any variables pass in through request first
        if (isset($_REQUEST[$this->getSessionVariableName($localVarName, $varName)])) {
            $this->setSessionVariable($localVarName, $varName, $_REQUEST[$this->getSessionVariableName($localVarName, $varName)]);
        }

        if (isset($_SESSION[$this->getSessionVariableName($localVarName, $varName)])) {
            return $_SESSION[$this->getSessionVariableName($localVarName, $varName)];
        }
        return '';
    }

    public function getUserVariable($localVarName, $varName)
    {
        global $current_user;
        if ($this->is_dynamic ||  $localVarName == 'CELL') {
            return;
        }
        if (isset($_REQUEST[$this->getSessionVariableName($localVarName, $varName)])) {
            $this->setUserVariable($localVarName, $varName, $_REQUEST[$this->getSessionVariableName($localVarName, $varName)]);
        }
        return $current_user->getPreference($this->getSessionVariableName($localVarName, $varName));
    }


    /**
     * helper method to determine sort order by priority of source
     * 1. explicit in request object
     * 2. in session variable
     * 3. subpaneldefs metadata
     * 4. default 'asc'
     * @param array $sortOrderList - contains options
     * @return string 'asc' | 'desc'
     */
    public function calculateSortOrder($sortOrderList)
    {
        $priority_map = array(
          'request',
          'session',
          'subpaneldefs',
          'default',
        );

        foreach ($priority_map as $p) {
            if (array_key_exists($p, $sortOrderList)) {
                $order = strtolower($sortOrderList[$p] ?? '');
                if (in_array($order, array('asc', 'desc'))) {
                    return $order;
                }
            }
        }
        return 'asc';
    }


    /**

    * @return void
    * @param unknown $localVarName
    * @param unknown $varName
    * @desc INTERNAL FUNCTION returns the session/query variable name
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
    * All Rights Reserved.
    * Contributor(s): ______________________________________..
    */
    public function getSessionVariableName($localVarName, $varName)
    {
        return $this->local_current_module."_".$localVarName."_".$varName;
    }

    /**

    * @return void
    * @param unknown $seed
    * @param unknown $xTemplateSection
    * @param unknown $html_varName
    * @desc INTERNAL FUNCTION Handles List Views using seeds that extend SugarBean
        $XTemplateSection is the section in the XTemplate file that should be parsed usually main
        $html_VarName is the variable name used in the XTemplateFile e.g. TASK
        $seed is a seed that extends SugarBean
        * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
        * All Rights Reserved..
        * Contributor(s): ______________________________________..
    */
    public function processSugarBean($xtemplateSection, $html_varName, $seed)
    {
        global $list_view_row_count;

        $current_offset = $this->getOffset($html_varName);
        $response = array();

        //ADDING VCR CONTROL
        SugarVCR::erase($seed->module_dir);
        $params = array();
        //$filter = array('id', 'full_name');
        $filter=array();
        $ret_array = $seed->create_new_list_query($this->query_orderby, $this->query_where, $filter, $params, 0, '', true, $seed, true);
        if (!is_array($params)) {
            $params = array();
        }
        if (!isset($params['custom_select'])) {
            $params['custom_select'] = '';
        }
        if (!isset($params['custom_from'])) {
            $params['custom_from'] = '';
        }
        if (!isset($params['custom_where'])) {
            $params['custom_where'] = '';
        }
        if (!isset($params['custom_order_by'])) {
            $params['custom_order_by'] = '';
        }
        $main_query = $ret_array['select'] . $params['custom_select'] . $ret_array['from'] . $params['custom_from'] . $ret_array['where'] . $params['custom_where'] . $ret_array['order_by'] . $params['custom_order_by'];
        SugarVCR::store($seed->module_dir, $main_query);
        //ADDING VCR CONTROL

        if (empty($this->related_field_name)) {
            $response = $seed->get_list($this->query_orderby, $this->query_where, $current_offset, $this->query_limit);
        } else {
            $related_field_name = $this->related_field_name;
            $response = $seed->get_related_list(
                $this->child_focus,
                $related_field_name,
                $this->query_orderby,
                $this->query_where,
                $current_offset,
                $this->query_limit
            );
        }

        $list = $response['list'];
        $row_count = $response['row_count'];
        $next_offset = $response['next_offset'];
        $previous_offset = $response['previous_offset'];

        if (!empty($response['current_offset'])) {
            $current_offset = $response['current_offset'];
        }

        $list_view_row_count = $row_count;
        $this->processListNavigation($xtemplateSection, $html_varName, $current_offset, $next_offset, $previous_offset, $row_count, null, null, empty($seed->column_fields) ? null : count($seed->column_fields));

        return $list;
    }



    public function processUnionBeans($sugarbean, $subpanel_def, $html_var = 'CELL', $countOnly = false)
    {
        $last_detailview_record = $this->getSessionVariable("detailview", "record");
        if (!empty($last_detailview_record) && $last_detailview_record != $sugarbean->id) {
            $GLOBALS['record_has_changed'] = true;
        }
        $this->setSessionVariable("detailview", "record", $sugarbean->id);

        $last_detailview_record = $this->getSessionVariable("detailview", "record");
        if (!empty($last_detailview_record) && $last_detailview_record != $sugarbean->id) {
            $GLOBALS['record_has_changed'] = true;
        }
        $this->setSessionVariable("detailview", "record", $sugarbean->id);

        $current_offset = $this->getOffset($html_var);
        $module = isset($_REQUEST['module']) ? $_REQUEST['module'] : '';
        $response = array();

        // choose sort order
        $sort_order = array();
        $sort_order['default'] = 'asc';

        // explicit request parameter gets priority over all
        $sort_order['request'] = isset($_REQUEST['sort_order']) ? $_REQUEST['sort_order'] : null;

        // see if the session data has a sort order
        if (isset($_SESSION['last_sub' . $this->subpanel_module . '_order'])) {
            $sort_order['session'] = $_SESSION['last_sub' . $this->subpanel_module . '_order'];

            // We swap the order when the request contains an offset (indicating a column sort issued);
            // otherwise we do not sort.  If we don't make this check, then the subpanel listview will
            // swap ordering each time a new record is entered via quick create forms
            if (isset($_REQUEST[$module . '_' . $html_var . '_offset'])) {
                $sort_order['session'] = $sort_order['session'] == 'asc' ? 'desc' : 'asc';
            }
        } else {
            $sort_order['session'] = null;
        }

        // does the metadata have a default sort order?
        $sort_order['subpaneldefs'] = isset($subpanel_def->_instance_properties['sort_order']) ?
            $subpanel_def->_instance_properties['sort_order'] : null;

        $this->sort_order = $this->calculateSortOrder($sort_order);


        if (isset($subpanel_def->_instance_properties['sort_by'])) {
            $this->query_orderby = $subpanel_def->_instance_properties['sort_by'];
        } else {
            $this->query_orderby = 'id';
        }

        $this->getOrderBy($html_var, $this->query_orderby, $this->sort_order);

        $_SESSION['last_sub' .$this->subpanel_module. '_order'] = $this->sort_order;
        $_SESSION['last_sub' .$this->subpanel_module. '_url'] = $this->getBaseURL($html_var);

        // Bug 8139 - Correct Subpanel sorting on 'name', when subpanel sorting default is 'last_name, first_name'
        $sortBy = $subpanel_def->_instance_properties['sort_by'] ?? '';
        if (($this->sortby == 'name' || $this->sortby == 'last_name') &&
            str_replace(' ', '', trim($sortBy)) == 'last_name,first_name') {
            $this->sortby = 'last_name '.$this->sort_order.', first_name ';
        }
        try {
            if (!empty($this->response)) {
                $response =& $this->response;
                echo 'cached';
            } else {
                $response = SugarBean::get_union_related_list(
                    $sugarbean,
                    $this->sortby,
                    $this->sort_order,
                    $this->query_where,
                    $current_offset,
                    -1,
                    $this->records_per_page,
                    $this->query_limit,
                    $subpanel_def
                );
                $this->response =& $response;
            }
        } catch (Exception $ex) {
            LoggerManager::getLogger()->fatal('[' . __METHOD__ . "] . {$ex->getMessage()}");

            return ['list' => [], 'parent_data' => [], 'query' => ''];
        }
        $list = $response['list'];

        if (!$countOnly) {
            $row_count = $response['row_count'];
            $next_offset = $response['next_offset'];
            $previous_offset = $response['previous_offset'];
            if (!empty($response['current_offset'])) {
                $current_offset = $response['current_offset'];
            }
            global $list_view_row_count;
            $list_view_row_count = $row_count;
            $this->processListNavigation('dyn_list_view', $html_var, $current_offset, $next_offset, $previous_offset, $row_count, $sugarbean, $subpanel_def);
        }

        return $response;
    }

    public function getBaseURL($html_varName)
    {
        static $cache = array();

        if (!empty($cache[$html_varName])) {
            return $cache[$html_varName];
        }
        $blockVariables = array('mass', 'uid', 'massupdate', 'delete', 'merge', 'selectCount','current_query_by_page');
        if (!empty($this->base_URL)) {
            return $this->base_URL;
        }

        $baseurl = $_SERVER['PHP_SELF'];
        if (empty($baseurl)) {
            $baseurl = 'index.php';
        }

        /*fixes an issue with deletes when doing a search*/
        foreach (array_merge($_GET, $_POST) as $name=>$value) {
            //echo ("$name = $value <br/>");
                if (!empty($value) && $name != 'sort_order' //&& $name != ListView::getSessionVariableName($html_varName,"ORDER_BY")
                        && $name != ListView::getSessionVariableName($html_varName, "offset")
                        /*&& substr_count($name, "ORDER_BY")==0*/ && !in_array($name, $blockVariables)) {
                    if (is_array($value)) {
                        foreach ($value as $valuename=>$valuevalue) {
                            if (substr_count($baseurl, '?') > 0) {
                                $baseurl	.= "&{$name}[]=".$valuevalue;
                            } else {
                                $baseurl	.= "?{$name}[]=".$valuevalue;
                            }
                        }
                    } else {
                        $value = urlencode($value);
                        if (substr_count($baseurl, '?') > 0) {
                            $baseurl	.= "&$name=$value";
                        } else {
                            $baseurl	.= "?$name=$value";
                        }
                    }
                }
        }


        if ($_SERVER['REQUEST_METHOD'] == 'POST') {
            // at this point it is possible that the above foreach already executed resulting in double ?'s in the url
            if (substr_count($baseurl, '?') == 0) {
                $baseurl .= '?';
            }
            if (isset($_REQUEST['action'])) {
                $baseurl.= '&action='.$_REQUEST['action'];
            }
            if (isset($_REQUEST['record'])) {
                $baseurl .= '&record='.$_REQUEST['record'];
            }
            if (isset($_REQUEST['module'])) {
                $baseurl .= '&module='.$_REQUEST['module'];
            }
        }

        $baseurl .= "&".ListView::getSessionVariableName($html_varName, "offset")."=";
        $cache[$html_varName] = $baseurl;
        return $baseurl;
    }
    /**
    * @return void
    * @param unknown $data
    * @param unknown $xTemplateSection
    * @param unknown $html_varName
    * @desc INTERNAL FUNCTION process the List Navigation
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
    * All Rights Reserved.
    * Contributor(s): ______________________________________..
    */
    public function processListNavigation($xtemplateSection, $html_varName, $current_offset, $next_offset, $previous_offset, $row_count, $sugarbean=null, $subpanel_def=null, $col_count = 20)
    {
        global $export_module;
        global $sugar_config;
        global $current_user;
        global $currentModule;
        global $app_strings;

        $start_record = (int)$current_offset + 1;

        if (!is_numeric($col_count)) {
            $col_count = 20;
        }

        if ($row_count == 0) {
            $start_record = 0;
        }

        $end_record = $start_record + $this->records_per_page;
        // back up the last page.
        if ($end_record > $row_count+1) {
            $end_record = $row_count+1;
        }
        // Determine the start location of the last page
        if ($row_count == 0) {
            $number_pages = 0;
        } else {
            $number_pages = floor(($row_count - 1) / $this->records_per_page);
        }

        $last_offset = $number_pages * $this->records_per_page;

        if (empty($this->query_limit)  || $this->query_limit > $this->records_per_page) {
            $this->base_URL = $this->getBaseURL($html_varName);
            $dynamic_url = '';

            if ($this->is_dynamic) {
                $dynamic_url .='&'. $this->getSessionVariableName($html_varName, 'ORDER_BY') . '='. $this->getSessionVariable($html_varName, 'ORDER_BY').'&sort_order='.$this->sort_order.'&to_pdf=true&action=SubPanelViewer&subpanel=' . $this->subpanel_module;
            }

            $current_URL = htmlentities($this->base_URL.$current_offset.$dynamic_url);
            $start_URL = htmlentities($this->base_URL."0".$dynamic_url);
            $previous_URL  = htmlentities($this->base_URL.$previous_offset.$dynamic_url);
            $next_URL  = htmlentities($this->base_URL.$next_offset.$dynamic_url);
            $end_URL  = htmlentities($this->base_URL.'end'.$dynamic_url);

            if (!empty($this->start_link_wrapper)) {
                $current_URL = $this->start_link_wrapper.$current_URL.$this->end_link_wrapper;
                $start_URL = $this->start_link_wrapper.$start_URL.$this->end_link_wrapper;
                $previous_URL = $this->start_link_wrapper.$previous_URL.$this->end_link_wrapper;
                $next_URL = $this->start_link_wrapper.$next_URL.$this->end_link_wrapper;
                $end_URL = $this->start_link_wrapper.$end_URL.$this->end_link_wrapper;
            }

            $moduleString = htmlspecialchars("{$currentModule}_{$html_varName}_offset");
            $moduleStringOrder = htmlspecialchars("{$currentModule}_{$html_varName}_ORDER_BY");
            if ($this->shouldProcess && !$this->multi_select_popup) {
                // check the checkboxes onload
                echo "<script>YAHOO.util.Event.addListener(window, \"load\", sListView.check_boxes);</script>\n";

                $massUpdateRun = isset($_REQUEST['massupdate']) && $_REQUEST['massupdate'] == 'true';
                $uids = empty($_REQUEST['uid']) || $massUpdateRun ? '' : $_REQUEST['uid'];
                $select_entire_list = ($massUpdateRun) ? 0 : (isset($_POST['select_entire_list']) ? $_POST['select_entire_list'] : (isset($_REQUEST['select_entire_list']) ? htmlspecialchars($_REQUEST['select_entire_list']) : 0));

                echo "<textarea style='display: none' name='uid'>{$uids}</textarea>\n" .
                    "<input type='hidden' name='select_entire_list' value='{$select_entire_list}'>\n".
                    "<input type='hidden' name='{$moduleString}' value='0'>\n".
                    "<input type='hidden' name='{$moduleStringOrder}' value='0'>\n";
            }


            $GLOBALS['log']->debug("Offsets: (start, previous, next, last)(0, $previous_offset, $next_offset, $last_offset)");

            if (0 == $current_offset) {
                $start_link = "<button type='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("start_off", "aborder='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_START'])."</button>";
                $previous_link = "<button type='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("previous_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
            } else {
                if ($this->multi_select_popup) {// nav links for multiselect popup, submit form to save checks.
                    $start_link = "<button type='button' class='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' onClick='javascript:save_checks(0, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("start", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_START'])."</button>";
                    $previous_link = "<button type='button' class='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick='javascript:save_checks($previous_offset, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("previous", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
                } elseif ($this->shouldProcess) {
                    $start_link = "<button type='button' class='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' onClick='location.href=\"$start_URL\"; sListView.save_checks(0, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("start", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_START'])."</button>";
                    $previous_link = "<button type='button' class='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick='location.href=\"$previous_URL\"; sListView.save_checks($previous_offset, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("previous", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
                } else {
                    $onClick = '';
                    if (0 != preg_match('/javascript.*/', $start_URL)) {
                        $onClick = "\"$start_URL;\"";
                    } else {
                        $onClick ="'location.href=\"$start_URL\";'";
                    }
                    $start_link = "<button type='button' class='button' name='listViewStartButton' title='{$this->local_app_strings['LNK_LIST_START']}' onClick=".$onClick.">".SugarThemeRegistry::current()->getImage("start", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_START'])."</button>";

                    $onClick = '';
                    if (0 != preg_match('/javascript.*/', $previous_URL)) {
                        $onClick = "\"$previous_URL;\"";
                    } else {
                        $onClick = "'location.href=\"$previous_URL\";'";
                    }
                    $previous_link = "<button type='button' class='button' name='listViewPrevButton' title='{$this->local_app_strings['LNK_LIST_PREVIOUS']}' onClick=".$onClick.">".SugarThemeRegistry::current()->getImage("previous", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_PREVIOUS'])."</button>";
                }
            }

            if ($last_offset <= $current_offset) {
                $end_link = "<button type='button' name='listViewEndButton' title='{$this->local_app_strings['LNK_LIST_END']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("end_off", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_END'])."</button>";
                $next_link = "<button type='button' name='listViewNextButton' title='{$this->local_app_strings['LNK_LIST_NEXT']}' class='button' disabled>".SugarThemeRegistry::current()->getImage("next_off", "aborder='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_NEXT'])."</button>";
            } else {
                if ($this->multi_select_popup) { // nav links for multiselect popup, submit form to save checks.
                    $end_link = "<button type='button' name='listViewEndButton' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick='javascript:save_checks($last_offset, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("end", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_END'])."</button>";
                    if (!empty($sugar_config['disable_count_query'])) {
                        $end_link = '';
                    }
                    $next_link = "<button type='button' name='listViewNextButton' title='{$this->local_app_strings['LNK_LIST_NEXT']}' class='button' onClick='javascript:save_checks($next_offset, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("next", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_NEXT'])."</button>";
                } elseif ($this->shouldProcess) {
                    $end_link = "<button type='button' name='listViewEndButton' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick='location.href=\"$end_URL\"; sListView.save_checks(\"end\", \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("end", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_END'])."</button>";
                    $next_link = "<button type='button' name='listViewNextButton' class='button' title='{$this->local_app_strings['LNK_LIST_NEXT']}' onClick='location.href=\"$next_URL\"; sListView.save_checks($next_offset, \"{$moduleString}\");'>".SugarThemeRegistry::current()->getImage("next", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_NEXT'])."</button>";
                } else {
                    $onClick = '';
                    if (0 != preg_match('/javascript.*/', $next_URL)) {
                        $onClick = "\"$next_URL;\"";
                    } else {
                        $onClick ="'location.href=\"$next_URL\";'";
                    }
                    $next_link = "<button type='button' name='listViewNextButton' class='button' title='{$this->local_app_strings['LNK_LIST_NEXT']}' onClick=".$onClick.">".SugarThemeRegistry::current()->getImage("next", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_NEXT'])."</button>";

                    $onClick = '';
                    if (0 != preg_match('/javascript.*/', $end_URL)) {
                        $onClick = "\"$end_URL;\"";
                    } else {
                        $onClick = "'location.href=\"$end_URL\";'";
                    }
                    $end_link = "<button type='button' name='listViewEndButton' class='button' title='{$this->local_app_strings['LNK_LIST_END']}' onClick=".$onClick.">".SugarThemeRegistry::current()->getImage("end", "border='0' align='absmiddle'", null, null, '.gif', $this->local_app_strings['LNK_LIST_END'])."</button>";
                }
            }

            $GLOBALS['log']->info("Offset (next, current, prev)($next_offset, $current_offset, $previous_offset)");
            $GLOBALS['log']->info("Start/end records ($start_record, $end_record)");

            $end_record = $end_record-1;

            $script_href = "<a  name=\"thispage\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'if (document.MassUpdate.select_entire_list.value==1){document.MassUpdate.select_entire_list.value=0;sListView.check_all(document.MassUpdate, \"mass[]\", true, $this->records_per_page)}else {sListView.check_all(document.MassUpdate, \"mass[]\", true)};\' href=\'#\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_CURRENT']}&nbsp;&#x28;{$this->records_per_page}&#x29;&#x200E;</a>"
 . "<a  name=\"selectall\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.check_entire_list(document.MassUpdate, \"mass[]\",true,{$row_count});\' href=\'#\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}&nbsp;&#x28;{$row_count}&#x29;&#x200E;</a>"
 . "<a  name=\"deselect\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.clear_all(document.MassUpdate, \"mass[]\", false);\' href=\'#\'>{$this->local_app_strings['LBL_LISTVIEW_NONE']}</a>";

            $close_inline_img = SugarThemeRegistry::current()->getImage('close_inline', 'border=0', null, null, ".gif", $app_strings['LBL_CLOSEINLINE']);

            echo "<script>
                function select_dialog() {
                	var \$dialog = \$('<div></div>')
					.html('<a  name=\"thispage\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'if (document.MassUpdate.select_entire_list.value==1){document.MassUpdate.select_entire_list.value=0;sListView.check_all(document.MassUpdate, \"mass[]\", true, $this->records_per_page)}else {sListView.check_all(document.MassUpdate, \"mass[]\", true)};\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_CURRENT']}&nbsp;&#x28;{$this->records_per_page}&#x29;&#x200E;</a>"
                . "<a  name=\"selectall\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.check_entire_list(document.MassUpdate, \"mass[]\",true,{$row_count});\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}&nbsp;&#x28;{$row_count}&#x29;&#x200E;</a>"
                . "<a  name=\"deselect\" class=\'menuItem\' onmouseover=\'hiliteItem(this,\"yes\");\' onmouseout=\'unhiliteItem(this);\' onclick=\'sListView.clear_all(document.MassUpdate, \"mass[]\", false);\' href=\'javascript:void(0)\'>{$this->local_app_strings['LBL_LISTVIEW_NONE']}</a>')
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
                    $total =  $pageTotal;
                    $total_label = $total.'+';
                } else {
                    $this->show_plus = false;
                    $total_label = $total;
                }
                echo "<input type='hidden' name='show_plus' value='{$this->show_plus}'>\n";

                //Bug#52931: Replace with actionMenu
                //$select_link = "<a id='select_link' onclick='return select_dialog();' href=\"javascript:void(0)\">".$this->local_app_strings['LBL_LINK_SELECT']."&nbsp;".SugarThemeRegistry::current()->getImage('MoreDetail', 'border=0', 11, 7, '.png', $app_strings['LBL_MOREDETAIL'])."</a>";
                $menuItems = array(
                    "<label class=\"hidden glyphicon bootstrap-checkbox glyphicon-unchecked\"><span class='suitepicon suitepicon-action-caret'></span></label><input title=\"".$app_strings['LBL_SELECT_ALL_TITLE']."\" type='checkbox' class='bootstrap-checkbox-hidden checkbox massall' name='massall' id='massall' value='' onclick='sListView.check_all(document.MassUpdate, \"mass[]\", this.checked);' /><a href='javascript: void(0);'></a>",
                    "<a  name='thispage' id='button_select_this_page' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick='if (document.MassUpdate.select_entire_list.value==1){document.MassUpdate.select_entire_list.value=0;sListView.check_all(document.MassUpdate, \"mass[]\", true, $pageTotal)}else {sListView.check_all(document.MassUpdate, \"mass[]\", true)};' href='#'>{$app_strings['LBL_LISTVIEW_OPTION_CURRENT']}&nbsp;&#x28;{$pageTotal}&#x29;&#x200E;</a>",
                    "<a  name='selectall' id='button_select_all' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick='sListView.check_entire_list(document.MassUpdate, \"mass[]\",true,{$total});' href='#'>{$app_strings['LBL_LISTVIEW_OPTION_ENTIRE']}&nbsp;&#x28;{$total_label}&#x29;&#x200E;</a>",
                    "<a name='deselect' id='button_deselect' class='menuItem' onmouseover='hiliteItem(this,\"yes\");' onmouseout='unhiliteItem(this);' onclick='sListView.clear_all(document.MassUpdate, \"mass[]\", false);' href='#'>{$app_strings['LBL_LISTVIEW_NONE']}</a>",
                );
                require_once('include/Smarty/plugins/function.sugar_action_menu.php');
                $select_link = smarty_function_sugar_action_menu(array(
                    'class' => 'clickMenu selectmenu',
                    'id' => 'selectLink',
                    'buttons' => $menuItems,
                    'flat' => false,
                ), $this->xTemplate);
            } else {
                $select_link = "&nbsp;";
            }

            $export_link = '<input class="button" type="button" value="'.$this->local_app_strings['LBL_EXPORT'].'" ' .
                    'onclick="return sListView.send_form(true, \''.$_REQUEST['module'].'\', \'index.php?entryPoint=export\',\''.$this->local_app_strings['LBL_LISTVIEW_NO_SELECTED'].'\')">';

            if ($this->show_delete_button) {
                $delete_link = '<input class="button" type="button" id="delete_button" name="Delete" value="'.$this->local_app_strings['LBL_DELETE_BUTTON_LABEL'].'" onclick="return sListView.send_mass_update(\'selected\',\''.$this->local_app_strings['LBL_LISTVIEW_NO_SELECTED'].'\', 1)">';
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

            $selected_objects_span = "&nbsp;|&nbsp;{$this->local_app_strings['LBL_LISTVIEW_SELECTED_OBJECTS']}<input  style='border: 0px; background: transparent; font-size: inherit; color: inherit' type='text' readonly name='selectCount[]' value='" . ((isset($_POST['mass'])) ? count($_POST['mass']): 0) . "' />";

            if ($_REQUEST['module'] == 'Home' || $this->local_current_module == 'Import'
                || $this->show_export_button == false
                || (!empty($sugar_config['disable_export']))
                || (!empty($sugar_config['admin_export_only'])
                && !(
                    is_admin($current_user)
                        || (ACLController::moduleSupportsACL($_REQUEST['module'])
                            && ACLAction::getUserAccessLevel($current_user->id, $_REQUEST['module'], 'access') == ACL_ALLOW_ENABLED
                            && (ACLAction::getUserAccessLevel($current_user->id, $_REQUEST['module'], 'admin') == ACL_ALLOW_ADMIN ||
                                ACLAction::getUserAccessLevel($current_user->id, $_REQUEST['module'], 'admin') == ACL_ALLOW_ADMIN_DEV))
                ))) {
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
                $html_text .= "</td>\n<td nowrap align=\"right\">".$start_link."&nbsp;&nbsp;".$previous_link."&nbsp;&nbsp;<span class='pageNumbers'>(".$start_record." - ".$end_record." ".$this->local_app_strings['LBL_LIST_OF']." ".$row_count.")</span>&nbsp;&nbsp;".$next_link."&nbsp;&nbsp;".$end_link."</td></tr></table>\n";
                $html_text .= "</td>\n";
                $html_text .= "</tr>\n";
                $this->xTemplate->assign("PAGINATION", $html_text);
            }

            //C.L. - Fix for 23461
            if (empty($_REQUEST['action']) || $_REQUEST['action'] != 'Popup') {
                $_SESSION['export_where'] = $this->query_where;
            }
            $this->xTemplate->parse($xtemplateSection.".list_nav_row");
        }
    } // end processListNavigation

    public function processOrderBy($html_varName)
    {
        if (!isset($this->base_URL)) {
            $this->base_URL = $_SERVER['PHP_SELF'];

            if (isset($_SERVER['QUERY_STRING'])) {
                $this->base_URL = preg_replace("/\&".$this->getSessionVariableName($html_varName, "ORDER_BY")."=[0-9a-zA-Z\_\.]*/", "", $this->base_URL .'?'.$_SERVER['QUERY_STRING']);
                $this->base_URL = preg_replace("/\&".$this->getSessionVariableName($html_varName, "offset")."=[0-9]*/", "", $this->base_URL);
            }
            if ($_SERVER['REQUEST_METHOD'] == 'POST') {
                $this->base_URL .= '?';
                if (isset($_REQUEST['action'])) {
                    $this->base_URL .= '&action='.$_REQUEST['action'];
                }
                if (isset($_REQUEST['record'])) {
                    $this->base_URL .= '&record='.$_REQUEST['record'];
                }
                if (isset($_REQUEST['module'])) {
                    $this->base_URL .= '&module='.$_REQUEST['module'];
                }
            }
            $this->base_URL .= "&".$this->getSessionVariableName($html_varName, "offset")."=";
        }

        if ($this->is_dynamic) {
            $this->base_URL.='&to_pdf=true&action=SubPanelViewer&subpanel=' . $this->source_module;
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

        $sort_URL_base = $this->base_URL. "&".$this->getSessionVariableName($html_varName, "ORDER_BY")."=";

        if ($sort_URL_base !== "") {
            $this->xTemplate->assign("ORDER_BY", $sort_URL_base);
            return $sort_URL_base;
        } else {
            return '';
        }
    }


    public function getAdditionalHeader()
    {
    }


    /**
    * @return void
    * @param unknown $data
    * @param unknown $xTemplateSection
    * @param unknown $html_varName
    * @desc INTERNAL FUNCTION handles the rows
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
    * All Rights Reserved.
    * Contributor(s): ______________________________________..
    */
    public function processListRows($data, $xtemplateSection, $html_varName)
    {
        global $odd_bg;
        global $even_bg;
        global $hilite_bg;
        global $app_strings, $sugar_version, $sugar_config;
        global $currentModule;

        $this->xTemplate->assign('BG_HILITE', $hilite_bg);
        $this->xTemplate->assign('CHECKALL', SugarThemeRegistry::current()->getImage('blank', '', 1, 1, ".gif", ''));
        //$this->xTemplate->assign("BG_CLICK", $click_bg);
        $oddRow = true;
        $count = 0;
        reset($data);

        //GETTING OFFSET
        $offset = (int)$this->getOffset($html_varName);
        $timeStamp = $this->unique_id();
        $_SESSION[$html_varName."_FROM_LIST_VIEW"] = $timeStamp;

        $associated_row_data = array();

        //mail merge list
        $mergeList = array();
        $module = '';
        //todo what is this?  It is using an array as a boolean
        foreach ($data as $aVal => $aItem) {
            if (isset($this->data_array)) {
                $fields = $this->data_array;
            } else {
                $aItem->check_date_relationships_load();
                $fields = $aItem->get_list_view_data();
            }

            if (is_object($aItem)) { // cn: bug 5349
                //add item id to merge list, if the button is clicked
                $mergeList[] = $aItem->id;
                if (empty($module)) {
                    $module = $aItem->module_dir;
                }
            }
            //ADD OFFSET TO ARRAY

            $fields['OFFSET'] = ($offset + $count + 1);

            $fields['STAMP'] = $timeStamp;
            if ($this->shouldProcess) {
                $prerow = '';
                if (!isset($this->data_array)) {
                    $prerow .= "<input onclick='sListView.check_item(this, document.MassUpdate)' type='checkbox' class='checkbox' name='mass[]' value='". $fields['ID']. "'>";
                }
                $this->xTemplate->assign('PREROW', $prerow);

                $this->xTemplate->assign('CHECKALL', "<label class=\"hidden glyphicon bootstrap-checkbox glyphicon-unchecked\"></label><span class='suitepicon suitepicon-action-caret'></span><input type='checkbox' class='bootstrap-checkbox-hidden checkbox'  title='".$GLOBALS['app_strings']['LBL_SELECT_ALL_TITLE']."'  name='massall' id='massall' value='' onclick='sListView.check_all(document.MassUpdate, \"mass[]\", this.checked)'>");
            }
            if (!isset($this->data_array)) {
                $tag = $aItem->listviewACLHelper();
                $this->xTemplate->assign('TAG', $tag) ;
            }

            if ($oddRow) {
                $ROW_COLOR = 'oddListRow';
                $BG_COLOR =  $odd_bg;
            } else {
                $ROW_COLOR = 'evenListRow';
                $BG_COLOR =  $even_bg;
            }
            $oddRow = !$oddRow;

            $this->xTemplate->assign('ROW_COLOR', $ROW_COLOR);
            $this->xTemplate->assign('BG_COLOR', $BG_COLOR);

            if (isset($this->data_array)) {
                $this->xTemplate->assign('KEY', $aVal);
                $this->xTemplate->assign('VALUE', $aItem);
                $this->xTemplate->assign('INDEX', $count);
            } else {
                //AED -- some modules do not have their additionalDetails.php established. Add a check to ensure require_once does not fail
                // Bug #2786
                if ($this->_additionalDetails && $aItem->ACLAccess('DetailView') && (file_exists('modules/' . $aItem->module_dir . '/metadata/additionalDetails.php') || file_exists('custom/modules/' . $aItem->module_dir . '/metadata/additionalDetails.php'))) {
                    $additionalDetailsFile = 'modules/' . $aItem->module_dir . '/metadata/additionalDetails.php';
                    if (file_exists('custom/modules/' . $aItem->module_dir . '/metadata/additionalDetails.php')) {
                        $additionalDetailsFile = 'custom/modules/' . $aItem->module_dir . '/metadata/additionalDetails.php';
                    }

                    require_once($additionalDetailsFile);
                    $ad_function = (empty($this->additionalDetailsFunction) ? 'additionalDetails' : $this->additionalDetailsFunction) . $aItem->object_name;
                    $results = $ad_function($fields);
                    $results['string'] = str_replace(array("&#039", "'"), '\&#039', $results['string']); // no xss!

                    if (trim($results['string']) == '') {
                        $results['string'] = $app_strings['LBL_NONE'];
                    }
                    $fields[$results['fieldToAddTo']] = $fields[$results['fieldToAddTo']].'</a>';
                }

                if ($aItem->ACLAccess('Delete')) {
                    $delete = '<a class="listViewTdToolsS1" onclick="return confirm(\''.$this->local_app_strings['NTC_DELETE_CONFIRMATION'].'\')" href="'.'index.php?action=Delete&module='.$aItem->module_dir.'&record='.$fields['ID'].'&return_module='.$aItem->module_dir.'&return_action=index&return_id=">'.$this->local_app_strings['LBL_DELETE_INLINE'].'</a>';
                    require_once('include/Smarty/plugins/function.sugar_action_menu.php');
                    $fields['DELETE_BUTTON'] = smarty_function_sugar_action_menu(array(
                        'id' => $aItem->module_dir.'_'.$fields['ID'].'_create_button',
                        'buttons' => array($delete),
                    ), $this);
                }

                $this->xTemplate->assign($html_varName, $fields);
                $aItem->setupCustomFields($aItem->module_dir);
                $aItem->custom_fields->populateAllXTPL($this->xTemplate, 'detail', $html_varName, $fields);
            }
            if (!isset($this->data_array) && $aItem->ACLAccess('DetailView')) {
                $count++;
            }
            if (isset($this->data_array)) {
                $count++;
            }
            if (!isset($this->data_array)) {
                $aItem->list_view_parse_additional_sections($this->xTemplate, $xtemplateSection);

                if ($this->xTemplate->exists($xtemplateSection.'.row.pro')) {
                    $this->xTemplate->parse($xtemplateSection.'.row.pro');
                }
            }
            $this->xTemplate->parse($xtemplateSection . '.row');

            if (isset($fields['ID'])) {
                $associated_row_data[$fields['ID']] = $fields;
                // Bug 38908: cleanup data for JS to avoid having &nbsp; shuffled around
                foreach ($fields as $key => $value) {
                    if ($value == '&nbsp;') {
                        $associated_row_data[$fields['ID']][$key] = '';
                    }
                }
            }
        }

        $_SESSION['MAILMERGE_RECORDS'] = $mergeList;
        $_SESSION['MAILMERGE_MODULE_FROM_LISTVIEW'] = $module;
        if (empty($_REQUEST['action']) || $_REQUEST['action'] != 'Popup') {
            $_SESSION['MAILMERGE_MODULE'] = $module;
        }

        if ($this->process_for_popups) {
            $json = getJSONobj();
            $is_show_fullname = showFullName() ? 1 : 0;
            $associated_javascript_data = '<script type="text/javascript">' . "\n"
                //. '<!-- // associated javascript data generated by ListView' . "\n"
                . 'var associated_javascript_data = '
                . $json->encode($associated_row_data) . ";\n"
                //. '-->' . "\n"
                . 'var is_show_fullname = '
                . $is_show_fullname . ";\n"
                . '</script>';
            $this->xTemplate->assign('ASSOCIATED_JAVASCRIPT_DATA', $associated_javascript_data);
        }

        $this->xTemplate->parse($xtemplateSection);
    }


    public function getLayoutManager()
    {
        require_once('include/generic/LayoutManager.php');
        if ($this->layout_manager == null) {
            $this->layout_manager = new LayoutManager();
        }
        return $this->layout_manager;
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
            $orderBy=  'amount';
        }
        $buttons = false;
        $col_count = 0;
        foreach ($subpanel_def->get_list_fields() as $column_name=>$widget_args) {
            $usage = empty($widget_args['usage']) ? '' : $widget_args['usage'];
            if ($usage != 'query_only' || !empty($widget_args['force_query_only_display'])) {
                $imgArrow = '';

                if ($orderBy == $column_name || (isset($widget_args['sort_by']) && str_replace('.', '_', $widget_args['sort_by']) == $orderBy)) {
                    $imgArrow = "_down";
                    if ($this->sort_order == 'asc') {
                        $imgArrow = "_up";
                    }
                }

                if (!preg_match("/_button/i", $column_name)) {
                    $widget_args['name']=$column_name;
                    $widget_args['sort'] = $imgArrow;
                    $widget_args['start_link_wrapper'] = $this->start_link_wrapper;
                    $widget_args['end_link_wrapper'] = $this->end_link_wrapper;
                    $widget_args['subpanel_module'] = $this->subpanel_module;

                    $widget_contents = $layout_manager->widgetDisplay($widget_args);
                    $cell_width = empty($widget_args['width']) ? '' : $widget_args['width'];
                    $this->xTemplate->assign('HEADER_CELL', $widget_contents);
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
                    $this->xTemplate->assign('FOOTABLE', $footable);
                    $this->xTemplate->assign('CELL_COUNT', $count);
                    $this->xTemplate->assign('CELL_WIDTH', $cell_width);
                    $this->xTemplate->parse('dyn_list_view.header_cell');
                } else {
                    $buttons = true;
                }
            }
            ++$col_count;
        }

        if ($buttons) {
            $this->xTemplate->assign('FOOTABLE', '');
            $this->xTemplate->assign('HEADER_CELL', "&nbsp;");
            $this->xTemplate->assign('CELL_COUNT', $count);
            $this->xTemplate->assign('CELL_WIDTH', $cell_width);
            $this->xTemplate->parse('dyn_list_view.header_cell');
        }
    }


    /**
    * @return void
    * @param unknown $seed
    * @param unknown $xTemplateSection
    * @param unknown $html_varName
    * @desc PUBLIC FUNCTION Handles List Views using seeds that extend SugarBean
        $XTemplateSection is the section in the XTemplate file that should be parsed usually main
        $html_VarName is the variable name used in the XTemplateFile e.g. TASK
        $seed is a seed there are two types of seeds one is a subclass of SugarBean, the other is a list usually created from a sugar bean using get_list
        if no XTemplate is set it will create  a new XTemplate
        * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc..
        * All Rights Reserved..
        * Contributor(s): ______________________________________..
    */

    public function processListViewTwo($seed, $xTemplateSection, $html_varName)
    {
        global $current_user;
        if (!isset($this->xTemplate)) {
            $this->createXTemplate();
        }

        $isSugarBean = is_subclass_of($seed, "SugarBean");
        $list = null;

        if ($isSugarBean) {
            $list = $this->processSugarBean($xTemplateSection, $html_varName, $seed);
        } else {
            $list = $seed;
        }

        if (is_object($seed) && isset($seed->object_name) && $seed->object_name == 'WorkFlow') {
            $tab=array();
            $access = get_workflow_admin_modules_for_user($current_user);
            for ($i = 0; $i < count($list); $i++) {
                if (!empty($access[$list[$i]->base_module])) {
                    $tab[]=$list[$i];
                }
            }
            $list = $tab;
        }

        if ($this->is_dynamic) {
            $this->processHeaderDynamic($xTemplateSection, $html_varName);
            $this->processListRows($list, $xTemplateSection, $html_varName);
        } else {
            $this->processSortArrows($html_varName);

            if ($isSugarBean) {
                $seed->parse_additional_headers($this->xTemplate, $xTemplateSection);
            }
            $this->xTemplateAssign('CHECKALL', SugarThemeRegistry::current()->getImage('blank', '', 1, 1, ".gif", ''));

            // Process the  order by before processing the pro_nav.  The pro_nav requires the order by values to be set
            $this->processOrderBy($html_varName);


            $this->processListRows($list, $xTemplateSection, $html_varName);
        }

        if ($this->display_header_and_footer) {
            $this->getAdditionalHeader();
            if (!empty($this->header_title)) {
                echo get_form_header($this->header_title, $this->header_text, false);
            }
        }

        $this->xTemplate->out($xTemplateSection);

        if (isset($_SESSION['validation'])) {
            print base64_decode('PGEgaHJlZj0naHR0cDovL3d3dy5zdWdhcmNybS5jb20nPlBPV0VSRUQmbmJzcDtCWSZuYnNwO1NVR0FSQ1JNPC9hPg==');
        }
    }

    /**
     * @deprecated
     * @return string
     */
    public function getArrowStart()
    {
        global $log;
        $log->deprecated('ListView::getArrowStart is now deprecate and will be removed in a future release');
        return '';
    }

    /**
     * @param $upDown
     * @return string
     */
    public function getArrowUpDownStart($upDown)
    {
        if ($upDown === '_down') {
            return '<span class="suitepicon suitepicon-action-sorting-ascending"></span>';
        } elseif (($upDown === '_up')) {
            return '<span class="suitepicon suitepicon-action-sorting-descending"></span>';
        } else {
            return '<span class="suitepicon suitepicon-action-sorting-none"></span>';
        }
    }

    /**
     * @deprecated
     * @return string
     */
    public function getArrowEnd()
    {
        global $log;
        $log->deprecated('ListView::getArrowEnd is now deprecate and will be removed in a future release');
        return '';
    }

    /**
     * @deprecated
     * @param $upDown
     * @return string
     */
    public function getArrowUpDownEnd($upDown)
    {
        global $log;
        $log->deprecated('ListView::getArrowUpDownEnd is now deprecate and will be removed in a future release');
        return '';
    }

    public function getArrowImageSize()
    {
        // jbasicChartDashletsExpColust get the non-sort image's size.. the up and down have be the same.
        $image = SugarThemeRegistry::current()->getImageURL("arrow.gif", false);

        $cache_key = 'arrow_size.'.$image;

        // Check the cache
        $result = sugar_cache_retrieve($cache_key);
        if (!empty($result)) {
            return $result;
        }

        // No cache hit.  Calculate the value and return.
        $result = getimagesize($image);
        sugar_cache_put($cache_key, $result);
        return $result;
    }

    public function getArrowUpDownImageSize($upDown)
    {
        // just get the non-sort image's size.. the up and down have be the same.
        $image = SugarThemeRegistry::current()->getImageURL("arrow{$upDown}.gif", false);

        $cache_key = 'arrowupdown_size.'.$image;

        // Check the cache
        $result = sugar_cache_retrieve($cache_key);
        if (!empty($result)) {
            return $result;
        }

        // No cache hit.  Calculate the value and return.
        $result = getimagesize($image);
        sugar_cache_put($cache_key, $result);
        return $result;
    }

    public function getOrderByInfo($html_varName)
    {
        $orderBy = $this->getSessionVariable($html_varName, "OBL");
        $desc = $this->getSessionVariable($html_varName, $orderBy.'S');
        $orderBy = str_replace('.', '_', $orderBy);
        return array($orderBy,$desc);
    }

    public function processSortArrows($html_varName)
    {
        $this->xTemplateAssign("arrow_start", $this->getArrowStart());

        list($orderBy, $desc) = $this->getOrderByInfo($html_varName);

        $imgArrow = "_up";
        if ($desc) {
            $imgArrow = "_down";
        }
        /**
         * @deprecated only used by legacy opportunites listview, nothing current. Leaving for BC
         */
        if ($orderBy == 'amount') {
            $this->xTemplateAssign('amount_arrow', $imgArrow);
        } else {
            if ($orderBy == 'amount_usdollar') {
                $this->xTemplateAssign('amount_usdollar_arrow', $imgArrow);
            } else {
                $this->xTemplateAssign($orderBy.'_arrow', $imgArrow);
            }
        }

        $this->xTemplateAssign('arrow_end', $this->getArrowEnd());
    }

    // this is where translation happens for dynamic list views
    public function loadListFieldDefs(&$subpanel_fields, &$child_focus)
    {
        $this->list_field_defs = $subpanel_fields;

        for ($i=0;$i < count($this->list_field_defs);$i++) {
            $list_field = $this->list_field_defs[$i];
            $field_def = null;
            $key = '';
            if (!empty($list_field['vname'])) {
                $key = $list_field['vname'];
            } else {
                if (isset($list_field['name']) &&  isset($child_focus->field_defs[$list_field['name']])) {
                    $field_def = $child_focus->field_defs[$list_field['name']];
                    $key = $field_def['vname'];
                }
            }
            if (!empty($key)) {
                $list_field['label'] = translate($key, $child_focus->module_dir);
                $this->list_field_defs[$i]['label'] = preg_replace('/:$/', '', $list_field['label']);
            } else {
                $this->list_field_defs[$i]['label'] ='&nbsp;';
            }
        }
    }

    public function unique_id()
    {
        return sugar_microtime();
    }

    /**INTERNAL FUNCTION sets a session variable keeping it local to the listview
    not the current_module
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
    * All Rights Reserved.
    * Contributor(s): ______________________________________.
    */
    public function setLocalSessionVariable($localVarName, $varName, $value)
    {
        $_SESSION[$localVarName."_".$varName] = $value;
    }

    /**INTERNAL FUNCTION returns a session variable that is local to the listview,
    not the current_module
    * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
    * All Rights Reserved.
    * Contributor(s): ______________________________________.
    */
    public function getLocalSessionVariable($localVarName, $varName)
    {
        if (isset($_SESSION[$localVarName."_".$varName])) {
            return $_SESSION[$localVarName."_".$varName];
        } else {
            return "";
        }
    }

    /* Set to true if you want Additional Details to appear in the listview
     */
    public function setAdditionalDetails($value = true, $function = '')
    {
        if (!empty($function)) {
            $this->additionalDetailsFunction = $function;
        }
        $this->_additionalDetails = $value;
    }
}
