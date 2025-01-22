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


require_once('include/tabs.php');
/**
 * Old search form
 * @api
 */
#[\AllowDynamicProperties]
class SearchForm
{
    /**
      * SearchForm Template to use (xtpl)
      * @var string
      */
    public $tpl;
    /**
     * SearchField meta data array to use. Populated from moduleDir/metadata/SearchFields
     * @var array
     */
    public $searchFields;
    /**
     * Seed bean to use
     * @var bean
     */
    public $bean;
    /**
     * Module the search from is for
     * @var string
     */
    public $module;
    /**
     * meta data for the tabs to display
     * @var array
     */
    public $tabs;
    /**
     * XTPL object
     * @var object
     */
    public $xtpl;
    /**
     * Use to determine whether or not to show the saved search options
     * @var boolean
     */
    public $showSavedSearchOptions = true;

    /**
     * loads SearchFields MetaData, sets member variables
     *
     * @param string $module moduleDir
     * @param bean $seedBean seed bean to use
     * @param string $tpl template to use, defaults to moduleDir/SearchForm.html
     *
     */
    public function __construct($module, &$seedBean, $tpl = null)
    {
        global $app_strings;

        $this->module = $module;
        require_once('modules/' . $module . '/metadata/SearchFields.php');
        if (file_exists('custom/modules/' . $module . '/metadata/SearchFields.php')) {
            require_once('custom/modules/' . $module . '/metadata/SearchFields.php');
        }


        //require_once('modules/' . $module . '/metadata/SearchFields.php');
        $this->searchFields = $searchFields[$module];
        if (empty($tpl)) {
            if (!empty($GLOBALS['layout_edit_mode'])) {
                $this->tpl = sugar_cached('studio/custom/working/modules/' . $module . '/SearchForm.html');
            } else {
                $this->tpl = get_custom_file_if_exists('modules/' . $module . '/SearchForm.html');
            }
        } else {
            $this->tpl = $tpl;
        }

        $this->bean = $seedBean;
        $this->tabs = array(array('title'  => $app_strings['LNK_BASIC_FILTER'],
                                  'link'   => $module . '|basic_search',
                                  'key'    => $module . '|basic_search'),
                            array('title'  => $app_strings['LNK_ADVANCED_FILTER'],
                                  'link'   => $module . '|advanced_search',
                                  'key'    => $module . '|advanced_search'));

        if (file_exists('modules/'.$this->module.'/index.php')) {
            $this->tabs[] = array('title'  => $app_strings['LNK_SAVED_VIEWS'],
                                  'link'   => $module . '|saved_views',
                                  'key'    => $module . '|saved_views');
        }
    }

    /**
     * Populate the searchFields from an array
     *
     * @param array $array array to search through
     * @param string $switchVar variable to use in switch statement
     * @param bool $addAllBeanFields true to process at all bean fields
     */
    public function populateFromArray(&$array, $switchVar = null, $addAllBeanFields = true)
    {

       //CL Bug:33176
        if (empty($array['searchFormTab']) && empty($switchVar)) {
            $array['searchFormTab'] = 'advanced_search';
        }

        if (!empty($array['searchFormTab']) || !empty($switchVar)) {
            $arrayKeys = array_keys($array);
            $searchFieldsKeys = array_keys($this->searchFields);
            if (empty($switchVar)) {
                $switchVar = $array['searchFormTab'];
            }
            switch ($switchVar) {
                case 'basic_search':
                    foreach ($this->searchFields as $name => $params) {
                        if (isset($array[$name . '_basic'])) {
                            $this->searchFields[$name]['value'] =
                                is_string($array[$name . '_basic'])?trim($array[$name . '_basic']):$array[$name . '_basic'];
                        }
                    }
                    if ($addAllBeanFields) {
                        foreach ($this->bean->field_name_map as $key => $params) {
                            if (in_array($key . '_basic', $arrayKeys) && !in_array($key, $searchFieldsKeys)) {
                                $this->searchFields[$key] = array('query_type' => 'default',
                                                                  'value'      => $array[$key . '_basic']);
                            }
                        }
                    }
                    break;
                case 'advanced_search':
                   foreach ($this->searchFields as $name => $params) {
                       if (isset($array[$name])) {
                           $this->searchFields[$name]['value'] = is_string($array[$name])?trim($array[$name]):$array[$name];
                       }
                   }
                    if ((empty($array['massupdate']) || $array['massupdate'] == 'false') && $addAllBeanFields) {
                        foreach ($this->bean->field_name_map as $key => $params) {
                            if (in_array($key, $arrayKeys) && !in_array($key, $searchFieldsKeys)) {
                                $this->searchFields[$key] = array('query_type' => 'default',
                                                                  'value'      => $array[$key]);
                            }
                        }
                    }
                    break;
                case 'saved_views':
                    foreach ($this->searchFields as $name => $params) {
                        if (isset($array[$name . '_basic'])) {  // save basic first
                            $this->searchFields[$name]['value'] = $array[$name . '_basic'];
                        }
                        if (isset($array[$name])) {  // overwrite by advanced if available
                            $this->searchFields[$name]['value'] = $array[$name];
                        }
                    }
                    if ($addAllBeanFields) {
                        foreach ($this->bean->field_name_map as $key => $params) {
                            if (!in_array($key, $searchFieldsKeys)) {
                                if (in_array($key . '_basic', $arrayKeys)) {
                                    $this->searchFields[$key] = array('query_type' => 'default',
                                                                      'value'      => $array[$key . '_basic']);
                                }
                                if (in_array($key, $arrayKeys)) {
                                    $this->searchFields[$key] = array('query_type' => 'default',
                                                                      'value'      => $array[$key]);
                                }
                            }
                        }
                    }
            }
        }
    }

    /**
     * Populate the searchFields from $_REQUEST
     *
     * @param string $switchVar variable to use in switch statement
     * @param bool $addAllBeanFields true to process at all bean fields
     */
    public function populateFromRequest($switchVar = null, $addAllBeanFields = true)
    {
        $this->populateFromArray($_REQUEST, $switchVar, $addAllBeanFields);
    }


    /**
     * The fuction will returns an array of filter conditions.
     *
     */
    public function generateSearchWhere($add_custom_fields = false, $module='')
    {
        global $timedate;
        $values = $this->searchFields;

        $where_clauses = array();
        //$like_char = '%';
        $table_name = $this->bean->object_name;

        foreach ($this->searchFields as $field=>$parms) {
            $customField = false;
            // Jenny - Bug 7462: We need a type check here to avoid database errors
            // when searching for numeric fields. This is a temporary fix until we have
            // a generic search form validation mechanism.
            $type = (!empty($this->bean->field_name_map[$field]['type']))?$this->bean->field_name_map[$field]['type']:'';
            if (!empty($this->bean->field_name_map[$field]['source']) && $this->bean->field_name_map[$field]['source'] == 'custom_fields') {
                $customField = true;
            }

            if ($type == 'int') {
                if (!empty($parms['value'])) {
                    $tempVal = explode(',', $parms['value']);
                    $newVal = '';
                    foreach ($tempVal as $key => $val) {
                        if (!empty($newVal)) {
                            $newVal .= ',';
                        }
                        if (!empty($val) && !(is_numeric($val))) {
                            $newVal .= -1;
                        } else {
                            $newVal .= $val;
                        }
                    }
                    $parms['value'] = $newVal;
                }
            }
            // do not include where clause for custom fields with checkboxes that are unchecked
            elseif ($type == 'bool' && empty($parms['value']) && $customField) {
                continue;
            } elseif ($type == 'bool' && !empty($parms['value'])) {
                if ($parms['value'] == 'on') {
                    $parms['value'] = 1;
                }
            }

            if (isset($parms['value']) && $parms['value'] != "") {
                $operator = 'like';
                if (!empty($parms['operator'])) {
                    $operator = strtolower($parms['operator']);
                }

                if (is_array($parms['value'])) {
                    $field_value = '';

                    // If it is a custom field of multiselect we have to do some special processing
                    if ($customField && !empty($this->bean->field_name_map[$field]['isMultiSelect']) && $this->bean->field_name_map[$field]['isMultiSelect']) {
                        $operator = 'custom_enum';
                        $db_field = $this->bean->table_name .  "_cstm." . $field;
                        foreach ($parms['value'] as $key => $val) {
                            if ($val != ' ' and $val != '') {
                                $qVal = DBManagerFactory::getInstance()->quote($val);
                                if (!empty($field_value)) {
                                    $field_value .= ' or ';
                                }
                                $field_value .= "$db_field like '$qVal' or $db_field like '%$qVal^%' or $db_field like '%^$qVal%' or $db_field like '%^$qVal^%'";
                            }
                        }
                    } else {
                        $operator = $operator != 'subquery' ? 'in' : $operator;
                        foreach ($parms['value'] as $key => $val) {
                            if ($val != ' ' and $val != '') {
                                if (!empty($field_value)) {
                                    $field_value .= ',';
                                }
                                $field_value .= "'" . DBManagerFactory::getInstance()->quote($val) . "'";
                            }
                        }
                    }
                } else {
                    $field_value = DBManagerFactory::getInstance()->quote($parms['value']);
                }

                //set db_fields array.
                if (!isset($parms['db_field'])) {
                    $parms['db_field'] = array($field);
                }

                if (isset($parms['my_items']) and $parms['my_items'] == true) {
                    global $current_user;
                    $field_value = DBManagerFactory::getInstance()->quote($current_user->id);
                    $operator = '=';
                }

                $where = '';
                $itr = 0;
                if ($field_value != '') {
                    foreach ($parms['db_field'] as $db_field) {
                        if (strstr($db_field, '.') === false) {
                            if (!$customField) {
                                $db_field = $this->bean->table_name .  "." . $db_field;
                            } else {
                                $db_field = $this->bean->table_name .  "_cstm." . $db_field;
                            }
                        }

                        if ($type == 'date') {
                            // The regular expression check is to circumvent special case YYYY-MM
                            $operator = '=';
                            if (preg_match('/^\d{4}.\d{1,2}$/', $field_value) == 0) {
                                $db_field = $this->bean->db->convert($db_field, "date_format", "%Y-%m");
                            } else {
                                $field_value = $timedate->to_db_date($field_value, false);
                                $db_field = $this->bean->db->convert($db_field, "date_format", "%Y-%m-%d");
                            }
                        }

                        if ($type == 'datetime'|| $type == 'datetimecombo') {
                            $dates = $timedate->getDayStartEndGMT($field_value);
                            $field_value = array($this->bean->db->convert($dates["start"], "datetime"),
                                $this->bean->db->convert($dates["end"], "datetime"));
                            $operator = 'between';
                        }

                        if ($this->bean->db->supports('case_sensitive') && isset($parms['query_type']) && $parms['query_type'] == 'case_insensitive') {
                            $db_field = 'upper(' . $db_field . ")";
                            $field_value = strtoupper($field_value);
                        }

                        $itr++;
                        if (!empty($where)) {
                            $where .= " OR ";
                        }
                        switch ($operator) {
                            case 'subquery':
                                $in = 'IN';
                                if (isset($parms['subquery_in_clause'])) {
                                    if (!is_array($parms['subquery_in_clause'])) {
                                        $in = $parms['subquery_in_clause'];
                                    } elseif (isset($parms['subquery_in_clause'][$field_value])) {
                                        $in = $parms['subquery_in_clause'][$field_value];
                                    }
                                }
                                $sq = $parms['subquery'];
                                if (is_array($sq)) {
                                    $and_or = ' AND ';
                                    if (isset($sq['OR'])) {
                                        $and_or = ' OR ';
                                    }
                                    $first = true;
                                    foreach ($sq as $q) {
                                        if (empty($q) || strlen($q)<2) {
                                            continue;
                                        }
                                        if (!$first) {
                                            $where .= $and_or;
                                        }
                                        $where .= " {$db_field} $in ({$q} '{$field_value}%') ";
                                        $first = false;
                                    }
                                } elseif (!empty($parms['query_type']) && $parms['query_type'] == 'format') {
                                    $stringFormatParams = array(0 => $field_value, 1 => $GLOBALS['current_user']->id);
                                    $where .= "{$db_field} $in (".string_format($parms['subquery'], $stringFormatParams).")";
                                } else {
                                    $where .= "{$db_field} $in ({$parms['subquery']} '{$field_value}%')";
                                }

                                break;
                            case 'like':
                                $where .=  $db_field . " like ".$this->bean->db->quoted($field_value.'%');
                                break;
                            case 'in':
                                $where .=  $db_field . " in (".$field_value.')';
                                break;
                            case '=':
                                $where .=  $db_field . " = ".$this->bean->db->quoted($field_value);
                                break;
                            case 'between':
                                if (!is_array($field_value)) {
                                    $field_value = explode('<>', $field_value);
                                }
                                $where .= "(". $db_field . " >= ".$this->bean->db->quoted($field_value[0]) .
                                    " AND " .$db_field . " <= ".$this->bean->db->quoted($field_value[1]).")";
                                break;
                        }
                    }
                }
                if (!empty($where)) {
                    if ($itr > 1) {
                        array_push($where_clauses, '( '.$where.' )');
                    } else {
                        array_push($where_clauses, $where);
                    }
                }
            }
        }

        return $where_clauses;
    }

    /**
     * displays the tabs (top of the search form)
     *
     * @param string $currentKey key in $this->tabs to show as the current tab
     *
     * @return string html
     */
    public function displayTabs($currentKey)
    {
        $GLOBALS['log']->debug('SearchForm.php->displayTabs(): tabs='.print_r($this->tabs, true));

        $tabPanel = new SugarWidgetTabs($this->tabs, $currentKey, 'SUGAR.searchForm.searchFormSelect');

        if (isset($_REQUEST['saved_search_select']) && $_REQUEST['saved_search_select']!='_none') {
            $saved_search=loadBean('SavedSearch');
            $saved_search->retrieveSavedSearch($_REQUEST['saved_search_select']);
        }

        $str = $tabPanel->display();
        $str .= '<script>';
        if (!empty($_REQUEST['displayColumns'])) {
            $str .= 'SUGAR.savedViews.displayColumns = "' . $_REQUEST['displayColumns'] . '";';
        } elseif (isset($saved_search->contents['displayColumns']) && !empty($saved_search->contents['displayColumns'])) {
            $str .= 'SUGAR.savedViews.displayColumns = "' . $saved_search->contents['displayColumns'] . '";';
        }
        if (!empty($_REQUEST['hideTabs'])) {
            $str .= 'SUGAR.savedViews.hideTabs = "' . $_REQUEST['hideTabs'] . '";';
        } elseif (isset($saved_search->contents['hideTabs']) && !empty($saved_search->contents['hideTabs'])) {
            $str .= 'SUGAR.savedViews.hideTabs = "' . $saved_search->contents['hideTabs'] . '";';
        }
        if (!empty($_REQUEST['orderBy'])) {
            $str .= 'SUGAR.savedViews.selectedOrderBy = "' . $_REQUEST['orderBy'] . '";';
        } elseif (isset($saved_search->contents['orderBy']) && !empty($saved_search->contents['orderBy'])) {
            $str .= 'SUGAR.savedViews.selectedOrderBy = "' . $saved_search->contents['orderBy'] . '";';
        }
        if (!empty($_REQUEST['sortOrder'])) {
            $str .= 'SUGAR.savedViews.selectedSortOrder = "' . $_REQUEST['sortOrder'] . '";';
        } elseif (isset($saved_search->contents['sortOrder']) && !empty($saved_search->contents['sortOrder'])) {
            $str .= 'SUGAR.savedViews.selectedSortOrder = "' . $saved_search->contents['sortOrder'] . '";';
        }

        $str .= '</script>';

        return $str;
    }

    /**
     * sets up the search forms, populates the preset values
     *
     */
    public function setup()
    {
        global $mod_strings, $app_strings, $app_list_strings, $theme, $timedate;
        $GLOBALS['log']->debug('SearchForm.php->setup()');
        $this->xtpl = new XTemplate($this->tpl);
        $this->xtpl->assign("MOD", $mod_strings);
        $this->xtpl->assign("APP", $app_strings);
        $this->xtpl->assign("THEME", $theme);
        $this->xtpl->assign("CALENDAR_DATEFORMAT", $timedate->get_cal_date_format());
        $this->xtpl->assign("USER_DATEFORMAT", '('. $timedate->get_user_date_format().')');

        foreach ($this->searchFields as $name => $params) {
            if (isset($params['template_var'])) {
                $templateVar = $params['template_var'];
            } else {
                $templateVar = strtoupper($name);
            }
            if (isset($params['value'])) { // populate w/ preselected values
                if (isset($params['options'])) {
                    $options = $app_list_strings[$params['options']];
                    if (isset($params['options_add_blank']) && $params['options_add_blank']) {
                        array_unshift($options, '');
                    }
                    $this->xtpl->assign($templateVar, get_select_options_with_id($options, $params['value']));
                } else {
                    if (isset($params['input_type'])) {
                        switch ($params['input_type']) {
                            case 'checkbox': // checkbox input
                                if ($params['value'] == 'on' || $params['value']) {
                                    $this->xtpl->assign($templateVar, 'checked');
                                }
                                break;
                        }
                    } else {// regular text input
                        $this->xtpl->assign($templateVar, to_html($params['value']));
                    }
                }
            } else { // populate w/o preselected values
                if (isset($params['options'])) {
                    $options = $app_list_strings[$params['options']];
                    if (isset($params['options_add_blank']) && $params['options_add_blank']) {
                        array_unshift($options, '');
                    }
                    $this->xtpl->assign($templateVar, get_select_options_with_id($options, ''));
                }
            }
        }
        if (!empty($_REQUEST['assigned_user_id'])) {
            $this->xtpl->assign("USER_FILTER", get_select_options_with_id(get_user_array(false), $_REQUEST['assigned_user_id']));
        } else {
            $this->xtpl->assign("USER_FILTER", get_select_options_with_id(get_user_array(false), ''));
        }

        // handle my items only
        if (isset($this->searchFields['current_user_only']) && isset($this->searchFields['current_user_only']['value'])) {
            $this->xtpl->assign("CURRENT_USER_ONLY", "checked");
        }
    }

    /**
     * displays the search form header
     *
     * @param string $view which view is currently being displayed
     *
     */
    public function displayHeader($view)
    {
        global $current_user;
        $GLOBALS['log']->debug('SearchForm.php->displayHeader()');
        $header_text = '';
        if (is_admin($current_user) && $_REQUEST['module'] != 'DynamicLayout' && !empty($_SESSION['editinplace'])) {
            $header_text = "<a href='index.php?action=index&module=DynamicLayout&from_action=SearchForm&from_module=".$_REQUEST['module'] ."'>".SugarThemeRegistry::current()->getImage("EditLayout", "border='0' align='bottom'", null, null, '.gif', 'Edit Layout')."</a>";
        }

        echo $header_text . $this->displayTabs($this->module . '|' . $view);
        echo "<form name='search_form' class='search_form'>" .
             "<input type='hidden' name='searchFormTab' value='{$view}'/>" .
             "<input type='hidden' name='module' value='{$_REQUEST['module']}'/>" .
             "<input type='hidden' name='action' value='{$_REQUEST['action']}'/>" .
             "<input type='hidden' name='query' value='true'/>";
    }

    /**
     * displays the search form body, for example if basic_search is being displayed, then the function call would be
     * displayWithHeaders('basic_search', $htmlForBasicSearchBody) {
     *
     * @param string $view which view is currently being displayed
     * @param string $basic_search_text body of the basic search tab
     * @param string $advanced_search_text body of the advanced search tab
     * @param string $saved_views_text body of the saved views tab
     *
     */
    public function displayWithHeaders($view, $basic_search_text = '', $advanced_search_text = '', $saved_views_text = '')
    {
        $GLOBALS['log']->debug('SearchForm.php->displayWithHeaders()');
        $this->displayHeader($view);
        echo "<div id='{$this->module}basic_searchSearchForm' " . (($view == 'basic_search') ? '' : "style='display: none'") . ">" . $basic_search_text . "</div>";
        echo "<div id='{$this->module}advanced_searchSearchForm' " . (($view == 'advanced_search') ? '' : "style='display: none'") . ">" . $advanced_search_text . "</div>";
        echo "<div id='{$this->module}saved_viewsSearchForm' " . (($view == 'saved_views') ? '' : "style='display: none'") . ">" . $saved_views_text . "</div>";
        echo $this->getButtons();
        echo '</form>';
        // echo '<script type="text/javascript">Calendar.setup ({inputField : "search_jscal_field", ifFormat : "'.$timedate->get_cal_date_format().'", showsTime : false, button : "search_jscal_trigger", singleClick : true, step : 1});</script>';
    }

    /**
     * displays the basic search form body
     *
     * @param bool $header display this with headers
     * @param bool $return echo or return the html
     *
     * @return string html of contents
     */
    public function displayBasic($header = true, $return = false)
    {
        global $current_user;

        $this->bean->custom_fields->populateAllXTPL($this->xtpl, 'search');
        $this->xtpl->parse("main");
        if (!empty($GLOBALS['layout_edit_mode'])) {
            $this->xtpl->parse("advanced");
        }
        $text = $this->xtpl->text("main");
        if (!empty($GLOBALS['layout_edit_mode'])) {
            $text .= $this->xtpl->text("advanced");
        }
        if ($header && empty($GLOBALS['layout_edit_mode'])) {
            $this->displayWithHeaders('basic_search', $text);
        } else {
            if ($return) {
                return $text;
            } else {
                echo $text;
            }
        }
    }

    /**
     * displays the advanced search form body
     *
     * @param bool $header display this with headers
     * @param bool $return echo or return the html
     *
     * @return string html of contents
     */
    public function displayAdvanced($header = true, $return = false, $listViewDefs='', $lv='')
    {
        global $current_user, $current_language;
        $GLOBALS['log']->debug('SearchForm.php->displayAdvanced()');
        $this->bean->custom_fields->populateAllXTPL($this->xtpl, 'search');
        if (!empty($listViewDefs) && !empty($lv)) {
            $GLOBALS['log']->debug('SearchForm.php->displayAdvanced(): showing saved search');
            $savedSearch = new SavedSearch($listViewDefs[$this->module], $lv->data['pageData']['ordering']['orderBy'], $lv->data['pageData']['ordering']['sortOrder']);
            $this->xtpl->assign('SAVED_SEARCH', $savedSearch->getForm($this->module, false));
            $this->xtpl->assign('MOD_SAVEDSEARCH', return_module_language($current_language, 'SavedSearch'));
            $this->xtpl->assign('ADVANCED_SEARCH_IMG', SugarThemeRegistry::current()->getImageURL('advanced_search.gif'));
            //this determines whether the saved search subform should be rendered open or not
            if (isset($_REQUEST['showSSDIV']) && $_REQUEST['showSSDIV']=='yes') {
                $this->xtpl->assign('SHOWSSDIV', 'yes');
                $this->xtpl->assign('DISPLAYSS', '');
            } else {
                $this->xtpl->assign('SHOWSSDIV', 'no');
                $this->xtpl->assign('DISPLAYSS', 'display:none');
            }
        }
        $this->xtpl->parse("advanced");
        $text = $this->xtpl->text("advanced");

        if ($header) {
            $this->displayWithHeaders('advanced_search', '', $text);
        } else {
            if ($return) {
                return $text;
            } else {
                echo $text;
            }
        }
    }

    /**
     * displays the saved views form body
     *
     * @param bool $header display this with headers
     * @param bool $return echo or return the html
     *
     * @return string html of contents
     */
    public function displaySavedViews($listViewDefs, $lv, $header = true, $return = false)
    {
        global $current_user;

        $savedSearch = new SavedSearch($listViewDefs[$this->module], $lv->data['pageData']['ordering']['orderBy'], $lv->data['pageData']['ordering']['sortOrder']);

        if ($header) {
            $this->displayWithHeaders('saved_views', $this->displayBasic(false, true), $this->displayAdvanced(false, true), $savedSearch->getForm($this->module));
            echo '<script>SUGAR.savedViews.handleForm();</script>';
        } else {
            echo $savedSearch->getForm($this->module, false);
        }
    }

    /**
     * get the search buttons
     *
     * @return string html of contents
     */
    public function getButtons()
    {
        global $app_strings;

        $SAVED_SEARCHES_OPTIONS = '';
        $savedSearch = BeanFactory::newBean('SavedSearch');
        $SAVED_SEARCHES_OPTIONS = $savedSearch->getSelect($this->module);
        $str = "<input tabindex='2' title='{$app_strings['LBL_SEARCH_BUTTON_TITLE']}' onclick='SUGAR.savedViews.setChooser()' class='button' type='submit' name='button' value='{$app_strings['LBL_SEARCH_BUTTON_LABEL']}' id='search_form_submit'/>&nbsp;";
        $str .= "<input tabindex='2' title='{$app_strings['LBL_CLEAR_BUTTON_TITLE']}' onclick='SUGAR.searchForm.clear_form(this.form); return false;' class='button' type='button' name='clear' value=' {$app_strings['LBL_CLEAR_BUTTON_LABEL']} ' id='search_form_clear'/>";

        if (!empty($SAVED_SEARCHES_OPTIONS) && $this->showSavedSearchOptions) {
            $str .= "   <span class='white-space'>
                        &nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;<b>{$app_strings['LBL_SAVED_FILTER_SHORTCUT']}</b>&nbsp;
                        {$SAVED_SEARCHES_OPTIONS}
                        <span id='go_btn_span' style='display:none'><input tabindex='2' title='go_select' id='go_select'  onclick='SUGAR.searchForm.clear_form(this.form);' class='button' type='button' name='go_select' value=' {$app_strings['LBL_GO_BUTTON_LABEL']} '/></span>
                    </span>
                    </form>";
        }
        $str .= "
                <script>
                    function toggleInlineSearch(){
                        if (document.getElementById('inlineSavedSearch').style.display == 'none'){
                            document.getElementById('showSSDIV').value = 'yes'
                            document.getElementById('inlineSavedSearch').style.display = '';

                            document.getElementById('up_down_img').src='".SugarThemeRegistry::current()->getImageURL('basic_search.gif')."';
                            document.getElementById('up_down_img').setAttribute('alt','".$GLOBALS['app_strings']['LBL_ALT_HIDE_OPTIONS']."');

                        }else{

                            document.getElementById('up_down_img').src='".SugarThemeRegistry::current()->getImageURL('advanced_search.gif')."';
                            document.getElementById('up_down_img').setAttribute('alt','".$GLOBALS['app_strings']['LBL_ALT_SHOW_OPTIONS']."');
                            document.getElementById('showSSDIV').value = 'no';
                            document.getElementById('inlineSavedSearch').style.display = 'none';
                        }
                    }


                </script>
            ";
        return $str;
    }
}
