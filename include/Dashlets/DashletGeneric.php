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

require_once('include/Dashlets/Dashlet.php');
require_once('include/ListView/ListViewSmarty.php');
require_once('include/generic/LayoutManager.php');

/**
 * Generic Dashlet class
 * @api
 */
class DashletGeneric extends Dashlet
{
    /**
      * Fields that are searchable
      * @var array
      */
    public $searchFields;
    /**
     * Displayable columns (ones available to display)
     * @var array
     */
    public $columns;
    /**
     * Bean file used in this Dashlet
     * @var bean
     */
    public $seedBean;
    /**
     * collection of filters to apply
     * @var array
     */
    public $filters = null;
    /**
     * Number of Rows to display
     * @var int
     */
    public $displayRows = '5';
    /**
     * Actual columns to display, will be a subset of $columns
     * @var array
     */
    public $displayColumns = null;
    /**
     * Flag to display only the current users's items.
     * @var bool
     */
    public $myItemsOnly = true;
    /**
     * Flag to display "myItemsOnly" checkbox in the DashletGenericConfigure.
     * @var bool
     */
    public $showMyItemsOnly = true;
    /**
     * location of Smarty template file for display
     * @var string
     */
    public $displayTpl = 'include/Dashlets/DashletGenericDisplay.tpl';
    /**
     * location of smarty template file for configuring
     * @var string
     */
    public $configureTpl = 'include/Dashlets/DashletGenericConfigure.tpl';
    /**
     * smarty object for the generic configuration template
     * @var string
     */
    public $configureSS;
    /** search inputs to be populated in configure template.
     *  modify this after processDisplayOptions, but before displayOptions to modify search inputs
     *  @var array
     */
    public $currentSearchFields;
    /**
     * ListView Smarty Class
     * @var Smarty
     */
    public $lvs;
    public $layoutManager;

    public function __construct($id, $options = null)
    {
        parent::__construct($id);
        $this->isConfigurable = true;
        if (isset($options)) {
            if (!empty($options['filters'])) {
                $this->filters = $options['filters'];
            }
            if (!empty($options['title'])) {
                $this->title = $options['title'];
            }
            if (!empty($options['displayRows'])) {
                $this->displayRows = $options['displayRows'];
            }
            if (!empty($options['displayColumns'])) {
                $this->displayColumns = $options['displayColumns'];
            }
            if (isset($options['myItemsOnly'])) {
                $this->myItemsOnly = $options['myItemsOnly'];
            }
            if (isset($options['autoRefresh'])) {
                $this->autoRefresh = $options['autoRefresh'];
            }
        }

        $this->layoutManager = new LayoutManager();
        $this->layoutManager->setAttribute('context', 'Report');
        // fake a reporter object here just to pass along the db type used in many widgets.
        // this should be taken out when sugarwidgets change
        $db = DBManagerFactory::getInstance();
        $temp = (object) array('db' => &$db, 'report_def_str' => '');
        $this->layoutManager->setAttributePtr('reporter', $temp);
        $this->lvs = new ListViewSmarty();
    }

    /**
     * Sets up the display options template
     *
     * @return string HTML that shows options
     */
    public function processDisplayOptions()
    {
        require_once('include/templates/TemplateGroupChooser.php');

        $this->configureSS = new Sugar_Smarty();
        // column chooser
        $chooser = new TemplateGroupChooser();

        $chooser->args['id'] = 'edit_tabs';
        $chooser->args['left_size'] = 5;
        $chooser->args['right_size'] = 5;
        $chooser->args['values_array'][0] = array();
        $chooser->args['values_array'][1] = array();

        $this->loadCustomMetadata();
        // Bug 39517 - Don't add custom fields automatically to the available fields to display in the listview
        //$this->addCustomFields();
        if ($this->displayColumns) {
            // columns to display
            foreach ($this->displayColumns as $num => $name) {
                // defensive code for array being returned
                $translated = translate($this->columns[$name]['label'], $this->seedBean->module_dir);
                if (is_array($translated)) {
                    $translated = $this->columns[$name]['label'];
                }
                $chooser->args['values_array'][0][$name] = trim($translated, ':');
            }
            // columns not displayed
            foreach (array_diff(array_keys($this->columns), array_values($this->displayColumns)) as $num => $name) {
                // defensive code for array being returned
                $translated = translate($this->columns[$name]['label'], $this->seedBean->module_dir);
                if (is_array($translated)) {
                    $translated = $this->columns[$name]['label'];
                }
                $chooser->args['values_array'][1][$name] = trim($translated, ':');
            }
        } else {
            foreach ($this->columns as $name => $val) {
                // defensive code for array being returned
                $translated = translate($this->columns[$name]['label'], $this->seedBean->module_dir);
                if (is_array($translated)) {
                    $translated = $this->columns[$name]['label'];
                }
                if (!empty($val['default']) && $val['default']) {
                    $chooser->args['values_array'][0][$name] = trim($translated, ':');
                } else {
                    $chooser->args['values_array'][1][$name] = trim($translated, ':');
                }
            }
        }

        $chooser->args['left_name'] = 'display_tabs';
        $chooser->args['right_name'] = 'hide_tabs';
        $chooser->args['max_left'] = '6';

        $chooser->args['left_label'] =  $GLOBALS['app_strings']['LBL_DISPLAY_COLUMNS'];
        $chooser->args['right_label'] =  $GLOBALS['app_strings']['LBL_HIDE_COLUMNS'];
        $chooser->args['title'] =  '';
        $this->configureSS->assign('columnChooser', $chooser->display());

        $query = false;
        $count = 0;

        if (!is_array($this->filters)) {
            // use default search params
            $this->filters = array();
            foreach ($this->searchFields as $name => $params) {
                if (!empty($params['default'])) {
                    $this->filters[$name] = $params['default'];
                }
            }
        }
        $currentSearchFields = array();
        foreach ($this->searchFields as $name=>$params) {
            if (!empty($name)) {
                $name = strtolower($name);
                $currentSearchFields[$name] = array();
                $widgetDef = $this->seedBean->field_defs[$name];
                if ($widgetDef['name'] == 'assigned_user_name') {
                    $widgetDef['name'] = 'assigned_user_id';
                }
                //bug 39170 - begin
                if ($widgetDef['name'] == 'created_by_name') {
                    $name = $widgetDef['name'] = 'created_by';
                }
                if ($widgetDef['name'] == 'modified_by_name') {
                    $name = $widgetDef['name'] = 'modified_user_id';
                }
                //bug 39170 - end
                if ($widgetDef['type']=='enum' || $widgetDef['type']=='multienum') {
                    $filterNotSelected = array(); // we need to have some value otherwise '' or null values make -none- to be selected by default
                } else {
                    $filterNotSelected = '';
                }
                $widgetDef['input_name0'] = empty($this->filters[$name]) ? $filterNotSelected : $this->filters[$name];

                $currentSearchFields[$name]['label'] = !empty($params['label']) ? translate($params['label'], $this->seedBean->module_dir) : translate($widgetDef['vname'], $this->seedBean->module_dir);
                $currentSearchFields[$name]['input'] = $this->layoutManager->widgetDisplayInput($widgetDef, true, (empty($this->filters[$name]) ? '' : $this->filters[$name]));
            } else { // ability to create spacers in input fields
                $currentSearchFields['blank' . $count]['label'] = '';
                $currentSearchFields['blank' . $count]['input'] = '';
                $count++;
            }
        }
        $this->currentSearchFields = $currentSearchFields;

        $this->configureSS->assign('strings', array('general' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_GENERAL'],
                                     'filters' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_FILTERS'],
                                     'myItems' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_MY_ITEMS_ONLY'],
                                     'displayRows' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_DISPLAY_ROWS'],
                                     'title' => $GLOBALS['mod_strings']['LBL_DASHLET_CONFIGURE_TITLE'],
                                     'save' => $GLOBALS['app_strings']['LBL_SAVE_BUTTON_LABEL'],
                                     'clear' => $GLOBALS['app_strings']['LBL_CLEAR_BUTTON_LABEL'],
                                     'autoRefresh' => $GLOBALS['app_strings']['LBL_DASHLET_CONFIGURE_AUTOREFRESH'],
                                     ));
        $this->configureSS->assign('id', $this->id);
        $this->configureSS->assign('showMyItemsOnly', $this->showMyItemsOnly);
        $this->configureSS->assign('myItemsOnly', $this->myItemsOnly);
        $this->configureSS->assign('searchFields', $this->currentSearchFields);
        $this->configureSS->assign('showClearButton', $this->isConfigPanelClearShown);
        // title
        $this->configureSS->assign('dashletTitle', $this->title);

        // display rows
        $displayRowOptions = $GLOBALS['sugar_config']['dashlet_display_row_options'];
        $this->configureSS->assign('displayRowOptions', $displayRowOptions);
        $this->configureSS->assign('displayRowSelect', $this->displayRows);

        if ($this->isAutoRefreshable()) {
            $this->configureSS->assign('isRefreshable', true);
            $this->configureSS->assign('autoRefreshOptions', $this->getAutoRefreshOptions());
            $this->configureSS->assign('autoRefreshSelect', $this->autoRefresh);
        }
    }
    /**
     * Displays the options for this Dashlet
     *
     * @return string HTML that shows options
     */
    public function displayOptions()
    {
        $this->processDisplayOptions();
        return parent::displayOptions() . $this->configureSS->fetch($this->configureTpl);
    }

    public function buildWhere()
    {
        global $current_user;

        $returnArray = array();

        if (!is_array($this->filters)) {
            // use defaults
            $this->filters = array();
            foreach ($this->searchFields as $name => $params) {
                if (!empty($params['default'])) {
                    $this->filters[$name] = $params['default'];
                }
            }
        }
        foreach ($this->filters as $name=>$params) {
            if (!empty($params)) {
                if ($name == 'assigned_user_id' && $this->myItemsOnly) {
                    continue;
                } // don't handle assigned user filter if filtering my items only
                $widgetDef = $this->seedBean->field_defs[$name];

                $widgetClass = $this->layoutManager->getClassFromWidgetDef($widgetDef, true);
                $widgetDef['table'] = $this->seedBean->table_name;
                $widgetDef['table_alias'] = $this->seedBean->table_name;
                if (!empty($widgetDef['source']) && $widgetDef['source'] == 'custom_fields') {
                    $widgetDef['table'] = $this->seedBean->table_name."_cstm";
                    $widgetDef['table_alias'] = $widgetDef['table'];
                }
                switch ($widgetDef['type']) {// handle different types
                    case 'date':
                    case 'datetime':
                    case 'datetimecombo':
                        if (is_array($params) && !empty($params)) {
                            if (!empty($params['date'])) {
                                $widgetDef['input_name0'] = $params['date'];
                            }
                            $filter = 'queryFilter' . $params['type'];
                        } else {
                            $filter = 'queryFilter' . $params;
                        }
                        array_push($returnArray, $widgetClass->$filter($widgetDef, true));
                        break;
                    case 'assigned_user_name':
                        // This type runs through the SugarWidgetFieldname class, and needs a little extra help to make it through
                        if (! isset($widgetDef['column_key'])) {
                            $widgetDef['column_key'] = $name;
                        }
                        // no break here, we want to run through the default handler
                    case 'relate':
                        if (isset($widgetDef['link']) && $this->seedBean->load_relationship($widgetDef['link'])) {
                            $widgetLink = $widgetDef['link'];
                            $widgetDef['module'] = $this->seedBean->$widgetLink->focus->module_name;
                            $widgetDef['link'] = $this->seedBean->$widgetLink->getRelationshipObject()->name;
                        }
                        // no break - run through the default handler
                    default:
                        $widgetDef['input_name0'] = $params;
                        if (is_array($params) && !empty($params)) { // handle array query
                            array_push($returnArray, $widgetClass->queryFilterone_of($widgetDef, false));
                        } else {
                            array_push($returnArray, $widgetClass->queryFilterStarts_With($widgetDef, true));
                        }
                        $widgetDef['input_name0'] = $params;
                    break;
                }
            }
        }

        if ($this->myItemsOnly) {
            array_push($returnArray, $this->seedBean->table_name . '.' . "assigned_user_id = '" . $current_user->id . "'");
        }

        return $returnArray;
    }

    protected function loadCustomMetadata()
    {
        $dashletData = [];
        $customMetadata = 'custom/modules/'.$this->seedBean->module_dir.'/metadata/dashletviewdefs.php';
        if (file_exists($customMetadata)) {
            require($customMetadata);
            $this->searchFields = $dashletData[$this->seedBean->module_dir.'Dashlet']['searchFields'];
            foreach ($this->searchFields  as $key =>$def) {
                if ($key == 'assigned_user_name') {
                    $this->searchFields['assigned_user_id'] = $def;
                    unset($this->searchFields['assigned_user_name']);
                    break;
                }
            }

            $this->columns = $dashletData[$this->seedBean->module_dir.'Dashlet']['columns'];
        }
    }

    /**
     * Does all dashlet processing, here's your chance to modify the rows being displayed!
     */
    public function process($lvsParams = array(), $id = null)
    {
        $currentSearchFields = array();
        $configureView = true; // configure view or regular view
        $query = false;
        $whereArray = array();
        $lvsParams['massupdate'] = false;

        $this->loadCustomMetadata();
        $this->addCustomFields();
        // apply filters
        if (isset($this->filters) || $this->myItemsOnly) {
            $whereArray = $this->buildWhere();
        }

        $this->lvs->export = false;
        $this->lvs->multiSelect = false;
        // columns
        $displayColumns = array();
        if (!empty($this->displayColumns)) { // use user specified columns
            foreach ($this->displayColumns as $name => $val) {
                $displayColumns[strtoupper($val)] = $this->columns[$val];
                $displayColumns[strtoupper($val)]['label'] = trim($displayColumns[strtoupper($val)]['label'], ':');// strip : at the end of headers
            }
        } else {
            if (isset($this->columns)) {
                // use the default
                foreach ($this->columns as $name => $val) {
                    if (!empty($val['default']) && $val['default']) {
                        $displayColumns[strtoupper($name)] = $val;
                        $displayColumns[strtoupper($name)]['label'] = trim($displayColumns[strtoupper($name)]['label'], ':');
                    }
                }
            }
        }
        $this->lvs->displayColumns = $displayColumns;


        $this->lvs->lvd->setVariableName($this->seedBean->object_name, array());
        $lvdOrderBy = $this->lvs->lvd->getOrderBy(); // has this list been ordered, if not use default

        $nameRelatedFields = array();

        //bug: 44592 - dashlet sort order was not being preserved between logins
        if (!empty($lvsParams['orderBy']) && !empty($lvsParams['sortOrder'])) {
            $lvsParams['overrideOrder'] = true;
        } else {
            if (empty($lvdOrderBy['orderBy'])) {
                foreach ($displayColumns as $colName => $colParams) {
                    if (!empty($colParams['defaultOrderColumn'])) {
                        $lvsParams['overrideOrder'] = true;
                        $lvsParams['orderBy'] = $colName;
                        $lvsParams['sortOrder'] = $colParams['defaultOrderColumn']['sortOrder'];
                    }
                }
            }
        }
        // Check for 'last_name' column sorting with related fields (last_name, first_name)
        // See ListViewData.php for actual sorting change.
        if ($lvdOrderBy['orderBy'] == 'last_name' && !empty($displayColumns['NAME']) && !empty($displayColumns['NAME']['related_fields']) &&
            in_array('last_name', $displayColumns['NAME']['related_fields']) &&
            in_array('first_name', $displayColumns['NAME']['related_fields'])) {
            $lvsParams['overrideLastNameOrder'] = true;
        }

        if (!empty($this->displayTpl)) {
            //MFH BUG #14296
            $where = '';
            if (!empty($whereArray)) {
                $where = '(' . implode(') AND (', $whereArray) . ')';
            }
            $this->lvs->setup($this->seedBean, $this->displayTpl, $where, $lvsParams, 0, $this->displayRows/*, $filterFields*/, array(), 'id', $id);
            if (array_key_exists('CREATED_BY', $displayColumns)) { // handle the created by field
                foreach ($this->lvs->data['data'] as $row => $data) {
                    $this->lvs->data['data'][$row]['CREATED_BY'] = get_assigned_user_name($data['CREATED_BY']);
                }
            }
            // assign a baseURL w/ the action set as DisplayDashlet
            foreach ($this->lvs->data['pageData']['urls'] as $type => $url) {
                // awu Replacing action=DisplayDashlet with action=DynamicAction&DynamicAction=DisplayDashlet
                if ($type == 'orderBy') {
                    $this->lvs->data['pageData']['urls'][$type] = preg_replace('/(action=.*&)/Ui', 'action=DynamicAction&DynamicAction=displayDashlet&', (string) $url);
                } else {
                    $this->lvs->data['pageData']['urls'][$type] = preg_replace('/(action=.*&)/Ui', 'action=DynamicAction&DynamicAction=displayDashlet&', (string) $url) . '&sugar_body_only=1&id=' . $this->id;
                }
            }

            $this->lvs->ss->assign('dashletId', $this->id);
        }
    }

    /**
      * Displays the Dashlet, must call process() prior to calling this
      *
      * @return string HTML that displays Dashlet
      */
    public function display()
    {
        return parent::display() . $this->lvs->display(false) . $this->processAutoRefresh();
    }

    /**
     * Filter the $_REQUEST and only save only the needed options
     * @param array $req the array to pull options from
     *
     * @return array options array
     */
    public function saveOptions($req)
    {
        $options = array();

        $this->loadCustomMetadata();
        foreach ($req as $name => $value) {
            if (!is_array($value)) {
                $req[$name] = trim($value);
            }
        }
        $options['filters'] = array();
        foreach ($this->searchFields as $name=>$params) {
            $widgetDef = $this->seedBean->field_defs[$name];
            //bug39170 - begin
            if ($widgetDef['name']=='created_by_name' && $req['created_by']) {
                $widgetDef['name'] = 'created_by';
            }
            if ($widgetDef['name']=='modified_by_name' && $req['modified_user_id']) {
                $widgetDef['name'] = 'modified_user_id';
            }
            //bug39170 - end
            if ($widgetDef['type'] == 'datetimecombo' || $widgetDef['type'] == 'datetime' || $widgetDef['type'] == 'date') { // special case datetime types
                $options['filters'][$widgetDef['name']] = array();
                if (!empty($req['type_' . $widgetDef['name']])) { // save the type of date filter
                    $options['filters'][$widgetDef['name']]['type'] = $req['type_' . $widgetDef['name']];
                }
                if (!empty($req['date_' . $widgetDef['name']])) { // save the date
                    $options['filters'][$widgetDef['name']]['date'] = $req['date_' . $widgetDef['name']];
                }
            } elseif (!empty($req[$widgetDef['name']])) {
                $options['filters'][$widgetDef['name']] = $req[$widgetDef['name']];
            }
        }
        if (!empty($req['dashletTitle'])) {
            $options['title'] = htmlentities(html_entity_decode($req['dashletTitle']));
        }

        // Don't save the options for myItemsOnly if we're not even showing the options.
        if ($this->showMyItemsOnly) {
            if (!empty($req['myItemsOnly'])) {
                $options['myItemsOnly'] = $req['myItemsOnly'];
            } else {
                $options['myItemsOnly'] = false;
            }
        }
        $options['displayRows'] = empty($req['displayRows']) ? '5' : $req['displayRows'];
        // displayColumns
        if (!empty($req['displayColumnsDef'])) {
            $options['displayColumns'] = explode('|', $req['displayColumnsDef']);
        }
        $options['autoRefresh'] = empty($req['autoRefresh']) ? '0' : $req['autoRefresh'];
        return $options;
    }

    /**
     * Internal function to add custom fields
     *
     */
    public function addCustomFields()
    {
        foreach ($this->seedBean->field_defs as $fieldName => $def) {
            if (!empty($def['type']) && $def['type'] == 'html') {
                continue;
            }
            if (isset($def['vname'])) {
                $translated = translate($def['vname'], $this->seedBean->module_dir);
                if (is_array($translated)) {
                    $translated = $def['vname'];
                }
                if (!empty($def['source']) && $def['source'] == 'custom_fields') {
                    if (isset($this->columns[$fieldName]['default']) && $this->columns[$fieldName]['default']) {
                        $this->columns[$fieldName] = array('width' => '10',
                                                       'label' => $translated,
                                                       'default' => 1);
                    } else {
                        $this->columns[$fieldName] = array('width' => '10',
                                                       'label' => $translated);
                    }
                }
            }
        }
    }
}
