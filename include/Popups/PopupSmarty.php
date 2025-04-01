<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2017 SalesAgility Ltd.
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

require_once('include/ListView/ListViewSmarty.php');

require_once('include/TemplateHandler/TemplateHandler.php');
require_once('include/SearchForm/SearchForm2.php');
define("NUM_COLS", 2);
#[\AllowDynamicProperties]
class PopupSmarty extends ListViewSmarty
{
    public $contextMenus = false;
    public $export = false;
    public $mailmerge = false;
    public $mergeduplicates = false;
    public $quickViewLinks = false;
    public $multiSelect = false;
    public $headerTpl;
    public $footerTpl;
    public $th;
    public $tpl;
    public $view;
    public $field_defs;
    public $formData;
    public $_popupMeta;
    public $_create = false;
    public $searchdefs = array();
    public $listviewdefs = array();
    public $searchFields = array();
    public $customFieldDefs;
    public $filter_fields = array();
    //rrs
    public $searchForm;
    public $module;
    public $massUpdateData = '';

    public function __construct($seed, $module)
    {
        parent::__construct();
        $this->th = new TemplateHandler();
        $this->th->loadSmarty();
        $this->seed = $seed;
        $this->view = 'Popup';
        $this->module = $module;
        $this->searchForm = new SearchForm($this->seed, $this->module);
        $this->th->deleteTemplate($module, $this->view);
        $this->headerTpl = 'include/Popups/tpls/header.tpl';
        $this->footerTpl = 'include/Popups/tpls/footer.tpl';
    }

    /**
     * Assign several arrow image attributes to TemplateHandler smarty. Such as width, height, etc.
     *
     * @return void
     */
    public function processArrowVars()
    {
        $pathParts = pathinfo(SugarThemeRegistry::current()->getImageURL('arrow.gif', false));

        list($width, $height) = getimagesize($pathParts['dirname'].'/'.$pathParts['basename']);

        $this->th->ss->assign('arrowExt', $pathParts['extension']);
        $this->th->ss->assign('arrowWidth', $width);
        $this->th->ss->assign('arrowHeight', $height);
        $this->th->ss->assign('arrowAlt', translate('LBL_SORT'));
    }

    /**
     * Processes the request. Calls ListViewData process. Also assigns all lang strings, export links,
     * This is called from ListViewDisplay
     *
     * @param file file Template file to use
     * @param data array from ListViewData
     * @param html_var string the corresponding html var in xtpl per row
     *
     */
    public function process($file, $data, $htmlVar)
    {
        global $odd_bg, $even_bg, $hilite_bg, $click_bg, $app_strings;
        parent::process($file, $data, $htmlVar);

        $this->tpl = $file;
        $this->data = $data;

        $totalWidth = 0;
        foreach ($this->displayColumns as $name => $params) {
            $totalWidth += $params['width'];
        }
        $adjustment = $totalWidth / 100;

        $contextMenuObjectsTypes = array();
        foreach ($this->displayColumns as $name => $params) {
            if (!empty($adjustment)) {
                $this->displayColumns[$name]['width'] = round($this->displayColumns[$name]['width'] / $adjustment, 2);
            }

            // figure out which contextMenu objectsTypes are required
            if (!empty($params['contextMenu']['objectType'])) {
                $contextMenuObjectsTypes[$params['contextMenu']['objectType']] = true;
            }
        }
        $this->th->ss->assign('displayColumns', $this->displayColumns);


        $this->th->ss->assign('bgHilite', $hilite_bg);
        $this->th->ss->assign('colCount', count($this->displayColumns) + 1);
        $this->th->ss->assign('htmlVar', strtoupper($htmlVar));
        $this->th->ss->assign('moduleString', $this->moduleString);
        $this->th->ss->assign('editLinkString', $GLOBALS['app_strings']['LBL_EDIT_BUTTON']);
        $this->th->ss->assign('viewLinkString', $GLOBALS['app_strings']['LBL_VIEW_BUTTON']);

        //rrs
        $this->searchForm->parsedView = 'popup_query_form';
        $this->searchForm->displayType = 'popupView';
        $this->th->ss->assign('searchForm', $this->searchForm->display(false));
        //rrs

        if ($this->export) {
            $this->th->ss->assign('exportLink', $this->buildExportLink());
        }
        $this->th->ss->assign('quickViewLinks', $this->quickViewLinks);
        if ($this->mailMerge) {
            $this->th->ss->assign('mergeLink', $this->buildMergeLink());
        } // still check for mailmerge access
        if ($this->mergeduplicates) {
            $this->th->ss->assign('mergedupLink', $this->buildMergeDuplicatesLink());
        }


        if (!empty($_REQUEST['mode']) && strtoupper($_REQUEST['mode']) == 'MULTISELECT') {
            $this->multiSelect = true;
        }
        // handle save checks and stuff
        if ($this->multiSelect) {
            $this->th->ss->assign('selectedObjectsSpan', $this->buildSelectedObjectsSpan());
            $this->th->ss->assign('multiSelectData', $this->getMultiSelectData());
            $this->th->ss->assign('MODE', "<input type='hidden' name='mode' value='MultiSelect'>");
            $pageTotal = $this->data['pageData']['offsets']['next'] - $this->data['pageData']['offsets']['current'];
            if ($this->data['pageData']['offsets']['next'] < 0) { // If we are on the last page, 'next' is -1, which means we have to have a custom calculation
                $pageTotal = $this->data['pageData']['offsets']['total'] - $this->data['pageData']['offsets']['current'];
            }
            $this->th->ss->assign('selectLink', $this->buildSelectLink('select_link', $this->data['pageData']['offsets']['total'], $pageTotal));
        }

        $this->processArrows($data['pageData']['ordering']);
        $this->th->ss->assign('prerow', $this->multiSelect);
        $this->th->ss->assign('rowColor', array('oddListRow', 'evenListRow'));
        $this->th->ss->assign('bgColor', array($odd_bg, $even_bg));
        $this->th->ss->assign('contextMenus', $this->contextMenus);


        if ($this->contextMenus && !empty($contextMenuObjectsTypes)) {
            $script = '';
            $cm = new contextMenu();
            foreach ($contextMenuObjectsTypes as $type => $value) {
                $cm->loadFromFile($type);
                $script .= $cm->getScript();
                $cm->menuItems = array(); // clear menuItems out
            }
            $this->th->ss->assign('contextMenuScript', $script);
        }

        //rrs
        $this->_build_field_defs();

        // arrow image attributes
        $this->processArrowVars();
    }

    /*
     * Display the Smarty template.  Here we are using the TemplateHandler for caching per the module.
     */
    public function display($end = true)
    {
        global $app_strings;

        if (!is_file(sugar_cached("jsLanguage/{$GLOBALS['current_language']}.js"))) {
            require_once('include/language/jsLanguage.php');
            jsLanguage::createAppStringsCache($GLOBALS['current_language']);
        }
        $jsLang = getVersionedScript("cache/jsLanguage/{$GLOBALS['current_language']}.js", $GLOBALS['sugar_config']['js_lang_version']);

        $this->th->ss->assign('data', $this->data['data']);
        $this->data['pageData']['offsets']['lastOffsetOnPage'] = $this->data['pageData']['offsets']['current'] + count($this->data['data']);
        $this->th->ss->assign('pageData', $this->data['pageData']);

        $navStrings = array('next' => $GLOBALS['app_strings']['LNK_LIST_NEXT'],
                            'previous' => $GLOBALS['app_strings']['LNK_LIST_PREVIOUS'],
                            'end' => $GLOBALS['app_strings']['LNK_LIST_END'],
                            'start' => $GLOBALS['app_strings']['LNK_LIST_START'],
                            'of' => $GLOBALS['app_strings']['LBL_LIST_OF']);
        $this->th->ss->assign('navStrings', $navStrings);


        $associated_row_data = array();

        //C.L. - Bug 44324 - Override the NAME entry to not display salutation so that the data returned from the popup can be searched on correctly
        $searchNameOverride = !empty($this->seed) && $this->seed instanceof Person && (isset($this->data['data'][0]['FIRST_NAME']) && isset($this->data['data'][0]['LAST_NAME'])) ? true : false;

        global $locale;
        foreach ($this->data['data'] as $val) {
            $associated_row_data[$val['ID']] = $val;
            if ($searchNameOverride) {
                $associated_row_data[$val['ID']]['NAME'] = $locale->getLocaleFormattedName($val['FIRST_NAME'], $val['LAST_NAME']);
            }
        }
        $is_show_fullname = showFullName() ? 1 : 0;
        $json = getJSONobj();
        $this->th->ss->assign('jsLang', $jsLang);
        $this->th->ss->assign('lang', substr($GLOBALS['current_language'], 0, 2));
        $this->th->ss->assign('headerTpl', $this->headerTpl);
        $this->th->ss->assign('footerTpl', $this->footerTpl);
        $this->th->ss->assign('ASSOCIATED_JAVASCRIPT_DATA', 'var associated_javascript_data = '.$json->encode($associated_row_data). '; var is_show_fullname = '.$is_show_fullname.';');
        $this->th->ss->assign('module', $this->seed->module_dir);
        $this->th->ss->assign('metadata', empty($_REQUEST['metadata']) ? '' : $_REQUEST['metadata']);
        $request_data = empty($_REQUEST['request_data']) ? '' : $_REQUEST['request_data'];

        $this->th->ss->assign('request_data', $request_data);
        $this->th->ss->assign('fields', $this->fieldDefs);
        $this->th->ss->assign('formData', $this->formData);
        $this->th->ss->assign('APP', $GLOBALS['app_strings']);
        $this->th->ss->assign('MOD', $GLOBALS['mod_strings']);
        if (isset($this->_popupMeta['create']['createButton'])) {
            $this->_popupMeta['create']['createButton'] = translate($this->_popupMeta['create']['createButton']);
        }
        $this->th->ss->assign('popupMeta', $this->_popupMeta);
        $this->th->ss->assign('current_query', htmlentities(json_encode(($_REQUEST))));
        $this->th->ss->assign('customFields', $this->customFieldDefs);
        $this->th->ss->assign('numCols', NUM_COLS);
        $this->th->ss->assign('massUpdateData', $this->massUpdateData);
        $this->th->ss->assign('sugarVersion', $GLOBALS['sugar_version']);
        $this->th->ss->assign('should_process', $this->should_process);

        if ($this->_create) {
            $this->th->ss->assign('ADDFORM', $this->getQuickCreate());//$this->_getAddForm());
            $this->th->ss->assign('ADDFORMHEADER', $this->_getAddFormHeader());
            $this->th->ss->assign('object_name', $this->seed->object_name);
        }
        $this->th->ss->assign(
            'LIST_HEADER',
            get_form_header($GLOBALS['mod_strings']['LBL_LIST_FORM_TITLE'], '', false)
        );
        $this->th->ss->assign(
            'SEARCH_FORM_HEADER',
            get_form_header($GLOBALS['mod_strings']['LBL_SEARCH_FORM_TITLE'], '', false)
        );

        $themeObject = SugarThemeRegistry::current();
        $this->th->ss->assign("STYLE_JS", ob_get_contents() . $themeObject->getJS());

        $str = $this->th->displayTemplate($this->seed->module_dir, $this->view, $this->tpl);
        return $str;
    }

    /**
     * Setup up the smarty template. we added an extra step here to add the order by from the popupdefs.
     *
     * @see ListViewDisplay::setup
     */
    public function setup(
        $seed,
        $file = null,
        $where = null,
        $params = array(),
        $offset = 0,
        $limit = -1,
        $filter_fields = array(),
        $id_field = 'id',
        $id = null
    ) {
        $args = func_get_args();
        return call_user_func_array(array($this, '_setup'), $args);
    }

    public function _setup($file)
    {
        if (isset($this->_popupMeta)) {
            if (isset($this->_popupMeta['create']['formBase'])) {
                require_once('modules/' . $this->seed->module_dir . '/' . $this->_popupMeta['create']['formBase']);
                $this->_create = true;
            }
        }
        if (!empty($this->_popupMeta['create'])) {
            $formBase = new $this->_popupMeta['create']['formBaseClass']();
            if (isset($_REQUEST['doAction']) && $_REQUEST['doAction'] == 'save') {
                //If it's a new record, set useRequired to false
                $useRequired = empty($_REQUEST['id']) ? false : true;
                $formBase->handleSave('', false, $useRequired);
            }
        }

        $params = array();
        if (!empty($this->_popupMeta['orderBy'])) {
            $params['orderBy'] = $this->_popupMeta['orderBy'];
        }

        if (file_exists('custom/modules/'.$this->module.'/metadata/metafiles.php')) {
            require('custom/modules/'.$this->module.'/metadata/metafiles.php');
        } elseif (file_exists('modules/'.$this->module.'/metadata/metafiles.php')) {
            require('modules/'.$this->module.'/metadata/metafiles.php');
        }

        if (!empty($metafiles[$this->module]['searchfields'])) {
            require($metafiles[$this->module]['searchfields']);
        } elseif (file_exists('modules/'.$this->module.'/metadata/SearchFields.php')) {
            require('modules/'.$this->module.'/metadata/SearchFields.php');
        }
        $this->searchdefs[$this->module]['templateMeta']['maxColumns'] = 2;
        $this->searchdefs[$this->module]['templateMeta']['widths']['label'] = 10;
        $this->searchdefs[$this->module]['templateMeta']['widths']['field'] = 30;

        $this->searchForm->view = 'PopupSearchForm';
        $this->searchForm->setup($this->searchdefs, $searchFields, 'SearchFormGenericAdvanced.tpl', 'advanced_search', $this->listviewdefs);

        $lv = new ListViewSmarty();
        $displayColumns = array();
        if (!empty($_REQUEST['displayColumns'])) {
            foreach (explode('|', $_REQUEST['displayColumns']) as $num => $col) {
                if (!empty($listViewDefs[$this->module][$col])) {
                    $displayColumns[$col] = $this->listviewdefs[$this->module][$col];
                }
            }
        } else {
            foreach ($this->listviewdefs[$this->module] as $col => $para) {
                if (!empty($para['default']) && $para['default']) {
                    $displayColumns[$col] = $para;
                }
            }
        }
        $params['massupdate'] = true;
        if (!empty($_REQUEST['orderBy'])) {
            $params['orderBy'] = $_REQUEST['orderBy'];
            $params['overrideOrder'] = true;
            if (!empty($_REQUEST['sortOrder'])) {
                $params['sortOrder'] = $_REQUEST['sortOrder'];
            }
        }

        $lv->displayColumns = $displayColumns;
        $this->searchForm->lv = $lv;
        $this->searchForm->displaySavedSearch = false;


        $this->searchForm->populateFromRequest('advanced_search');
        $searchWhere = $this->_get_where_clause();
        $this->searchColumns = $this->searchForm->searchColumns;
        //parent::setup($this->seed, $file, $searchWhere, $params, 0, -1, $this->filter_fields);

        $this->should_process = true;

        if (isset($params['export'])) {
            $this->export = $params['export'];
        }
        if (!empty($params['multiSelectPopup'])) {
            $this->multi_select_popup = $params['multiSelectPopup'];
        }
        if (!empty($params['massupdate']) && $params['massupdate'] != false) {
            $this->show_mass_update_form = true;
            $this->mass = new MassUpdate();
            $this->mass->setSugarBean($this->seed);
            if (!empty($params['handleMassupdate']) || !isset($params['handleMassupdate'])) {
                $this->mass->handleMassUpdate();
            }
        }

        // create filter fields based off of display columns
        if (empty($this->filter_fields) || $this->mergeDisplayColumns) {
            foreach ($this->displayColumns as $columnName => $def) {
                $this->filter_fields[strtolower($columnName)] = true;
                if (!empty($def['related_fields'])) {
                    foreach ($def['related_fields'] as $field) {
                        //id column is added by query construction function. This addition creates duplicates
                        //and causes issues in oracle. #10165
                        if ($field != 'id') {
                            $this->filter_fields[$field] = true;
                        }
                    }
                }
                if (!empty($this->seed->field_defs[strtolower($columnName)]['db_concat_fields'])) {
                    foreach ($this->seed->field_defs[strtolower($columnName)]['db_concat_fields'] as $index=>$field) {
                        if (!isset($this->filter_fields[strtolower($field)]) || !$this->filter_fields[strtolower($field)]) {
                            $this->filter_fields[strtolower($field)] = true;
                        }
                    }
                }
            }
            foreach ($this->searchColumns as $columnName => $def) {
                $this->filter_fields[strtolower($columnName)] = true;
            }
        }

        if (isset($_REQUEST['request_data'])) {
            $request_data = json_decode(html_entity_decode($_REQUEST['request_data']), true);
            $_POST['field_to_name'] = $_REQUEST['field_to_name'] = array_keys($request_data['field_to_name_array']);
        }

        /**
         * Bug #46842 : The relate field field_to_name_array fails to copy over custom fields
         * By default bean's create_new_list_query function loads fields displayed on the page or used in the search
         * add fields used to populate forms from _viewdefs :: field_to_name_array to retrive from db
         */
        if (isset($_REQUEST['field_to_name']) && $_REQUEST['field_to_name']) {
            $_REQUEST['field_to_name'] = is_array($_REQUEST['field_to_name']) ? $_REQUEST['field_to_name'] : array($_REQUEST['field_to_name']);
            foreach ($_REQUEST['field_to_name'] as $add_field) {
                $add_field = strtolower($add_field);
                if ($add_field != 'id' && !isset($this->filter_fields[$add_field]) && isset($this->seed->field_defs[$add_field])) {
                    $this->filter_fields[$add_field] = true;
                }
            }
        }


        if (!empty($_REQUEST['query']) || (!empty($GLOBALS['sugar_config']['save_query']) && $GLOBALS['sugar_config']['save_query'] != 'populate_only')) {
            $data = $this->lvd->getListViewData($this->seed, $searchWhere, 0, -1, $this->filter_fields, $params, 'id');
        } else {
            $this->should_process = false;
            $data = array(
                'data'=>array(),
                'pageData'=>array(
                    'bean'=>array('moduleDir'=>$this->seed->module_dir),
                    'ordering'=>'',
                    'offsets'=>array('total'=>0,'next'=>0,'current'=>0),
                ),
            );
        }

        $this->fillDisplayColumnsWithVardefs();

        $this->process($file, $data, $this->seed->object_name);
    }

    /*
     * Return the where clause as per the REQUEST.
     */
    public function _get_where_clause()
    {
        $where = '';
        $where_clauses = $this->searchForm->generateSearchWhere(true, $this->seed->module_dir);

        // Bug 43452 - FG - Changed the way generated Where array is imploded into the string.
        //                  Now it's imploding in the same way view.list.php do.
        if (count($where_clauses) > 0) {
            $where = '( ' . implode(' and ', $where_clauses) . ' )';
        }

        // Need to include the default whereStatement
        if (!empty($this->_popupMeta['whereStatement'])) {
            if (!empty($where)) {
                $where .= ' AND ';
            }
            $where .= $this->_popupMeta['whereStatement'];
        }

        return $where;
    }

    /*
     * Generate the data for the search form on the header of the Popup.
     */
    public function _build_field_defs()
    {
        $this->formData = array();
        $this->customFieldDefs = array();
        foreach ($this->searchdefs[$this->module]['layout']['advanced_search'] as $data) {
            if (is_array($data)) {
                $this->formData[] = array('field' => $data);
                $value = '';
                $this->customFieldDefs[$data['name']]= $data;
                if (!empty($_REQUEST[$data['name']])) {
                    $value = $_REQUEST[$data['name']];
                }
                $this->customFieldDefs[$data['name']]['value'] = $value;
            } else {
                $this->formData[] = array('field' => array('name'=>$data));
            }
        }
        $this->fieldDefs = array();
        if ($this->seed) {
            $this->seed->fill_in_additional_detail_fields();

            foreach ($this->seed->toArray() as $name => $value) {
                $this->fieldDefs[$name] = $this->seed->field_defs[$name];
                //if we have a relate type then reset to name so that we end up with a textbox
                //rather than a select button
                $this->fieldDefs[$name]['name'] = $this->fieldDefs[$name]['name'];
                if ($this->fieldDefs[$name]['type'] == 'relate') {
                    $this->fieldDefs[$name]['type'] = 'name';
                }
                if (isset($this->fieldDefs[$name]['options']) && isset($GLOBALS['app_list_strings'][$this->fieldDefs[$name]['options']])) {
                    $this->fieldDefs[$name]['options'] = $GLOBALS['app_list_strings'][$this->fieldDefs[$name]['options']]; // fill in enums
                }
                if (!empty($_REQUEST[$name])) {
                    $value = $_REQUEST[$name];
                }
                $this->fieldDefs[$name]['value'] = $value;
            }
        }
    }

    public function _getAddForm()
    {
        $addform = '';
        if (!$this->seed->ACLAccess('save')) {
            return;
        }
        if (!empty($this->_popupMeta['create'])) {
            $formBase = new $this->_popupMeta['create']['formBaseClass']();



            // TODO: cleanup the construction of $addform
            $prefix = empty($this->_popupMeta['create']['getFormBodyParams'][0]) ? '' : $this->_popupMeta['create']['getFormBodyParams'][0];
            $mod = empty($this->_popupMeta['create']['getFormBodyParams'][1]) ? '' : $this->_popupMeta['create']['getFormBodyParams'][1];
            $formBody = empty($this->_popupMeta['create']['getFormBodyParams'][2]) ? '' : $this->_popupMeta['create']['getFormBodyParams'][2];

            $getFormMethod = (empty($this->_popupMeta['create']['getFormMethod']) ? 'getFormBody' : $this->_popupMeta['create']['getFormMethod']);
            $formbody = $formBase->$getFormMethod($prefix, $mod, $formBody);

            $addform = '<table><tr><td nowrap="nowrap" valign="top">'
                    . str_replace('<br>', '</td><td nowrap="nowrap" valign="top">&nbsp;', $formbody)
                    . '</td></tr></table>'
                    . '<input type="hidden" name="action" value="Popup" />';

            return $addform;
        }
    }

    public function _getAddFormHeader()
    {
        $lbl_save_button_title = $GLOBALS['app_strings']['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $GLOBALS['app_strings']['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $GLOBALS['app_strings']['LBL_SAVE_BUTTON_LABEL'];
        $module_dir = $this->seed->module_dir;
        $formSave = <<<EOQ
			<input type="hidden" name="create" value="true">
			<input type="hidden" name="popup" value="true">
			<input type="hidden" name="to_pdf" value="true">
			<input type="hidden" name="return_module" value="$module_dir">
			<input type="hidden" name="return_action" value="Popup">
EOQ;
        // if metadata contains custom inputs for the quickcreate
        if (!empty($this->_popupMeta['customInput']) && is_array($this->_popupMeta['customInput'])) {
            foreach ($this->_popupMeta['customInput'] as $key => $value) {
                $formSave .= '<input type="hidden" name="' . $key . '" value="'. $value .'">\n';
            }
        }


        $addformheader = get_form_header(translate($this->_popupMeta['create']['createButton']), $formSave, false);
        return $addformheader;
    }

    public function getQuickCreate()
    {
        require_once("include/EditView/PopupQuickCreate.php");
        $qc = new PopupQuickCreate($this->module);
        return $qc->process($this->module);
    }
}
