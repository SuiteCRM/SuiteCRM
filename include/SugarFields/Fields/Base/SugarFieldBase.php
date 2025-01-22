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
 * SugarFieldBase translates and displays fields from a vardef definition into different formats
 * including DetailView, ListView, EditView. It also provides Search Inputs and database queries
 * to handle searching
 *
 */
#[\AllowDynamicProperties]
class SugarFieldBase
{
    /**
     * @var Sugar_Smarty $ss
     */
    public $ss;

    /**
     * @var bool $hasButton;
     */
    public $hasButton = false;

    /**
     * SugarFieldBase constructor.
     * @param $type
     */
    public function __construct($type)
    {
        $this->type = $type;
        $this->ss = new Sugar_Smarty();
    }

    /**
     * parse and fetch template
     * @param string $path template
     * @return string
     */
    public function fetch($path)
    {
        $additional = '';
        if (!$this->hasButton && !empty($this->button)) {
            $additional .= '<input type="button" class="button" ' . $this->button . '>';
        }
        if (!empty($this->buttons)) {
            foreach ($this->buttons as $v) {
                $additional .= ' <input type="button" class="button" ' . $v . '>';
            }
        }
        if (!empty($this->image)) {
            $additional .= ' <img ' . $this->image . '>';
        }

        return $this->ss->fetch($path) . $additional;
    }

    /**
     * @param string $view Eg EditView
     * @return string
     */
    public function findTemplate($view)
    {
        static $tplCache = array();

        if (isset($tplCache[$this->type][$view])) {
            return $tplCache[$this->type][$view];
        }

        $lastClass = get_class($this);
        $classList = array($this->type, str_replace('SugarField', '', $lastClass));
        while ($lastClass = get_parent_class($lastClass)) {
            $classList[] = str_replace('SugarField', '', $lastClass);
        }

        $tplName = '';
        foreach ($classList as $className) {
            global $current_language;
            if (isset($current_language)) {
                $tplName = 'include/SugarFields/Fields/' . $className . '/' . $current_language . '.' . $view . '.tpl';
                if (file_exists('custom/' . $tplName)) {
                    $tplName = 'custom/' . $tplName;
                    break;
                }
                if (file_exists($tplName)) {
                    break;
                }
            }
            $tplName = 'include/SugarFields/Fields/' . $className . '/' . $view . '.tpl';
            if (file_exists('custom/' . $tplName)) {
                $tplName = 'custom/' . $tplName;
                break;
            }
            if (file_exists($tplName)) {
                break;
            }
        }

        $tplCache[$this->type][$view] = $tplName;

        return $tplName;
    }

    /**
     * @param array $rawField
     * @param array $vardef
     * @return mixed
     */
    public function formatField($rawField, $vardef)
    {
        // The base field doesn't do any formatting, so override it in subclasses for more specific actions
        return $rawField;
    }

    /**
     * @param array $rawField
     * @param array $vardef
     * @return mixed
     */
    public function unformatField($formattedField, $vardef)
    {
        // The base field doesn't do any formatting, so override it in subclasses for more specific actions
        return $formattedField;
    }

    /**
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param integer $tabindex
     * @param string $view
     * @return string
     */
    public function getSmartyView($parentFieldArray, $vardef, $displayParams, $tabindex, $view)
    {
        if (is_null($tabindex) || !is_numeric($tabindex)) {
            $tabindex = 0;
        }

        $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);


        return $this->fetch($this->findTemplate($view));
    }

    /**
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param string $col (unused)
     * @return string
     */
    public function getListViewSmarty($parentFieldArray, $vardef, $displayParams, $col)
    {
        $tabindex = 1;
        //fixing bug #46666: don't need to format enum and radioenum fields
        //because they are already formated in SugarBean.php in the function get_list_view_array() as fix of bug #21672
        if ($this->type != 'Enum' && $this->type != 'Radioenum') {
            $parentFieldArray = $this->setupFieldArray($parentFieldArray, $vardef);
        } else {
            $vardef['name'] = strtoupper($vardef['name']);
        }

        $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex, false);

        $this->ss->left_delimiter = '{';
        $this->ss->right_delimiter = '}';
        $this->ss->assign('col', $vardef['name']);

        return $this->fetch($this->findTemplate('ListView'));
    }

    /**
     * Returns a smarty template for the DetailViews
     *
     * @param array $parentFieldArray string name of the variable in the parent template for the bean's data
     * @param array $vardef field defintion
     * @param array $displayParam parameters for display
     *      available paramters are:
     *      * labelSpan - column span for the label
     *      * fieldSpan - column span for the field
     * @param integer $tabindex
     * @returns string
     */
    public function getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        return $this->getSmartyView($parentFieldArray, $vardef, $displayParams, $tabindex, 'DetailView');
    }

    /**
     *  99% of all fields will just format like a listview, but just in case, it's here to override
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param integer $tabindex
     * @return mixed
     */
    public function getChangeLogSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        return $this->formatField($parentFieldArray[$vardef['name']], $vardef);
    }

    /**
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param integer $tabindex
     * @return string
     */
    public function getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (!empty($vardef['function']['returns']) && $vardef['function']['returns'] == 'html') {
            $type = $this->type;
            $this->type = 'Base';
            $result = $this->getDetailViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
            $this->type = $type;

            return $result;
        }
        // jpereira@dri - #Bug49513 - Readonly type not working as expected
        // If readonly is set in displayParams, the vardef will be displayed as in DetailView.
        if (isset($displayParams['readonly']) && $displayParams['readonly']) {
            return $this->getSmartyView($parentFieldArray, $vardef, $displayParams, $tabindex, 'DetailView');
        }

        // ~ jpereira@dri - #Bug49513 - Readonly type not working as expected
        return $this->getSmartyView($parentFieldArray, $vardef, $displayParams, $tabindex, 'EditView');
    }

    /**
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param integer $tabindex
     * @return string
     */
    public function getImportViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        return $this->getEditViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
    }

    /**
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param integer $tabindex
     * @return string
     */
    public function getSearchViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (!empty($vardef['auto_increment'])) {
            $vardef['len'] = 255;
        }

        return $this->getSmartyView($parentFieldArray, $vardef, $displayParams, $tabindex, 'EditView');
    }
    /**
     * @param array $parentFieldArray
     * @param array $vardef
     * @param array $displayParams
     * @param integer $tabindex
     * @return string
     */
    public function getPopupViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex)
    {
        if (is_array($displayParams) && !isset($displayParams['formName'])) {
            $displayParams['formName'] = 'popup_query_form';
        } else {
            if (empty($displayParams)) {
                $displayParams = array('formName' => 'popup_query_form');
            }
        }

        return $this->getSearchViewSmarty($parentFieldArray, $vardef, $displayParams, $tabindex);
    }

    /**
     * @param array $inputField
     * @param array $vardef
     * @param null $context
     * @return mixed
     */
    public function getEmailTemplateValue($inputField, $vardef, $context = null)
    {
        // This does not return a smarty section, instead it returns a direct value
        return $this->formatField($inputField, $vardef);
    }

    public function displayFromFunc($displayType, $parentFieldArray, $vardef, $displayParams, $tabindex = 0)
    {
        if (!is_array($vardef['function'])) {
            $funcName = $vardef['function'];
            $includeFile = '';
            $onListView = false;
            $returnsHtml = false;
        } else {
            $funcName = $vardef['function']['name'];
            $includeFile = '';
            if (isset($vardef['function']['include'])) {
                $includeFile = $vardef['function']['include'];
            }
            if (isset($vardef['function']['onListView']) && $vardef['function']['onListView'] == true) {
                $onListView = true;
            } else {
                $onListView = false;
            }
            if (isset($vardef['function']['returns']) && $vardef['function']['returns'] == 'html') {
                $returnsHtml = true;
            } else {
                $returnsHtml = false;
            }
        }

        if ($displayType === 'ListView'
            || $displayType === 'popupView'
            || $displayType === 'searchView'
        ) {
            // Traditionally, before 6.0, additional functions were never called, so this code doesn't get called unless the vardef forces it
            if ($onListView) {
                if (!empty($includeFile)) {
                    require_once($includeFile);
                }

                return $funcName(
                    $parentFieldArray,
                    $vardef['name'],
                    $parentFieldArray[strtoupper($vardef['name'])],
                    $displayType
                );
            } else {
                $displayTypeFunc = 'get' . $displayType . 'Smarty';

                return $this->$displayTypeFunc($parentFieldArray, $vardef, $displayParams, $tabindex);
            }
        } else {
            if (!empty($displayParams['idName'])) {
                $fieldName = $displayParams['idName'];
            } else {
                $fieldName = $vardef['name'];
            }
            if ($returnsHtml) {
                $this->setup($parentFieldArray, $vardef, $displayParams, $tabindex);
                $tpl = $this->findTemplate($displayType . 'Function');
                if ($tpl === '') {
                    // Can't find a function template, just use the base
                    $tpl = $this->findTemplate('DetailViewFunction');
                }

                return "<span id='{$vardef['name']}_span'>" . $this->fetch($tpl) . '</span>';
            } else {
                return '{sugar_run_helper include="' . $includeFile . '" func="' . $funcName . '" bean=$bean field="' . $fieldName . '" value=$fields.' . $fieldName . '.value displayType="' . $displayType . '"}';
            }
        }
    }

    public function getEditView()
    {
    }

    /**
     * getSearchWhereValue
     *
     * Checks and returns a sane value based on the field type that can be used when building the where clause in a
     * search form.
     *
     * @param $value Mixed value being searched on
     * @return Mixed the value for the where clause used in search
     */
    public function getSearchWhereValue($value)
    {
        return $value;
    }

    /**
     * getSearchInput
     *
     * This function allows the SugarFields to handle returning the search input value given arguments (typically from $_REQUEST/$_POST)
     * and a search string.
     *
     * @param $key String value of key to search for
     * @param $args Mixed value containing haystack to search for value in
     * @return $value Mixed value that the SugarField should return
     */
    public function getSearchInput($key = '', $args = array())
    {
        //Nothing specified return empty string
        if (empty($key) || empty($args)) {
            return '';
        }

        return isset($args[$key]) ? $args[$key] : '';
    }

    public function getQueryLike()
    {
    }

    public function getQueryIn()
    {
    }

    /**
     * Setup function to assign values to the smarty template, should be called before every display function
     * @param $parentFieldArray
     * @param $vardef
     * @param $displayParams
     * @param $tabindex
     * @param bool $twopass
     */
    public function setup($parentFieldArray, $vardef, $displayParams, $tabindex, $twopass = true)
    {
        $this->button = '';
        $this->buttons = '';
        $this->image = '';
        if ($twopass) {
            $this->ss->left_delimiter = '{{';
            $this->ss->right_delimiter = '}}';
        } else {
            $this->ss->left_delimiter = '{';
            $this->ss->right_delimiter = '}';
        }
        $this->ss->assign('parentFieldArray', $parentFieldArray);
        $this->ss->assign('vardef', $vardef);
        $this->ss->assign('tabindex', $tabindex);

        //for adding attributes to the field

        if (!empty($displayParams['field'])) {
            $plusField = '';
            foreach ($displayParams['field'] as $key => $value) {
                $plusField .= ' ' . $key . '="' . $value . '"';//bug 27381
            }
            $displayParams['field'] = $plusField;
        }
        //for adding attributes to the button
        if (!empty($displayParams['button'])) {
            $plusField = '';
            foreach ($displayParams['button'] as $key => $value) {
                $plusField .= ' ' . $key . '="' . $value . '"';
            }
            $displayParams['button'] = $plusField;
            $this->button = $displayParams['button'];
        }
        if (!empty($displayParams['buttons'])) {
            $plusField = '';
            foreach ($displayParams['buttons'] as $keys => $values) {
                foreach ($values as $key => $value) {
                    $plusField[$keys] .= ' ' . $key . '="' . $value . '"';
                }
            }
            $displayParams['buttons'] = $plusField;
            $this->buttons = $displayParams['buttons'];
        }
        if (!empty($displayParams['image'])) {
            $plusField = '';
            foreach ($displayParams['image'] as $key => $value) {
                $plusField .= ' ' . $key . '="' . $value . '"';
            }
            $displayParams['image'] = $plusField;
            $this->image = $displayParams['image'];
        }
        $this->ss->assign('displayParams', $displayParams);
    }

    protected function getAccessKey($vardef, $fieldType = null, $module = null)
    {
        global $app_strings;

        $labelList = array(
            'accessKey' => array(),
            'accessKeySelect' => array(),
            'accessKeyClear' => array(),
        );

        // Labels are always in uppercase
        if (isset($fieldType)) {
            $fieldType = strtoupper($fieldType);
        }

        if (isset($module)) {
            $module = strtoupper($module);
        }

        // The vardef is the most specific, then the module + fieldType, then the module, then the fieldType
        if (isset($vardef['accessKey'])) {
            $labelList['accessKey'][] = $vardef['accessKey'];
        }
        if (isset($vardef['accessKeySelect'])) {
            $labelList['accessKeySelect'][] = $vardef['accessKeySelect'];
        }
        if (isset($vardef['accessKeyClear'])) {
            $labelList['accessKeyClear'][] = $vardef['accessKeyClear'];
        }

        if (isset($fieldType) && isset($module)) {
            $labelList['accessKey'][] = 'LBL_ACCESSKEY_' . $fieldType . '_' . $module;
            $labelList['accessKeySelect'][] = 'LBL_ACCESSKEY_SELECT_' . $fieldType . '_' . $module;
            $labelList['accessKeyClear'][] = 'LBL_ACCESSKEY_CLEAR_' . $fieldType . '_' . $module;
        }

        if (isset($module)) {
            $labelList['accessKey'][] = 'LBL_ACCESSKEY_' . $module;
            $labelList['accessKeySelect'][] = 'LBL_ACCESSKEY_SELECT_' . $module;
            $labelList['accessKeyClear'][] = 'LBL_ACCESSKEY_CLEAR_' . $module;
        }

        if (isset($fieldType)) {
            $labelList['accessKey'][] = 'LBL_ACCESSKEY_' . $fieldType;
            $labelList['accessKeySelect'][] = 'LBL_ACCESSKEY_SELECT_' . $fieldType;
            $labelList['accessKeyClear'][] = 'LBL_ACCESSKEY_CLEAR_' . $fieldType;
        }

        // Attach the defaults to the ends
        $labelList['accessKey'][] = 'LBL_ACCESSKEY';
        $labelList['accessKeySelect'][] = 'LBL_SELECT_BUTTON';
        $labelList['accessKeyClear'][] = 'LBL_CLEAR_BUTTON';

        // Figure out the label and the key for the button.
        // Later on we may attempt to make sure there are no two buttons with the same keys, but for now we will just use whatever is specified.
        $keyTypes = array('accessKey', 'accessKeySelect', 'accessKeyClear');
        $accessKeyList = array(
            'accessKey' => '',
            'accessKeyLabel' => '',
            'accessKeyTitle' => '',
            'accessKeySelect' => '',
            'accessKeySelectLabel' => '',
            'accessKeySelectTitle' => '',
            'accessKeyClear' => '',
            'accessKeyClearLabel' => '',
            'accessKeyClearTitle' => '',
        );
        foreach ($keyTypes as $type) {
            foreach ($labelList[$type] as $tryThis) {
                if (isset($app_strings[$tryThis . '_KEY']) && isset($app_strings[$tryThis . '_TITLE']) && isset($app_strings[$tryThis . '_LABEL'])) {
                    $accessKeyList[$type] = $tryThis . '_KEY';
                    $accessKeyList[$type . 'Title'] = $tryThis . '_TITLE';
                    $accessKeyList[$type . 'Label'] = $tryThis . '_LABEL';
                    break;
                }
            }
        }

        return $accessKeyList;
    }

    /**
     * This should be called when the bean is saved. The bean itself will be passed by reference
     * @param SugarBean bean - the bean performing the save
     * @param array params - an array of paramester relevant to the save, most likely will be $_REQUEST
     */
    public function save(&$bean, $params, $field, $properties, $prefix = '')
    {
        if (isset($params[$prefix . $field])) {
            if (isset($properties['len']) && isset($properties['type']) && $this->isTrimmable($properties['type'])) {
                $bean->$field = trim($this->unformatField($params[$prefix . $field], $properties));
            } else {
                $bean->$field = $this->unformatField($params[$prefix . $field], $properties);
            }
        }
    }

    /**
     * Check if the field is allowed to be trimmed
     *
     * @param string $type
     * @return boolean
     */
    protected function isTrimmable($type)
    {
        return in_array($type, array('varchar', 'name'));
    }

    /**
     * Handles import field sanitizing for an field type
     *
     * @param  $value    string value to be sanitized
     * @param  $vardefs  array
     * @param  $focus    SugarBean object
     * @param  $settings ImportFieldSanitize object
     * @return string sanitized value or boolean false if there's a problem with the value
     */
    public function importSanitize(
        $value,
        $vardef,
        $focus,
        ImportFieldSanitize $settings
    ) {
        if (isset($vardef['len'])) {
            // check for field length
            $value = sugar_substr($value, $vardef['len']);
        }

        return $value;
    }

    /**
     * isRangeSearchView
     * This method helps determine whether or not to display the range search view code for the sugar field
     * @param array $vardef entry representing the sugar field's definition
     * @return boolean true if range search view should be displayed, false otherwise
     */
    protected function isRangeSearchView($vardef)
    {
        return !empty($vardef['enable_range_search']) && !empty($_REQUEST['action']) && $_REQUEST['action'] != 'Popup';
    }

    /**
     * setupFieldArray
     * This method takes the $parentFieldArray mixed variable which may be an Array or object and attempts
     * to call any custom fieldSpecific formatting to the value depending on the field type.
     *
     * @see SugarFieldEnum.php, SugarFieldInt.php, SugarFieldFloat.php, SugarFieldRelate.php
     * @param    mixed $parentFieldArray Array or Object of data where the field's value comes from
     * @param    array $vardef The vardef entry linked to the SugarField instance
     * @return    array   $parentFieldArray The formatted $parentFieldArray with the formatField method possibly applied
     */
    protected function setupFieldArray($parentFieldArray, $vardef)
    {
        $fieldName = $vardef['name'];
        if (is_array($parentFieldArray)) {
            $fieldNameUpper = strtoupper($fieldName);
            if (isset($parentFieldArray[$fieldNameUpper])) {
                $parentFieldArray[$fieldName] = $this->formatField($parentFieldArray[$fieldNameUpper], $vardef);
            } else {
                $parentFieldArray[$fieldName] = '';
            }
        } elseif (is_object($parentFieldArray)) {
            if (isset($parentFieldArray->$fieldName)) {
                $parentFieldArray->$fieldName = $this->formatField($parentFieldArray->$fieldName, $vardef);
            } else {
                $parentFieldArray->$fieldName = '';
            }
        }

        return $parentFieldArray;
    }
}
