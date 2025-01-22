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


require_once('modules/ModuleBuilder/parsers/views/ListLayoutMetaDataParser.php');
require_once('modules/ModuleBuilder/parsers/views/SearchViewMetaDataParser.php');
require_once 'modules/ModuleBuilder/parsers/constants.php';

class PopupMetaDataParser extends ListLayoutMetaDataParser
{

    /**
     * @var array $columns
     *  Columns is used by the view to construct the listview - each column is built by calling the named function
     */
    public $columns = array(
        'LBL_DEFAULT' => 'getDefaultFields',
        'LBL_AVAILABLE' => 'getAdditionalFields',
        'LBL_HIDDEN' => 'getAvailableFields'
    );

    /**
     * @var array $reserveProperties
     */
    public static $reserveProperties = array(
        'moduleMain',
        'varName',
        'orderBy',
        'whereClauses',
        'searchInputs',
        'create',
        'addToReserve'
    );

    /**
     * @var array $defsMap
     */
    public static $defsMap = array(MB_POPUPSEARCH => 'searchdefs', MB_POPUPLIST => 'listviewdefs');

    /**
     * @var bool $search
     */
    protected $search;


    /**
     * @var string $_view
     */
    protected $_view;

    /**
     * Constructor
     * Must set:
     * $this->columns   Array of 'Column LBL'=>function_to_retrieve_fields_for_this_column() - expected by the view
     *
     * @param string $view
     * @param string $moduleName     The name of the module to which this listview belongs
     * @param string $packageName    If not empty, the name of the package to which this listview belongs
     */
    public function __construct($view, $moduleName, $packageName = '')
    {
        $this->search = ($view == MB_POPUPSEARCH) ? true : false;
        $this->_moduleName = $moduleName;
        $this->_packageName = $packageName;
        $this->_view = $view;
        $this->columns = array('LBL_DEFAULT' => 'getDefaultFields', 'LBL_HIDDEN' => 'getAvailableFields');

        if ($this->search) {
            $this->columns = array('LBL_DEFAULT' => 'getSearchFields', 'LBL_HIDDEN' => 'getAvailableFields');
            parent::__construct(MB_POPUPSEARCH, $moduleName, $packageName);
        } else {
            parent::__construct(MB_POPUPLIST, $moduleName, $packageName);
        }

        $this->_viewdefs = $this->mergeFieldDefinitions($this->_viewdefs, $this->_fielddefs);
    }

    /**
     * Dashlets contain both a searchview and list view definition,
     * therefore we need to merge only the relevant info
     * @param array $viewdefs
     * @param array $fielddefs
     * @return array
     */
    public function mergeFieldDefinitions($viewdefs, $fielddefs)
    {
        $viewdefs = $this->_viewdefs = array_change_key_case($viewdefs);
        $viewdefs = $this->_viewdefs = $this->convertSearchToListDefs($viewdefs);

        return $viewdefs;
    }

    /**
     * @param array $defs
     * @return array
     */
    public function convertSearchToListDefs($defs)
    {
        $temp = array();
        foreach ($defs as $key => $value) {
            if (!is_array($value)) {
                $temp[$value] = array('name' => $value);
            } else {
                $temp[$key] = $value;
                if (isset($value['name']) && $value['name'] != $key) {
                    $temp[$value['name']] = $value;
                    unset($temp[$key]);
                } else {
                    if (!isset($value['name'])) {
                        $temp[$key]['name'] = $key;
                    }
                }
            }
        }

        return $temp;
    }

    /**
     * @return array
     */
    public function getOriginalViewDefs()
    {
        $defs = parent::getOriginalViewDefs();

        return $this->convertSearchToListDefs($defs);
    }

    /**
     * @return array
     */
    public function getSearchFields()
    {
        $searchFields = array();
        foreach ($this->_viewdefs as $key => $def) {
            if (isset($this->_fielddefs [$key])) {
                $searchFields [$key] = self::_trimFieldDefs($this->_fielddefs [$key]);
                if (!empty($def['label'])) {
                    $searchFields [$key]['label'] = $def['label'];
                }
            } else {
                $searchFields [$key] = $def;
            }
        }

        return $searchFields;
    }

    /**
     * @param bool $populate
     */
    public function handleSave($populate = true)
    {
        if (empty($this->_packageName)) {
            foreach (array(MB_CUSTOMMETADATALOCATION, MB_BASEMETADATALOCATION) as $value) {
                $file = $this->implementation->getFileName(MB_POPUPLIST, $this->_moduleName, null, $value);
                if (file_exists($file)) {
                    break;
                }
            }
            $writeFile = $this->implementation->getFileName(MB_POPUPLIST, $this->_moduleName, null);
            if (!file_exists($writeFile)) {
                mkdir_recursive(dirname($writeFile));
            }
        } else {
            $writeFile = $file = $this->implementation->getFileName(
                MB_POPUPLIST,
                $this->_moduleName,
                null,
                $this->_packageName
            );
        }
        $this->implementation->getHistory()->append($file);
        if ($populate) {
            $this->_populateFromRequest();
        }
        $out = "<?php\n";

        //Load current module languages
        global $mod_strings, $current_language;
        $oldModStrings = $mod_strings;
        $GLOBALS['mod_strings'] = return_module_language($current_language, $this->_moduleName);
        require($file);
        if (!isset($popupMeta)) {
            sugar_die("unable to load Module Popup Definition");
        }

        if ($this->_view == MB_POPUPSEARCH) {
            foreach ($this->_viewdefs as $k => $v) {
                if (isset($this->_viewdefs[$k]) && isset($this->_viewdefs[$k]['default'])) {
                    unset($this->_viewdefs[$k]['default']);
                }
            }
            $this->_viewdefs = $this->convertSearchToListDefs($this->_viewdefs);
            $popupMeta['searchdefs'] = $this->_viewdefs;
            $this->addNewSearchDef($this->_viewdefs, $popupMeta);
        } else {
            $popupMeta['listviewdefs'] = array_change_key_case($this->_viewdefs, CASE_UPPER);
        }

        //provide a way for users to add to the reserve properties list via the 'addToReserve' element
        $totalReserveProps = self::$reserveProperties;
        if (!empty($popupMeta['addToReserve'])) {
            $totalReserveProps = array_merge(self::$reserveProperties, $popupMeta['addToReserve']);
        }
        $allProperties = array_merge($totalReserveProps, array('searchdefs', 'listviewdefs'));

        $out .= "\$popupMeta = array (\n";
        foreach ($allProperties as $p) {
            if (isset($popupMeta[$p])) {
                $out .= "    '$p' => " . var_export_helper($popupMeta[$p]) . ",\n";
            }
        }
        $out .= ");\n";
        file_put_contents($writeFile, $out);

        //return back mod strings
        $GLOBALS['mod_strings'] = $oldModStrings;
    }

    /**
     * @param array $searchDefs
     * @param array $popupMeta
     */
    public function addNewSearchDef($searchDefs, &$popupMeta)
    {
        if (!empty($searchDefs)) {
            $this->_diffAndUpdate($searchDefs, $popupMeta['whereClauses'], true);
            $this->_diffAndUpdate($searchDefs, $popupMeta['searchInputs']);
        }
    }

    /**
     * @param array $newDefs
     * @param array $targetDefs
     * @param bool $forWhere
     */
    private function _diffAndUpdate($newDefs, &$targetDefs, $forWhere = false)
    {
        if (!is_array($targetDefs)) {
            $targetDefs = array();
        }
        foreach ($newDefs as $key => $def) {
            if (!isset($targetDefs[$key]) && $forWhere) {
                $targetDefs[$key] = $this->_getTargetModuleName($def) . '.' . $key;
            } else {
                if (!in_array($key, $targetDefs) && !$forWhere) {
                    array_push($targetDefs, $key);
                }
            }
        }

        if ($forWhere) {
            foreach (array_diff(array_keys($targetDefs), array_keys($newDefs)) as $key) {
                unset($targetDefs[$key]);
            }
        } else {
            foreach ($targetDefs as $key => $value) {
                if (!isset($newDefs[$value])) {
                    unset($targetDefs[$key]);
                }
            }
        }
    }

    /**
     * @param array $def
     * @return string
     */
    private function _getTargetModuleName($def)
    {
        $dir = strtolower($this->implementation->getModuleDir());
        if (isset($this->_fielddefs[$def['name']]) && isset($this->_fielddefs[$def['name']]['source']) && $this->_fielddefs[$def['name']]['source'] == 'custom_fields') {
            return $dir . '_cstm';
        }

        return $dir;
    }
}
