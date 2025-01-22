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

#[\AllowDynamicProperties]
abstract class AbstractMetaDataParser
{

    /**
     * @var array $_fielddefs
     */
    public $_fielddefs;

    /**
     * @var array $_viewdefs
     */
    public $_viewdefs;

    /**
     * @var string $_moduleName
     */
    protected $_moduleName;

    /**
     * @var AbstractMetaDataImplementation|DeployedMetaDataImplementation|DeployedSubpanelImplementation|UndeployedMetaDataImplementation|UndeployedSubpanelImplementation $implementation
     * object to handle the reading and writing of files and field data
     */
    protected $implementation;

    /**
     * @var History $_history
     */
    protected $_history;

    /**
     * @var string $_packageName
     */
    protected $_packageName;

    /**
     * @var string $view
     */
    protected $view;

    /**
     * @var array $_panels
     */
    protected $_panels;

    /**
     * @see AbstractMetaDataParser::_panels
     */
    public function getLayoutAsArray()
    {
        return $this->_panels;
    }

    /**
     * @return string
     */
    public function getLanguage()
    {
        return $this->implementation->getLanguage();
    }

    /**
     * @return mixed
     */
    public function getHistory()
    {
        return $this->implementation->getHistory();
    }

    /**
     * @return array field definitions
     */
    public function getFieldDefs()
    {
        return $this->_fielddefs;
    }

    /**
     * @param string $fieldName
     * @return bool
     */
    public function removeField($fieldName)
    {
        return false;
    }

    /**
     * Is this field something we wish to show in Studio/ModuleBuilder layout editors?
     * @param array $def Field definition in the standard SugarBean field definition format - name, vname, type and so on
     * @param string $view Field definition in the standard SugarBean field definition format - name, vname, type and so on
     * @return boolean      True if ok to show, false otherwise
     */
    public static function validField($def, $view = '')
    {
        //Studio invisible fields should always be hidden
        if (isset($def['studio'])) {
            if (is_array($def ['studio'])) {
                if (!empty($view) && isset($def ['studio'][$view])) {
                    return $def ['studio'][$view] !== false && $def ['studio'][$view] !== 'false' && $def ['studio'][$view] !== 'hidden';
                }
                if (isset($def ['studio']['visible'])) {
                    return $def ['studio']['visible'];
                }
            } else {
                return ($def ['studio'] !== 'false' && $def ['studio'] !== 'hidden' && $def ['studio'] !== false);
            }
        }

        // bug 19656: this test changed after 5.0.0b - we now remove all ID type fields - whether set as type, or dbtype, from the fielddefs
        return
            (
                (
                    (empty($def ['source']) || $def ['source'] === 'db' || $def ['source'] === 'custom_fields')
                    && isset($def ['type']) && $def ['type'] !== 'id' && $def ['type'] !== 'parent_type'
                    && (empty($def ['dbType']) || $def ['dbType'] !== 'id')
                    && (isset($def ['name']) && strcmp($def ['name'], 'deleted') != 0)
                ) // db and custom fields that aren't ID fields
                ||
                // exclude fields named *_name regardless of their type...just convention
                (isset($def ['name']) && substr((string) $def ['name'], -5) === '_name'));
    }

    /**
     * @param array $fielddefs
     */
    protected function _standardizeFieldLabels(&$fielddefs)
    {
        foreach ($fielddefs as $key => $def) {
            if (!isset($def ['label'])) {
                $fielddefs [$key] ['label'] = (isset($def ['vname'])) ? $def ['vname'] : $key;
            }
        }
    }

    /**
     * @param array $fieldDefinitions
     */
    public static function _trimFieldDefs($fieldDefinitions)
    {
    }

    /**
     * @return array
     */
    public function getRequiredFields()
    {
        $fieldDefs = $this->implementation->getFielddefs();
        $newAry = array();
        foreach ($fieldDefs as $field) {
            if (isset($field['required']) && $field['required'] && isset($field['name']) && empty($field['readonly'])) {
                array_push($newAry, '"' . $field['name'] . '"');
            }
        }

        return $newAry;
    }

    /**
     * Used to determine if a field property is true or false given that it could be
     * the boolean value or a string value such use 'false' or '0'
     * @static
     * @param $val
     * @return bool
     */
    protected static function isTrue($val)
    {
        if (is_string($val)) {
            $str = strtolower($val);

            return ($str !== '0' && $str !== 'false' && $str !== '');
        } else {
            // For non-string types, juse use PHP's normal boolean conversion
            return ($val === true);
        }
    }

    /**
     * @param bool $populate
     */
    abstract public function handleSave($populate = true);
}
