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
require_once 'modules/ModuleBuilder/parsers/constants.php';

class SubpanelMetaDataParser extends ListLayoutMetaDataParser
{
    public $_invisibleFields;
    /**
     * @var array $columns
     * Columns is used by the view to construct the listview - each column is built by calling the named function
     */
    public $columns = array('LBL_DEFAULT' => 'getDefaultFields', 'LBL_HIDDEN' => 'getAvailableFields');
    /**
     * @var string $labelIdentifier labels in the subpanel defs are tagged 'vname' =>
     */
    protected $labelIdentifier = 'vname';

    /**
     * SubpanelMetaDataParser constructor.
     * Must set:
     * $this->columns   Array of 'Column LBL'=>function_to_retrieve_fields_for_this_column() - expected by the view
     *
     * @param string $subpanelName
     * @param string $moduleName
     * @param string $packageName
     */
    public function __construct($subpanelName, $moduleName, $packageName = '')
    {
        $GLOBALS ['log']->debug(get_class($this) . ": __construct()");

        // TODO: check the implementations
        if (empty($packageName)) {
            require_once 'modules/ModuleBuilder/parsers/views/DeployedSubpanelImplementation.php';
            $this->implementation = new DeployedSubpanelImplementation($subpanelName, $moduleName);
        //$this->originalViewDef = $this->implementation->getOriginalDefs ();
        } else {
            require_once 'modules/ModuleBuilder/parsers/views/UndeployedSubpanelImplementation.php';
            $this->implementation = new UndeployedSubpanelImplementation($subpanelName, $moduleName, $packageName);
        }

        $this->_viewdefs = array_change_key_case($this->implementation->getViewdefs()); // force to lower case so don't have problems with case mismatches later
        $this->_fielddefs = $this->implementation->getFielddefs();
        $this->_standardizeFieldLabels($this->_fielddefs);
        $GLOBALS['log']->debug(get_class($this) . "->__construct(): viewdefs = " . print_r($this->_viewdefs, true));
        $GLOBALS['log']->debug(get_class($this) . "->__construct(): viewdefs = " . print_r($this->_viewdefs, true));
        $this->_invisibleFields = $this->findInvisibleFields($this->_viewdefs);

        $GLOBALS['log']->debug(get_class($this) . "->__construct(): invisibleFields = " . print_r(
            $this->_invisibleFields,
            true
        ));
    }

    /**
     * Save the layout
     * @param bool $populate
     */
    public function handleSave($populate = true)
    {
        if ($populate) {
            $this->_populateFromRequest();
            if (isset($_REQUEST['subpanel_title']) && isset($_REQUEST['subpanel_title_key'])) {
                $selected_lang = (!empty($_REQUEST['selected_lang']) ? $_REQUEST['selected_lang'] : $_SESSION['authenticated_user_language']);
                if (empty($selected_lang)) {
                    $selected_lang = $GLOBALS['sugar_config']['default_language'];
                }
                require_once 'modules/ModuleBuilder/parsers/parser.label.php';
                $labelParser = new ParserLabel(
                    $_REQUEST['view_module'],
                    isset($_REQUEST ['view_package']) ? $_REQUEST ['view_package'] : null
                );
                $labelParser::addLabels(
                    $selected_lang,
                    array($_REQUEST['subpanel_title_key'] => remove_xss(from_html($_REQUEST['subpanel_title']))),
                    $_REQUEST['view_module']
                );
            }
        }
        // Bug 46291 - Missing widget_class for edit_button and remove_button
        foreach ($this->_viewdefs as $key => $def) {
            if (isset($this->_fielddefs [$key] ['widget_class'])) {
                $this->_viewdefs [$key] ['widget_class'] = $this->_fielddefs [$key] ['widget_class'];
            }
        }
        $defs = $this->restoreInvisibleFields(
            $this->_invisibleFields,
            $this->_viewdefs
        ); // unlike our parent, do not force the field names back to upper case
        $defs = $this->makeRelateFieldsAsLink($defs);
        $this->implementation->deploy($defs);
    }

    /**
     * Return a list of the default fields for a subpanel
     * TODO: have this return just a list of fields, without definitions
     * @return array    List of default fields as an array, where key = value = <field name>
     */
    public function getDefaultFields()
    {
        $defaultFields = array();
        foreach ($this->_viewdefs as $key => $def) {
            if (empty($def ['usage']) || strcmp($def ['usage'], 'query_only') == 1) {
                $defaultFields [strtolower($key)] = $this->_viewdefs [$key];
            }
        }

        return $defaultFields;
    }

    /**
     * Find the query_only fields in the viewdefs
     * Query_only fields are used by the MVC to generate the subpanel but are not editable - they must be maintained in the layout
     * @param array $viewdefs The viewdefs to be searched for invisible fields
     * @return array of invisible fields, ready to be provided to $this->restoreInvisibleFields
     */
    public function findInvisibleFields($viewdefs)
    {
        $invisibleFields = array();
        foreach ($viewdefs as $name => $def) {
            if (isset($def ['usage']) && ($def ['usage'] == 'query_only')) {
                $invisibleFields [$name] = $def;
            }
        }

        return $invisibleFields;
    }

    /**
     * @param $invisibleFields
     * @param $viewdefs
     * @return mixed
     */
    public function restoreInvisibleFields($invisibleFields, $viewdefs)
    {
        foreach ($invisibleFields as $name => $def) {
            $viewdefs [$name] = $def;
        }

        return $viewdefs;
    }

    /**
     * @param array $fieldDefinitions
     * @return array
     */
    public static function _trimFieldDefs($fieldDefinitions)
    {
        $listDef = parent::_trimFieldDefs($fieldDefinitions);
        if (isset($listDef ['label'])) {
            $listDef ['vname'] = $listDef ['label'];
            unset($listDef ['label']);
        }

        return $listDef;
    }

    /**
     * makeRelateFieldsAsLink
     * This method will go through the subpanel definition entries being saved and then apply formatting to any that are
     * relate field so that a link to the related record may be shown in the subpanel code.  This is done by adding the
     * widget_class, target_module and target_record_key deltas to the related subpanel definition entry.
     *
     * @param Array of subpanel definitions to possibly alter
     * @return $defs Array of formatted subpanel definition entries to include any relate field attributes for Subpanels
     */
    protected function makeRelateFieldsAsLink($defs)
    {
        foreach ($defs as $index => $fieldData) {
            if ((isset($fieldData['type']) && $fieldData['type'] == 'relate')
                || (isset($fieldData['link']) && self::isTrue($fieldData['link']))
            ) {
                $defs[$index]['widget_class'] = 'SubPanelDetailViewLink';
                $defs[$index]['target_module'] = isset($this->_fielddefs[$index]['module']) ? $this->_fielddefs[$index]['module'] : $this->_moduleName;
                $defs[$index]['target_record_key'] = $this->_fielddefs[$index]['id_name'];
            }
        }

        return $defs;
    }
}
