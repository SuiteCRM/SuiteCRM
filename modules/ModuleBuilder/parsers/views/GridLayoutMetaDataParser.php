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

require_once 'modules/ModuleBuilder/parsers/views/AbstractMetaDataParser.php';
require_once 'modules/ModuleBuilder/parsers/views/MetaDataParserInterface.php';
require_once 'modules/ModuleBuilder/parsers/constants.php';

class GridLayoutMetaDataParser extends AbstractMetaDataParser implements MetaDataParserInterface
{

    public $_view;
    /**
     * @var array $variableMap
     */
    public static $variableMap = array(
        MB_EDITVIEW => 'EditView',
        MB_DETAILVIEW => 'DetailView',
        MB_QUICKCREATE => 'QuickCreate',
    );

    /**
     * @var array $FILLER
     */
    protected $FILLER;

    /**
     * @var array $_originalViewDef
     */
    protected $_originalViewDef;

    /**
     * Constructor
     * @param string $view The view type, that is, editview, searchview etc
     * @param string $moduleName The name of the module to which this view belongs
     * @param string $packageName If not empty, the name of the package to which this view belongs
     */
    public function __construct($view, $moduleName, $packageName = '')
    {
        $GLOBALS ['log']->debug(get_class($this) . "->__construct( {$view} , {$moduleName} , {$packageName} )");

        $view = strtolower($view);

        $this->FILLER = array(
            'name' => MBConstants::$FILLER['name'],
            'label' => translate(MBConstants::$FILLER['label'])
        );

        $this->_moduleName = $moduleName;
        $this->_view = $view;

        if (empty($packageName)) {
            require_once 'modules/ModuleBuilder/parsers/views/DeployedMetaDataImplementation.php';
            $this->implementation = new DeployedMetaDataImplementation($view, $moduleName, self::$variableMap);
        } else {
            require_once 'modules/ModuleBuilder/parsers/views/UndeployedMetaDataImplementation.php';
            $this->implementation = new UndeployedMetaDataImplementation($view, $moduleName, $packageName);
        }

        $viewdefs = $this->implementation->getViewdefs();
        if (!isset(self::$variableMap [$view])) {
            self::$variableMap [$view] = $view;
        }

        if (!isset($viewdefs [self::$variableMap [$view]])) {
            sugar_die(get_class($this) . ": incorrect view variable for $view");
        }

        $viewdefs = $viewdefs [self::$variableMap [$view]];
        if (!isset($viewdefs ['templateMeta'])) {
            sugar_die(get_class($this) . ": missing templateMeta section in layout definition (case sensitive)");
        }

        if (!isset($viewdefs ['panels'])) {
            sugar_die(get_class($this) . ": missing panels section in layout definition (case sensitive)");
        }

        $this->_viewdefs = $viewdefs;
        if ($this->getMaxColumns() < 1) {
            sugar_die(get_class($this) . ": maxColumns=" . $this->getMaxColumns() . " - must be greater than 0!");
        }

        $this->_fielddefs = $this->implementation->getFielddefs();
        $this->_standardizeFieldLabels($this->_fielddefs);
        $this->_viewdefs ['panels'] = $this->_convertFromCanonicalForm(
            $this->_viewdefs ['panels'],
            $this->_fielddefs
        ); // put into our internal format
        $this->_originalViewDef = $this->getFieldsFromLayout($this->implementation->getOriginalViewdefs());
    }

    /**
     * Save a draft layout
     * @param bool $populate
     */
    public function writeWorkingFile($populate = true)
    {
        if ($populate) {
            $this->_populateFromRequest($this->_fielddefs);
        }

        $viewdefs = $this->_viewdefs;
        $viewdefs ['panels'] = $this->_convertToCanonicalForm($this->_viewdefs ['panels'], $this->_fielddefs);
        $this->implementation->save(array(self::$variableMap [$this->_view] => $viewdefs));
    }

    /**
     * Deploy the layout
     * @param boolean $populate If true (default), then update the layout first with new layout information from the $_REQUEST array
     */
    public function handleSave($populate = true)
    {
        $GLOBALS ['log']->info(get_class($this) . "->handleSave()");

        if ($populate) {
            $this->_populateFromRequest($this->_fielddefs);
        }

        $viewdefs = $this->_viewdefs;
        $viewdefs ['panels'] = $this->_convertToCanonicalForm($this->_viewdefs ['panels'], $this->_fielddefs);
        $this->implementation->deploy(array(self::$variableMap [$this->_view] => $viewdefs));
    }

    /**
     * Return the layout, padded out with (empty) and (filler) fields ready for display
     * @return array
     */
    public function getLayout()
    {
        $viewDefinitions = array();

        foreach ($this->_viewdefs ['panels'] as $panelID => $panel) {
            foreach ($panel as $rowID => $row) {
                foreach ($row as $colID => $fieldName) {
                    if (isset($this->_fielddefs [$fieldName])) {
                        $viewDefinitions [$panelID] [$rowID] [$colID] = self::_trimFieldDefs($this->_fielddefs [$fieldName]);
                    } else {
                        if (isset($this->_originalViewDef [$fieldName]) && is_array($this->_originalViewDef [$fieldName])) {
                            $viewDefinitions [$panelID] [$rowID] [$colID] = self::_trimFieldDefs($this->_originalViewDef [$fieldName]);
                        } else {
                            $viewDefinitions [$panelID] [$rowID] [$colID] = array(
                                "name" => $fieldName,
                                "label" => $fieldName
                            );
                        }
                    }
                }
            }
        }

        return $viewDefinitions;
    }

    /**
     * Return the tab definitions for tab/panel combo
     * @return array
     */
    public function getTabDefs()
    {
        $tabDefs = array();
        $this->setUseTabs(false);
        foreach ($this->_viewdefs ['panels'] as $panelID => $panel) {
            $tabDefs [strtoupper($panelID)] = array();

            // panel or tab setting
            if (isset($this->_viewdefs ['templateMeta'] ['tabDefs'] [strtoupper($panelID)] ['newTab'])
                && is_bool($this->_viewdefs ['templateMeta'] ['tabDefs'] [strtoupper($panelID)] ['newTab'])
            ) {
                $tabDefs [strtoupper($panelID)] ['newTab'] = $this->_viewdefs ['templateMeta'] ['tabDefs'] [strtoupper($panelID)] ['newTab'];
                if ($tabDefs [strtoupper($panelID)] ['newTab'] == true) {
                    $this->setUseTabs(true);
                }
            } else {
                $tabDefs [strtoupper($panelID)] ['newTab'] = false;
            }

            // collapsed panels
            if (isset($this->_viewdefs ['templateMeta'] ['tabDefs'] [strtoupper($panelID)] ['panelDefault'])
                && $this->_viewdefs ['templateMeta'] ['tabDefs'] [strtoupper($panelID)] ['panelDefault'] == 'collapsed'
            ) {
                $tabDefs [strtoupper($panelID)] ['panelDefault'] = 'collapsed';
            } else {
                $tabDefs [strtoupper($panelID)] ['panelDefault'] = 'expanded';
            }
        }

        return $tabDefs;
    }

    /**
     * Set tab definitions
     * @param $tabDefs
     */
    public function setTabDefs($tabDefs)
    {
        $this->_viewdefs ['templateMeta'] ['tabDefs'] = $tabDefs;
    }

    /**
     * @return int
     */
    public function getMaxColumns()
    {
        if (!empty($this->_viewdefs) && isset($this->_viewdefs ['templateMeta'] ['maxColumns'])) {
            return $this->_viewdefs ['templateMeta'] ['maxColumns'];
        } else {
            return 2;
        }
    }

    /**
     * @return array
     */
    public function getAvailableFields()
    {

        // Obtain the full list of valid fields in this module
        $availableFields = array();
        foreach ($this->_fielddefs as $key => $def) {
            if (GridLayoutMetaDataParser::validField($def, $this->_view) || isset($this->_originalViewDef[$key])) {
                //If the field original label existing, we should use the original label instead the label in its fielddefs.
                if (isset($this->_originalViewDef[$key]) && is_array($this->_originalViewDef[$key]) && isset($this->_originalViewDef[$key]['label'])) {
                    $availableFields [$key] = array('name' => $key, 'label' => $this->_originalViewDef[$key]['label']);
                } else {
                    $availableFields [$key] = array(
                        'name' => $key,
                        'label' => isset($def ['label']) ? $def ['label'] : $def['vname']
                    ); // layouts use 'label' not 'vname' for the label entry
                }

                $availableFields[$key]['translatedLabel'] = translate(
                    isset($def ['label']) ? $def ['label'] : $def['vname'],
                    $this->_moduleName
                );
            }
        }

        // Available fields are those that are in the Model and the original layout definition, but not already shown in the View
        // So, because the formats of the two are different we brute force loop through View and unset the fields we find in a copy of Model
        if (!empty($this->_viewdefs)) {
            foreach ($this->_viewdefs ['panels'] as $panel) {
                foreach ($panel as $row) {
                    foreach ($row as $field) {
                        unset($availableFields [$field]);
                    }
                }
            }
        }

        //eggsurplus: Bug 10329 - sort on intuitive display labels
        //sort by translatedLabel
        function cmpLabel($a, $b)
        {
            return strcmp($a["translatedLabel"], $b["translatedLabel"]);
        }

        usort($availableFields, 'cmpLabel');

        return $availableFields;
    }

    /**
     * @param $panelID
     * @return bool
     */
    public function getPanelDependency($panelID)
    {
        if (!isset($this->_viewdefs ['templateMeta']['dependency']) && !isset($this->_viewdefs ['templateMeta']['dependency'] [$panelID])) {
            return false;
        }

        return $this->_viewdefs  ['templateMeta']['dependency'] [$panelID];
    }

    /**
     * Add a new field to the layout
     * If $panelID is passed in, attempt to add to that panel, otherwise add to the first panel
     * The field is added in place of the first empty (not filler) slot after the last field in the panel; if that row is full, then a new row will be added to the end of the panel
     * and the field added to the start of it.
     * @param array $def Set of properties for the field, in same format as in the viewdefs
     * @param string|bool $panelID Identifier of the panel to add the field to; empty or false if we should use the first panel
     * @return bool
     */
    public function addField($def, $panelID = false)
    {
        if ((is_countable($this->_viewdefs ['panels']) ? count($this->_viewdefs ['panels']) : 0) == 0) {
            $GLOBALS ['log']->error(get_class($this) . "->addField(): _viewdefs empty for module {$this->_moduleName} and view {$this->_view}");
        }

        // if a panelID was not provided, use the first available panel in the list
        if (!$panelID) {
            $panelIdList = array_keys($this->_viewdefs ['panels']);
            $panelID = array_shift($panelIdList);
        }

        if (isset($this->_viewdefs ['panels'] [$panelID])) {
            $panel = $this->_viewdefs ['panels'] [$panelID];
            $lastrow = (is_countable($panel) ? count($panel) : 0) - 1; // index starts at 0
            $maxColumns = $this->getMaxColumns();
            $lastRowDef = $this->_viewdefs ['panels'] [$panelID] [$lastrow];
            for ($column = 0; $column < $maxColumns; $column++) {
                if (!isset($lastRowDef [$column])
                    || (is_array($lastRowDef [$column]) && $lastRowDef [$column]['name'] == '(empty)')
                    || (is_string($lastRowDef [$column]) && $lastRowDef [$column] == '(empty)')
                ) {
                    break;
                }
            }

            // if we're on the last column of the last row, start a new row
            if ($column >= $maxColumns) {
                $lastrow++;
                $this->_viewdefs ['panels'] [$panelID] [$lastrow] = array();
                $column = 0;
            }

            $this->_viewdefs ['panels'] [$panelID] [$lastrow] [$column] = $def ['name'];
            // now update the fielddefs
            if (isset($this->_fielddefs [$def ['name']])) {
                $this->_fielddefs [$def ['name']] = array_merge($this->_fielddefs [$def ['name']], $def);
            } else {
                $this->_fielddefs [$def ['name']] = $def;
            }
        }

        return true;
    }

    /**
     * Remove all instances of a field from the layout, and replace by (filler)
     * Filler because we attempt to preserve the customized layout as much as possible - replacing by (empty) would mean that the positions or sizes of adjacent fields may change
     * If the last row of a panel only consists of (filler) after removing the fields, then remove the row also. This undoes the standard addField() scenario;
     * If the fields had been moved around in the layout however then this will not completely undo any addField()
     * @param string $fieldName Name of the field to remove
     * @return boolean True if the field was removed; false otherwise
     */
    public function removeField($fieldName)
    {
        $GLOBALS ['log']->info(get_class($this) . "->removeField($fieldName)");

        $result = false;
        reset($this->_viewdefs);
        $firstPanelID = array_shift(array_keys($this->_viewdefs['panels']));

        foreach ($this->_viewdefs ['panels'] as $panelID => $panel) {
            $lastRowTouched = false;
            $lastRowID = (is_countable($this->_viewdefs ['panels'] [$panelID]) ? count($this->_viewdefs ['panels'] [$panelID]) : 0) - 1; // zero offset

            foreach ($panel as $rowID => $row) {
                foreach ($row as $colID => $field) {
                    if ($field == $fieldName) {
                        $lastRowTouched = $rowID;
                        $this->_viewdefs ['panels'] [$panelID] [$rowID] [$colID] = $this->FILLER ['name'];
                    }
                }
            }

            // if we removed a field from the last row of this panel, tidy up if the last row now consists only of (empty) or (filler)

            if ($lastRowTouched == $lastRowID) {
                $lastRow = $this->_viewdefs ['panels'] [$panelID] [$lastRowID]; // can't use 'end' for this as we need the key as well as the value...

                $empty = true;

                foreach ($lastRow as $colID => $field) {
                    $empty &= $field == MBConstants::$EMPTY ['name'] || $field == $this->FILLER ['name'];
                }

                if ($empty) {
                    unset($this->_viewdefs ['panels'] [$panelID] [$lastRowID]);
                    // if the row was the only one in the panel, and the panel is not the first (default) panel, then remove the panel also
                    if ((is_countable($this->_viewdefs ['panels'] [$panelID]) ? count($this->_viewdefs ['panels'] [$panelID]) : 0) == 0 && $panelID != $firstPanelID) {
                        unset($this->_viewdefs ['panels'] [$panelID]);
                    }
                }
            }

            $result |= ($lastRowTouched !== false); // explicitly compare to false as row 0 will otherwise evaluate as false
        }

        return $result;
    }

    /**
     * @param string $panelID
     * @param $dependency
     * @return bool
     */
    public function setPanelDependency($panelID, $dependency)
    {
        // only accept dependencies for pre-existing panels
        if (!isset($this->_viewdefs ['panels'] [$panelID])) {
            return false;
        }

        $this->_viewdefs  ['templateMeta'] ['dependency'] [$panelID] = $dependency;

        return true;
    }

    /**
     * Return an integer value for the next unused panel identifier, such that it and any larger numbers are guaranteed to be unused already in the layout
     * Necessary when adding new panels to a layout
     * @return integer First unique panel ID suffix
     */
    public function getFirstNewPanelId()
    {
        $firstNewPanelId = 0;
        foreach ($this->_viewdefs ['panels'] as $panelID => $panel) {
            // strip out all but the numerics from the panelID - can't just use a cast as numbers may not be first in the string
            for ($i = 0, $result = ''; $i < strlen((string) $panelID); $i++) {
                if (is_numeric($panelID [$i])) {
                    $result .= $panelID [$i];
                }
            }

            $firstNewPanelId = max(( int )$result, $firstNewPanelId);
        }

        return $firstNewPanelId + 1;
    }

    /**
     * Load the panel layout from the submitted form and update the _viewdefs
     * @param array $fieldDefinitions
     */
    protected function _populateFromRequest(&$fieldDefinitions)
    {
        $GLOBALS ['log']->debug(get_class($this) . "->populateFromRequest()");
        $i = 1;

        // set up the map of panel# (as provided in the _REQUEST) to panel ID (as used in $this->_viewdefs['panels'])
        $i = 1;
        foreach ($this->_viewdefs ['panels'] as $panelID => $panel) {
            $panelMap [$i++] = $panelID;
        }

        foreach ($_REQUEST as $key => $displayLabel) {
            $components = explode('-', $key);
            if ($components [0] == 'panel' && $components [2] == 'label') {
                $panelMap [$components ['1']] = $displayLabel;
            }
        }

        $this->_viewdefs ['panels'] = array(); // because the new field properties should replace the old fields, not be merged

        // run through the $_REQUEST twice - first to obtain the fieldnames, the second to update the field properties
        for ($pass = 1; $pass <= 2; $pass++) {
            foreach ($_REQUEST as $slot => $value) {
                $slotComponents = explode('-', $slot); // [0] = 'slot', [1] = panel #, [2] = slot #, [3] = property name

                if ($slotComponents [0] == 'slot') {
                    $slotNumber = $slotComponents ['2'];
                    $panelID = $panelMap [$slotComponents ['1']];
                    $rowID = floor($slotNumber / $this->getMaxColumns());
                    $colID = $slotNumber - ($rowID * $this->getMaxColumns());
                    $property = $slotComponents ['3'];

                    //If this field has a custom definition, copy that over
                    if ($pass == 1) {
                        if ($property == 'name') {
                            $this->_viewdefs ['panels'] [$panelID] [$rowID] [$colID] = $value;
                        }
                    } else {
                        // update fielddefs for this property in the provided position
                        if (isset($this->_viewdefs ['panels'] [$panelID] [$rowID] [$colID])) {
                            $fieldname = $this->_viewdefs ['panels'] [$panelID] [$rowID] [$colID];
                            $fieldDefinitions [$fieldname] [$property] = $value;
                        }
                    }
                }
            }
        }

        //Set the tab definitions
        $tabDefs = array();
        $this->setUseTabs(false);
        foreach ($this->_viewdefs ['panels'] as $panelID => $panel) {
            // panel or tab setting
            $tabDefs [strtoupper($panelID)] = array();
            if (isset($_REQUEST['tabDefs_' . $panelID . '_newTab'])) {
                $tabDefs [strtoupper($panelID)] ['newTab'] = ($_REQUEST['tabDefs_' . $panelID . '_newTab'] == '1') ? true : false;
                if ($tabDefs [strtoupper($panelID)] ['newTab'] == true) {
                    $this->setUseTabs(true);
                }
            } else {
                $tabDefs [strtoupper($panelID)] ['newTab'] = false;
            }

            // collapse panel
            if (isset($_REQUEST['tabDefs_' . $panelID . '_panelDefault'])) {
                $tabDefs [strtoupper($panelID)] ['panelDefault'] = ($_REQUEST['tabDefs_' . $panelID . '_panelDefault'] == 'collapsed') ? 'collapsed' : 'expanded';
            } else {
                $tabDefs [strtoupper($panelID)] ['panelDefault'] = 'expanded';
            }
        }
        $this->setTabDefs($tabDefs);

        //bug: 38232 - Set the sync detail and editview settings
        if (isset($_REQUEST['sync_detail_and_edit'])) {
            if ($_REQUEST['sync_detail_and_edit'] === false || $_REQUEST['sync_detail_and_edit'] === "false") {
                $this->setSyncDetailEditViews(false);
            } elseif (!empty($_REQUEST['sync_detail_and_edit'])) {
                $this->setSyncDetailEditViews(true);
            }
        }

        $GLOBALS ['log']->debug(print_r($this->_viewdefs ['panels'], true));
    }

    /**
     *  Convert our internal format back to the standard Canonical MetaData layout
     *  First non-(empty) field goes in at column 0; all other (empty)'s removed
     *  Studio required fields are also added to the layout.
     *  Do this AFTER reading in all the $_REQUEST parameters as can't guarantee the order of those, and we need to operate on complete rows
     * @param array $panels
     * @param array $fieldDefinitions
     * @return mixed
     */
    protected function _convertToCanonicalForm($panels, $fieldDefinitions)
    {
        $previousViewDef = $this->getFieldsFromLayout($this->implementation->getViewdefs());
        $oldDefs = $this->implementation->getViewdefs();
        $currentFields = $this->getFieldsFromLayout($this->_viewdefs);
        foreach ($fieldDefinitions as $field => $def) {
            if (self::fieldIsRequired($def) && !isset($currentFields[$field])) {
                //Use the previous viewdef if this field was on it.
                if (isset($previousViewDef[$field])) {
                    $def = $previousViewDef[$field];
                } //next see if the field was on the original layout.
                else {
                    if (isset($this->_originalViewDef [$field])) {
                        $def = $this->_originalViewDef [$field];
                    } //Otherwise make up a viewdef for it from field_defs
                    else {
                        $def = self::_trimFieldDefs($def);
                    }
                }
                $this->addField($def);
            }
        }

        foreach ($panels as $panelID => $panel) {
            // remove all (empty)s
            foreach ($panel as $rowID => $row) {
                $startOfRow = true;
                $offset = 0;
                foreach ($row as $colID => $fieldname) {
                    if ($fieldname == MBConstants::$EMPTY['name']) {
                        // if a leading (empty) then remove (by noting that remaining fields need to be shuffled along)
                        if ($startOfRow) {
                            $offset++;
                        }
                        unset($row [$colID]);
                    } else {
                        $startOfRow = false;
                    }
                }

                // reindex to remove leading (empty)s and replace fieldnames by full definition from fielddefs
                $newRow = array();
                foreach ($row as $colID => $fieldname) {
                    if ($fieldname == null) {
                        continue;
                    }
                    //Backwards compatibility and a safeguard against multiple calls to _convertToCanonicalForm
                    if (is_array($fieldname)) {
                        $newRow [$colID - $offset] = $fieldname;
                        continue;
                    }
                    if (!isset($fieldDefinitions[$fieldname]) && $fieldname != $this->FILLER['name']) {
                        continue;
                    }

                    //Replace (filler) with the empty string
                    if ($fieldname == $this->FILLER['name']) {
                        $newRow [$colID - $offset] = '';
                    } //Use the previous viewdef if this field was on it.
                    else {
                        if (isset($previousViewDef[$fieldname])) {
                            $newRow[$colID - $offset] = $this->getNewRowItem(
                                $previousViewDef[$fieldname],
                                $fieldDefinitions[$fieldname]
                            );
                        } //next see if the field was on the original layout.
                        else {
                            if (isset($this->_originalViewDef [$fieldname])) {
                                $newRow[$colID - $offset] = $this->getNewRowItem(
                                    $this->_originalViewDef[$fieldname],
                                    $fieldDefinitions[$fieldname]
                                );
                            } //Otherwise make up a viewdef for it from field_defs
                            else {
                                if (isset($fieldDefinitions [$fieldname])) {
                                    $newRow [$colID - $offset] = self::_trimFieldDefs($fieldDefinitions [$fieldname]);
                                } //No additional info on this field can be found, jsut use the name;
                                else {
                                    $newRow [$colID - $offset] = $fieldname;
                                }
                            }
                        }
                    }
                }
                $panels [$panelID] [$rowID] = $newRow;
            }
        }

        return $panels;
    }

    /**
     * fixing bug #44428: Studio | Tab Order causes layout errors
     * @param string|array $source it can be a string which contain just a name of field
     *                                  or an array with field attributes including name
     * @param array $fieldDefinitions stores field defs from request
     * @return string|array definition of new row item
     */
    public function getNewRowItem($source, $fieldDefinitions)
    {
        //We should copy over the tabindex if it is set.
        $newRow = array();
        if (isset($fieldDefinitions) && !empty($fieldDefinitions['tabindex'])) {
            if (is_array($source)) {
                $newRow = $source;
            } else {
                $newRow['name'] = $source;
            }
            $newRow['tabindex'] = $fieldDefinitions['tabindex'];
        } else {
            $newRow = $source;
        }

        return $newRow;
    }

    /**
     * Convert from the standard MetaData format to our internal format
     * Replace NULL with (filler) and missing entries with (empty)
     * @param array $panels
     * @param array $fieldDefinitions
     * @return array
     */
    protected function _convertFromCanonicalForm($panels, $fieldDefinitions)
    {
        $newPanels = array();
        if (empty($panels)) {
            return $newPanels;
        }

        // Fix for a flexibility in the format of the panel sections - if only one panel, then we don't have a panel level defined,
        // it goes straight into rows
        // See EditView2 for similar treatment
        if (!empty($panels) && count($panels) > 0) {
            $keys = array_keys($panels);
            if (is_numeric($keys [0])) {
                $defaultPanel = $panels;
                unset($panels); //blow away current value
                $panels ['default'] = $defaultPanel;
            }
        }


        // replace '' with (filler)
        foreach ($panels as $panelID => $panel) {
            foreach ($panel as $rowID => $row) {
                $cols = 0;
                foreach ($row as $colID => $col) {
                    if (!empty($col)) {
                        if (is_string($col)) {
                            $fieldname = $col;
                        } else {
                            if (!empty($col ['name'])) {
                                $fieldname = $col ['name'];
                            }
                        }
                    } else {
                        $fieldname = $this->FILLER['name'];
                    }

                    $newPanels [$panelID] [$rowID] [$cols] = $fieldname;
                    $cols++;
                }
            }
        }

        // replace missing fields with (empty)
        foreach ($newPanels as $panelID => $panel) {
            $column = 0;
            foreach ($panel as $rowID => $row) {
                // pad between fields on a row
                foreach ($row as $colID => $col) {
                    for ($i = $column + 1; $i < $colID; $i++) {
                        $row [$i] = MBConstants::$EMPTY ['name'];
                    }
                    $column = $colID;
                }
                // now pad out to the end of the row
                if (($column + 1) < $this->getMaxColumns()) { // last column is maxColumns-1
                    for ($i = $column + 1; $i < $this->getMaxColumns(); $i++) {
                        $row [$i] = MBConstants::$EMPTY ['name'];
                    }
                }
                ksort($row);
                $newPanels [$panelID] [$rowID] = $row;
            }
        }

        return $newPanels;
    }

    /**
     * @param $viewDefinitions
     * @return array
     */
    protected function getFieldsFromLayout($viewDefinitions)
    {
        if (isset($viewDefinitions['panels'])) {
            $panels = $viewDefinitions['panels'];
        } else {
            $panels = $viewDefinitions[self::$variableMap [$this->_view]]['panels'];
        }

        $ret = array();
        if (is_array($panels)) {
            foreach ($panels as $rows) {
                foreach ($rows as $fields) {
                    if (!is_array($fields)) {
                        $ret[$fields] = $fields;
                        continue;
                    }
                    foreach ($fields as $field) {
                        if (is_array($field) && !empty($field['name'])) {
                            $ret[$field['name']] = $field;
                        } else {
                            if (!is_array($field)) {
                                $ret[$field] = $field;
                            }
                        }
                    }
                }
            }
        }

        return $ret;
    }

    /**
     * @param array $fieldDefinitions
     * @return bool
     */
    protected function fieldIsRequired($fieldDefinitions)
    {
        if (isset($fieldDefinitions['studio'])) {
            if (is_array($fieldDefinitions['studio'])) {
                if (!empty($fieldDefinitions['studio'][$this->_view]) && $fieldDefinitions['studio'][$this->_view] == "required") {
                    return true;
                } else {
                    if (!empty($fieldDefinitions['studio']['required']) && $fieldDefinitions['studio']['required'] == true) {
                        return true;
                    }
                }
            } else {
                if ($fieldDefinitions['studio'] == "required") {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * @param array $fieldDefinitions
     * @return array
     */
    public static function _trimFieldDefs($fieldDefinitions)
    {
        $ret = array_intersect_key(
            $fieldDefinitions,
            array(
                'studio' => true,
                'name' => true,
                'label' => true,
                'displayParams' => true,
                'comment' => true,
                'customCode' => true,
                'customLabel' => true,
                'tabindex' => true,
                'hideLabel' => true
            )
        );
        if (!empty($fieldDefinitions['vname']) && empty($fieldDefinitions['label'])) {
            $ret['label'] = $fieldDefinitions['vname'];
        }

        return $ret;
    }

    /**
     * @return bool
     */
    public function getUseTabs()
    {
        if (isset($this->_viewdefs  ['templateMeta']['useTabs'])) {
            return $this->_viewdefs  ['templateMeta']['useTabs'];
        }

        return false;
    }

    /**
     * @param $useTabs
     */
    public function setUseTabs($useTabs)
    {
        $this->_viewdefs  ['templateMeta']['useTabs'] = $useTabs;
    }

    /**
     * Return whether the Detail & EditView should be in sync.
     * @return bool
     */
    public function getSyncDetailEditViews()
    {
        if (isset($this->_viewdefs  ['templateMeta']['syncDetailEditViews'])) {
            return $this->_viewdefs  ['templateMeta']['syncDetailEditViews'];
        }

        return false;
    }

    /**
     * Sync DetailView & EditView. This should only be set on the EditView
     * @param bool $syncDetailEditViews
     */
    public function setSyncDetailEditViews($syncDetailEditViews)
    {
        $this->_viewdefs  ['templateMeta']['syncDetailEditViews'] = $syncDetailEditViews;
    }

    /**
     * Getter function to get the implementation method which is a private variable
     * @return DeployedMetaDataImplementation
     */
    public function getImplementation()
    {
        return $this->implementation;
    }

    /**
     * Public access to _convertFromCanonicalForm
     * @param  array $panels
     * @param  array $fieldDefinitions
     * @return array
     */
    public function convertFromCanonicalForm($panels, $fieldDefinitions)
    {
        return $this->_convertFromCanonicalForm($panels, $fieldDefinitions);
    }

    /**
     * Public access to _convertToCanonicalForm
     * @param  $panels
     * @param  $fieldDefinitions
     * @return array
     */
    public function convertToCanonicalForm($panels, $fieldDefinitions)
    {
        return $this->_convertToCanonicalForm($panels, $fieldDefinitions);
    }


    /**
     * @return array list of fields in this module that have the calculated property
     */
    public function getCalculatedFields()
    {
        $ret = array();
        foreach ($this->_fielddefs as $field => $def) {
            if (!empty($def['calculated']) && !empty($def['formula'])) {
                $ret[] = $field;
            }
        }

        return $ret;
    }

    /**
     * @param string $targetPanel
     * @return array|RecursiveIteratorIterator array fields in the given panel
     */
    public function getFieldsInPanel($targetPanel)
    {
        return iterator_to_array(new RecursiveIteratorIterator(new RecursiveArrayIterator($this->_viewdefs['panels'][$targetPanel])));
    }
}
