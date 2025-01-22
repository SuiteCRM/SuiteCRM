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

define('CONNECTOR_DISPLAY_CONFIG_FILE', 'custom/modules/Connectors/metadata/display_config.php');
require_once('include/connectors/ConnectorFactory.php');

/**
 * Source sorting by order value
 * @internal
 */
function sources_sort_function($a, $b)
{
    if (isset($a['order']) && isset($b['order'])) {
        if ($a['order'] == $b['order']) {
            return 0;
        }

        return ($a['order'] < $b['order']) ? -1 : 1;
    }

    return 0;
}

/**
 * Connector utilities
 * @api
 */
#[\AllowDynamicProperties]
class ConnectorUtils
{
    /**
     * Cached connectors data
     * @var array
     */
    protected static $connectors_cache;

    /**
     * Get connector data by ID
     * @param string $id
     * @param bool $refresh
     * @return null|array Connector data
     */
    public static function getConnector(
        $id,
        $refresh = false
        ) {
        $s = self::getConnectors($refresh);
        return !empty($s[$id]) ? $s[$id] : null;
    }

    /**
     * Check if external accounts are enabled for this connector
     * @param string $id
     */
    public static function eapmEnabled($id, $refresh = false)
    {
        $data = self::getConnector($id, $refresh);
        if (!$data || !isset($data["eapm"])) {
            // TODO: if we don't know this connector, should we decide it's enabled or disabled?
            return true;
        }
        return !empty($data["eapm"]["enabled"]);
    }

    /**
     * getSearchDefs
     * Returns an Array of the search field defintions Connector module to
     * search entries from the connector.  If the searchdefs.php file in the custom
     * directory is not found, it defaults to using the mapping.php file entries to
     * create a default version of the file.
     *
     * @param boolean $refresh boolean value to manually refresh the search definitions
     * @return mixed $searchdefs Array of the search definitions
     */
    public static function getSearchDefs(
        $refresh = false
        ) {
        $searchdefs = [];
        if ($refresh || !file_exists('custom/modules/Connectors/metadata/searchdefs.php')) {
            require('modules/Connectors/metadata/searchdefs.php');

            if (!file_exists('custom/modules/Connectors/metadata')) {
                mkdir_recursive('custom/modules/Connectors/metadata');
            }

            if (!write_array_to_file('searchdefs', $searchdefs, 'custom/modules/Connectors/metadata/searchdefs.php')) {
                $GLOBALS['log']->fatal("Cannot write file custom/modules/Connectors/metadata/searchdefs.php");
                return array();
            }
        }

        require('custom/modules/Connectors/metadata/searchdefs.php');
        return $searchdefs;
    }


    /**
     * getViewDefs
     * Returns an Array of the merge definitions used by the Connector module to
     * merge values into the bean instance
     *
     * @param mixed $filter_sources Array optional Array value of sources to only use
     * @return mixed $mergedefs Array of the merge definitions
     */
    public static function getViewDefs(
        $filter_sources = array()
        ) {
        //Go through all connectors and get their mapping keys and merge them across each module
        $connectors = self::getConnectors();
        $modules_sources = self::getDisplayConfig();
        $view_defs = array();
        foreach ($connectors as $id=>$ds) {
            if (!empty($filter_sources) && !isset($filter_sources[$id])) {
                continue;
            }

            if (file_exists('custom/' . $ds['directory'] . '/mapping.php')) {
                require('custom/' . $ds['directory'] . '/mapping.php');
            } else {
                if (file_exists($ds['directory'] . '/mapping.php')) {
                    require($ds['directory'] . '/mapping.php');
                }
            }

            if (!empty($mapping['beans'])) {
                foreach ($mapping['beans'] as $module=>$map) {
                    if (!empty($modules_sources[$module][$id])) {
                        if (!empty($view_defs['Connector']['MergeView'][$module])) {
                            $view_defs['Connector']['MergeView'][$module] = array_merge($view_defs['Connector']['MergeView'][$module], array_flip($map));
                        } else {
                            $view_defs['Connector']['MergeView'][$module] = array_flip($map);
                        }
                    }
                }
            }
        }

        if (!empty($view_defs['Connector']['MergeView'])) {
            foreach ($view_defs['Connector']['MergeView'] as $module=>$map) {
                $view_defs['Connector']['MergeView'][$module] = array_keys($view_defs['Connector']['MergeView'][$module]);
            }
        }

        return $view_defs;
    }


    /**
     * getMergeViewDefs
     * Returns an Array of the merge definitions used by the Connector module to
     * merge values into the bean instance
     *
     * @deprecated This method has been replaced by getViewDefs
     * @param boolean $refresh boolean value to manually refresh the mergeview definitions
     * @return mixed $mergedefs Array of the merge definitions
     */
    public static function getMergeViewDefs(
        $refresh = false
        ) {
        $viewdefs = [];
        if ($refresh || !file_exists('custom/modules/Connectors/metadata/mergeviewdefs.php')) {

            //Go through all connectors and get their mapping keys and merge them across each module
            $connectors = self::getConnectors($refresh);
            $modules_sources = self::getDisplayConfig();
            $view_defs = array();
            foreach ($connectors as $id=>$ds) {
                if (file_exists('custom/' . $ds['directory'] . '/mapping.php')) {
                    require('custom/' . $ds['directory'] . '/mapping.php');
                } else {
                    if (file_exists($ds['directory'] . '/mapping.php')) {
                        require($ds['directory'] . '/mapping.php');
                    }
                }

                if (!empty($mapping['beans'])) {
                    foreach ($mapping['beans'] as $module=>$map) {
                        if (!empty($modules_sources[$module][$id])) {
                            if (!empty($view_defs['Connector']['MergeView'][$module])) {
                                $view_defs['Connector']['MergeView'][$module] = array_merge($view_defs['Connector']['MergeView'][$module], array_flip($map));
                            } else {
                                $view_defs['Connector']['MergeView'][$module] = array_flip($map);
                            }
                        }
                    }
                }
            }

            if (!empty($view_defs['Connector']['MergeView'])) {
                foreach ($view_defs['Connector']['MergeView'] as $module=>$map) {
                    $view_defs['Connector']['MergeView'][$module] = array_keys($view_defs['Connector']['MergeView'][$module]);
                }
            }

            if (!file_exists('custom/modules/Connectors/metadata')) {
                mkdir_recursive('custom/modules/Connectors/metadata');
            }

            if (!write_array_to_file('viewdefs', $view_defs, 'custom/modules/Connectors/metadata/mergeviewdefs.php')) {
                $GLOBALS['log']->fatal("Cannot write file custom/modules/Connectors/metadata/mergeviewdefs.php");
                return array();
            }
        }

        require('custom/modules/Connectors/metadata/mergeviewdefs.php');
        return $viewdefs;
    }


    /**
     * getConnectors
     * Returns an Array of the connectors that have been loaded into the system
     * along with attributes pertaining to each connector.
     *
     * @param boolean $refresh boolean flag indicating whether or not to force rewriting the file; defaults to false
     * @returns mixed $connectors Array of the connector entries found
     */
    public static function getConnectors(
        $refresh = false
        ) {
        if (inDeveloperMode()) {
            $refresh = true;
        }

        if (!empty(self::$connectors_cache) && !$refresh) {
            return self::$connectors_cache;
        }
        //define paths
        $src1 = 'modules/Connectors/connectors/sources';
        $src2 = 'custom/modules/Connectors/connectors/sources';
        $src3 = 'custom/modules/Connectors/metadata';
        $src4 = 'custom/modules/Connectors/metadata/connectors.php';

        //if this is a templated environment, then use utilities to get the proper paths
        if (defined('TEMPLATE_URL')) {
            $src1 = SugarTemplateUtilities::getFilePath($src1);
            $src2 = SugarTemplateUtilities::getFilePath($src2);
            $src3 = SugarTemplateUtilities::getFilePath($src3);
            $src4 = SugarTemplateUtilities::getFilePath($src4);
        }

        if ($refresh || !file_exists($src4)) {
            $sources = array_merge(self::getSources($src1), self::getSources($src2));
            if (!file_exists($src3)) {
                mkdir_recursive($src3);
            }
            if (file_exists($src4)) {
                require($src4);

                //define connectors if it doesn't exist or is not an array
                if (!isset($connectors) || !is_array($connectors)) {
                    $connectors = array();
                    $err_str = string_format($GLOBALS['app_strings']['ERR_CONNECTOR_NOT_ARRAY'], array($src4));
                    $GLOBALS['log']->error($err_str);
                }

                $sources = array_merge($sources, $connectors);
            }

            if (!self::saveConnectors($sources, $src4)) {
                return array();
            }
        } //if

        require($src4);
        self::$connectors_cache = $connectors;
        return $connectors;
    }

    /**
     * Save connectors array to file
     * @param array $connectors Source data to write
     * @param string $toFile filename to use
     * @return bool success
     */
    public static function saveConnectors($connectors, $toFile = '')
    {
        if (empty($toFile)) {
            $toFile = 'custom/modules/Connectors/metadata/connectors.php';
            if (defined('TEMPLATE_URL')) {
                $toFile = SugarTemplateUtilities::getFilePath($toFile);
            }
        }

        if (!is_array($connectors)) {
            $connectors = array();
        }

        if (!write_array_to_file('connectors', $connectors, $toFile)) {
            //Log error and return empty array
            $GLOBALS['log']->fatal("Cannot write sources to file");
            return false;
        }
        self::$connectors_cache = $connectors;
        return true;
    }

    /**
     * getSources
     * Returns an Array of source entries found under the given directory
     * @param String $directory The directory to search
     * @return mixed $sources An Array of source entries
     */
    private static function getSources(
        $directory = 'modules/Connectors/connectors/sources'
        ) {
        if (file_exists($directory)) {
            $files = array();
            $files = findAllFiles($directory, $files, false, 'config\.php');
            $start = strrpos($directory, '/') == strlen($directory)-1 ? strlen($directory) : strlen($directory) + 1;
            $sources = array();
            $sources_ordering = array();
            foreach ($files as $file) {
                require($file);
                $end = strrpos((string) $file, '/') - $start;
                $source = array();
                $source['id'] = str_replace('/', '_', substr((string) $file, $start, $end));
                $source['name'] = !empty($config['name']) ? $config['name'] : $source['id'];
                $source['enabled'] = true;
                $source['directory'] = $directory . '/' . str_replace('_', '/', $source['id']);
                $order = isset($config['order']) ? $config['order'] : 99; //default to end using 99 if no order set

                $instance = ConnectorFactory::getInstance($source['id']);
                $source['eapm'] = empty($config['eapm'])?false:$config['eapm'];
                $mapping = $instance->getMapping();
                $modules = array();
                if (!empty($mapping['beans'])) {
                    foreach ($mapping['beans'] as $module=>$mapping_entry) {
                        $modules[]=$module;
                    }
                }
                $source['modules'] = $modules;
                $sources_ordering[$source['id']] = array('order'=>$order, 'source'=>$source);
            }

            usort($sources_ordering, 'sources_sort_function');
            foreach ($sources_ordering as $entry) {
                $sources[$entry['source']['id']] = $entry['source'];
            }
            return $sources;
        }
        return array();
    }


    /**
     * getDisplayConfig
     *
     */
    public static function getDisplayConfig(
        $refresh = false
        ) {
        $modules_sources = [];
        if (!file_exists(CONNECTOR_DISPLAY_CONFIG_FILE) || $refresh) {
            $sources = self::getConnectors();
            $modules_sources = array();

            //Make the directory for the config file
            if (!file_exists('custom/modules/Connectors/metadata')) {
                mkdir_recursive('custom/modules/Connectors/metadata');
            }

            if (!write_array_to_file('modules_sources', $modules_sources, CONNECTOR_DISPLAY_CONFIG_FILE)) {
                //Log error and return empty array
                $GLOBALS['log']->fatal("Cannot write \$modules_sources to " . CONNECTOR_DISPLAY_CONFIG_FILE);
            }
        }

        require(CONNECTOR_DISPLAY_CONFIG_FILE);
        return $modules_sources;
    }


    /**
     * getModuleConnectors
     *
     * @param String $module the module to get the connectors for
     * @param mixed $connectors Array of connectors mapped to the module or empty if none
     * @return array
     */
    public static function getModuleConnectors(
        $module
        ) {
        $modules_sources = self::getDisplayConfig();
        if (!empty($modules_sources) && !empty($modules_sources[$module])) {
            $sources = array();
            foreach ($modules_sources[$module] as $index => $id) {
                $sources[$id] = self::getConnector($id);
            }
            return $sources;
        } else {
            return array();
        }
    }

    /**
     * isModuleEnabled
     * Given a module name, checks to see if the module is enabled to be serviced by the connector module
     * @param String $module String name of the module
     * @return boolean $enabled boolean value indicating whether or not the module is enabled to be serviced by the connector module
     */
    public static function isModuleEnabled(
        $module
        ) {
        $modules_sources = self::getDisplayConfig();
        return !empty($modules_sources) && !empty($modules_sources[$module]) ? true : false;
    }


    /**
     * isSourceEnabled
     * Given a source id, checks to see if the source is enabled for at least one module
     * @param String $source String name of the source
     * @return boolean $enabled boolean value indicating whether or not the source is displayed in at least one module
     */
    public static function isSourceEnabled(
        $source
        ) {
        $modules_sources = self::getDisplayConfig();
        foreach ($modules_sources as $module=>$mapping) {
            foreach ($mapping as $s) {
                if ($s == $source) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * When a module has all of the sources removed from it we do not properly remove it from the viewdefs. This function
     * will handle that.
     *
     * @param String $module	 - the module in question
     */
    public static function cleanMetaDataFile(
        $module
        ) {
        $metadata_file = file_exists("custom/modules/{$module}/metadata/detailviewdefs.php") ? "custom/modules/{$module}/metadata/detailviewdefs.php" : "modules/{$module}/metadata/detailviewdefs.php";
        require($metadata_file);

        $insertConnectorButton = true;




        self::removeHoverField($viewdefs, $module);

        //Make the directory for the metadata file
        if (!file_exists("custom/modules/{$module}/metadata")) {
            mkdir_recursive("custom/modules/{$module}/metadata");
        }

        if (!write_array_to_file('viewdefs', $viewdefs, "custom/modules/{$module}/metadata/detailviewdefs.php")) {
            $GLOBALS['log']->fatal("Cannot update file custom/modules/{$module}/metadata/detailviewdefs.php");
            return false;
        }

        if (file_exists($cachedfile = sugar_cached("modules/{$module}/DetailView.tpl")) && !unlink($cachedfile)) {
            $GLOBALS['log']->fatal("Cannot delete file $cachedfile");
            return false;
        }
    }


    /**
     * updateMetaDataFiles
     * This method updates the metadata files (detailviewdefs.php) according to the settings in display_config.php
     * @return $result boolean value indicating whether or not the method successfully completed.
     */
    public static function updateMetaDataFiles()
    {
        if (file_exists(CONNECTOR_DISPLAY_CONFIG_FILE)) {
            $modules_sources = array();

            require(CONNECTOR_DISPLAY_CONFIG_FILE);

            $GLOBALS['log']->debug(var_export($modules_sources, true));
            if (!empty($modules_sources)) {
                foreach ($modules_sources as $module=>$mapping) {
                    $metadata_file = file_exists("custom/modules/{$module}/metadata/detailviewdefs.php") ? "custom/modules/{$module}/metadata/detailviewdefs.php" : "modules/{$module}/metadata/detailviewdefs.php";


                    $viewdefs = array();
                    if (!file_exists($metadata_file)) {
                        $GLOBALS['log']->info("Unable to update metadata file for module: {$module}");
                        continue;
                    } else {
                        require($metadata_file);
                    }

                    $insertConnectorButton = true;




                    self::removeHoverField($viewdefs, $module);

                    //Insert the hover field if available
                    if (!empty($mapping)) {
                        require_once('include/connectors/sources/SourceFactory.php');
                        require_once('include/connectors/formatters/FormatterFactory.php');
                        $shown_formatters = array();
                        foreach ($mapping as $id) {
                            $source = SourceFactory::getSource($id, false);
                            if ($source->isEnabledInHover() && $source->isRequiredConfigFieldsForButtonSet()) {
                                $shown_formatters[$id] = FormatterFactory::getInstance($id);
                            }
                        }

                        //Now we have to decide which field to put it on... use the first one for now
                        if (!empty($shown_formatters)) {
                            foreach ($shown_formatters as $id=>$formatter) {
                                $added_field = false;
                                $formatter_mapping = $formatter->getSourceMapping();

                                $source = $formatter->getComponent()->getSource();
                                //go through the mapping and add the hover to every field define in the mapping
                                //1) check for hover fields
                                $hover_fields = $source->getFieldsWithParams('hover', true);

                                foreach ($hover_fields as $key => $def) {
                                    if (!empty($formatter_mapping['beans'][$module][$key])) {
                                        $added_field = self::setHoverField($viewdefs, $module, $formatter_mapping['beans'][$module][$key], $id);
                                    }
                                }

                                //2) check for first mapping field
                                if (!$added_field && !empty($formatter_mapping['beans'][$module])) {
                                    foreach ($formatter_mapping['beans'][$module] as $key => $val) {
                                        $added_field = self::setHoverField($viewdefs, $module, $val, $id);
                                        if ($added_field) {
                                            break;
                                        }
                                    }
                                }
                            } //foreach



                            //Log an error message
                            if (!$added_field) {
                                $GLOBALS['log']->fatal("Unable to place hover field link on metadata for module {$module}");
                            }
                        }
                    }


                    //Make the directory for the metadata file
                    if (!file_exists("custom/modules/{$module}/metadata")) {
                        mkdir_recursive("custom/modules/{$module}/metadata");
                    }

                    if (!write_array_to_file('viewdefs', $viewdefs, "custom/modules/{$module}/metadata/detailviewdefs.php")) {
                        $GLOBALS['log']->fatal("Cannot update file custom/modules/{$module}/metadata/detailviewdefs.php");
                        return false;
                    }

                    if (file_exists($cachedfile = sugar_cached("modules/{$module}/DetailView.tpl")) && !unlink($cachedfile)) {
                        $GLOBALS['log']->fatal("Cannot delete file $cachedfile");
                        return false;
                    }
                }
            }
        }
        return true;
    }

    public static function removeHoverField(
        &$viewdefs,
        $module
        ) {
        require_once('include/SugarFields/Parsers/MetaParser.php');
        $metaParser = new MetaParser();
        if (!$metaParser->hasMultiplePanels($viewdefs[$module]['DetailView']['panels'])) {
            $keys = array_keys($viewdefs[$module]['DetailView']['panels']);
            if (!empty($keys) && count($keys) != 1) {
                $viewdefs[$module]['DetailView']['panels'] = array('default'=>$viewdefs[$module]['DetailView']['panels']);
            }
        }

        foreach ($viewdefs[$module]['DetailView']['panels'] as $panel_id=>$panel) {
            foreach ($panel as $row_id=>$row) {
                foreach ($row as $field_id=>$field) {
                    if (is_array($field) && !empty($field['displayParams']['enableConnectors'])) {
                        unset($field['displayParams']['enableConnectors']);
                        unset($field['displayParams']['module']);
                        unset($field['displayParams']['connectors']);
                        $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id] = $field;
                    }
                } //foreach
            } //foreach
        } //foreach
        return false;
    }

    public function setHoverField(
        &$viewdefs,
        $module,
        $hover_field,
        $source_id
        ) {
        //Check for metadata files that aren't correctly created
        require_once('include/SugarFields/Parsers/MetaParser.php');
        $metaParser = new MetaParser();
        if (!$metaParser->hasMultiplePanels($viewdefs[$module]['DetailView']['panels'])) {
            $keys = array_keys($viewdefs[$module]['DetailView']['panels']);
            if (!empty($keys) && count($keys) != 1) {
                $viewdefs[$module]['DetailView']['panels'] = array('default'=>$viewdefs[$module]['DetailView']['panels']);
            }
        }

        foreach ($viewdefs[$module]['DetailView']['panels'] as $panel_id=>$panel) {
            foreach ($panel as $row_id=>$row) {
                foreach ($row as $field_id=>$field) {
                    $name = is_array($field) ? $field['name'] : $field;
                    if ($name == $hover_field) {
                        if (is_array($field)) {
                            if (!empty($viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams'])) {
                                $newDisplayParam = $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams'];
                                $newDisplayParam['module'] = $module;
                                $newDisplayParam['enableConnectors'] = true;
                                if (!is_null($source_id) && !in_array($source_id, $newDisplayParam['connectors'])) {
                                    $newDisplayParam['connectors'][] = $source_id;
                                }
                                $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams'] = $newDisplayParam;
                            } else {
                                $field['displayParams'] = array('enableConnectors'=>true, 'module'=>$module, 'connectors' => array(0 => $source_id));
                                $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id] = $field;
                            }
                        } else {
                            $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id] = array('name'=>$field, 'displayParams'=>array('enableConnectors'=>true, 'module'=>$module, 'connectors' => array(0 => $source_id)));
                        }
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * setDefaultHoverField
     * Sets the hover field to the first element in the detailview screen
     *
     * @param Array $viewdefs the metadata of the detailview
     * @param String $module the Module to which the hover field should be added to
     * @return boolean True if field was added; false otherwise
     */
    private function setDefaultHoverField(
        &$viewdefs,
        $module,
        $source_id
        ) {
        foreach ($viewdefs[$module]['DetailView']['panels'] as $panel_id=>$panel) {
            foreach ($panel as $row_id=>$row) {
                foreach ($row as $field_id=>$field) {
                    if (is_array($field)) {
                        if (!empty($viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams'])) {
                            $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams']['enableConnectors'] = true;
                            $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams']['module'] = $module;
                            if (!is_null($source_id) && !in_array($source_id, $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams']['connectors'])) {
                                $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id]['displayParams']['connectors'][] = $source_id;
                            }
                        } else {
                            $field['displayParams'] = array('enableConnectors'=>true, 'module'=>$module, 'connectors' => array(0 => $source_id));
                            $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id] = $field;
                        }
                    } else {
                        $viewdefs[$module]['DetailView']['panels'][$panel_id][$row_id][$field_id] = array('name'=>$field, 'displayParams'=>array('enableConnectors'=>true, 'module'=>$module, 'connectors' => array(0 => $source_id)));
                    }
                    return true;
                } //foreach
            } //foreach
        } //foreach
      return false;
    }


    /**
     * getConnectorButtonScript
     * This method builds the HTML code for the hover link field
     *
     * @param mixed $displayParams Array value of display parameters passed from the SugarField code
     * @param mixed $smarty The Smarty object from the calling smarty code
     * @return String $code The HTML code for the hover link
     */
    public static function getConnectorButtonScript(
        $displayParams,
        $smarty
        ) {
        $module = $displayParams['module'];
        $modules_sources = self::getDisplayConfig();
        $code = '';
        $shown_sources = array();
        if (!empty($module) && !empty($displayParams['connectors'])) {
            foreach ($displayParams['connectors'] as $id) {
                if (!empty($modules_sources[$module]) && in_array($id, $modules_sources[$module])) {
                    $shown_sources[] = $id;
                }
            }

            if (empty($shown_sources)) {
                return '';
            }


            require_once('include/connectors/utils/ConnectorHtmlHelperFactory.php');
            $code = ConnectorHtmlHelperFactory::build()->getConnectorButtonCode($shown_sources, $module, $smarty);
        } //if
        return $code;
    }


    /**
     * getConnectorStrings
     * This method returns the language Strings for a given connector instance
     *
     * @param String $source_id String value of the connector id to retrive language strings for
     * @param String $language optional String value for the language to use (defaults to $GLOBALS['current_language'])
     */
    public static function getConnectorStrings(
        $source_id,
        $language = ''
        ) {
        $lang = empty($language) ? $GLOBALS['current_language'] : $language;
        $lang .= '.lang.php';
        $dir = str_replace('_', '/', $source_id);
        if (file_exists("custom/modules/Connectors/connectors/sources/{$dir}/language/{$lang}")) {
            require("custom/modules/Connectors/connectors/sources/{$dir}/language/{$lang}");
            return !empty($connector_strings) ? $connector_strings : array();
        } else {
            if (file_exists("modules/Connectors/connectors/sources/{$dir}/language/{$lang}")) {
                require("modules/Connectors/connectors/sources/{$dir}/language/{$lang}");
                return !empty($connector_strings) ? $connector_strings : array();
            } else {
                $GLOBALS['log']->error("Unable to locate language string file for source {$source_id}");
                return array();
            }
        }
    }

    /**
    * setConnectorStrings
    * This method outputs the language Strings for a given connector instance
    *
    * @param String $source_id String value of the connector id to write language strings for (e.g., ext_soap_marketo)
    * @param String $connector_strings array value of the connector_strings
    * @param String $language optional String value for the language to use (defaults to $GLOBALS['current_language'])
    */
    public static function setConnectorStrings(
        $source_id,
        $connector_strings,
        $language = ''
        ) {
        $lang = empty($language) ? $GLOBALS['current_language'] : $language;
        $lang .= '.lang.php';
        $dir = str_replace('_', '/', $source_id);

        if (!write_array_to_file("connector_strings", $connector_strings, "custom/modules/Connectors/connectors/sources/{$dir}/language/{$lang}")) {
            //Log error and return empty array
            $GLOBALS['log']->fatal("Cannot write connectory_strings to file custom/modules/Connectors/connectors/sources/{$dir}/language/{$lang}");
            return false;
        }
    }

    /**
     * installSource
     * Install the name of the source (called from ModuleInstaller.php).  Modifies the files in the custom
     * directory to add the new source in.
     *
     * @param String $source String value of the id of the connector to install
     * @return boolean $result boolean value indicating whether or not connector was installed
     */
    public static function installSource(
        $source
        ) {
        if (empty($source)) {
            return false;
        }
        //Add the source to the connectors.php file
        self::getConnectors(true);

        //Get the display config file
        self::getDisplayConfig();
        //Update the display_config.php file to show this new source
        $modules_sources = array();
        require(CONNECTOR_DISPLAY_CONFIG_FILE);
        foreach ($modules_sources as $module=>$mapping) {
            foreach ($mapping as $id=>$src) {
                if ($src == $source) {
                    unset($modules_sources[$module][$id]);
                    break;
                }
            }
        }

        //Make the directory for the config file
        if (!file_exists('custom/modules/Connectors/metadata')) {
            mkdir_recursive('custom/modules/Connectors/metadata');
        }

        if (!write_array_to_file('modules_sources', $modules_sources, CONNECTOR_DISPLAY_CONFIG_FILE)) {
            //Log error and return empty array
            $GLOBALS['log']->fatal("Cannot write \$modules_sources to " . CONNECTOR_DISPLAY_CONFIG_FILE);
        }
        return true;
    }


    /**
     * uninstallSource
     *
     * @param String $source String value of the id of the connector to un-install
     * @return boolean $result boolean value indicating whether or not connector was un-installed
     */
    public static function uninstallSource(
        $source
        ) {
        if (empty($source)) {
            return false;
        }

        //Remove the source from the connectors.php file
        self::getConnectors(true);

        //Update the display_config.php file to remove this source
        $modules_sources = array();
        require(CONNECTOR_DISPLAY_CONFIG_FILE);
        foreach ($modules_sources as $module=>$mapping) {
            foreach ($mapping as $id=>$src) {
                if ($src == $source) {
                    unset($modules_sources[$module][$id]);
                }
            }
        }

        //Make the directory for the config file
        if (!file_exists('custom/modules/Connectors/metadata')) {
            mkdir_recursive('custom/modules/Connectors/metadata');
        }

        if (!write_array_to_file('modules_sources', $modules_sources, CONNECTOR_DISPLAY_CONFIG_FILE)) {
            //Log error and return empty array
            $GLOBALS['log']->fatal("Cannot write \$modules_sources to " . CONNECTOR_DISPLAY_CONFIG_FILE);
            return false;
        }



        return true;
    }

    /**
     * hasWizardSourceEnabledForModule
     * This is a private method that returns a boolean value indicating whether or not at least one
     * source is enabled for a given module.  By enabled we mean that the source has the neccessary
     * configuration properties set as determined by the isRequiredConfigFieldsForButtonSet method.  In
     * addition, a check is made to ensure that it is a source that has been enabled for the wizard.
     *
     * @param String $module String value of module to check
     * @return boolean $enabled boolean value indicating whether or not module has at least one source enabled
     */
    private static function hasWizardSourceEnabledForModule(
        $module = ''
        ) {
        if (file_exists(CONNECTOR_DISPLAY_CONFIG_FILE)) {
            require_once('include/connectors/sources/SourceFactory.php');
            require(CONNECTOR_DISPLAY_CONFIG_FILE);
            if (!empty($modules_sources) && !empty($modules_sources[$module])) {
                foreach ($modules_sources[$module] as $id) {
                    $source = SourceFactory::getSource($id, false);
                    if (!is_null($source) && $source->isEnabledInWizard() && $source->isRequiredConfigFieldsForButtonSet()) {
                        return true;
                    }
                }
            }
            return false;
        }
        return false;
    }
}
