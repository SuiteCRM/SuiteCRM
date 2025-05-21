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


require_once('include/connectors/sources/SourceFactory.php');

#[\AllowDynamicProperties]
class ViewMappingProperties extends ViewList
{
    /**
     * @see SugarView::process()
     */
    public function process()
    {
        $this->options['show_all'] = false;
        $this->options['show_javascript'] = true;
        $this->options['show_footer'] = false;
        $this->options['show_header'] = false;
        parent::process();
    }
    
    /**
     * @see SugarView::display()
     */
    public function display()
    {
        require_once('include/connectors/utils/ConnectorUtils.php');
        require_once('include/connectors/sources/SourceFactory.php');
        $connector_strings = ConnectorUtils::getConnectorStrings($_REQUEST['source_id']);
        $sources = ConnectorUtils::getConnectors();
        $source_id = $_REQUEST['source_id'];
        $source = SourceFactory::getSource($source_id);
        $is_enabled = ConnectorUtils::isSourceEnabled($source_id);
        $script = '';
        $display_data = array();
        if ($is_enabled) {
            $mapping = $source->getMapping();
            $source_defs = $source->getFieldDefs();
        
            //Create the Javascript code to dynamically add the tables
            $json = getJSONobj();
            foreach ($mapping['beans'] as $module=>$field_mapping) {
                $mod_strings = return_module_language($GLOBALS['current_language'], $module);
                $bean = loadBean($module);
                if (!is_object($bean)) {
                    continue;
                }
                $field_defs = $bean->getFieldDefinitions();
                $available_fields = array();
    
                $labels = array();
                $duplicate_labels = array();
                foreach ($field_defs as $id=>$def) {
                    
                    //We are filtering out some fields here
                    if ($def['type'] == 'relate' || $def['type'] == 'link' || (isset($def['dbType']) && $def['dbType'] == 'id')) {
                        continue;
                    }
                       
                    
                    if (isset($def['vname'])) {
                        $available_fields[$id] = !empty($mod_strings[$def['vname']]) ? $mod_strings[$def['vname']] : $id;
                    } else {
                        $available_fields[$id] = $id;
                    }
                    
                    //Remove the ':' character in some labels
                    if (preg_match('/\:$/', (string) $available_fields[$id])) {
                        $available_fields[$id] = substr((string) $available_fields[$id], 0, strlen((string) $available_fields[$id])-1);
                    }
                    
                    if (isset($labels[$available_fields[$id]])) {
                        $duplicate_labels[$labels[$available_fields[$id]]] = $labels[$available_fields[$id]];
                        $duplicate_labels[$id] = $id;
                    } else {
                        $labels[$available_fields[$id]] = $id;
                    }
                }
    
                foreach ($duplicate_labels as $id) {
                    $available_fields[$id] = $available_fields[$id] . " ({$id})";
                }
                
                asort($available_fields);
                
                $field_keys = array();
                $field_values = array();
                
                $source_fields = array();
                foreach ($field_mapping as $id=>$field) {
                    if (!empty($source_defs[$id])) {
                        $source_fields[$id] = $source_defs[$id];
                    }
                }
                $source_fields = array_merge($source_fields, $source_defs);
                
                foreach ($source_fields as $id=>$def) {
                    if (empty($def['hidden'])) {
                        $field_keys[strtolower($id)] = !empty($connector_strings[$source_fields[$id]['vname']]) ? $connector_strings[$source_fields[$id]['vname']] : $id;
                        $field_values[] = !empty($field_mapping[strtolower($id)]) ? $field_mapping[strtolower($id)] : '';
                    }
                }

                $display_data[$module] = array('field_keys' => $field_keys,
                                               'field_values' => $field_values,
                                               'available_fields' => $available_fields,
                                               'field_mapping' => $field_mapping,
                                               'module_name' => isset($GLOBALS['app_list_strings']['moduleList'][$module]) ? $GLOBALS['app_list_strings']['moduleList'][$module] : $module
                                                );
            }
        }

        $this->ss->assign('display_data', $display_data);
        $this->ss->assign('empty_mapping', empty($display_data) ? true : false);
        $this->ss->assign('dynamic_script', $script);
        $this->ss->assign('sources', $sources);
        $this->ss->assign('mod', $GLOBALS['mod_strings']);
        $this->ss->assign('APP', $GLOBALS['app_strings']);
        $this->ss->assign('source_id', $source_id);
        $this->ss->assign('source_name', $sources[$source_id]['name']);
        $this->ss->assign('theme', $GLOBALS['theme']);
        
        echo $this->ss->fetch($this->getCustomFilePathIfExists('modules/Connectors/tpls/mapping_properties.tpl'));
    }
}
