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
class ViewSearchProperties extends ViewList
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
        $source_id = $_REQUEST['source_id'];
        $connector_strings = ConnectorUtils::getConnectorStrings($source_id);
        $is_enabled = ConnectorUtils::isSourceEnabled($source_id);
        $modules_sources = array();
        $sources = ConnectorUtils::getConnectors();
        $display_data = array();
        
        if ($is_enabled) {
            $searchDefs = ConnectorUtils::getSearchDefs();
            $searchDefs = !empty($searchDefs[$_REQUEST['source_id']]) ? $searchDefs[$_REQUEST['source_id']] : array();
                    
            $source = SourceFactory::getSource($_REQUEST['source_id']);
            $field_defs = $source->getFieldDefs();
           

            //Create the Javascript code to dynamically add the tables
            $json = getJSONobj();
            foreach ($searchDefs as $module=>$fields) {
                $disabled = array();
                $enabled = array();
            
                $enabled_fields = array_flip($fields);
                $field_keys = array_keys($field_defs);
    
                foreach ($field_keys as $index=>$key) {
                    if (!empty($field_defs[$key]['hidden']) || empty($field_defs[$key]['search'])) {
                        continue;
                    }

                    if (!isset($enabled_fields[$key])) {
                        $disabled[$key] = !empty($connector_strings[$field_defs[$key]['vname']]) ? $connector_strings[$field_defs[$key]['vname']] : $key;
                    } else {
                        $enabled[$key] = !empty($connector_strings[$field_defs[$key]['vname']]) ? $connector_strings[$field_defs[$key]['vname']] : $key;
                    }
                }
    
                $modules_sources[$module] = array_merge($enabled, $disabled);

                asort($disabled);
                $display_data[$module] = array('enabled' => $enabled, 'disabled' => $disabled,
                                               'module_name' => isset($GLOBALS['app_list_strings']['moduleList'][$module]) ? $GLOBALS['app_list_strings']['moduleList'][$module] : $module);
            }
        }
        
        $this->ss->assign('no_searchdefs_defined', !$is_enabled);
        $this->ss->assign('display_data', $display_data);
        $this->ss->assign('modules_sources', $modules_sources);
        $this->ss->assign('sources', $sources);
        $this->ss->assign('mod', $GLOBALS['mod_strings']);
        $this->ss->assign('APP', $GLOBALS['app_strings']);
        $this->ss->assign('source_id', $_REQUEST['source_id']);
        $this->ss->assign('theme', $GLOBALS['theme']);
        $this->ss->assign('connector_language', $connector_strings);
        echo $this->ss->fetch($this->getCustomFilePathIfExists('modules/Connectors/tpls/search_properties.tpl'));
    }
}
