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



#[\AllowDynamicProperties]
class ViewSourceProperties extends ViewList
{
    public function __construct()
    {
        parent::__construct();
    }




    public function display()
    {
        global $sugar_config;

        require_once('include/connectors/sources/SourceFactory.php');
        require_once('include/connectors/utils/ConnectorUtils.php');

        $source_id = $_REQUEST['source_id'];
        $connector_language = ConnectorUtils::getConnectorStrings($source_id);
        $source = SourceFactory::getSource($source_id);
        $properties = $source->getProperties();

        $required_fields = array();
        $config_fields = $source->getRequiredConfigFields();
        $fields = $source->getRequiredConfigFields();
        foreach ($fields as $field_id) {
            $label = isset($connector_language[$field_id]) ? $connector_language[$field_id] : $field_id;
            $required_fields[$field_id]=$label;
        }

        // treat string as a template (the string resource plugin is unavailable in the current Smarty version)
        if (isset($connector_language['LBL_LICENSING_INFO'])) {
            $siteUrl = rtrim($sugar_config['site_url'], '/');
            $connector_language['LBL_LICENSING_INFO'] = str_replace(
                '{$SITE_URL}',
                $siteUrl,
                (string) $connector_language['LBL_LICENSING_INFO']
            );
        }

        $this->ss->assign('required_properties', $required_fields);
        $this->ss->assign('source_id', $source_id);
        $this->ss->assign('properties', $properties);
        $this->ss->assign('mod', $GLOBALS['mod_strings']);
        $this->ss->assign('app', $GLOBALS['app_strings']);
        $this->ss->assign('connector_language', $connector_language);
        $this->ss->assign('hasTestingEnabled', $source->hasTestingEnabled());

        echo $this->ss->fetch($this->getCustomFilePathIfExists('modules/Connectors/tpls/source_properties.tpl'));
    }
}
