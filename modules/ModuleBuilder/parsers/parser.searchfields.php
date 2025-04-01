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

require_once('modules/ModuleBuilder/parsers/ModuleBuilderParser.php') ;
require_once('modules/ModuleBuilder/MB/MBPackage.php');

#[\AllowDynamicProperties]
class ParserSearchFields extends ModuleBuilderParser
{
    public $searchFields;
    public $packageKey;

    public function __construct($moduleName, $packageName='')
    {
        $this->moduleName = $moduleName;
        if (!empty($packageName)) {
            $this->packageName = $packageName;
            $mbPackage = new MBPackage($this->packageName);
            $this->packageKey = $mbPackage->key;
        }

        $this->searchFields = $this->getSearchFields();
    }

    public function addSearchField($name, $searchField)
    {
        if (empty($name) || empty($searchField) || !is_array($searchField)) {
            return;
        }

        $key = isset($this->packageKey) ? $this->packageKey . '_' . $this->moduleName : $this->moduleName;
        $this->searchFields[$key][$name] = $searchField;
    }

    public function removeSearchField($name)
    {
        $key = isset($this->packageKey) ? $this->packageKey . '_' . $this->moduleName : $this->moduleName;

        if (isset($this->searchFields[$key][$name])) {
            unset($this->searchFields[$key][$name]);
        }
    }

    public function getSearchFields()
    {
        $searchFields = array();
        if (!empty($this->packageName) && file_exists("custom/modulebuilder/packages/{$this->packageName}/modules/{$this->moduleName}/metadata/SearchFields.php")) { //we are in Module builder
            include("custom/modulebuilder/packages/{$this->packageName}/modules/{$this->moduleName}/metadata/SearchFields.php");
        } else {
            if (file_exists("custom/modules/{$this->moduleName}/metadata/SearchFields.php")) {
                include("custom/modules/{$this->moduleName}/metadata/SearchFields.php");
            } else {
                if (file_exists("modules/{$this->moduleName}/metadata/SearchFields.php")) {
                    include("modules/{$this->moduleName}/metadata/SearchFields.php");
                }
            }
        }

        return $searchFields;
    }

    public function saveSearchFields($searchFields)
    {
        if (!empty($this->packageName)) { //we are in Module builder
            $header = file_get_contents('modules/ModuleBuilder/MB/header.php');
            if (!file_exists("custom/modulebuilder/packages/{$this->packageName}/modules/{$this->moduleName}/metadata/SearchFields.php")) {
                mkdir_recursive("custom/modulebuilder/packages/{$this->packageName}/modules/{$this->moduleName}/metadata");
            }
            write_array_to_file("searchFields['{$this->packageKey}_{$this->moduleName}']", $searchFields["{$this->packageKey}_{$this->moduleName}"], "custom/modulebuilder/packages/{$this->packageName}/modules/{$this->moduleName}/metadata/SearchFields.php", 'w', $header);
        } else {
            $header = file_get_contents('modules/ModuleBuilder/MB/header.php');
            if (!file_exists("custom/modules/{$this->moduleName}/metadata/SearchFields.php")) {
                mkdir_recursive("custom/modules/{$this->moduleName}/metadata");
            }
            write_array_to_file("searchFields['{$this->moduleName}']", $searchFields[$this->moduleName], "custom/modules/{$this->moduleName}/metadata/SearchFields.php", 'w', $header);
        }
        $this->searchFields = $searchFields;
    }
}
