<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

namespace SuiteCRM\Search;

use LoggerManager;
use RuntimeException;
use UnifiedSearchAdvanced;
use BeanFactory;
use VardefManager;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once __DIR__ . '/../../modules/Home/UnifiedSearchAdvanced.php';

/**
 * Class SearchModules fetches the search modules.
 */
#[\AllowDynamicProperties]
class SearchModules
{
    /**
     * Returns the list of modules from the search defs.
     *
     * Each entry is defined as key (module name) => value (module translation).
     *
     * @return string[]
     */
    public static function getModulesList(): array
    {
        $allModules = self::getAllModules();
        $allModules = array_merge($allModules['enabled'], $allModules['disabled']);

        $modules = [];

        foreach ($allModules as $module) {
            $modules[$module['module']] = $module['label'];
        }

        return $modules;
    }

    /**
     * Returns an array with all the enabled modules names.
     *
     * @return string[]
     */
    public static function getEnabledModules(): array
    {
        $allModules = self::getAllModules();
        $enabledModules = $allModules['enabled'];

        $modules = [];

        foreach ($enabledModules as $module) {
            $modules[] = $module['module'];
        }

        return $modules;
    }

    /**
     * saveGlobalSearchSettings
     * This method handles the administrator's request to save the searchable modules selected and stores
     * the results in the unified_search_modules_display.php file
     *
     */
    public static function saveGlobalSearchSettings(): void
    {
        if (isset($_REQUEST['enabled_modules'])) {
            $unified_search_modules_display = self::getUnifiedSearchModulesDisplay();

            $new_unified_search_modules_display = [];

            foreach (explode(',', $_REQUEST['enabled_modules']) as $module) {
                $new_unified_search_modules_display[$module]['visible'] = true;
            }

            foreach ($unified_search_modules_display as $module => $data) {
                if (!isset($new_unified_search_modules_display[$module])) {
                    $new_unified_search_modules_display[$module]['visible'] = false;
                }
            }

            self::writeUnifiedSearchModulesDisplayFile($new_unified_search_modules_display);
        }
    }

    /**
     * getUnifiedSearchModulesDisplay
     *
     * Returns the value of the $unified_search_modules_display variable which is based on the $unified_search_modules
     * entries that have been selected to be allowed for searching.
     *
     * @return array $unified_search_modules_display Array value of modules that have enabled for searching
     */
    public static function getUnifiedSearchModulesDisplay(): array
    {
        $unified_search_modules_display = [];

        if (!file_exists(__DIR__ . '/../../custom/modules/unified_search_modules_display.php')) {
            $unified_search_modules = self::getUnifiedSearchModules();

            if (!empty($unified_search_modules)) {
                foreach ($unified_search_modules as $module => $data) {
                    $unified_search_modules_display[$module]['visible'] = (!empty($data['default']));
                }
            }

            self::writeUnifiedSearchModulesDisplayFile($unified_search_modules_display);
        }

        include(__DIR__ . '/../../custom/modules/unified_search_modules_display.php');

        return $unified_search_modules_display;
    }

    /**
     * getUnifiedSearchModules
     *
     * Returns the value of the $unified_search_modules variable based on the module's vardefs.php file
     * and which fields are marked with the unified_search attribute.
     *
     * @return array metadata module definitions along with their fields
     */
    public static function getUnifiedSearchModules(): array
    {
        // Make directory if it doesn't exist
        $cachedir = sugar_cached('modules');
        if (!file_exists($cachedir)) {
            mkdir_recursive($cachedir);
        }

        $unified_search_modules = [];

        // Load unified_search_modules.php file
        $cachedFile = sugar_cached('modules/unified_search_modules.php');
        if (!is_file($cachedFile)) {
            self::buildCache();
        }

        /** @noinspection PhpIncludeInspection */
        include $cachedFile;

        return $unified_search_modules;
    }

    /**
     * writeUnifiedSearchModulesDisplayFile
     * Private method to handle writing the unified_search_modules_display value to file
     *
     * @param mixed $unified_search_modules_display The array of the unified search modules and their display attributes
     * @return bool value indication whether or not file was successfully written
     */
    private static function writeUnifiedSearchModulesDisplayFile($unified_search_modules_display): bool
    {
        if (is_null($unified_search_modules_display) || empty($unified_search_modules_display)) {
            return false;
        }

        if (!write_array_to_file(
            'unified_search_modules_display',
            $unified_search_modules_display,
            'custom/modules/unified_search_modules_display.php'
        )) {
            global $app_strings;
            $msg = string_format($app_strings['ERR_FILE_WRITE'], ['custom/modules/unified_search_modules_display.php']);
            LoggerManager::getLogger()->error($msg);
            throw new RuntimeException($msg);
        }

        return true;
    }

    /** @noinspection PhpIncludeInspection */
    public static function buildCache(): void
    {
        global $beanList, $beanFiles, $dictionary;

        $supportedModules = [];
        $metafiles = [];
        $searchFields = [];

        foreach ($beanList as $moduleName => $beanName) {
            if (!isset($beanFiles[$beanName])) {
                continue;
            }

            $beanName = BeanFactory::getObjectName($moduleName);
            VardefManager::loadVardef($moduleName, $beanName);

            // Obtain the field definitions used by generateSearchWhere (duplicate code in view.list.php)
            if (file_exists('custom/modules/' . $moduleName . '/metadata/metafiles.php')) {
                require('custom/modules/' . $moduleName . '/metadata/metafiles.php');
            } elseif (file_exists('modules/' . $moduleName . '/metadata/metafiles.php')) {
                require('modules/' . $moduleName . '/metadata/metafiles.php');
            }


            if (!empty($metafiles[$moduleName]['searchfields'])) {
                /** @noinspection PhpIncludeInspection */
                require $metafiles[$moduleName]['searchfields'];
            } elseif (is_file("modules/{$moduleName}/metadata/SearchFields.php")) {
                require "modules/{$moduleName}/metadata/SearchFields.php";
            }

            // Load custom SearchFields.php if it exists
            if (is_file("custom/modules/{$moduleName}/metadata/SearchFields.php")) {
                require "custom/modules/{$moduleName}/metadata/SearchFields.php";
            }

            // If there are $searchFields are empty, just continue, there are no search fields defined for the module
            if (empty($searchFields[$moduleName])) {
                continue;
            }

            $isCustomModule = preg_match('/^([a-z0-9]{1,5})_([a-z0-9_]+)$/i', (string) $moduleName);

            // If the bean supports unified search or if it's a custom module bean and unified search is not defined
            if (!empty($dictionary[$beanName]['unified_search']) || $isCustomModule) {
                $fields = [];
                foreach ($dictionary [$beanName]['fields'] as $field => $def) {
                    // We cannot enable or disable unified_search for email in the vardefs as we don't actually have a vardef entry for 'email'
                    // the searchFields entry for 'email' doesn't correspond to any vardef entry. Instead it contains SQL to directly perform the search.
                    // So as a proxy we allow any field in the vardefs that has a name starting with 'email...' to be tagged with the 'unified_search' parameter

                    if (str_contains($field, 'email')) {
                        $field = 'email';
                    }

                    if (str_contains($field, 'phone')) {
                        $field = 'phone';
                    }

                    if (!empty($def['unified_search']) && isset($searchFields [$moduleName] [$field])) {
                        $fields [$field] = $searchFields [$moduleName] [$field];
                    }
                }

                foreach ($searchFields[$moduleName] as $field => $def) {
                    if (!empty($def['force_unifiedsearch'])) {
                        $fields[$field] = $def;
                    }
                }

                if (!empty($fields)) {
                    $supportedModules[$moduleName]['fields'] = $fields;
                    if (isset($dictionary[$beanName]['unified_search_default_enabled']) && $dictionary[$beanName]['unified_search_default_enabled'] === true) {
                        $supportedModules[$moduleName]['default'] = true;
                    } else {
                        $supportedModules[$moduleName]['default'] = false;
                    }
                }
            }
        }
        $cacheSearch = sugar_cached('modules/unified_search_modules.php');

        ksort($supportedModules);
        write_array_to_file('unified_search_modules', $supportedModules, $cacheSearch);
    }

    /**
     * Retrieve the enabled and disabled modules used for global search.
     * @return array
     */
    public static function getAllModules(): array
    {
        global $app_list_strings;

        $unified_search_modules_display = self::getUnifiedSearchModulesDisplay();

        $json_enabled = [];
        $json_disabled = [];
        foreach ($unified_search_modules_display as $module=>$data) {
            $label = $app_list_strings['moduleList'][$module] ?? $module;
            if ($data['visible'] === true) {
                $json_enabled[] = ["module" => $module, 'label' => $label];
            } else {
                $json_disabled[] = ["module" => $module, 'label' => $label];
            }
        }

        $cacheSearch = sugar_cached('modules/unified_search_modules.php');

        if (!file_exists($cacheSearch)) {
            self::buildCache();
        }

        include($cacheSearch);

        //Now add any new modules that may have since been added to unified_search_modules.php
        foreach ($unified_search_modules as $module=>$data) {
            if (!isset($unified_search_modules_display[$module])) {
                $label = $app_list_strings['moduleList'][$module] ?? $module;
                if ($data['default']) {
                    $json_enabled[] = ["module" => $module, 'label' => $label];
                } else {
                    $json_disabled[] = ["module" => $module, 'label' => $label];
                }
            }
        }

        return ['enabled' => $json_enabled, 'disabled' => $json_disabled];
    }
}
