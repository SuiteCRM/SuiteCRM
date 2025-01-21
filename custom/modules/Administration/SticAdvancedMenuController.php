<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

// Handle different management modes for advanced menu configuration
if ($_REQUEST['manageMode'] ?? false) {
    switch ($_REQUEST['manageMode']) {
        case 'save':
            // Decode HTML entities in the JSON string to ensure proper parsing
            $decodedJson = html_entity_decode($_POST['menuJson']);

            $manageLang = $_POST['manageLang'];

            // Convert the decoded JSON to a PHP associative array
            $GLOBALS["SticTabStructure"] = json_decode($decodedJson, true);

            // Flatten the menu structure to extract all labels
            $flatArray = [];
            $stack = $GLOBALS["SticTabStructure"]; // Initialize stack with the original array

            while (!empty($stack)) {
                $item = array_pop($stack); // Extract the last element from the stack

                // If the item has an 'id' and 'text', add it to the flat array
                if (isset($item['id']) && isset($item['text'])) {
                    $flatArray[$item['id']] = $item['text'];
                }

                // If the item has 'children', add them to the stack for processing
                if (isset($item['children']) && is_array($item['children'])) {
                    $stack = array_merge($stack, $item['children']); // Add children to the stack
                }
            }

            // Write the tab structure to a custom configuration file
            $fileContents = "<?php \n" . '$GLOBALS["SticTabStructure"] =' . var_export($GLOBALS['SticTabStructure'], true) . ';';

            sugar_file_put_contents('custom/include/AdvancedTabConfig.php', $fileContents);
            ob_clean();
            SugarApplication::appendSuccessMessage("<div id='saved-notice' class='alert alert-success' role='alert'>{$app_strings['LBL_SAVED']}</div>");

            // Save menu configuration options
            require_once 'modules/Configurator/Configurator.php';
            $configurator = new Configurator();

            // Update configuration with new menu settings
            $configurator->config['stic_advanced_menu_icons'] = $_POST['sticAdvancedMenuIcons'];
            $configurator->config['stic_advanced_menu_all'] = $_POST['sticAdvancedMenuAll'];
            $configurator->saveConfig();

            // Manage language labels
            require_once 'modules/Administration/Common.php';
            
            // Update language strings for menu labels
            foreach ($flatArray as $labelID => $labelValue) {
                
                $menuStrings = array_merge(return_application_language($manageLang),  return_app_list_strings_language($manageLang)['moduleList']);
                
                // Only update if the label doesn't exist or has changed
                if (empty($menuStrings[$labelID]) || $menuStrings[$labelID] != $labelValue) {
                    $contents = return_custom_app_list_strings_file_contents($manageLang);
                    $newContents = replace_or_add_app_string($labelID, $labelValue, $contents);
                    save_custom_app_list_strings_contents($newContents, $manageLang);

                    // Propagate label to other languages if not already set
                    $languages = get_languages();
                    foreach ($languages as $language => $langlabel) {
                        if ($manageLang == $language) {
                            continue;
                        }
                        $menuStrings = array_merge(return_application_language($language),  return_app_list_strings_language($language)['moduleList']);
                        if (!isset($menuStrings[$labelID])) {
                            $contents = return_custom_app_list_strings_file_contents($language);
                            $newContents = replace_or_add_app_string($labelID, $labelValue, $contents);
                            save_custom_app_list_strings_contents($newContents, $language);
                        }
                    }

                    $menuStrings[$labelID] = $labelValue;
                }
            }
            die('ok');
            break;

        case 'legacy_mode':
            // Disable advanced menu and revert to legacy menu
            require_once 'modules/Configurator/Configurator.php';
            $configurator = new Configurator();
            $configurator->config['stic_advanced_menu_enabled'] = false;
            $configurator->saveConfig();

            die('ok');
            break;

        case 'advanced_mode':
            // Enable advanced menu and convert existing menu
            require_once 'modules/Configurator/Configurator.php';
            $configurator = new Configurator();
            $configurator->config['stic_advanced_menu_enabled'] = true;
            $configurator->saveConfig();
            SticAdvancedMenu::ConvertSuiteCRMMenuToAdvancedMenu();

            die('ok');
            break;

        case 'restore':
            // Remove custom tab configuration and reset global variable
            // This allows reverting to the default menu configuration
            unlink('custom/include/AdvancedTabConfig.php');
            unset($GLOBALS["SticTabStructure"]);
            die('ok');
            break;

        default:
            die('no action!');
            break;
    }
}

/**
 * Class SticAdvancedMenu
 *
 * This class provides functionality to convert and manage advanced menu structures.
 */
class SticAdvancedMenu
{
    /**
     * Converts the existing SuiteCRM menu structure to a new advanced format.
     *
     * This function reads the current tab configuration, transforms it using convertFromSuiteCRMMenu(),
     * and saves the new structure to custom/include/AdvancedTabConfig.php.
     * The conversion reorganizes the menu groups into a numerically indexed array
     * with 'id' and 'children' properties for each group.
     *
     * @return bool Returns true after completion, regardless of the outcome.
     */
    public static function ConvertSuiteCRMMenuToAdvancedMenu()
    {
        if (file_exists('custom/include/tabConfig.php')) {
            require_once 'custom/include/tabConfig.php';

            $newTabStructure = self::convertFromSuiteCRMMenu($GLOBALS['tabStructure']);

            $fileContent = "<?php\n\n";
            $fileContent .= "\$GLOBALS[\"SticTabStructure\"] = " . var_export($newTabStructure, true) . ";\n";

            $filePath = 'custom/include/AdvancedTabConfig.php';

            if (file_put_contents($filePath, $fileContent)) {
                echo "File successfully saved to $filePath";
            } else {
                echo "An error occurred while saving the file";
            }
        }
        $GLOBALS['log']->info(__METHOD__ . '(' . __LINE__ . ') Successfully switched to advanced menu');

        return true;
    }

    /**
     * Converts the SuiteCRM menu structure to the new advanced tab structure.
     *
     * This function transforms the original SuiteCRM tab structure into a new format
     * where each group is represented as an array with 'id' and 'children' properties.
     * The 'children' property contains an array of modules, each represented by its ID.
     *
     * @param array $tabStructure The original SuiteCRM tab structure
     * @return array The new advanced tab structure
     */
    public static function convertFromSuiteCRMMenu($tabStructure)
    {
        $newStructure = array();
        $index = 0;

        foreach ($tabStructure as $groupId => $group) {
            $newGroup = array(
                'id' => $groupId,
                'children' => array(),
            );

            foreach ($group['modules'] as $moduleId) {
                $newGroup['children'][] = array('id' => $moduleId);
            }

            $newStructure[$index] = $newGroup;
            $index++;
        }

        return $newStructure;
    }
}
