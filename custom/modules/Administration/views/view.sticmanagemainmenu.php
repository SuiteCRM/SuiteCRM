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

class ViewSticManageMainMenu extends SugarView
{

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
            "<a href='index.php?module=Administration&action=index'>" . $mod_strings['LBL_MODULE_NAME'] . "</a>",
            $mod_strings['LBL_STIC_MENU_CONFIGURE_TITLE'],
        );
    }
    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;

        if (!is_admin($current_user)) {
            sugar_die("Unauthorized access to administration.");
        }
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        // Include necessary files and initialize global variables
        global $app_list_strings, $current_user, $sugar_config;
        require_once 'modules/MySettings/TabController.php';
        require_once 'SticInclude/SticAdvancedMenu.php';
        require_once 'custom/include/SticAdvancedTabConfig.php';

        // Get the current user's tabs
        $controller = new TabController();
        $tabs = $controller->get_tabs($current_user)[0];

        // Get the selected language
        $tabGroupSelected_lang = (!empty($_GET['lang']) ? $_GET['lang'] : $_SESSION['authenticated_user_language']);

        // Load the custom menu structure if available, otherwise create a default one
        if (isset($GLOBALS["SticTabStructure"])) {
            $menu = $GLOBALS["SticTabStructure"];
            addMenuProperties($menu);
        } else {
            // Create default menu structure
            $menu = $this::createDefaultMenuStructure($tabs);
        }

        // Encode menu structure as JSON for JavaScript use
        $jsonMenu = json_encode($menu, JSON_UNESCAPED_UNICODE);
        $this->ss->assign('jsonMenu', $jsonMenu);

        // Create a list of all available modules not in the main menu
        $allModules = [];
        foreach ($tabs as $key => $value) {
            if (!$this::findIdInArray($menu, $key)) {
                $allModules[] = ['id' => $key, 'text' => $app_list_strings['moduleList'][$key]];
            }
        }
        usort($allModules, function ($a, $b) {return strcmp($a['text'], $b['text']);});

        // Encode all modules as JSON for JavaScript use
        $jsonAll = json_encode($allModules, JSON_UNESCAPED_UNICODE);
        $this->ss->assign('jsonAll', $jsonAll);

        // Assign variables to the Smarty template
        $this->ss->assign('MOD', $GLOBALS['mod_strings']);
        $this->ss->assign('sticAdvancedMenuIcons', $sugar_config['stic_advanced_menu_icons']);
        $this->ss->assign('sticAdvancedMenuAll', $sugar_config['stic_advanced_menu_all']);

        $this->ss->assign('tabGroupSelected_lang', $tabGroupSelected_lang);
        $this->ss->assign('available_languages', get_languages());

        $newNodeString = return_module_language($tabGroupSelected_lang, 'Administration')['LBL_STIC_MENU_COMMAND_CREATE_DEFAULT'];
        $this->ss->assign('newNodeString', $newNodeString);

        // Display the Smarty template
        $this->ss->display("custom/modules/Administration/SticAdvancedMenu/SticAdvancedMenuEdit.tpl");

    }

    /**
     * Recursively search for an ID in a nested array structure
     */
    public static function findIdInArray($array, $idToFind)
    {
        foreach ($array as $element) {
            if (is_array($element)) {
                if (array_key_exists('id', $element) && $element['id'] === $idToFind) {
                    return true;
                }
                if (isset($element['children']) && self::findIdInArray($element['children'], $idToFind)) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Create a default menu structure based on the user's tabs
     */
    public static function createDefaultMenuStructure($tabs)
    {
        global $app_strings, $app_list_strings;
        $menu = [];
        foreach ($tabs as $mainTab => $subModules) {
            $children = [];
            foreach ($subModules as $key) {
                $children[] = ['id' => $key, 'text' => $app_list_strings['moduleList'][$key]];
            }
            $menu[] = [
                'id' => $mainTab,
                'text' => $app_strings[$mainTab],
                'children' => $children,
            ];
        }
        return $menu;
    }
}
