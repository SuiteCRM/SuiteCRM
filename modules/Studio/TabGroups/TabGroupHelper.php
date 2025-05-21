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




require_once('modules/Administration/Common.php');
#[\AllowDynamicProperties]
class TabGroupHelper
{
    public $modules = array();
    public function getAvailableModules($lang = '')
    {
        static $availableModules = array();
        if (!empty($availableModules)) {
            return $availableModules;
        }
        $specifyLanguageAppListStrings = $GLOBALS['app_list_strings'];
        if (!empty($lang)) {
            $specifyLanguageAppListStrings = return_app_list_strings_language($lang);
        }
        foreach ($GLOBALS['moduleList'] as $value) {
            $availableModules[$value] = array('label'=>$specifyLanguageAppListStrings['moduleList'][$value], 'value'=>$value);
        }

        if (should_hide_iframes() && isset($availableModules['iFrames'])) {
            unset($availableModules['iFrames']);
        }
        return $availableModules;
    }

    /**
     * Takes in the request params from a save request and processes
     * them for the save.
     *
     * @param REQUEST params  $params
     */
    public function saveTabGroups($params)
    {
        //#30205
        global $sugar_config;

        //Get the selected tab group language
        $grouptab_lang = (!empty($params['grouptab_lang'])?$params['grouptab_lang']:$_SESSION['authenticated_user_language']);

        $tabGroups = array();
        $selected_lang = (!empty($params['dropdown_lang'])?$params['dropdown_lang']:$_SESSION['authenticated_user_language']);
        $slot_count = $params['slot_count'];
        $completedIndexes = array();
        for ($count = 0; $count < $slot_count; $count++) {
            if ($params['delete_' . $count] == 1 || !isset($params['slot_' . $count])) {
                continue;
            }


            $index = $params['slot_' . $count];
            if (isset($completedIndexes[$index])) {
                continue;
            }

            $labelID = (!empty($params['tablabelid_' . $index]))?$params['tablabelid_' . $index]: 'LBL_GROUPTAB' . $count . '_'. time();
            $labelValue = SugarCleaner::stripTags(from_html($params['tablabel_' . $index]), false);
            $app_strings = return_application_language($grouptab_lang);
            if (empty($app_strings[$labelID]) || $app_strings[$labelID] != $labelValue) {
                $contents = return_custom_app_list_strings_file_contents($grouptab_lang);
                $new_contents = replace_or_add_app_string($labelID, $labelValue, $contents);
                save_custom_app_list_strings_contents($new_contents, $grouptab_lang);

                $languages = get_languages();
                foreach ($languages as $language => $langlabel) {
                    if ($grouptab_lang == $language) {
                        continue;
                    }
                    $app_strings = return_application_language($language);
                    if (!isset($app_strings[$labelID])) {
                        $contents = return_custom_app_list_strings_file_contents($language);
                        $new_contents = replace_or_add_app_string($labelID, $labelValue, $contents);
                        save_custom_app_list_strings_contents($new_contents, $language);
                    }
                }

                $app_strings[$labelID] = $labelValue;
            }
            $tabGroups[$labelID] = array('label'=>$labelID);
            $tabGroups[$labelID]['modules']= array();
            for ($subcount = 0; isset($params[$index.'_' . $subcount]); $subcount++) {
                $tabGroups[$labelID]['modules'][] = $params[$index.'_' . $subcount];
            }

            $completedIndexes[$index] = true;
        }

        // Force a rebuild of the app language
        global $current_user;
        include(get_custom_file_if_exists('modules/Administration/RebuildJSLang.php'));
        sugar_cache_clear('app_strings.'.$grouptab_lang);
        $newFile = create_custom_directory('include/tabConfig.php');
        write_array_to_file("GLOBALS['tabStructure']", $tabGroups, $newFile);
        $GLOBALS['tabStructure'] = $tabGroups;
    }
}
