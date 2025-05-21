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
require_once('modules/Administration/QuickRepairAndRebuild.php');
#[\AllowDynamicProperties]
class DropDownHelper
{
    public $modules = array();
    public function getDropDownModules()
    {
        $dir = dir('modules');
        while ($entry = $dir->read()) {
            if (file_exists('modules/'. $entry . '/EditView.php')) {
                $this->scanForDropDowns('modules/'. $entry . '/EditView.php', $entry);
            }
        }
    }

    public function scanForDropDowns($filepath, $module)
    {
        $contents = file_get_contents($filepath);
        $matches = array();
        preg_match_all('/app_list_strings\s*\[\s*[\'\"]([^\]]*)[\'\"]\s*]/', $contents, $matches);
        if (!empty($matches[1])) {
            foreach ($matches[1] as $match) {
                $this->modules[$module][$match] = $match;
            }
        }
    }

    /**
     * Allow for certain dropdowns to be filtered when edited by pre 5.0 studio (eg. Rename Tabs)
     *
     * @param string name
     * @param array dropdown
     * @return array Filtered dropdown list
     */
    public function filterDropDown($name, $dropdown)
    {
        $results = array();
        switch ($name) {
            //When renaming tabs ensure that the modList dropdown is filtered properly.
            case 'moduleList':
                $hiddenModList = array_flip($GLOBALS['modInvisList']);
                $moduleList = array_flip($GLOBALS['moduleList']);

                foreach ($dropdown as $k => $v) {
                    if (isset($moduleList[$k])) { // && !$hiddenModList[$k])
                        $results[$k] = $v;
                    }
                }
                break;
            default: //By default perform no filtering
                $results = $dropdown;

        }

        return $results;
    }


    /**
     * Takes in the request params from a save request and processes
     * them for the save.
     *
     * @param REQUEST params  $params
     */
    public function saveDropDown($params)
    {
        $count = 0;
        $dropdown = array();
        $dropdown_name = $params['dropdown_name'];
        $selected_lang = (!empty($params['dropdown_lang'])?$params['dropdown_lang']:$_SESSION['authenticated_user_language']);
        $my_list_strings = return_app_list_strings_language($selected_lang);
        while (isset($params['slot_' . $count])) {
            $index = $params['slot_' . $count];
            $key = (isset($params['key_' . $index]))?SugarCleaner::stripTags($params['key_' . $index]): 'BLANK';
            $value = (isset($params['value_' . $index]))?SugarCleaner::stripTags($params['value_' . $index]): '';
            if ($key == 'BLANK') {
                $key = '';
            }
            $key = trim($key);
            $value = trim($value);
            if (empty($params['delete_' . $index])) {
                $dropdown[$key] = $value;
            }
            $count++;
        }

        if ($selected_lang == $GLOBALS['current_language']) {
            $GLOBALS['app_list_strings'][$dropdown_name] = $dropdown;
        }
        $contents = return_custom_app_list_strings_file_contents($selected_lang);



        //get rid of closing tags they are not needed and are just trouble
        $contents = str_replace("?>", '', (string) $contents);
        if (empty($contents)) {
            $contents = "<?php";
        }
        //add new drop down to the bottom
        if (!empty($params['use_push'])) {
            //this is for handling moduleList and such where nothing should be deleted or anything but they can be renamed
            foreach ($dropdown as $key=>$value) {
                //only if the value has changed or does not exist do we want to add it this way
                if (!isset($my_list_strings[$dropdown_name][$key]) || strcmp($my_list_strings[$dropdown_name][$key], $value) != 0) {
                    //clear out the old value
                    $contents = preg_replace(self::getPatternMatchGlobal($dropdown_name), "\n", (string) $contents);
                    $contents = preg_replace(self::getPatternMatch($dropdown_name), "\n", $contents);
                    //add the new ones
                    $contents .= "\n\$app_list_strings['$dropdown_name']['$key']=" . var_export_helper($value) . ";";
                }
            }
        } else {
            //clear out the old value
            $contents = preg_replace(self::getPatternMatchGlobal($dropdown_name), "\n", $contents);
            $contents = preg_replace(self::getPatternMatch($dropdown_name), "\n", $contents);
            //add the new ones
            $contents .= "\n\$app_list_strings['$dropdown_name']=" . var_export_helper($dropdown) . ";";
        }

        // Bug 40234 - If we have no contents, we don't write the file. Checking for "<?php" because above it's set to that if empty
        if ($contents != "<?php") {
            save_custom_app_list_strings_contents($contents, $selected_lang);
            sugar_cache_reset();
        }
        // Bug38011
        $repairAndClear = new RepairAndClear();
        $repairAndClear->module_list = array(translate('LBL_ALL_MODULES'));
        $repairAndClear->show_output = false;
        $repairAndClear->clearJsLangFiles();
        // ~~~~~~~~
    }

    public static function getPatternMatchGlobal($dropdown_name)
    {
        return '/\s*\$GLOBALS\s*\[\s*\'app_list_strings\s*\'\s*\]\[\s*\''
            . $dropdown_name.'\'\s*\]\s*=\s*array\s*\([^\)]*\)\s*;\s*/ism';
    }

    public static function getPatternMatch($dropdown_name)
    {
        return '/\s*\$app_list_strings\s*\[\s*\''.$dropdown_name.'\'\s*\]\s*=\s*array\s*\([^\)]*\)\s*;\s*/ism';
    }
}
