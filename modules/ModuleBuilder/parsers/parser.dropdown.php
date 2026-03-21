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


require_once('modules/ModuleBuilder/parsers/ModuleBuilderParser.php');

 #[\AllowDynamicProperties]
 class ParserDropDown extends ModuleBuilderParser
 {

    /**
     * Takes in the request params from a save request and processes
     * them for the save.
     *
     * @param REQUEST params  $params
     */
     public function saveDropDown($params)
     {
         require_once('modules/Administration/Common.php');
         $emptyMarker = translate('LBL_BLANK');
         $selected_lang = (!empty($params['dropdown_lang'])?$params['dropdown_lang']:$_SESSION['authenticated_user_language']);
         $type = $_REQUEST['view_package'];
         $dir = '';
         $dropdown_name = $params['dropdown_name'];
         $json = getJSONobj();

         $list_value = str_replace('&quot;&quot;:&quot;&quot;', '&quot;__empty__&quot;:&quot;&quot;', (string) $params['list_value']);
         //Bug 21362 ENT_QUOTES- convert single quotes to escaped single quotes.
         $rawurldecode = rawurldecode($list_value);
         $htmldecode = html_entity_decode($rawurldecode, ENT_QUOTES);
         $temp = $json->decode($htmldecode);
         $dropdown = array() ;
         // dropdown is received as an array of (name,value) pairs - now extract to name=>value format preserving order
         // we rely here on PHP to preserve the order of the received name=>value pairs - associative arrays in PHP are ordered
         if (is_array($temp)) {
             foreach ($temp as $item) {
                 $keytemp = SugarCleaner::stripTags(from_html($item [ 0 ]), false);
                 $valuetemp = SugarCleaner::stripTags(from_html($item [ 1 ]), false);
                 $dropdown[ $keytemp ] =  $valuetemp;
             }
         }
         if (array_key_exists($emptyMarker, $dropdown)) {
             $output=array();
             foreach ($dropdown as $key => $value) {
                 if ($emptyMarker===$key) {
                     $output['']='';
                 } else {
                     $output[$key]=$value;
                 }
             }
             $dropdown=$output;
         }

         if ($type != 'studio') {
             $mb = new ModuleBuilder();
             $module = $mb->getPackageModule($params['view_package'], $params['view_module']);
             $this->synchMBDropDown($dropdown_name, $dropdown, $selected_lang, $module);
             //Can't use synch on selected lang as we want to overwrite values, not just keys
             $module->mblanguage->appListStrings[$selected_lang.'.lang.php'][$dropdown_name] = $dropdown;
             $module->mblanguage->save($module->key_name); // tyoung - key is required parameter as of
         } else {
             $contents = return_custom_app_list_strings_file_contents($selected_lang);
             $my_list_strings = return_app_list_strings_language($selected_lang);
             if ($selected_lang == $GLOBALS['current_language']) {
                 $GLOBALS['app_list_strings'][$dropdown_name] = $dropdown;
             }
             //write to contents
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
                         $pattern_match = '/\s*\$app_list_strings\s*\[\s*\''.$dropdown_name.'\'\s*\]\[\s*\''.$key.'\'\s*\]\s*=\s*[\'\"]{1}.*?[\'\"]{1};\s*/ism';
                         $contents = preg_replace($pattern_match, "\n", (string) $contents);
                         //add the new ones
                         $contents .= "\n\$app_list_strings['$dropdown_name']['$key']=" . var_export_helper($value) . ";";
                     }
                 }
             } else {
                 //Now synch up the keys in other langauges to ensure that removed/added Drop down values work properly under all langs.
                 $this->synchDropDown($dropdown_name, $dropdown, $selected_lang, $dir);
                 $contents = $this->getNewCustomContents($dropdown_name, $dropdown, $selected_lang);
             }
             if (!empty($dir) && !is_dir($dir)) {
                 $continue = mkdir_recursive($dir);
             }
             save_custom_app_list_strings_contents($contents, $selected_lang, $dir);
         }
         sugar_cache_reset();
         clearAllJsAndJsLangFilesWithoutOutput();
     }

     /**
     * function synchDropDown
     * 	Ensures that the set of dropdown keys is consistant accross all languages.
     *
     * @param $dropdown_name string The name of the dropdown to be synched
     * @param $dropdown array The dropdown currently being saved
     * @param $selected_lang String the language currently selected in Studio/MB
     * @param $saveLov String the path to the directory to save the new lang file in.
     */
     public function synchDropDown($dropdown_name, $dropdown, $selected_lang, $saveLoc)
     {
         $allLanguages =  get_languages();
         foreach ($allLanguages as $lang => $langName) {
             if ($lang != $selected_lang) {
                 $listStrings = return_app_list_strings_language($lang);
                 $langDropDown = array();
                 if (isset($listStrings[$dropdown_name]) && is_array($listStrings[$dropdown_name])) {
                     $langDropDown = $this->synchDDKeys($dropdown, $listStrings[$dropdown_name]);
                 } else {
                     //if the dropdown does not exist in the language, justt use what we have.
                     $langDropDown = $dropdown;
                 }
                 $contents = $this->getNewCustomContents($dropdown_name, $langDropDown, $lang);
                 save_custom_app_list_strings_contents($contents, $lang, $saveLoc);
             }
         }
     }

     /**
     * function synchMBDropDown
     * 	Ensures that the set of dropdown keys is consistant accross all languages in a ModuleBuilder Module
     *
     * @param $dropdown_name String The name of the dropdown to be synched
     * @param $dropdown array The dropdown currently being saved
     * @param $selected_lang String the language currently selected in Studio/MB
     * @param $module MBModule the module to update the languages in
     */
     public function synchMBDropDown($dropdown_name, $dropdown, $selected_lang, $module)
     {
         $selected_lang	= $selected_lang . '.lang.php';
         foreach ($module->mblanguage->appListStrings as $lang => $listStrings) {
             if ($lang != $selected_lang) {
                 $langDropDown = array();
                 if (isset($listStrings[$dropdown_name]) && is_array($listStrings[$dropdown_name])) {
                     $langDropDown = $this->synchDDKeys($dropdown, $listStrings[$dropdown_name]);
                 } else {
                     $langDropDown = $dropdown;
                 }
                 $module->mblanguage->appListStrings[$lang][$dropdown_name] = $langDropDown;
                 $module->mblanguage->save($module->key_name);
             }
         }
     }

     private function synchDDKeys($dom, $sub)
     {
         //check for extra keys
         foreach ($sub as $key=>$value) {
             if (!isset($dom[$key])) {
                 unset($sub[$key]);
             }
         }
         //check for missing keys
         foreach ($dom as $key=>$value) {
             if (!isset($sub[$key])) {
                 $sub[$key] = $value;
             }
         }
         return $sub;
     }

     public function getPatternMatchGlobal($dropdown_name)
     {
         return '/\s*\$GLOBALS\s*\[\s*\'app_list_strings\s*\'\s*\]\[\s*\''
             . $dropdown_name.'\'\s*\]\s*=\s*array\s*\([^\)]*\)\s*;\s*/ism';
     }

     public function getPatternMatch($dropdown_name)
     {
         return '/\s*\$app_list_strings\s*\[\s*\''.$dropdown_name.'\'\s*\]\s*=\s*array\s*\([^\)]*\)\s*;\s*/ism';
     }

     public function getNewCustomContents($dropdown_name, $dropdown, $lang)
     {
         $contents = return_custom_app_list_strings_file_contents($lang);
         $contents = str_replace("?>", '', (string) $contents);
         if (empty($contents)) {
             $contents = "<?php";
         }
         $contents = preg_replace($this->getPatternMatchGlobal($dropdown_name), "\n", $contents);
         $contents = preg_replace($this->getPatternMatch($dropdown_name), "\n", $contents);
         $contents .= "\n\$app_list_strings['$dropdown_name']=" . var_export_helper($dropdown) . ";";
         return $contents;
     }
 }
