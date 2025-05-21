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





require_once('include/utils/array_utils.php');

/**
 * @return bool
 * @desc Creates the include language directory under the custom directory.
 */
function create_include_lang_dir()
{
    if (!is_dir("custom/include/language")) {
        return sugar_mkdir("custom/include/language", null, true);
    }

    return true;
}

/**
 * @return bool
 * @param module string
 * @desc Creates the module's language directory under the custom directory.
 */
function create_module_lang_dir($module)
{
    if (!is_dir("custom/modules/$module/language")) {
        return sugar_mkdir("custom/modules/$module/language", null, true);
    }

    return true;
}

/**
 * @return string&
 * @param the_array array, language string, module string
 * @desc Returns the contents of the customized language pack.
 */
function &create_field_lang_pak_contents($old_contents, $key, $value, $language, $module)
{
    if (!empty($old_contents)) {
        $old_contents = preg_replace("'[^\[\n\r]+\[\'{$key}\'\][^\;]+;[\ \r\n]*'i", '', (string) $old_contents);
        $contents = str_replace("\n?>", "\n\$mod_strings['{$key}'] = '$value';\n?>", $old_contents);
    } else {
        $contents = "<?php\n"
            . '// Creation date: ' . date('Y-m-d H:i:s') . "\n"
            . "// Module: $module\n"
            . "// Language: $language\n\n"
            . "\$mod_strings['$key'] = '$value';"
            . "\n?>";
    }

    return $contents;
}

/**
 * @return string&
 * @param the_array array, language string
 * @desc Returns the contents of the customized language pack.
 */
function &create_dropdown_lang_pak_contents(&$the_array, $language)
{
    $contents = "<?php\n" .
               '// ' . date('Y-m-d H:i:s') . "\n" .
               "// Language: $language\n\n" .
               '$app_list_strings = ' .
               var_export($the_array, true) .
               ";\n?>";

    return $contents;
}

/**
 * @return bool
 * @param module string, key string, value string
 * @desc Wrapper function that will create a field label for every language.
 */
function create_field_label_all_lang($module, $key, $value, $overwrite = false)
{
    $languages = get_languages();
    $return_value = false;

    foreach ($languages as $lang_key => $lang_value) {
        $return_value = create_field_label($module, $lang_key, $key, $value, $overwrite);
        if (!$return_value) {
            break;
        }
    }

    return $return_value;
}

/**
 * @return bool
 * @param module string, language string, key string, value string
 * @desc Returns true if new field label can be created, false otherwise.
 *       Probable reason for returning false: new_field_key already exists.
 */
function create_field_label($module, $language, $key, $value, $overwrite=false)
{
    $return_value = false;
    $mod_strings = return_module_language($language, $module);

    if (isset($mod_strings[$key]) && !$overwrite) {
        $GLOBALS['log']->info("Tried to create a key that already exists: $key");
    } else {
        $mod_strings = array_merge($mod_strings, array($key => $value));
        $dirname = "custom/modules/$module/language";
        $dir_exists = is_dir($dirname);

        if (!$dir_exists) {
            $dir_exists = create_module_lang_dir($module);
        }

        if ($dir_exists) {
            $filename = "$dirname/$language.lang.php";
            if (is_file($filename) && filesize($filename) > 0) {
                $old_contents = file_get_contents($filename);
            } else {
                $old_contents = '';
            }

            if (sugar_file_put_contents($filename, create_field_lang_pak_contents(
                $old_contents,
                $key,
                $value,
                $language,
                $module
            )) !== false) {
                $return_value = true;
                $GLOBALS['log']->info("Successful write to: $filename");
            } else {
                $GLOBALS['log']->info("Unable to write edited language pak to file: $filename");
            }
        } else {
            $GLOBALS['log']->info("Unable to create dir: $dirname");
        }
    }

    return $return_value;
}

/**
 * @return bool
 * @param dropdown_name string
 * @desc Wrapper function that creates a dropdown type for all languages.
 */
function create_dropdown_type_all_lang($dropdown_name)
{
    $languages = get_languages();
    $return_value = false;

    foreach ($languages as $lang_key => $lang_value) {
        $return_value = create_dropdown_type($dropdown_name, $lang_key);
        if (!$return_value) {
            break;
        }
    }

    return $return_value;
}

/**
 * @return bool
 * @param app_list_strings array
 * @desc Saves the app_list_strings to file in the 'custom' dir.
 */
function save_custom_app_list_strings_contents(&$contents, $language, $custom_dir_name = '')
{
    $return_value = false;
    $dirname = 'custom/include/language';
    if (!empty($custom_dir_name)) {
        $dirname = $custom_dir_name;
    }

    $dir_exists = is_dir($dirname);

    if (!$dir_exists) {
        $dir_exists = create_include_lang_dir($dirname);
    }

    if ($dir_exists) {
        $filename = "$dirname/$language.lang.php";

        if (sugar_file_put_contents($filename, $contents) !== false) {
                $return_value = true;
                LoggerManager::getLogger()->info("Successful write to: $filename");
        } else {
            LoggerManager::getLogger()->info("Unable to write edited language pack to file: $filename");
        }
    } else {
        LoggerManager::getLogger()->info("Unable to create dir: $dirname");
    }
    if ($return_value) {
        $cache_key = 'app_list_strings.'.$language;
        sugar_cache_clear($cache_key);
    }

    return $return_value;
}

/**
 * @return bool
 * @param app_list_strings array
 * @desc Saves the app_list_strings to file in the 'custom' dir.
 */
function save_custom_app_list_strings(&$app_list_strings, $language)
{
    $return_value = false;
    $dirname = 'custom/include/language';

    $dir_exists = is_dir($dirname);

    if (!$dir_exists) {
        $dir_exists = create_include_lang_dir($dirname);
    }

    if ($dir_exists) {
        $filename = "$dirname/$language.lang.php";

        $contents =create_dropdown_lang_pak_contents(
            $app_list_strings,
            $language
        );

        if (sugar_file_put_contents($filename, $contents) !== false) {
            $return_value = true;
            $GLOBALS['log']->info("Successful write to: $filename");
        } else {
            $GLOBALS['log']->info("Unable to write edited language pak to file: $filename");
        }
    } else {
        $GLOBALS['log']->info("Unable to create dir: $dirname");
    }
    if ($return_value) {
        $cache_key = 'app_list_strings.'.$language;
        sugar_cache_clear($cache_key);
    }

    return $return_value;
}

function return_custom_app_list_strings_file_contents($language, $custom_filename = '')
{
    $contents = '';

    $filename = "custom/include/language/$language.lang.php";
    if (!empty($custom_filename)) {
        $filename = $custom_filename;
    }

    if (is_file($filename)) {
        $contents = file_get_contents($filename);
    }

    return $contents;
}

/**
 * @return bool
 * @param dropdown_name string, language string
 * @desc Creates a new dropdown type.
 */
function create_dropdown_type($dropdown_name, $language)
{
    $return_value = false;
    $app_list_strings = return_app_list_strings_language($language);

    if (isset($app_list_strings[$dropdown_name])) {
        $GLOBALS['log']->info("Tried to create a dropdown list key that already exists: $dropdown_name");
    } else {
        // get the contents of the custom app list strings file
        $contents = return_custom_app_list_strings_file_contents($language);

        // add the new dropdown_name to it
        if ($contents == '') {
            $new_contents = "<?php\n\$app_list_strings['$dropdown_name'] = array(''=>'');\n?>";
        } else {
            $new_contents = str_replace('?>', "\$app_list_strings['$dropdown_name'] = array(''=>'');\n?>", (string) $contents);
        }

        // save the new contents to file
        $return_value = save_custom_app_list_strings_contents($new_contents, $language);
    }

    return $return_value;
}

/**
 * @return string&
 * @param identifier string, pairs array, first_entry string, selected_key string
 * @desc Generates the HTML for a dropdown list.
 */
function &create_dropdown_html($identifier, &$pairs, $first_entry='', $selected_key='')
{
    $html = "<select name=\"$identifier\">\n";

    if ('' != $first_entry) {
        $html .= "<option name=\"\">$first_entry</option>\n";
    }

    foreach ($pairs as $key => $value) {
        $html .= $selected_key == $key ?
               "<option name=\"$key\" selected=\"selected\">$value</option>\n" :
               "<option name=\"$key\">$value</option>\n";
    }

    $html .= "</select>\n";

    return $html;
}


function dropdown_item_delete($dropdown_type, $language, $index)
{
    $app_list_strings_to_edit = return_app_list_strings_language($language);
    $dropdown_array =$app_list_strings_to_edit[$dropdown_type];
    helper_dropdown_item_delete($dropdown_array, $index);

    $contents = return_custom_app_list_strings_file_contents($language);
    $new_contents = replace_or_add_dropdown_type(
        $dropdown_type,
        $dropdown_array,
        $contents
    );

    save_custom_app_list_strings_contents($new_contents, $language);
}

function helper_dropdown_item_delete(&$dropdown_array, $index)
{
    // perform the delete from the array
    $sliced_off_array = array_splice($dropdown_array, $index);
    array_shift($sliced_off_array);
    $dropdown_array = array_merge($dropdown_array, $sliced_off_array);
}

function dropdown_item_move_up($dropdown_type, $language, $index)
{
    $app_list_strings_to_edit = return_app_list_strings_language($language);
    $dropdown_array =$app_list_strings_to_edit[$dropdown_type];

    if ($index > 0 && $index < (is_countable($dropdown_array) ? count($dropdown_array) : 0)) {
        $key = '';
        $value = '';
        $i = 0;

        reset($dropdown_array);
        foreach ($dropdown_array as $k => $v) {
            if ($i == $index) {
                $key = $k;
                $value = $v;
                break;
            }

            $i++;
        }

        helper_dropdown_item_delete($dropdown_array, $index);
        helper_dropdown_item_insert($dropdown_array, $index - 1, $key, $value);

        // get the contents of the custom app list strings file
        $contents = return_custom_app_list_strings_file_contents($language);
        $new_contents = replace_or_add_dropdown_type(
            $dropdown_type,
            $dropdown_array,
            $contents
        );

        save_custom_app_list_strings_contents($new_contents, $language);
    }
}

function dropdown_item_move_down($dropdown_type, $language, $index)
{
    $app_list_strings_to_edit = return_app_list_strings_language($language);
    $dropdown_array =$app_list_strings_to_edit[$dropdown_type];

    if ($index >= 0 && $index < (is_countable($dropdown_array) ? count($dropdown_array) : 0) - 1) {
        $key = '';
        $value = '';
        $i = 0;

        reset($dropdown_array);
        foreach ($dropdown_array as $k => $v) {
            if ($i == $index) {
                $key = $k;
                $value = $v;
                break;
            }

            $i++;
        }

        helper_dropdown_item_delete($dropdown_array, $index);
        helper_dropdown_item_insert($dropdown_array, $index + 1, $key, $value);

        // get the contents of the custom app list strings file
        $contents = return_custom_app_list_strings_file_contents($language);
        $new_contents = replace_or_add_dropdown_type(
            $dropdown_type,
            $dropdown_array,
            $contents
        );

        save_custom_app_list_strings_contents($new_contents, $language);
    }
}

function dropdown_item_insert($dropdown_type, $language, $index, $key, $value)
{
    $app_list_strings_to_edit = return_app_list_strings_language($language);
    $dropdown_array =$app_list_strings_to_edit[$dropdown_type];
    helper_dropdown_item_insert($dropdown_array, $index, $key, $value);

    // get the contents of the custom app list strings file
    $contents = return_custom_app_list_strings_file_contents($language);
    $new_contents = replace_or_add_dropdown_type(
        $dropdown_type,
        $dropdown_array,
        $contents
    );

    save_custom_app_list_strings_contents($new_contents, $language);
}

function helper_dropdown_item_insert(&$dropdown_array, $index, $key, $value)
{
    $pair = array($key => $value);
    if ($index <= 0) {
        $dropdown_array = array_merge($pair, $dropdown_array);
    }
    if ($index >= (is_countable($dropdown_array) ? count($dropdown_array) : 0)) {
        $dropdown_array = array_merge($dropdown_array, $pair);
    } else {
        $sliced_off_array = array_splice($dropdown_array, $index);
        $dropdown_array = array_merge($dropdown_array, $pair);
        $dropdown_array = array_merge($dropdown_array, $sliced_off_array);
    }
}

function dropdown_item_edit($dropdown_type, $language, $key, $value)
{
    $app_list_strings_to_edit = return_app_list_strings_language($language);
    $dropdown_array =$app_list_strings_to_edit[$dropdown_type];

    $dropdown_array[$key] = $value;

    $contents = return_custom_app_list_strings_file_contents($language);

    // get the contents of the custom app list strings file
    $new_contents = replace_or_add_dropdown_type(
        $dropdown_type,
        $dropdown_array,
        $contents
    );

    save_custom_app_list_strings_contents($new_contents, $language);
}

function replace_or_add_dropdown_type(
    $dropdown_type,
    &$dropdown_array,
    &$file_contents
) {
    $new_contents = "<?php\n?>";
    $new_entry = override_value_to_string(
        'app_list_strings',
        $dropdown_type,
        $dropdown_array
    );

    if (empty($file_contents)) {
        // empty file, must create the php tags
        $new_contents = "<?php\n$new_entry\n?>";
    } else {
        // existing file, try to replace
        $new_contents = replace_dropdown_type(
            $dropdown_type,
            $dropdown_array,
            $file_contents
        );

        $new_contents = dropdown_duplicate_check($dropdown_type, $new_contents);

        if ($new_contents == $file_contents) {
            // replace failed, append to end of file
            $new_contents = str_replace("?>", '', (string) $file_contents);
            $new_contents .= "\n$new_entry\n?>";
        }
    }

    return $new_contents;
}

function replace_or_add_app_string(
    $name,
    $value,
    &$file_contents
) {
    $new_contents = "<?php\n?>";
    $new_entry = override_value_to_string(
        'app_strings',
        $name,
        $value
    );

    if (empty($file_contents)) {
        // empty file, must create the php tags
        $new_contents = "<?php\n$new_entry\n?>";
    } else {
        // existing file, try to replace
        $new_contents = replace_app_string(
            $name,
            $value,
            $file_contents
        );

        $new_contents = app_string_duplicate_check($name, $new_contents);

        if ($new_contents == $file_contents) {
            // replace failed, append to end of file
            $new_contents = str_replace("?>", '', (string) $file_contents);
            $new_contents .= "\n$new_entry\n?>";
        }
    }

    return $new_contents;
}


function dropdown_duplicate_check($dropdown_type, &$file_contents)
{
    if (!empty($dropdown_type) &&
        !empty($file_contents)) {
        $pattern = '/\$app_list_strings\[\''. $dropdown_type .
            '\'\][\ ]*=[\ ]*array[\ ]*\([^\)]*\)[\ ]*;/';

        $result = array();
        preg_match_all($pattern, (string) $file_contents, $result);

        if ((is_countable($result[0]) ? count($result[0]) : 0) > 1) {
            $new_entry = $result[0][0];
            $new_contents = preg_replace($pattern, '', (string) $file_contents);

            // Append the new entry.
            $new_contents = str_replace("?>", '', $new_contents);
            $new_contents .= "\n$new_entry\n?>";
            return $new_contents;
        }

        return $file_contents;
    }

    return $file_contents;
}

function replace_dropdown_type(
    $dropdown_type,
    &$dropdown_array,
    &$file_contents
) {
    $new_contents = $file_contents;

    if (!empty($dropdown_type) &&
        is_array($dropdown_array) &&
        !empty($file_contents)) {
        $pattern = '/\$app_list_strings\[\''. $dropdown_type .
            '\'\][\ ]*=[\ ]*array[\ ]*\([^\)]*\)[\ ]*;/';
        $replacement = override_value_to_string(
            'app_list_strings',
            $dropdown_type,
            $dropdown_array
        );
        $new_contents = preg_replace($pattern, $replacement, (string) $file_contents, 1);
    }

    return $new_contents;
}

function replace_app_string(
    $name,
    $value,
    &$file_contents
) {
    $new_contents = $file_contents;

    if (!empty($name) &&
        is_string($value) &&
        !empty($file_contents)) {
        $pattern = '/\$app_strings\[\''. $name .'\'\][\ ]*=[\ ]*\'[^\']*\'[\ ]*;/';
        $replacement = override_value_to_string(
            'app_strings',
            $name,
            $value
        );
        $new_contents = preg_replace($pattern, $replacement, (string) $file_contents, 1);
    }

    return $new_contents;
}

function app_string_duplicate_check($name, &$file_contents)
{
    if (!empty($name) &&
        !empty($file_contents)) {
        $pattern = '/\$app_strings\[\''. $name .'\'\][\ ]*=[\ ]*\'[^\']*\'[\ ]*;/';

        $result = array();
        preg_match_all($pattern, (string) $file_contents, $result);

        if ((is_countable($result[0]) ? count($result[0]) : 0) > 1) {
            $new_entry = $result[0][0];
            $new_contents = preg_replace($pattern, '', (string) $file_contents);

            // Append the new entry.
            $new_contents = str_replace("?>", '', $new_contents);
            $new_contents .= "\n$new_entry\n?>";
            return $new_contents;
        }
        return $file_contents;
    }

    return $file_contents;
}
