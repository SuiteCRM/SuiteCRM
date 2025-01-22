<?php
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
class MBLanguage
{
    public $iTemplates = array();
    public $templates = array();
    public function __construct($name, $path, $label, $key_name)
    {
        $this->path = $path;
        $this->name = $name;
        $this->key_name = $key_name;
        $this->label = $label;
    }

    public function load()
    {
        $this->generateModStrings();
        $this->generateAppStrings();
    }

    public function loadStrings($file)
    {
        global $mod_strings;
        $mod_strings = $mod_strings ?? [];
        $module = strtoupper($this->name);
        $object_name = strtoupper($this->key_name);
        $_object_name = strtolower($this->name);
        if (!file_exists($file)) {
            return;
        }

        $d = dir($file);
        while ($e = $d->read()) {
            if (substr($e, 0, 1) != '.' && is_file($file . '/' . $e)) {
                include($file.'/'. $e);
                if (empty($this->strings[$e])) {
                    $this->strings[$e] = $mod_strings;
                } else {
                    $this->strings[$e] = array_merge($this->strings[$e], $mod_strings);
                }
            }
        }
    }

    public function loadAppListStrings($file)
    {
        global $app_list_strings;
        $app_list_strings = $app_list_strings ?? [];
        if (!file_exists($file)) {
            return;
        }
        //we may not need this when loading in the app strings, but there is no harm
        //in setting it.
        $object_name = strtolower($this->key_name);

        $d = dir($file);
        while ($e = $d->read()) {
            if (substr($e, 0, 1) != '.' && is_file($file . '/' . $e)) {
                include($file.'/'. $e);
                if (empty($this->appListStrings[$e])) {
                    $this->appListStrings[$e] = $app_list_strings;
                } else {
                    $this->appListStrings[$e] = array_merge($this->appListStrings[$e], $app_list_strings);
                }
            }
        }
    }

    public function generateModStrings()
    {
        $this->strings = array();
        $this->loadTemplates();
        $current_mod_strings = $GLOBALS['mod_strings'];

        foreach ($this->iTemplates as $template=>$val) {
            $file = MB_IMPLEMENTS . '/' . $template . '/language';
            $this->loadStrings($file);
        }
        foreach ($this->templates as $template=>$val) {
            $file = MB_TEMPLATES . '/' . $template . '/language';
            $this->loadStrings($file);
        }
        $this->loadStrings($this->path . '/language');

        //reset global mod_string to prevent conflicts with Module Builder modules 
        $GLOBALS['mod_strings'] = $current_mod_strings;
    }

    public function getModStrings($language='en_us')
    {
        $language .= '.lang.php';
        if (!empty($this->strings[$language]) && $language != 'en_us.lang.php') {
            return sugarLangArrayMerge($this->strings['en_us.lang.php'], $this->strings[$language]);
        }
        if (!empty($this->strings['en_us.lang.php'])) {
            return $this->strings['en_us.lang.php'];
        }
        $empty = array();
        return $empty;
    }
    public function getAppListStrings($language='en_us')
    {
        $language .= '.lang.php';
        if (!empty($this->appListStrings[$language]) && $language != 'en_us.lang.php') {
            return sugarLangArrayMerge($this->appListStrings['en_us.lang.php'], $this->appListStrings[$language]);
        }
        if (!empty($this->appListStrings['en_us.lang.php'])) {
            return $this->appListStrings['en_us.lang.php'];
        }
        $empty = array();
        return $empty;
    }

    public function generateAppStrings($buildFromTemplate = true)
    {
        $this->appListStrings = array('en_us.lang.php'=>array());
        //By default, generate app strings for the current language as well.
        $this->appListStrings[$GLOBALS [ 'current_language' ] . ".lang.php"] = array();
        $this->loadAppListStrings($this->path . '/../../language/application');

        if ($buildFromTemplate) {
            //go through the templates application strings and load anything that is needed
            foreach ($this->iTemplates as $template=>$val) {
                $file = MB_IMPLEMENTS . '/' . $template . '/language/application';
                $this->loadAppListStrings($file);
            }
            foreach ($this->templates as $template=>$val) {
                $file = MB_TEMPLATES . '/' . $template . '/language/application';
                $this->loadAppListStrings($file);
            }
        }
    }
    public function save($key_name, $duplicate=false, $rename=false)
    {
        $header = file_get_contents('modules/ModuleBuilder/MB/header.php');
        $save_path = $this->path . '/language';
        mkdir_recursive($save_path);
        foreach ($this->strings as $lang=>$values) {
            //Check if the module Label has changed.
            $renameLang = $rename || empty($values) || (isset($values['LBL_MODULE_NAME']) && $this->label != $values['LBL_MODULE_NAME']);
            $mod_strings = return_module_language(str_replace('.lang.php', '', (string) $lang), 'ModuleBuilder');
            $required = array(
                'LBL_LIST_FORM_TITLE'=>$this->label . " " . $mod_strings['LBL_LIST'],
                'LBL_MODULE_NAME'=>$this->label,
                'LBL_MODULE_TITLE'=>$this->label,
                'LBL_HOMEPAGE_TITLE'=>$mod_strings['LBL_HOMEPAGE_PREFIX'] . " " . $this->label,
                //FOR GENERIC MENU
                'LNK_NEW_RECORD'=>$mod_strings['LBL_CREATE'] ." ". $this->label,
                'LNK_LIST'=>$mod_strings['LBL_VIEW'] ." ". $this->label,
                'LNK_IMPORT_'.strtoupper($this->key_name)=>translate('LBL_IMPORT') ." ". $this->label,
                'LBL_SEARCH_FORM_TITLE'=>$mod_strings['LBL_SEARCH_BUTTON'] ." ". $this->label,
                'LBL_HISTORY_SUBPANEL_TITLE'=>$mod_strings['LBL_HISTORY'],
                'LBL_ACTIVITIES_SUBPANEL_TITLE'=>$mod_strings['LBL_ACTIVITIES'],
                'LBL_'.strtoupper($this->key_name).'_SUBPANEL_TITLE'=>$this->label,
                'LBL_NEW_FORM_TITLE' => $mod_strings['LBL_NEW'] ." ". $this->label,
                );
            foreach ($required as $k=>$v) {
                if (empty($values[$k]) || $renameLang) {
                    $values[$k] = $v;
                }
            }
            write_array_to_file('mod_strings', $values, $save_path .'/'.$lang, 'w', $header);
        }
        $app_save_path = $this->path . '/../../language/application';
        mkdir_recursive($app_save_path);
        $key_changed = ($this->key_name != $key_name);

        foreach ($this->appListStrings as $lang=>$values) {
            // Load previously created modules data
            // $app_list_strings = array (); --- fix for issue #305
            $neededFile = $app_save_path . '/'. $lang;
            if (file_exists($neededFile)) {
                include $neededFile;
            }


            if (!$duplicate) {
                unset($values['moduleList'][$this->key_name]);
            }


            // $values = sugarLangArrayMerge($values, $app_list_strings); --- fix for issue #305
            $values['moduleList'][$key_name]= $this->label;


            $appFile = $header. "\n";
            require_once('include/utils/array_utils.php');
            $this->getGlobalAppListStringsForMB($values);
            foreach ($values as $key=>$array) {
                if ($duplicate) {
                    //keep the original when duplicating
                    $appFile .= override_value_to_string_recursive2('app_list_strings', $key, $array);
                }
                $okey = $key;
                if ($key_changed) {
                    $key = str_replace($this->key_name, $key_name, (string) $key);
                }
                if ($key_changed) {
                    $key = str_replace(strtolower($this->key_name), strtolower($key_name), (string) $key);
                }
                // if we aren't duplicating or the key has changed let's add it
                if (!$duplicate || $okey != $key) {
                    $appFile .= override_value_to_string_recursive2('app_list_strings', $key, $array);
                }
            }

            sugar_file_put_contents($app_save_path . '/'. $lang, $appFile);
        }
    }

    /**
    *  If there is no this dropdown list  in  custom\modulebuilder\packages\$package\language\application\$lang.lang.php ,
    *  we will include it from global app_list_string array into custom\modulebuilder\packages\$package\language\application\$lang.lang.php
    *  when we create a dropdown filed  and the value is created in MB.(#20728 )
    **/
    public function getGlobalAppListStringsForMB(&$values)
    {
        //Ensure it comes from MB
        if (!empty($_REQUEST['view_package']) && !empty($_REQUEST['type']) && $_REQUEST['type'] == 'enum'  && !empty($_REQUEST['options'])) {
            if (!isset($values[$_REQUEST['options']])) {
                global $app_list_strings;
                if (!empty($app_list_strings[$_REQUEST['options']])) {
                    $values[$_REQUEST['options']]  = $app_list_strings[$_REQUEST['options']];
                }
            }
        }
    }

    public function build($path)
    {
        if (file_exists($this->path.'/language/')) {
            copy_recursive($this->path.'/language/', $path . '/language/');
        }
    }

    public function loadTemplates()
    {
        if (empty($this->templates)) {
            if (file_exists("$this->path/config.php")) {
                $config = [];
                include "$this->path/config.php";
                $this->templates = $config['templates'];
                $this->iTemplates = array();
            }
        }
    }

    /**
     * Reset the templates and load the language files again.  This is called from
     * MBModule->save() once the config file has been written.
     */
    public function reload()
    {
        $this->templates = null;
        $this->load();
    }

    /**
     * Attempts to translate the given label if it is contained in this
     * undeployed module's language strings
     *
     * @param string $label Label to translate
     * @param string $language Language to use to translate the label
     * @return string
     */
    public function translate($label, $language = "en_us")
    {
        $language = $language . ".lang.php";
        if (isset($this->strings[$language][$label])) {
            return $this->strings[$language][$label];
        }

        if (isset($this->appListStrings[$language][$label])) {
            return $this->appListStrings[$language][$label];
        }

        return $label;
    }
}
