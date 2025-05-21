<?php

/**
 * This class provides SinergiaTIC's own functionality to include in the display and preDisplay methods
 * used in the view.xxx.php files that exist in the custom/modules/<moduleName>/view.xxxx.php or modules/<moduleName>/view.xxxx.php
 * depending on whether they are SuiteCRM base modules or SinergiaTIC's own modules
 */
#[\AllowDynamicProperties]
class SticViews {
    /**
     * Add the SinergiaCRM functionality in the view(s) display method
     *
     * @param Object $view object, usually $this if we call this method from some view.xxxx.php.
     * This allows all view properties and the related $ bean to be available, if it is a detail or edit view.
     * @return void
     */
    public static function preDisplay($view) {
        global $sugar_config, $current_user;

        // Link basic stic css and JS files
        echo getVersionedScript("SticInclude/js/Utils.js");

        // Enable bean values to
        $parsedValuesToJS = array();
        $parsedValuesTextToJS = array();
        foreach ($view->bean as $key => $value) {
            if (is_string($value)) {
                // We separate the TextArea fields because they can contain line breaks. These produce an error when encoding them to JSON from PHP and decoding them from javascript. 
                if (isset($view->bean->field_name_map[$key]["type"]) && $view->bean->field_name_map[$key]["type"] == 'text') {
                    $parsedValuesTextToJS[$key] = $value;
                } else {
                    $parsedValuesToJS[$key] = $value;
                }
            }
        }

        // We encode the text type fields to JSON
        $parsedValuesTextToJS = json_encode($parsedValuesTextToJS);
        $parsedValuesTextToJS = str_replace("\\", "\\\\", $parsedValuesTextToJS);

        //  We encode the rest of the fields to JSON
        $parsedValuesToJS = json_encode($parsedValuesToJS);

        // Pass the following PHP variables to use in JS
        $recordId = $view->bean->id;
        $siteUrl = $sugar_config["site_url"];
        $userDateFormat = $current_user->getPreference('datef');
        echo <<<SCRIPT
        <script type="text/javascript">
        if(typeof STIC == "undefined"){
            var STIC={};
        }
            STIC.record = Object.assign($parsedValuesToJS, $parsedValuesTextToJS);
            STIC.siteUrl = '$siteUrl';
            STIC.userDateFormat = '$userDateFormat';
        </script>
SCRIPT;

        switch ($view->type) {
        case 'edit':
            break;

        case 'detail':
            break;

        case 'list':
            // Some Suite modules extend the ListViewSmarty class and others do not.
            // Depending on the module, we will load a file that inherits from the ListViewSmarty class of the module, or a generic one.
            $genericFile = 'custom/include/SticListViewSmarty.php';
            $customFile = "custom/modules/{$view->module}/SticListViewSmarty.php";

            // Enable mass updating for all field types.
            if (file_exists($customFile)) {
                require_once $customFile;
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading the next ListViewSmarty File:' . $customFile);

            } else {
                require_once $genericFile;
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading the next ListViewSmarty:' . $genericFile);
            }

            $view->lv = new SticListViewSmarty();

            break;

        default:

            break;
        }

        // Loading Custom View Files, if there is any
        $customViewClass = "{$view->module}_CustomViews";
        $customViewFile = "custom/modules/{$view->module}/{$customViewClass}.php";
        if (file_exists($customViewFile)) {
            require_once $customViewFile;
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading custom view file:' . $customViewFile);
            if (class_exists($customViewClass)) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading custom view class: ' . $customViewClass);
                $customView = new $customViewClass;
                if (method_exists($customViewClass, 'preDisplay')) {
                    $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading custom view method: ' . 'preDisplay');
                    $customView::preDisplay($view);
                }
            }
        }
    }

    /**
     * Add the SinergiaCRM functionality in the view(s) display method
     *
     * @param Object $view object, usually $this if we call this method from some view.xxxx.php.
     * This allows all view properties and the related $ bean to be available, if it is a detail or edit view.
     * @return void
     */
    public static function display($view) {
        
        switch ($view->type) {
        case 'edit':
            // Load the module language files. Required in case of loading from a subpanel
            $module = empty($view->moduleName) ? $view->bean->object_name : $view->moduleName;
            if (!is_file($GLOBALS['sugar_config']['cache_dir'] . 'jsLanguage/' . $module . '/' . $GLOBALS['current_language'] . '.js')) {
                require_once 'include/language/jsLanguage.php';
                jsLanguage::createModuleStringsCache($module, $GLOBALS['current_language']);
            }
            $jsSrc = $GLOBALS['sugar_config']['cache_dir'] . 'jsLanguage/' . $module . '/' . $GLOBALS['current_language'] . '.js?s=' . $GLOBALS['js_version_key'] . '&c=' . $GLOBALS['sugar_config']['js_custom_version'] . '&j=' . $GLOBALS['sugar_config']['js_lang_version'];
            echo '<script type="text/javascript" src="' . $jsSrc . '"></script>';

            break;
        case 'detail':

            break;
        case 'list':

            break;
        default:

            break;
        }

        // Loading Custom View Files, if there is any
        $module = !empty($view->module) ? $view->module : $module;
        $customViewClass = "{$module}_CustomViews";
        $customViewFile = "custom/modules/{$module}/{$customViewClass}.php";
        if (file_exists($customViewFile)) {
            require_once $customViewFile;
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading custom view file: ' . $customViewFile);
            if (class_exists($customViewClass)) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading custom view class: ' . $customViewClass);
                $customView = new $customViewClass;
                if (method_exists($customViewClass, 'display')) {
                    $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Loading custom view method: ' . 'display');
                    $customView::display($view);
                }
            }
        }
    }
}
