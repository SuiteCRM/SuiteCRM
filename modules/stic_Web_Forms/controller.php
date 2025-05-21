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
#[\AllowDynamicProperties]
class stic_Web_FormsController extends SugarController
{
    public $webFormClass; // Its value is indicated in the url that is executed from the menu option

    /**
     * Overwrite the preprocess function to perform common checks.
     */
    public function preProcess()
    {
        $this->getFilteredParams();

        // The webFormClass attribute is necessary to load the appropriate driver, if it is empty it cannot be continued
        if (empty($this->webFormClass)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Cannot set the module to map the fields");
            $this->no_action(); // Display the No Action screen
        }
    }

    /**
     * Assistant Actions Controller.
     */
    public function action_assistant()
    {
        $this->target_module = "stic_Web_Forms/Assistant/{$this->webFormClass}";
        $this->commonAction();
    }

    /**
     * Data capture actions controller
     */
    public function action_save()
    {
        $this->target_module = "stic_Web_Forms/Catcher/{$this->webFormClass}";
        $this->commonAction();
    }

    /**
     * Load the appropriate driver for requests of any type using the target_module property
     */
    private function commonAction()
    {
        $controllerClass = "{$this->webFormClass}Controller";
        $controllerFile = "modules/{$this->target_module}/{$controllerClass}.php";
        $customControllerFile = "custom/Extension/{$controllerFile}";
        $customControllerClass = "Custom{$controllerClass}";

        // Try loading the custom driver from the custom folder if it exists
        if (file_exists($customControllerFile)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading custom driver [{$customControllerClass}] from file [{$customControllerFile}] ... ");
            require_once $customControllerFile;
            $controller = new $customControllerClass();
        } else if (file_exists($controllerFile)) {
            // If the driver does not exist in the custom folder, look for it in the standard folder
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Loading driver [{$controllerClass}] from file [{$controllerFile}] ... ");
            require_once $controllerFile;
            $controller = new $controllerClass();
        } else {
            // If it is not in the standard folder, an error returns
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ":  No driver found for [{$this->webFormClass}] in {$customControllerFile} or in {$controllerFile}.");
            $this->no_action(); // Display the No Action screen
        }

        // Manage the request through the loaded controller
        $controller->import($this); // Copy the data of the main driver into the loaded driver
        $controller->doAction();
        $this->import($controller); // Copy the data of the loaded driver into the main driver
    }

    /**
     * Import the properties of an object into the object itself.
     * Necessary to move the properties of the main controller (loaded by sugar) to the secondary (loaded by this class) and vice versa
     * @param $object
     */
    public function import($object)
    {
        $object_vars = get_object_vars($object);

        foreach ($object_vars as $key => &$value) {
            switch (gettype($value)) {
                case 'object':
                    $this->$key = clone $value;
                    break;
                default:
                    $this->$key = $value;
            }
        }
    }

    /**
     * Returns the filtered input data
     * @param Array $fields
     * @param Array $defaultValues
     * @param Const $filter
     * @return Array array with filtered values
     */
    
    // STIC Custom 20220312 JCH - Set a less restrictive filter to avoid removing valid code in the generated form.
    // STIC#633
    // protected function getFilteredParams($defaultValues = null, $filter = FILTER_SANITIZE_STRING)
    protected function getFilteredParams($defaultValues = null, $filter = FILTER_DEFAULT)
    // END STIC
    {
        $ret = array();
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  RAW input parameters: " . print_r($_REQUEST, true));

        // Filter the input parameters
        $ret = $this->arrayFilter($_REQUEST, $filter);

        // If there are default parameters, check if they have been read, otherwise the default value is assigned
        if (!empty($defaultValues)) {
            $keys = array_keys($defaultValues);
            foreach ($keys as $key) {
                if (!isset($ret[$key])) {$ret[$key] = $defaultValues[$key];}
            }
        }

        // Save array fields as object properties
        $keys = array_keys($ret);
        foreach ($keys as $key) {
            $this->$key = $ret[$key];
        }

        return $ret;
    }

    /**
     * Filter parameters of an array
     * @param Array $array
     * @param Object $filter Php filter value
     * @return Array array with filtered values
     */
    protected function arrayFilter($array, $filter, $level = 0)
    {
        $maxRecLevel = 50; // Maximum recursion level
        $keys = array_keys($array);
        $ret = array();

        foreach ($keys as $key) {
            if (is_array($array[$key])) {
                if ($level > $maxRecLevel) {
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Maximum recursion level reached: {$maxRecLevel}");
                } else {
                    $ret[$key] = $this->arrayFilter($array[$key], $filter, $level + 1);
                }
            } else {
                $ret[$key] = filter_var($array[$key], $filter);
            }
        }
        return $ret;
    }

    /**
     * Returns the url of the server considering if we are working in https or http
     * @return String The server base URL
     */
    public static function getServerURL()
    {
        global $sugar_config;
        $protocol = $_SERVER['HTTPS'] ?? null;
        $protocol = (empty($protocol) || $protocol == 'off') ? 'http' : 'https';
        $siteUrl = $sugar_config['site_url'];
        $rootApp = substr($siteUrl, strpos($siteUrl, '//') + 2);
        return "$protocol://$rootApp";
    }

    /**
     * Returns all well defined reCAPTCHA configurations, in an array 
     * (allows multiple reCAPTCHA configurations):
     * 
     * ['(General)']['NAME'] -> (Empty if is "General" reCAPTCHA configuration)
     * ['(General)']['KEY'] -> Private Key for "General" reCAPTCHA configuration
     * ['(General)']['WEBKEY'] -> Public Web Key for "General" reCAPTCHA configuration
     * ['(General)']['VERSION'] -> reCAPTCHA version for "General" configuration
     * 
     * ['FOOTBALL']['NAME'] -> "FOOTBALL", the name for the reCAPTCHA configuration
     * ['FOOTBALL']['KEY'] -> Private Key for "FOOTBALL" reCAPTCHA configuration
     * ['FOOTBALL']['WEBKEY'] -> Public Web Key for "FOOTBALL" reCAPTCHA configuration
     * ['FOOTBALL']['VERSION'] -> reCAPTCHA version for "FOOTBALL" configuration
     *
     * @return array
     */
    protected function getRecaptchaConfigurations()
    {
        require_once "modules/stic_Web_Forms/sticUtils.php";
        return SticWebFormsUtils::getRecaptchaConfigurations();
    }
}
