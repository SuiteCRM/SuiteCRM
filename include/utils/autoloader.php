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
class SugarAutoLoader
{
    public static $map = array(
        'XTemplate' => 'XTemplate/xtpl.php',
        'ListView' => 'include/ListView/ListView.php',
        'Sugar_Smarty' => 'include/Sugar_Smarty.php',
        'Javascript' => 'include/javascript/javascript.php',
    );

    public static $noAutoLoad = array(
        'Tracker' => true,
    );

    public static $moduleMap = array();

    /**
     * @param $class
     * @return bool
     */
    public static function autoload($class)
    {
        $uclass = ucfirst($class);
        if (!empty(self::$noAutoLoad[$class])) {
            return false;
        }
        if (!empty(self::$map[$uclass])) {
            require_once self::$map[$uclass];

            return true;
        }

        if (empty(self::$moduleMap)) {
            if (isset($GLOBALS['beanFiles'])) {
                self::$moduleMap = $GLOBALS['beanFiles'];
            } else {
                include 'include/modules.php';
                self::$moduleMap = $beanFiles;
            }
        }
        if (!empty(self::$moduleMap[$class]) && file_exists(self::$moduleMap[$class])) {
            require_once self::$moduleMap[$class];

            return true;
        }
        $viewPath = self::getFilenameForViewClass($class);
        if (!empty($viewPath)) {
            require_once $viewPath;

            return true;
        }
        $reportWidget = self::getFilenameForSugarWidget($class);
        if (!empty($reportWidget)) {
            require_once $reportWidget;

            return true;
        }

        return false;
    }

    /**
     * @param $class
     * @return bool|string
     */
    protected static function getFilenameForViewClass($class)
    {
        $module = false;
        if (!empty($_REQUEST['module']) && substr((string) $class, 0, strlen((string) $_REQUEST['module'])) == $_REQUEST['module']) {
            //This is a module view
            $module = $_REQUEST['module'];
            $class = substr((string) $class, strlen((string) $module));
        }

        if (substr((string) $class, 0, 4) == 'View') {
            $view = strtolower(substr((string) $class, 4));
            if ($module) {
                $modulepath = "modules/$module/views/view.$view.php";
                if (file_exists("custom/$modulepath")) {
                    return "custom/$modulepath";
                }
                if (file_exists($modulepath)) {
                    return $modulepath;
                }
            } else {
                $basepath = "include/MVC/View/views/view.$view.php";
                if (file_exists("custom/$basepath")) {
                    return "custom/$basepath";
                }
                if (file_exists($basepath)) {
                    return $basepath;
                }
            }
        }

        return false;
    }

    /**
     * getFilenameForSugarWidget
     * This method attempts to autoload classes starting with name "SugarWidget".  It first checks for the file
     * in custom/include/generic/SugarWidgets directory and if not found defaults to include/generic/SugarWidgets.
     * This method is used so that we can easily customize and extend these SugarWidget classes.
     *
     * @static
     *
     * @param $class String name of the class to load
     *
     * @return string file of the SugarWidget class; false if none found
     */
    protected static function getFilenameForSugarWidget($class)
    {
        //Only bother to check if the class name starts with SugarWidget
        if (strpos((string) $class, 'SugarWidget') !== false) {
            if (strpos((string) $class, 'SugarWidgetField') !== false) {
                //We need to lowercase the portion after SugarWidgetField
                $name = substr((string) $class, 16);
                if (!empty($name)) {
                    $class = 'SugarWidgetField'.strtolower($name);
                }
            }

            $file = get_custom_file_if_exists("include/generic/SugarWidgets/{$class}.php");
            if (file_exists($file)) {
                return $file;
            }
        }

        return false;
    }

    public static function loadAll()
    {
        foreach (self::$map as $class => $file) {
            require_once $file;
        }

        if (isset($GLOBALS['beanFiles'])) {
            $files = $GLOBALS['beanFiles'];
        } else {
            include 'include/modules.php';
            $files = $beanFiles;
        }
        foreach ($files as $class => $file) {
            require_once $file;
        }
    }
}
