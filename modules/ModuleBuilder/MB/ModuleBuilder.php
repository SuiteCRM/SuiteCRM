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

/*********************************************************************************

 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/
 

define('MB_PACKAGE_PATH', 'custom/modulebuilder/packages') ;
define('MB_PACKAGE_BUILD', 'custom' . DIRECTORY_SEPARATOR . 'modulebuilder' . DIRECTORY_SEPARATOR . 'builds');
require_once('modules/ModuleBuilder/MB/MBPackage.php') ;

#[\AllowDynamicProperties]
class ModuleBuilder
{
    public $packages = array( ) ;

    public function getPackageList()
    {
        static $list = array( ) ;
        if (! empty($list) || ! file_exists(MB_PACKAGE_PATH)) {
            return $list ;
        }
        $d = dir(MB_PACKAGE_PATH) ;
        while ($e = $d->read()) {
            if (file_exists(MB_PACKAGE_PATH . '/' . $e . '/manifest.php')) {
                $list [] = $e ;
            }
        }
        sort($list) ; // order important as generate_nodes_array in Tree.php later loops over this by foreach to generate the package list
        return $list ;
    }

    /**
     * @param $name
     * @return MBPackage
     */
    public function getPackage($name)
    {
        if (empty($this->packages [ $name ])) {
            $this->packages [ $name ] = new MBPackage($name) ;
        }

        return $this->packages [ $name ] ;
    }
    
    public function getPackageKey($name)
    {
        $manifestPath = MB_PACKAGE_PATH . '/' . $name . '/manifest.php' ;
        if (file_exists($manifestPath)) {
            require($manifestPath) ;
            if (!empty($manifest)) {
                return $manifest['key'];
            }
        }
        return false ;
    }

    /**
     * @param $package
     * @param $module
     * @return mixed
     */
    public function &getPackageModule($package, $module)
    {
        $this->getPackage($package) ;
        $this->packages [ $package ]->getModule($module) ;
        return $this->packages [ $package ]->modules [ $module ] ;
    }

    public function save()
    {
        $packages = array_keys($this->packages) ;
        foreach ($packages as $package) {
            $this->packages [ $package ]->save() ;
        }
    }

    public function build()
    {
        $packages = array_keys($this->packages) ;
        foreach ($packages as $package) {
            if (count($packages) == 1) {
                $this->packages [ $package ]->build(true) ;
            } else {
                $this->packages [ $package ]->build(false) ;
            }
        }
    }

    public function getPackages()
    {
        if (empty($this->packages)) {
            $list = $this->getPackageList() ;
            foreach ($list as $package) {
                if (! empty($this->packages [ $package ])) {
                    continue ;
                }
                $this->packages [ $package ] = new MBPackage($package) ;
            }
        }
    }

    public function getNodes()
    {
        $this->getPackages() ;
        $nodes = array( ) ;
        foreach (array_keys($this->packages) as $name) {
            $nodes [] = $this->packages [ $name ]->getNodes() ;
        }
        return $nodes ;
    }

    /**
     * Function return module name and this aliases
     *
     * @param string $module
     * @return array $aliases
     */
    public static function getModuleAliases($module)
    {
        $aliases = array($module);
        $relate_arr = array(
            'Users' => 'Employees',
            'Employees' => 'Users'
        );

        if (isset($relate_arr[$module])) {
            $aliases[] = $relate_arr[$module];
        }

        return $aliases;
    }
}
