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

/**
 * Connector factory
 * @api
 */
#[\AllowDynamicProperties]
class ConnectorFactory
{
    public static $source_map = array();

    public static function getInstance($source_name)
    {
        if (empty(self::$source_map[$source_name])) {
            require_once('include/connectors/sources/SourceFactory.php');
            require_once('include/connectors/component.php');
            $source = SourceFactory::getSource($source_name);

            if ($source === null){
                return null;
            }

            $component = new component();
            $component->setSource($source);
            $component->init();
            self::$source_map[$source_name] = $component;
        }
        return self::$source_map[$source_name];
    }

    /**
     * Split the class name by _ and go through the class name
     * which represents the inheritance structure to load up all required parents.
     * @param string $class the root class we want to load.
     */
    public static function load($class, $type)
    {
        self::loadClass($class, $type);
    }

    /**
     * include a source class file.
     * @param string $class a class file to include.
     */
    public static function loadClass($class, $type)
    {
        $dir = str_replace('_', '/', $class);

        if (strpos($dir, '..') !== false) {
            return;
        }

        $parts = explode("/", $dir);
        $file = $parts[count($parts)-1] . '.php';
        if (file_exists("custom/modules/Connectors/connectors/{$type}/{$dir}/$file")) {
            require_once("custom/modules/Connectors/connectors/{$type}/{$dir}/$file");
        } else {
            if (file_exists("modules/Connectors/connectors/{$type}/{$dir}/$file")) {
                require_once("modules/Connectors/connectors/{$type}/{$dir}/$file");
            } else {
                if (file_exists("connectors/{$type}/{$dir}/$file")) {
                    require_once("connectors/{$type}/{$dir}/$file");
                }
            }
        }
    }
}
