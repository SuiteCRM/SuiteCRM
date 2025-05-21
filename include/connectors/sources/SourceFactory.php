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
 * Provides a factory to loading a connector along with any key->value options to initialize on the
 * source.  The name of the class to be loaded, corresponds to the path on the file system. For example a source
 * with the name ext_soap_hoovers would be ext/soap/hoovers.php
 * @api
 */
#[\AllowDynamicProperties]
class SourceFactory
{

    /**
     * Given a source param, load the correct source and return the object
     * @param string $source string representing the source to load
     * @return source
     */
    public static function getSource($class, $call_init = true)
    {
        $dir = str_replace('_', '/', (string) $class);

        if (strpos($dir, '..') !== false) {
            return null;
        }

        $parts = explode("/", $dir);
        $file = $parts[count($parts)-1];
        $pos = strrpos($file, '/');
        //if(file_exists("connectors/sources/{$dir}/{$file}.php") || file_exists("custom/connectors/sources/{$dir}/{$file}.php")){
        require_once('include/connectors/sources/default/source.php');
        require_once('include/connectors/ConnectorFactory.php');
        ConnectorFactory::load($class, 'sources');
        try {
            $instance = new $class();
            if ($call_init) {
                $instance->init();
            }
            return $instance;
        } catch (Exception $ex) {
            return null;
        }
        //}

        return null;
    }
}
