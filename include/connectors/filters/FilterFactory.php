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
 * Filter factory
 * @api
 */
#[\AllowDynamicProperties]
class FilterFactory
{
    public static $filter_map = array();

    public static function getInstance($source_name, $filter_name='')
    {
        require_once('include/connectors/filters/default/filter.php');
        $key = $source_name . $filter_name;
        if (empty(self::$filter_map[$key])) {
            if (empty($filter_name)) {
                $filter_name = $source_name;
            }

            //split the wrapper name to find the path to the file.
            $dir = str_replace('_', '/', (string) $filter_name);
            $parts = explode("/", $dir);
            $file = $parts[count($parts)-1];

            //check if this override wrapper file exists.
            require_once('include/connectors/ConnectorFactory.php');
            if (file_exists("modules/Connectors/connectors/filters/{$dir}/{$file}.php") ||
               file_exists("custom/modules/Connectors/connectors/filters/{$dir}/{$file}.php")) {
                ConnectorFactory::load($filter_name, 'filters');
                try {
                    $filter_name .= '_filter';
                } catch (Exception $ex) {
                    return null;
                }
            } else {
                //if there is no override wrapper, use the default.
                $filter_name = 'default_filter';
            }

            $component = ConnectorFactory::getInstance($source_name);
            $filter = new $filter_name();
            $filter->setComponent($component);
            self::$filter_map[$key] = $filter;
        } //if
        return self::$filter_map[$key];
    }
}
