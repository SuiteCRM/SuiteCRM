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
 * Formatter factory
 * @api
 */
#[\AllowDynamicProperties]
class FormatterFactory
{
    public static $formatter_map = array();

    /**
     * getInstance
     * This method returns a formatter instance for the given source name and
     * formatter name.  If no formatter name is specified, the default formatter
     * for the source is used.
     *
     * @param $source_name The data source name to retreive formatter for
     * @param $formatter_name Optional formatter name to use
     * @param $wrapper_name Optional wrapper name to use
     * @return $instance The formatter instance
     */
    public static function getInstance($source_name, $formatter_name='')
    {
        require_once('include/connectors/formatters/default/formatter.php');
        $key = $source_name . $formatter_name;
        if (empty(self::$formatter_map[$key])) {
            if (empty($formatter_name)) {
                $formatter_name = $source_name;
            }

            //split the wrapper name to find the path to the file.
            $dir = str_replace('_', '/', (string) $formatter_name);
            $parts = explode("/", $dir);
            $file = $parts[count($parts)-1];

            //check if this override wrapper file exists.
            require_once('include/connectors/ConnectorFactory.php');
            if (file_exists("modules/Connectors/connectors/formatters/{$dir}/{$file}.php") ||
               file_exists("custom/modules/Connectors/connectors/formatters/{$dir}/{$file}.php")) {
                ConnectorFactory::load($formatter_name, 'formatters');
                try {
                    $formatter_name .= '_formatter';
                } catch (Exception $ex) {
                    return null;
                }
            } else {
                //if there is no override wrapper, use the default.
                $formatter_name = 'default_formatter';
            }

            $component = ConnectorFactory::getInstance($source_name);
            $formatter = new $formatter_name();
            $formatter->setComponent($component);
            if (file_exists("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/{$file}.tpl")) {
                $formatter->setTplFileName("custom/modules/Connectors/connectors/formatters/{$dir}/tpls/{$file}.tpl");
            } else {
                if ("modules/Connectors/connectors/formatters/{$dir}/tpls/{$file}.tpl") {
                    $formatter->setTplFileName("modules/Connectors/connectors/formatters/{$dir}/tpls/{$file}.tpl");
                }
            }
            self::$formatter_map[$key] = $formatter;
        } //if
        return self::$formatter_map[$key];
    }
}
