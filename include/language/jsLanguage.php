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


#[\AllowDynamicProperties]
class jsLanguage
{

    /**
     * Creates javascript versions of language files
     */
    public function __construct()
    {
    }




    public static function createAppStringsCache($lang = 'en_us')
    {
        // cn: bug 8242 - non-US langpack chokes
        $app_strings = return_application_language($lang);
        $app_list_strings = return_app_list_strings_language($lang);

        $json = getJSONobj();
        $app_list_strings_encoded = $json->encode($app_list_strings);
        $app_strings_encoded = $json->encode($app_strings);

        $str = <<<EOQ
SUGAR.language.setLanguage('app_strings', $app_strings_encoded);
SUGAR.language.setLanguage('app_list_strings', $app_list_strings_encoded);
EOQ;

        $cacheDir = create_cache_directory('jsLanguage/');
        if ($fh = @sugar_fopen($cacheDir . $lang . '.js', "w")) {
            fwrite($fh, $str);
            fclose($fh);
        }
    }

    public static function createModuleStringsCache($moduleDir, $lang = 'en_us', $return = false)
    {
        $json = getJSONobj();

        // cn: bug 8242 - non-US langpack chokes
        $mod_strings = return_module_language($lang, $moduleDir);
        $mod_strings_encoded = $json->encode($mod_strings);
        $str = "SUGAR.language.setLanguage('" . $moduleDir . "', " . $mod_strings_encoded . ");";

        $cacheDir = create_cache_directory('jsLanguage/' . $moduleDir . '/');

        if ($fh = @fopen($cacheDir . $lang . '.js', 'wb')) {
            fwrite($fh, $str);
            fclose($fh);
        }

        if ($return) {
            return $str;
        }
    }
}
