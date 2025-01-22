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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Retrieves the requested js language file, building it if it doesn't exist.
 */
function getJSLanguage()
{
    require_once 'include/language/jsLanguage.php';

    global $app_list_strings;

    if (empty($_REQUEST['lang'])) {
        echo 'No language specified';

        return;
    }

    $lang = clean_path($_REQUEST['lang']);
    $languages = get_languages();

    if (!preg_match("/^\w\w_\w\w$/", (string) $lang) || !isset($languages[$lang])) {
        if (!preg_match("/^\w\w_\w\w$/", (string) $lang)) {
            echo 'did not match regex<br/>';
        } else {
            echo  "$lang was not in list . <pre>".print_r($languages, true).'</pre>';
        }
        echo 'Invalid language specified';

        return;
    }
    if (empty($_REQUEST['modulename']) || $_REQUEST['modulename'] === 'app_strings') {
        $file = sugar_cached('jsLanguage/').$lang.'.js';
        if (!is_file($file)) {
            jsLanguage::createAppStringsCache($lang);
        }
    } else {
        $module = clean_path($_REQUEST['modulename']);
        $fullModuleList = array_merge($GLOBALS['moduleList'], $GLOBALS['modInvisList']);
        if (!isset($app_list_strings['moduleList'][$module]) && !in_array($module, $fullModuleList)) {
            echo 'Invalid module specified';

            return;
        }
        $file = sugar_cached('jsLanguage/').$module.'/'.$lang.'.js';
        if (!is_file($file)) {
            jsLanguage::createModuleStringsCache($module, $lang);
        }
    }

    //Setup cache headers
    header('Content-Type: application/javascript');
    header('Cache-Control: max-age=31556940, private');
    header('Pragma: ');
    header('Expires: '.gmdate('D, d M Y H:i:s \G\M\T', time() + 31556940));

    readfile($file);
}

getJSLanguage();
