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


require_once('include/connectors/utils/ConnectorUtils.php');
require_once('include/connectors/sources/SourceFactory.php');
/**
 * Provides a factory to list, discover and create external API calls
 *
 * Main features are to list available external API's by supported features, modules and which ones have access for the user.
 * @api
 */
#[\AllowDynamicProperties]
class ExternalAPIFactory
{
    /**
     * Filter the list of APIs, removing disabled ones
     * @param array $apiFullList
     * @return array Filtered list
     */
    public static function filterAPIList($apiFullList)
    {
        $filteredList = array();
        foreach ($apiFullList as $name => $data) {
            if (isset($data['connector'])) {
                if (ConnectorUtils::eapmEnabled($data['connector'])) {
                    if (isset($data['authMethod']) && $data['authMethod'] == 'oauth') {
                        $connector = SourceFactory::getSource($data['connector'], false);
                        if (!empty($connector) && $connector->propertyExists('oauth_consumer_key')
                            && $connector->isRequiredConfigFieldsSet()) {
                            $filteredList[$name] = $data;
                        }
                    } elseif (isset($data['authMethod']) && $data['authMethod'] == 'oauth2') {
                        $connector = SourceFactory::getSource($data['connector'], false);
                        if (!empty($connector) && $connector->isRequiredConfigFieldsSet()) {
                            $filteredList[$name] = $data;
                        }
                    } else {
                        $filteredList[$name] = $data;
                    }
                }
            } else {
                $filteredList[$name] = $data;
            }
        }
        return $filteredList;
    }

    /**
     * Get the list of available APIs
     * @param bool $forceRebuild
     * @param bool $ignoreDisabled Should we ignore disabled status?
     * @return array
     */
    public static function loadFullAPIList($forceRebuild=false, $ignoreDisabled = false)
    {
        if (inDeveloperMode()) {
            static $beenHereBefore = false;
            if (!$beenHereBefore) {
                $forceRebuild = true;
                $beenHereBefore = true;
            }
        }
        $fullAPIList = [];
        $cached=sugar_cached('include/externalAPI.cache.php');
        if (!$forceRebuild && file_exists($cached)) {
            // Already have a cache file built, no need to rebuild
            require $cached;

            return $ignoreDisabled?$fullAPIList:self::filterAPIList($fullAPIList);
        }

        $apiFullList = array();
        $meetingPasswordList = array();
        $needUrlList = array();

        $baseDirList = array('include/externalAPI/','custom/include/externalAPI/');
        foreach ($baseDirList as $baseDir) {
            $dirList = glob($baseDir.'*', GLOB_ONLYDIR);
            foreach ($dirList as $dir) {
                if ($dir == $baseDir.'.' || $dir == $baseDir.'..' || $dir == $baseDir.'Base') {
                    continue;
                }

                $apiName = str_replace($baseDir, '', (string) $dir);
                if (file_exists($dir.'/ExtAPI'.$apiName.'.php')) {
                    $apiFullList[$apiName]['className'] = 'ExtAPI'.$apiName;
                    $apiFullList[$apiName]['file'] = $dir.'/'.$apiFullList[$apiName]['className'].'.php';
                }
                if (file_exists($dir.'/ExtAPI'.$apiName.'_cstm.php')) {
                    $apiFullList[$apiName]['className'] = 'ExtAPI'.$apiName.'_cstm';
                    $apiFullList[$apiName]['file_cstm'] = $dir.'/'.$apiFullList[$apiName]['className'].'.php';
                }
            }
        }

        $optionList = array('supportedModules','useAuth','requireAuth','supportMeetingPassword','docSearch', 'authMethod', 'oauthFixed','needsUrl','canInvite','sendsInvites','sharingOptions','connector', 'oauthParams','restrictUploadsByExtension');
        foreach ($apiFullList as $apiName => $apiOpts) {
            require_once($apiOpts['file']);
            if (!empty($apiOpts['file_cstm'])) {
                require_once($apiOpts['file_cstm']);
            }
            $className = $apiOpts['className'];
            $apiClass = new $className();
            foreach ($optionList as $opt) {
                if (isset($apiClass->$opt)) {
                    $apiFullList[$apiName][$opt] = $apiClass->$opt;
                }
            }

            // Special handling for the show/hide of the Meeting Password field, we need to create a dropdown for the Sugar Logic code.
            if (isset($apiClass->supportMeetingPassword) && $apiClass->supportMeetingPassword == true) {
                $meetingPasswordList[$apiName] = $apiName;
            }
        }

        create_cache_directory('/include/');
        $cached_tmp = sugar_cached('include/externalAPI.cache-tmp.php');
        $fd = fopen($cached_tmp, 'wb');
        fwrite($fd, "<"."?php\n//This file is auto generated by ".basename(__FILE__)."\n\$fullAPIList = ".var_export($apiFullList, true).";\n\n");
        fclose($fd);
        rename($cached_tmp, $cached);

        $fd = fopen(sugar_cached('include/externalAPI.cache-tmp.js'), 'wb');
        fwrite($fd, "//This file is auto generated by ".basename(__FILE__)."\nSUGAR.eapm = ".json_encode($apiFullList).";\n\n");
        fclose($fd);
        rename(sugar_cached('include/externalAPI.cache-tmp.js'), sugar_cached('include/externalAPI.cache.js'));


        if (!isset($GLOBALS['app_list_strings']['extapi_meeting_password']) || (is_array($GLOBALS['app_list_strings']['extapi_meeting_password']) && count(array_diff($meetingPasswordList, $GLOBALS['app_list_strings']['extapi_meeting_password'])) != 0)) {
            // Our meeting password list is different... we need to do something about this.
            require_once('modules/Administration/Common.php');
            $languages = get_languages();
            foreach ($languages as $lang => $langLabel) {
                $contents = return_custom_app_list_strings_file_contents($lang);
                $new_contents = replace_or_add_dropdown_type('extapi_meeting_password', $meetingPasswordList, $contents);
                save_custom_app_list_strings_contents($new_contents, $lang);
            }
        }

        return $ignoreDisabled?$apiFullList:self::filterAPIList($apiFullList);
    }

    /**
    * Clear API cache file
    */
    public static function clearCache()
    {
        $cached=sugar_cached('include/externalAPI.cache.php');
        if (file_exists($cached)) {
            unlink($cached);
        }
        $cached=sugar_cached('include/externalAPI.cache.js');
        if (file_exists($cached)) {
            unlink($cached);
        }
    }


    /**
     * This will hand back an initialized class for the requested external API, it will also load in the external API password information into the bean.
     * @param string $apiName The name of the requested API ( known API's can be listed by the listAPI() call )
     * @param bool $apiName Ignore authentication requirements (optional)
     * @return ExternalAPIBase API plugin
     */
    public static function loadAPI($apiName, $ignoreAuth=false)
    {
        $apiList = self::loadFullAPIList();
        if (! isset($apiList[$apiName])) {
            return false;
        }

        $myApi = $apiList[$apiName];
        require_once($myApi['file']);
        if (!empty($myApi['file_cstm'])) {
            require_once($myApi['file_cstm']);
        }

        $apiClassName = $myApi['className'];

        $apiClass = new $apiClassName();
        if ($ignoreAuth) {
            return $apiClass;
        }

        if ($myApi['useAuth']) {
            $eapmBean = EAPM::getLoginInfo($apiName);

            if (!isset($eapmBean->application) && $myApi['requireAuth']) {
                // We need authentication, and they don't have it, don't load the API
                return false;
            }
        }

        if ($myApi['useAuth'] && isset($eapmBean->application)) {
            $apiClass->loadEAPM($eapmBean);
        }

        return $apiClass;
    }

    /**
     * Lists the available API's for a module or all modules, and possibly ignoring if the user has auth information for that API even if it is required
     * @param string $module Which module name you are searching for, leave blank to find all API's
     * @param bool $ignoreAuth Ignore API's demands for authentication (used to get a complete list of modules
     * @return API class
     */
    public static function listAPI($module = '', $ignoreAuth = false)
    {
        $apiList = self::loadFullAPIList();

        if ($module == '' && $ignoreAuth == true) {
            // Simplest case, return everything.
            return($apiList);
        }

        $apiFinalList = array();

        // Not such an easy case, we need to limit to specific modules and see if we have authentication (or not)
        foreach ($apiList as $apiName => $apiOpts) {
            if ($module == '' || in_array($module, $apiOpts['supportedModules'])) {
                // This matches the module criteria
                if ($ignoreAuth || !$apiOpts['useAuth'] || !$apiOpts['requireAuth']) {
                    // Don't need to worry about authentication
                    $apiFinalList[$apiName] = $apiOpts;
                } else {
                    // We need to worry about authentication
                    $eapmBean = EAPM::getLoginInfo($apiName);
                    if (isset($eapmBean->application)) {
                        // We have authentication
                        $apiFinalList[$apiName] = $apiOpts;
                    }
                }
            }
        }

        return $apiFinalList;
    }

    /**
     * Get the array of API names available for cetain module
     * @param string $moduleName
     * @param bool $ignoreAuth Ignore if we have authentication details or not
     * @param bool $addEmptyEntry Add empty entry?
     * @return array
     */
    public static function getModuleDropDown($moduleName, $ignoreAuth = false, $addEmptyEntry = false)
    {
        global $app_list_strings;

        $apiList = self::listAPI($moduleName, $ignoreAuth);

        $apiDropdown = array();
        if ($addEmptyEntry) {
            $apiDropdown[''] = '';
        }

        foreach ($apiList as $apiName => $ignore) {
            $appStringTranslKey = 'eapm_list_' .strtolower($moduleName);
            if (isset($app_list_strings[$appStringTranslKey]) && !empty($app_list_strings[$appStringTranslKey][$apiName])) {
                $apiDropdown[$apiName] = $app_list_strings[$appStringTranslKey][$apiName];
            } else {
                if (!empty($app_list_strings['eapm_list'][$apiName])) {
                    $apiDropdown[$apiName] = $app_list_strings['eapm_list'][$apiName];
                } else {
                    $apiDropdown[$apiName] = $apiName;
                }
            }
        }

        return $apiDropdown;
    }
}
