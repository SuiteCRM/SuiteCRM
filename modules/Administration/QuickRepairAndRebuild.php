<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2019 SalesAgility Ltd.
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

use Api\Core\Config\ApiConfig;

#[\AllowDynamicProperties]
class RepairAndClear
{
    public $module_list;
    public $show_output;
    protected $actions;
    public $execute;
    protected $module_list_from_cache;

    public function repairAndClearAll($selected_actions, $modules, $autoexecute=false, $show_output=true)
    {
        global $mod_strings;
        $this->module_list= $modules;
        $this->show_output = $show_output;
        $this->actions = $selected_actions;
        $this->actions[] = 'repairDatabase';
        $this->execute=$autoexecute;

        //clear vardefs always..
        $this->clearVardefs();
        //first  clear the language cache.
        $this->clearLanguageCache();
        foreach ($this->actions as $current_action) {
            switch ($current_action) {
            case 'repairDatabase':
                if (isset($mod_strings['LBL_ALL_MODULES']) && in_array($mod_strings['LBL_ALL_MODULES'], $this->module_list)) {
                    $this->repairDatabase();
                } else {
                    $this->repairDatabaseSelectModules();
                }
                break;
            case 'rebuildExtensions':
                $this->rebuildExtensions();
                break;
            case 'clearTpls':
                $this->clearTpls();
                break;
            case 'clearJsFiles':
                $this->clearJsFiles();
                break;
            case 'clearDashlets':
                $this->clearDashlets();
                break;
            case 'clearSugarFeedCache':
                $this->clearSugarFeedCache();
                break;
            case 'clearThemeCache':
                $this->clearThemeCache();
                break;
            case 'clearVardefs':
                $this->clearVardefs();
                break;
            case 'clearJsLangFiles':
                $this->clearJsLangFiles();
                break;
            case 'rebuildAuditTables':
                $this->rebuildAuditTables();
                break;
            case 'clearSearchCache':
                $this->clearSearchCache();
                break;
            case 'clearAll':
                $this->clearTpls();
                $this->clearJsFiles();
                $this->clearVardefs();
                $this->clearJsLangFiles();
                $this->clearLanguageCache();
                $this->clearDashlets();
                $this->clearSugarFeedCache();
                $this->clearSmarty();
                $this->clearThemeCache();
                $this->clearXMLfiles();
                $this->clearSearchCache();
                $this->clearExternalAPICache();
                $this->rebuildExtensions();
                $this->rebuildAuditTables();
                $this->repairDatabase();
                break;
        }
        }
    }

    /////////////OLD


    public function repairDatabase()
    {
        global $dictionary, $mod_strings;
        if (false == $this->show_output) {
            $_REQUEST['repair_silent']='1';
        }
        $_REQUEST['execute']=$this->execute;
        $GLOBALS['reload_vardefs'] = true;
        $hideModuleMenu = true;
        include_once('modules/Administration/repairDatabase.php');
    }

    public function repairDatabaseSelectModules()
    {
        global $current_user, $mod_strings, $dictionary;
        set_time_limit(3600);

        include('include/modules.php'); //bug 15661
        $db = DBManagerFactory::getInstance();

        if (is_admin($current_user) || is_admin_for_any_module($current_user)) {
            $export = false;
            if ($this->show_output) {
                echo getClassicModuleTitle($mod_strings['LBL_REPAIR_DATABASE'], array($mod_strings['LBL_REPAIR_DATABASE']), false);
            }
            if ($this->show_output) {
                echo "<h1 id=\"rdloading\">{$mod_strings['LBL_REPAIR_DATABASE_PROCESSING']}</h1>";
                ob_flush();
            }
            $sql = '';
            if (!isset($mod_strings['LBL_ALL_MODULES']) || ($this->module_list && !in_array($mod_strings['LBL_ALL_MODULES'], $this->module_list))) {
                $repair_related_modules = array_keys($dictionary);
                //repair DB
                $dm = inDeveloperMode();
                $GLOBALS['sugar_config']['developerMode'] = true;
                foreach ($this->module_list as $bean_name) {
                    if (isset($beanFiles[$bean_name]) && file_exists($beanFiles[$bean_name])) {
                        require_once($beanFiles[$bean_name]);
                        $GLOBALS['reload_vardefs'] = true;
                        $focus = new $bean_name();
                        #30273
                        if ($focus->disable_vardefs == false) {
                            include('modules/' . $focus->module_dir . '/vardefs.php');


                            if ($this->show_output) {
                                print_r("<p>" .$mod_strings['LBL_REPAIR_DB_FOR'].' '. $bean_name . "</p>");
                            }
                            $sql .= $db->repairTable($focus, $this->execute);
                        }
                    }
                }

                $GLOBALS['sugar_config']['developerMode'] = $dm;

                if ($this->show_output) {
                    echo "<script type=\"text/javascript\">document.getElementById('rdloading').style.display = \"none\";</script>";
                }
                if (isset($sql) && !empty($sql)) {
                    $qry_str = "";
                    foreach (explode("\n", $sql) as $line) {
                        if (!empty($line) && substr($line, -2) != "*/") {
                            $line .= ";";
                        }

                        $qry_str .= $line . "\n";
                    }
                    if ($this->show_output) {
                        echo "<h3>{$mod_strings['LBL_REPAIR_DATABASE_DIFFERENCES']}</h3>";
                        echo "<p>{$mod_strings['LBL_REPAIR_DATABASE_TEXT']}</p>";

                        echo "<form method=\"post\" action=\"index.php?module=Administration&amp;action=repairDatabase\">";
                        echo "<textarea name=\"sql\" rows=\"24\" cols=\"150\" id=\"repairsql\">$qry_str</textarea>";
                        echo "<br /><input type=\"submit\" value=\"".$mod_strings['LBL_REPAIR_DATABASE_EXECUTE']."\" name=\"raction\" /> <input type=\"submit\" name=\"raction\" value=\"".$mod_strings['LBL_REPAIR_DATABASE_EXPORT']."\" />";
                    }
                } else {
                    if ($this->show_output) {
                        echo "<h3>{$mod_strings['LBL_REPAIR_DATABASE_SYNCED']}</h3>";
                    }
                }
            }
        } else {
            sugar_die($GLOBALS['app_strings']['ERR_NOT_ADMIN']);
        }
    }

    public function rebuildExtensions()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo $mod_strings['LBL_QR_REBUILDEXT'];
        }
        global $current_user;
        require_once('ModuleInstall/ModuleInstaller.php');
        $mi = new ModuleInstaller();
        $mi->rebuild_all(!$this->show_output);

        // Remove the "Rebuild Extensions" red text message on admin logins

        if ($this->show_output) {
            echo $mod_strings['LBL_REBUILD_REL_UPD_WARNING'];
        }

        // unset the session variable so it is not picked up in DisplayWarnings.php
        if (isset($_SESSION['rebuild_extensions'])) {
            unset($_SESSION['rebuild_extensions']);
        }
    }

    //Cache Clear Methods
    public function clearSmarty()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARSMARTY']}</h3>";
        }
        $this->_clearCache(sugar_cached('smarty/templates_c'), '.tpl.php');
    }
    public function clearXMLfiles()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_XMLFILES']}</h3>";
        }
        $this->_clearCache(sugar_cached("xml"), '.xml');
    }
    public function clearDashlets()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARDASHLET']}</h3>";
        }
        $this->_clearCache(sugar_cached('dashlets'), '.php');
    }
    public function clearThemeCache()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARTHEMECACHE']}</h3>";
        }
        SugarThemeRegistry::clearAllCaches();
    }
    public function clearSugarFeedCache()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARSUITEFEEDCACHE']}</h3>";
        }

        SugarFeed::flushBackendCache();
    }
    public function clearTpls()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARTEMPLATE']}</h3>";
        }
        if (!empty($this->module_list) && !in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
            foreach ($this->module_list as $module_name_singular) {
                $this->_clearCache(sugar_cached('modules/').$this->_getModuleNamePlural($module_name_singular), '.tpl');
            }
        } else {
            $this->_clearCache(sugar_cached('modules/'), '.tpl');
            $this->_clearCache(sugar_cached('include/InlineEditing/'), '.tpl');
        }
    }
    public function clearVardefs()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARVADEFS']}</h3>";
        }
        if (!empty($this->module_list) && is_array($this->module_list) && !in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
            foreach ($this->module_list as $module_name_singular) {
                $this->_clearCache(sugar_cached('modules/').$this->_getModuleNamePlural($module_name_singular), 'vardefs.php');
            }
        } else {
            $this->_clearCache(sugar_cached('modules/'), 'vardefs.php');
        }
    }
    public function clearJsFiles()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARJS']}</h3>";
        }

        if (!empty($this->module_list) && !in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
            foreach ($this->module_list as $module_name_singular) {
                $this->_clearCache(sugar_cached('modules/').$this->_getModuleNamePlural($module_name_singular), '.js');
            }
        } else {
            $this->_clearCache(sugar_cached('modules/'), '.js');
        }
    }
    public function clearJsLangFiles()
    {
        global $mod_strings;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARJSLANG']}</h3>";
        }
        if (!empty($this->module_list) && !in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
            foreach ($this->module_list as $module_name_singular) {
                $this->_clearCache(sugar_cached('jsLanguage/').$this->_getModuleNamePlural($module_name_singular), '.js');
            }
        } else {
            $this->_clearCache(sugar_cached('jsLanguage'), '.js');
        }
    }
    /**
     * Remove the language cache files from cache/modules/<module>/language
     */
    public function clearLanguageCache()
    {
        global $mod_strings;

        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARLANG']}</h3>";
        }
        //clear cache using the list $module_list_from_cache
        if (!empty($this->module_list) && is_array($this->module_list)) {
            if (in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
                LanguageManager::clearLanguageCache();
            } else { //use the modules selected thrut the select list.
                foreach ($this->module_list as $module_name) {
                    LanguageManager::clearLanguageCache($module_name);
                }
            }
        }
        // Clear app* cache values too
        if (!empty($GLOBALS['sugar_config']['languages'])) {
            $languages = $GLOBALS['sugar_config']['languages'];
        } else {
            $languages = array($GLOBALS['current_language'] => $GLOBALS['current_language']);
        }
        foreach (array_keys($languages) as $language) {
            sugar_cache_clear('app_strings.'.$language);
            sugar_cache_clear('app_list_strings.'.$language);
        }
    }

    /**
     * Remove the cached unified_search_modules.php file
     */
    public function clearSearchCache()
    {
        global $mod_strings, $sugar_config;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEARSEARCH']}</h3>";
        }
        $search_dir=sugar_cached('');
        $src_file = $search_dir . 'modules/unified_search_modules.php';
        if (file_exists($src_file)) {
            unlink($src_file);
        }
    }
    public function clearExternalAPICache()
    {
        global $mod_strings, $sugar_config;
        if ($this->show_output) {
            echo "<h3>{$mod_strings['LBL_QR_CLEAR_EXT_API']}</h3>";
        }
        require_once('include/externalAPI/ExternalAPIFactory.php');
        ExternalAPIFactory::clearCache();
    }

    //////////////////////////////////////////////////////////////
    /////REPAIR AUDIT TABLES
    public function rebuildAuditTables()
    {
        global $mod_strings;
        include('include/modules.php');	//bug 15661
        if ($this->show_output) {
            echo "<h3> {$mod_strings['LBL_QR_REBUILDAUDIT']}</h3>";
        }

        if (!empty($this->module_list) && !in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
            foreach ($this->module_list as $bean_name) {
                if (isset($beanFiles[$bean_name]) && file_exists($beanFiles[$bean_name])) {
                    require_once($beanFiles[$bean_name]);
                    $this->_rebuildAuditTablesHelper(new $bean_name());
                }
            }
        } else {
            if (in_array(translate('LBL_ALL_MODULES'), $this->module_list)) {
                foreach ($beanFiles as $bean => $file) {
                    if (file_exists($file)) {
                        require_once($file);
                        $this->_rebuildAuditTablesHelper(new $bean());
                    }
                }
            }
        }
        if ($this->show_output) {
            echo $mod_strings['LBL_DONE'];
        }
    }

    private function _rebuildAuditTablesHelper($focus)
    {
        global $mod_strings;

        // skip if not a SugarBean object
        if (!($focus instanceof SugarBean)) {
            return;
        }

        if ($focus->is_AuditEnabled()) {
            if (!$focus->db->tableExists($focus->get_audit_table_name())) {
                if ($this->show_output) {
                    echo $mod_strings['LBL_QR_CREATING_TABLE']." ".$focus->get_audit_table_name().' '.$mod_strings['LBL_FOR'].' '. $focus->object_name.'.<br/>';
                }
                $focus->create_audit_table();
            } else {
                if ($this->show_output) {
                    $echo=str_replace('%1$', $focus->object_name, (string) $mod_strings['LBL_REBUILD_AUDIT_SKIP']);
                    echo $echo;
                }
            }
        } else {
            if ($this->show_output) {
                echo $focus->object_name.$mod_strings['LBL_QR_NOT_AUDIT_ENABLED'];
            }
        }
    }

    ///////////////////////////////////////////////////////////////
    ////END REPAIR AUDIT TABLES

    ///////////////////////////////////////////////////////////////
    //// Recursively unlink all files of the given $extension in the given $thedir.
    //
    private function _clearCache($thedir, $extension)
    {
        if (!file_exists($thedir)) {
            LoggerManager::getLogger()->warn('QRR: directory not found: ' . $thedir);
            return false;
        }

        if (!is_dir($thedir)) {
            LoggerManager::getLogger()->warn('QRR: it is not a directory: ' . $thedir);
            return false;
        }


        if ($current = opendir($thedir)) {
            while (false !== ($children = readdir($current))) {
                if ($children != "." && $children != "..") {
                    if (is_dir($thedir . "/" . $children)) {
                        $this->_clearCache($thedir . "/" . $children, $extension);
                    } elseif (is_file($thedir . "/" . $children) && (substr_count($children, (string) $extension))) {
                        unlink($thedir . "/" . $children);
                    }
                }
            }
        }

        return true;
    }

    /////////////////////////////////////////////////////////////
    ////////
    private function _getModuleNamePlural($module_name_singular)
    {
        global $beanList;
        while ($curr_module = current($beanList)) {
            if ($curr_module == $module_name_singular) {
                return key($beanList);
            } //name of the module, plural.
            next($beanList);
        }
    }
}
