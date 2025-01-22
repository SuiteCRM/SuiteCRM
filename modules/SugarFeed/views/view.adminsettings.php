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
class ViewAdminsettings extends SugarView
{
    /**
     * @see SugarView::_getModuleTab()
     */
    protected function _getModuleTab()
    {
        return 'Administration';
    }

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        return array(
            "<a href='index.php?module=Administration&action=index'>" . translate('LBL_MODULE_NAME', 'Administration') . "</a>",
            $mod_strings['LBL_MODULE_NAME'],
        );
    }

    /**
     * @see SugarView::display()
     */
    public function display()
    {
        global $mod_strings, $app_strings;

        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        // Handle posts
        if (!empty($_REQUEST['process'])) {
            // Check the cleanup logic hook, make sure it is still there
            check_logic_hook_file('Users', 'after_login', array(1, 'SugarFeed old feed entry remover', 'modules/SugarFeed/SugarFeedFlush.php', 'SugarFeedFlush', 'flushStaleEntries'));

            // We have data posted
            if ($_REQUEST['process'] == 'true') {
                // They want us to process it, the false will just fall outside of this statement
                if ($_REQUEST['feed_enable'] == '1') {
                    // The feed is enabled, pay attention to what categories should be enabled or disabled

                    $db = DBManagerFactory::getInstance();
                    $ret = $db->query("SELECT * FROM config WHERE category = 'sugarfeed' AND name LIKE 'module_%'");
                    $current_modules = array();
                    while ($row = $db->fetchByAssoc($ret)) {
                        $current_modules[$row['name']] = $row['value'];
                    }

                    $active_modules = $_REQUEST['modules'];
                    if (!is_array($active_modules)) {
                        $active_modules = array();
                    }

                    foreach ($active_modules as $name => $is_active) {
                        $module = substr($name, 7);

                        if ($is_active == '1') {
                            // They are activating something that was disabled before
                            SugarFeed::activateModuleFeed($module);
                        } else {
                            // They are disabling something that was active before
                            SugarFeed::disableModuleFeed($module);
                        }
                    }

                    $admin->saveSetting('sugarfeed', 'enabled', '1');
                } else {
                    $admin->saveSetting('sugarfeed', 'enabled', '0');
                    // Now we need to remove all of the logic hooks, so they don't continue to run
                    // We also need to leave the database alone, so they can enable/disable modules with the system disabled
                    $modulesWithFeeds = SugarFeed::getAllFeedModules();

                    foreach ($modulesWithFeeds as $currFeedModule) {
                        SugarFeed::disableModuleFeed($currFeedModule, false);
                    }
                }

                $admin->retrieveSettings(false, true);
                SugarFeed::flushBackendCache();
            } else {
                if ($_REQUEST['process'] == 'deleteRecords') {
                    if (!isset($db)) {
                        $db = DBManagerFactory::getInstance();
                    }
                    $db->query("UPDATE sugarfeed SET deleted = '1'");
                    echo(translate('LBL_RECORDS_DELETED', 'SugarFeed'));
                }
            }


            if ($_REQUEST['process'] == 'true' || $_REQUEST['process'] == 'false') {
                header('Location: index.php?module=Administration&action=index');
                return;
            }
        }

        $sugar_smarty = new Sugar_Smarty();
        $sugar_smarty->assign('mod', $mod_strings);
        $sugar_smarty->assign('app', $app_strings);

        if (isset($admin->settings['sugarfeed_enabled']) && $admin->settings['sugarfeed_enabled'] == '1') {
            $sugar_smarty->assign('enabled_checkbox', 'checked');
        }

        $possible_feeds = SugarFeed::getAllFeedModules();
        $possible_feeds['facebook'] = 'Facebook';
        $possible_feeds['twitter'] = 'Twitter';
        $module_list = array();
        $userFeedEnabled = 0;
        foreach ($possible_feeds as $module) {
            $currModule = array();
            if (isset($admin->settings['sugarfeed_module_' . $module]) && $admin->settings['sugarfeed_module_' . $module] == '1') {
                $currModule['enabled'] = 1;
            } else {
                $currModule['enabled'] = 0;
            }

            $currModule['module'] = $module;
            if ($module == 'UserFeed') {
                // Fake module, need to handle specially
                $userFeedEnabled = $currModule['enabled'];
                continue;
            } elseif ($module == 'Facebook' || $module == 'Twitter') {
                $currModule['label'] = $module;
            } else {
                $currModule['label'] = $GLOBALS['app_list_strings']['moduleList'][$module];
            }

            $module_list[] = $currModule;
        }
        $sugar_smarty->assign('module_list', $module_list);
        $sugar_smarty->assign('user_feed_enabled', $userFeedEnabled);

        echo getClassicModuleTitle(
            "Administration",
            array(
                "<a href='index.php?module=Administration&action=index'>" . translate('LBL_MODULE_NAME', 'Administration') . "</a>",
                $mod_strings['LBL_MODULE_NAME'],
            ),
            false
        );
        $sugar_smarty->display('modules/SugarFeed/tpls/AdminSettings.tpl');
    }
}
