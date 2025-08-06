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
class TabController
{
    public $required_modules = array('Home');

    /**
     * @var bool flag of validation of the cache
     */
    protected static $isCacheValid = false;

    public function is_system_tabs_in_db()
    {
        $administration = BeanFactory::newBean('Administration');
        $administration->retrieveSettings('MySettings');
        if (isset($administration->settings) && isset($administration->settings['MySettings_tab'])) {
            return true;
        } else {
            return false;
        }
    }

    public function get_system_tabs()
    {
        global $moduleList;
    
        static $system_tabs_result = null;
    
        // if the value is not already cached, then retrieve it.
        if (empty($system_tabs_result) || !self::$isCacheValid) {
            $administration = BeanFactory::newBean('Administration');
            $administration->retrieveSettings('MySettings');
            if (isset($administration->settings) && isset($administration->settings['MySettings_tab'])) {
                $tabs= $administration->settings['MySettings_tab'];
                $trimmed_tabs = trim($tabs);
                //make sure serialized string is not empty
                if (!empty($trimmed_tabs)) {
                    $tabs = unserialize(base64_decode($tabs), ['allowed_classes' => false]);
                    //Ensure modules saved in the prefences exist.
                    foreach ($tabs as $id => $tab) {
                        if (!in_array($tab, $moduleList)) {
                            unset($tabs[$id]);
                        }
                    }
                    ACLController::filterModuleList($tabs);
                    $tabs = self::get_key_array($tabs);
                    $system_tabs_result = $tabs;
                } else {
                    $system_tabs_result = self::get_key_array($moduleList);
                }
            } else {
                $system_tabs_result = self::get_key_array($moduleList);
            }
            self::$isCacheValid = true;
        }
        
        return $system_tabs_result;
    }

    public function get_tabs_system()
    {
        global $moduleList;
        $tabs = $this->get_system_tabs();
        $unsetTabs = self::get_key_array($moduleList);
        foreach ($tabs as $tab) {
            unset($unsetTabs[$tab]);
        }
    
        $should_hide_iframes = !file_exists('modules/iFrames/iFrame.php');
        if ($should_hide_iframes) {
            if (isset($unsetTabs['iFrames'])) {
                unset($unsetTabs['iFrames']);
            } else {
                if (isset($tabs['iFrames'])) {
                    unset($tabs['iFrames']);
                }
            }
        }

        return array($tabs,$unsetTabs);
    }




    public function set_system_tabs($tabs)
    {
        $administration = BeanFactory::newBean('Administration');
        $serialized = base64_encode(serialize($tabs));
        $administration->saveSetting('MySettings', 'tab', $serialized);
        self::$isCacheValid = false;
    }

    public function get_users_can_edit()
    {
        $administration = BeanFactory::newBean('Administration');
        $administration->retrieveSettings('MySettings');
        if (isset($administration->settings) && isset($administration->settings['MySettings_disable_useredit'])) {
            if ($administration->settings['MySettings_disable_useredit'] == 'yes') {
                return false;
            }
        }
        return true;
    }

    public function set_users_can_edit($boolean)
    {
        global $current_user;
        if (is_admin($current_user)) {
            $administration = BeanFactory::newBean('Administration');
            if ($boolean) {
                $administration->saveSetting('MySettings', 'disable_useredit', 'no');
            } else {
                $administration->saveSetting('MySettings', 'disable_useredit', 'yes');
            }
        }
    }


    public static function get_key_array($arr)
    {
        $new = array();
        if (!empty($arr)) {
            foreach ($arr as $val) {
                $new[$val] = $val;
            }
        }
        return $new;
    }

    public function set_user_tabs($tabs, &$user, $type='display')
    {
        if (empty($user)) {
            global $current_user;
            $current_user->setPreference($type .'_tabs', $tabs);
        } else {
            $user->setPreference($type .'_tabs', $tabs);
        }
    }

    public function get_user_tabs(&$user, $type='display')
    {
        $system_tabs = $this->get_system_tabs();
        $tabs = $user->getPreference($type .'_tabs');
        if (!empty($tabs)) {
            $tabs = self::get_key_array($tabs);
            if ($type == 'display') {
                $tabs['Home'] =  'Home';
            }
            return $tabs;
        } else {
            if ($type == 'display') {
                return $system_tabs;
            } else {
                return array();
            }
        }
    }

    public function get_unset_tabs($user)
    {
        global $moduleList;
        $tabs = $this->get_user_tabs($user);
        $unsetTabs = self::get_key_array($moduleList);
        foreach ($tabs as $tab) {
            unset($unsetTabs[$tab]);
        }
        return $unsetTabs;
    }

    public function get_old_user_tabs($user)
    {
        $system_tabs = $this->get_system_tabs();
    
        $tabs = $user->getPreference('tabs');
    
        if (!empty($tabs)) {
            $tabs = self::get_key_array($tabs);
            $tabs['Home'] =  'Home';
            foreach ($tabs as $tab) {
                if (!isset($system_tabs[$tab])) {
                    unset($tabs[$tab]);
                }
            }
            return $tabs;
        } else {
            return $system_tabs;
        }
    }

    public function get_old_tabs($user)
    {
        global $moduleList;
        $tabs = $this->get_old_user_tabs($user);
        $system_tabs = $this->get_system_tabs();
        foreach ($tabs as $tab) {
            unset($system_tabs[$tab]);
        }
    
        return array($tabs,$system_tabs);
    }

    public function get_tabs($user)
    {
        $display_tabs = $this->get_user_tabs($user, 'display');
        $hide_tabs = $this->get_user_tabs($user, 'hide');
        $remove_tabs = $this->get_user_tabs($user, 'remove');
        $system_tabs = $this->get_system_tabs();
    
        // remove access to tabs that roles do not give them permission to

        foreach ($system_tabs as $key=>$value) {
            if (!isset($display_tabs[$key])) {
                $display_tabs[$key] = $value;
            }
        }

        // removes tabs from display_tabs if not existant in roles
        // or exist in the hidden tabs
        foreach ($display_tabs as $key=>$value) {
            if (!isset($system_tabs[$key])) {
                unset($display_tabs[$key]);
            }
            if (isset($hide_tabs[$key])) {
                unset($display_tabs[$key]);
            }
        }

        // removes tabs from hide_tabs if not existant in roles
        foreach ($hide_tabs as $key=>$value) {
            if (!isset($system_tabs[$key])) {
                unset($hide_tabs[$key]);
            }
        }
        
        // remove tabs from user if admin has removed specific tabs
        foreach ($remove_tabs as $key=>$value) {
            if (isset($display_tabs[$key])) {
                unset($display_tabs[$key]);
            }
            if (isset($hide_tabs[$key])) {
                unset($hide_tabs[$key]);
            }
        }

        return array($display_tabs, $hide_tabs, $remove_tabs);
    }

    public function restore_tabs($user)
    {
        global $moduleList;
        $this->set_user_tabs($moduleList, $user);
    }

    public function restore_system_tabs()
    {
        global $moduleList;
        $this->set_system_tabs($moduleList);
    }
}
