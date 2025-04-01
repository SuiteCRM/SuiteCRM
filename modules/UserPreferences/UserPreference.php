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

/*********************************************************************************

 * Description: Handles the User Preferences and stores them in a separate table.
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

#[\AllowDynamicProperties]
class UserPreference extends SugarBean
{
    public $db;
    public $field_name_map;

    // Stored fields
    public $id;
    public $date_entered;
    public $date_modified;
    public $assigned_user_id;
    public $assigned_user_name;
    public $name;
    public $category;
    public $contents;
    public $deleted;

    public $object_name = 'UserPreference';
    public $table_name = 'user_preferences';

    public $disable_row_level_security = true;
    public $module_dir = 'UserPreferences';
    public $field_defs = array();
    public $field_defs_map = array();
    public $new_schema = true;

    protected $_userFocus;

    // Do not actually declare, use the functions statically
    public function __construct(
        User $user = null
        ) {
        parent::__construct();

        $this->_userFocus = $user;
        $this->tracker_visibility = false;
    }

    /**
     * Get preference by name and category. Lazy loads preferences from the database per category
     *
     * @param string $name name of the preference to retreive
     * @param string $category name of the category to retreive, defaults to global scope
     * @return mixed the value of the preference (string, array, int etc)
     */
    public function getPreference(
        $name,
        $category = 'global'
        ) {
        global $sugar_config, $current_user;

        $user = $this->_userFocus;

        if ($user->user_name !== $current_user->user_name){
            $this->loadPreferences($category);
            return $user->user_preferences[$category][$name] ?? $this->getDefaultPreference($name, $category);
        }

        // if the unique key in session doesn't match the app or prefereces are empty
        if (!isset($_SESSION[$user->user_name.'_PREFERENCES'][$category]) || (!empty($_SESSION['unique_key']) && $_SESSION['unique_key'] != $sugar_config['unique_key'])) {
            $this->loadPreferences($category);
        }
        if (isset($_SESSION[$user->user_name.'_PREFERENCES'][$category][$name])) {
            return $_SESSION[$user->user_name.'_PREFERENCES'][$category][$name];
        }

        // check to see if a default preference ( i.e. $sugar_config setting ) exists for this value )
        // if so, return it
        $value = $this->getDefaultPreference($name, $category);
        if (!is_null($value)) {
            return $value;
        }
        return null;
    }

    /**
     * Get preference by name and category from the system settings.
     *
     * @param string $name name of the preference to retreive
     * @param string $category name of the category to retreive, defaults to global scope
     * @return mixed the value of the preference (string, array, int etc)
     */
    public function getDefaultPreference(
        $name,
        $category = 'global'
        ) {
        global $sugar_config;

        // Doesn't support any prefs but global ones
        if ($category != 'global') {
            return null;
        }

        // First check for name matching $sugar_config variable
        if (isset($sugar_config[$name])) {
            return $sugar_config[$name];
        }

        // Next, check to see if it's one of the common problem ones
        if (isset($sugar_config['default_'.$name])) {
            return $sugar_config['default_'.$name];
        }
        if ($name == 'datef') {
            return $sugar_config['default_date_format'];
        }
        if ($name == 'timef') {
            return $sugar_config['default_time_format'];
        }
        if ($name == 'email_link_type') {
            return $sugar_config['email_default_client'];
        }
    }

    /**
     * Set preference by name and category. Saving will be done in utils.php -> sugar_cleanup
     *
     * @param string $name name of the preference to retreive
     * @param mixed $value value of the preference to set
     * @param string $category name of the category to retreive, defaults to global scope
     */
    public function setPreference(
        $name,
        $value,
        $category = 'global'
        ) {
        $user = $this->_userFocus;

        if (empty($user->user_name)) {
            return;
        }

        if (!isset($_SESSION[$user->user_name.'_PREFERENCES'][$category])) {
            if (!$user->loadPreferences($category)) {
                $_SESSION[$user->user_name.'_PREFERENCES'][$category] = array();
            }
        }

        // preferences changed or a new preference, save it to DB
        if (!isset($_SESSION[$user->user_name.'_PREFERENCES'][$category][$name])
            || (isset($_SESSION[$user->user_name.'_PREFERENCES'][$category][$name]) && $_SESSION[$user->user_name.'_PREFERENCES'][$category][$name] != $value)) {
            $GLOBALS['savePreferencesToDB'] = true;
            if (!isset($GLOBALS['savePreferencesToDBCats'])) {
                $GLOBALS['savePreferencesToDBCats'] = array();
            }
            $GLOBALS['savePreferencesToDBCats'][$category] = true;
        }

        $_SESSION[$user->user_name.'_PREFERENCES'][$category][$name] = $value;
    }

    /**
     * Loads preference by category from database. Saving will be done in utils.php -> sugar_cleanup
     *
     * @param string $category name of the category to retreive, defaults to global scope
     * @return bool successful?
     */
    public function loadPreferences(
        $category = 'global'
        ) {
        global $sugar_config, $current_user;

        $user = $this->_userFocus;

        if ($user->object_name != 'User') {
            return;
        }

        if ($user->user_name !== $current_user->user_name){
            return $this->reloadPreferences($category);
        }

        if (!empty($user->id) && (!isset($_SESSION[$user->user_name . '_PREFERENCES'][$category]) || (!empty($_SESSION['unique_key']) && $_SESSION['unique_key'] != $sugar_config['unique_key']))) {
            // cn: moving this to only log when valid - throwing errors on install
            return $this->reloadPreferences($category);
        }
        return false;
    }

    /**
     * Unconditionally reloads user preferences from the DB and updates the session
     * @param string $category name of the category to retreive, defaults to global scope
     * @return bool successful?
     */
    public function reloadPreferences($category = 'global')
    {
        $user = $this->_userFocus;

        if ($user->object_name != 'User' || empty($user->id) || empty($user->user_name)) {
            return false;
        }
        $GLOBALS['log']->debug('Loading Preferences DB ' . $user->user_name);
        if ($GLOBALS['current_user']->user_name === $user->user_name){
            if (!isset($_SESSION[$user->user_name . '_PREFERENCES'])) {
                $_SESSION[$user->user_name . '_PREFERENCES'] = array();
            }
        }
        if (!isset($user->user_preferences) || !is_array($user->user_preferences)) {
            $user->user_preferences = array();
        }
        $db = DBManagerFactory::getInstance();
        $result = $db->query("SELECT contents FROM user_preferences WHERE assigned_user_id='$user->id' AND category = '" . $category . "' AND deleted = 0", false, 'Failed to load user preferences');
        $row = $db->fetchByAssoc($result);
        if ($row) {
            if ($GLOBALS['current_user']->user_name === $user->user_name){
                $_SESSION[$user->user_name . '_PREFERENCES'][$category] = unserialize(base64_decode($row['contents']));
            }
            $user->user_preferences[$category] = unserialize(base64_decode($row['contents']));
            return true;
        } else {
            if ($GLOBALS['current_user']->user_name === $user->user_name){
                $_SESSION[$user->user_name . '_PREFERENCES'][$category] = array();
            }
            $user->user_preferences[$category] = array();
        }
        return false;
    }

    /**
     * Loads users timedate preferences
     *
     * @return array 'date' - date format for user ; 'time' - time format for user
     */
    public function getUserDateTimePreferences()
    {
        global $sugar_config, $db, $timedate, $current_user;

        $user = $this->_userFocus;

        $prefDate = array();

        if (!empty($user) && $this->loadPreferences('global')) {
            // forced to set this to a variable to compare b/c empty() wasn't working
            $timeZone = TimeDate::userTimezone($user);
            $timeFormat = $user->getPreference("timef");
            $dateFormat = $user->getPreference("datef");

            // cn: bug xxxx cron.php fails because of missing preference when admin hasn't logged in yet
            $timeZone = empty($timeZone) ? 'America/Los_Angeles' : $timeZone;

            if (empty($timeFormat)) {
                $timeFormat = $sugar_config['default_time_format'];
            }
            if (empty($dateFormat)) {
                $dateFormat = $sugar_config['default_date_format'];
            }

            $prefDate['date'] = $dateFormat;
            $prefDate['time'] = $timeFormat;
            $prefDate['userGmt'] = TimeDate::tzName($timeZone);
            $prefDate['userGmtOffset'] = $timedate->getUserUTCOffset($user);

            return $prefDate;
        } else {
            $prefDate['date'] = $timedate->get_date_format();
            $prefDate['time'] = $timedate->get_time_format();

            if (!empty($user) && $user->object_name == 'User') {
                $timeZone = TimeDate::userTimezone($user);
                // cn: bug 9171 - if user has no time zone, cron.php fails for InboundEmail
                if (!empty($timeZone)) {
                    $prefDate['userGmt'] = TimeDate::tzName($timeZone);
                    $prefDate['userGmtOffset'] = $timedate->getUserUTCOffset($user);
                }
            } else {
                $timeZone = TimeDate::userTimezone($current_user);
                if (!empty($timeZone)) {
                    $prefDate['userGmt'] = TimeDate::tzName($timeZone);
                    $prefDate['userGmtOffset'] = $timedate->getUserUTCOffset($current_user);
                }
            }

            return $prefDate;
        }
    }

    /**
     * Saves all preferences into the database that are in the session. Expensive, this is called by default in
     * sugar_cleanup if a setPreference has been called during one round trip.
     *
     * @global user will use current_user if no user specificed in $user param
     * @param user $user User object to retrieve, otherwise user current_user
     * @param bool $all save all of the preferences? (Dangerous)
     *
     */
    public function savePreferencesToDB(
        $all = false
        ) {
        global $sugar_config;
        $GLOBALS['savePreferencesToDB'] = false;

        $user = $this->_userFocus;

        // these are not the preferences you are looking for [ hand waving ]
        if (empty($GLOBALS['installing']) && !empty($_SESSION['unique_key']) && $_SESSION['unique_key'] != $sugar_config['unique_key']) {
            return;
        }

        $GLOBALS['log']->debug('Saving Preferences to DB ' . $user->user_name);
        if (isset($_SESSION[$user->user_name. '_PREFERENCES']) && is_array($_SESSION[$user->user_name. '_PREFERENCES'])) {
            $GLOBALS['log']->debug("Saving Preferences to DB: {$user->user_name}");
            // only save the categories that have been modified or all?
            if (!$all && isset($GLOBALS['savePreferencesToDBCats']) && is_array($GLOBALS['savePreferencesToDBCats'])) {
                $catsToSave = array();
                foreach ($GLOBALS['savePreferencesToDBCats'] as $category => $value) {
                    if (isset($_SESSION[$user->user_name. '_PREFERENCES'][$category])) {
                        $catsToSave[$category] = $_SESSION[$user->user_name. '_PREFERENCES'][$category];
                    }
                }
            } else {
                $catsToSave = $_SESSION[$user->user_name. '_PREFERENCES'];
            }

            foreach ($catsToSave as $category => $contents) {
                $focus = new UserPreference($this->_userFocus);
                $result = $focus->retrieve_by_string_fields(array(
                    'assigned_user_id' => $user->id,
                    'category' => $category,
                    ));
                $focus->assigned_user_id = $user->id; // MFH Bug #13862
                $focus->deleted = 0;
                $focus->contents = base64_encode(serialize($contents));
                $focus->category = $category;
                $focus->save();
            }
        }
    }

    /**
     * Resets preferences for a particular user. If $category is null all user preferences will be reset
     *
     * @param string $category category to reset
     */
    public function resetPreferences(
        $category = null
        ) {
        $user = $this->_userFocus;

        $GLOBALS['log']->debug('Reseting Preferences for user ' . $user->user_name);

        $remove_tabs = $this->getPreference('remove_tabs');
        $favorite_reports = $this->getPreference('favorites', 'Reports');
        $home_pages = $this->getPreference('pages', 'home');
        $home_dashlets = $this->getPreference('dashlets', 'home');
        $ut = $this->getPreference('ut');
        $timezone = $this->getPreference('timezone');

        $query = "UPDATE user_preferences SET deleted = 1 WHERE assigned_user_id = '" . $user->id . "'";
        if ($category) {
            $query .= " AND category = '" . $category . "'";
        }
        $this->db->query($query);


        if ($category) {
            unset($_SESSION[$user->user_name."_PREFERENCES"][$category]);
        } else {
            if (!empty($_COOKIE['sugar_user_theme']) && !headers_sent()) {
                SugarApplication::setCookie('sugar_user_theme', '', time() - 3600, null, null, isSSL(), true); // expire the sugar_user_theme cookie
            }
            unset($_SESSION[$user->user_name."_PREFERENCES"]);
            if ($user->id == $GLOBALS['current_user']->id) {
                session_destroy();
            }
            $this->setPreference('remove_tabs', $remove_tabs);
            $this->setPreference('favorites', $favorite_reports, 'Reports');
            $this->setPreference('pages', $home_pages, 'home');
            $this->setPreference('dashlets', $home_dashlets, 'home');
            $this->setPreference('ut', $ut);
            $this->setPreference('timezone', $timezone);
            $this->savePreferencesToDB();
        }
    }

    /**
     * Updates every user pref with a new key value supports 2 levels deep, use append to
     * array if you want to append the value to an array
     */
    public static function updateAllUserPrefs(
        $key,
        $new_value,
        $sub_key = '',
        $is_value_array = false,
        $unset_value = false
    ) {
        global $current_user, $db;

        // Admin-only function; die if calling as a non-admin
        if (!is_admin($current_user)) {
            sugar_die('only admins may call this function');
        }

        // we can skip this if we've already upgraded to the user_preferences format.
        if (!array_key_exists('user_preferences', $db->getHelper()->get_columns('users'))) {
            return;
        }

        $result = $db->query("SELECT id, user_preferences, user_name FROM users");
        while ($row = $db->fetchByAssoc($result)) {
            $prefs = array();
            $newprefs = array();

            $prefs = unserialize(base64_decode($row['user_preferences']));

            if (!empty($sub_key)) {
                if ($is_value_array) {
                    if (!isset($prefs[$key][$sub_key])) {
                        continue;
                    }

                    if (empty($prefs[$key][$sub_key])) {
                        $prefs[$key][$sub_key] = array();
                    }
                    $already_exists = false;
                    foreach ($prefs[$key][$sub_key] as $k=>$value) {
                        if ($value == $new_value) {
                            $already_exists = true;
                            if ($unset_value) {
                                unset($prefs[$key][$sub_key][$k]);
                            }
                        }
                    }
                    if (!$already_exists && !$unset_value) {
                        $prefs[$key][$sub_key][] = $new_value;
                    }
                } else {
                    if (!$unset_value) {
                        $prefs[$key][$sub_key] = $new_value;
                    }
                }
            } else {
                if ($is_value_array) {
                    if (!isset($prefs[$key])) {
                        continue;
                    }

                    if (empty($prefs[$key])) {
                        $prefs[$key] = array();
                    }
                    $already_exists = false;
                    foreach ($prefs[$key] as $k=>$value) {
                        if ($value == $new_value) {
                            $already_exists = true;

                            if ($unset_value) {
                                unset($prefs[$key][$k]);
                            }
                        }
                    }
                    if (!$already_exists && !$unset_value) {
                        $prefs[$key][] = $new_value;
                    }
                } else {
                    if (!$unset_value) {
                        $prefs[$key] = $new_value;
                    }
                }
            }

            $newstr = $db->quote(base64_encode(serialize($prefs)));
            $db->query("UPDATE users SET user_preferences = '{$newstr}' WHERE id = '{$row['id']}'");
        }

        unset($prefs);
        unset($newprefs);
        unset($newstr);
    }
}
