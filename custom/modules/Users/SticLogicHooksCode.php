<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

class UsersLogicHooks
{
    public function before_save(&$bean, $event, $arguments)
    {
        // Let's check ut property (1 for existing users, other for new ones). This property launches the user wizzard on first login.
        if ($bean->object_name == 'User') // We check that a user is being saved and not another object belonging to a user's configuration such as the user signature
        {
            $ut = $bean->getPreference('ut');
            if ($ut != "1") {
                // Activate 'count_collapsed_subpanels' and set 'monday' as 'first day of the week' on new users.
                // The options are sent to the Users controller using the $_POST array. It is not possible to do it through
                // setPreferences function because they will be overriden by the default global preferences set by the wizard.
                $_POST['user_count_collapsed_subpanels'] = 'on';
                $_POST['fdow'] = 1;
            }   
        }
    }

    public function after_login($event, $arguments)
    {
        // Create cookie with SinergiaCRM version number if not exists
        if (!isset($_COOKIE['SticVersion'])) {
            global $sugar_config;

            $_COOKIE['SticVersion'] = $sugar_config['sinergiacrm_version'];
            setcookie('SticVersion', $_COOKIE['SticVersion'], ['expires' => time() + 999999999, 'path' => '/']);
        }
    }
}
