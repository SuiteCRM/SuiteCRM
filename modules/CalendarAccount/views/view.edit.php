<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class CalendarAccountViewEdit extends ViewEdit
{

    public function preDisplay()
    {
        parent::preDisplay();

        global $app_list_strings, $current_user, $log;

        if (empty($app_list_strings['calendar_source_types'])) {
            require_once 'include/CalendarSync/CalendarSync.php';
            $app_list_strings['calendar_source_types'] = CalendarSync::getInstance()->getCalendarSourceTypes();
        }

        if (empty($this->bean->calendar_user_id) && !empty($current_user->id)) {
            $this->bean->calendar_user_id = $current_user->id;
            $this->bean->calendar_user_name = $current_user->user_name;
        }

        $newAccount = empty($this->bean->id);
        if ($newAccount && !is_admin($current_user)) {
            require_once 'include/CalendarSync/CalendarSync.php';
            $hasPersonalAccount = CalendarSync::getInstance()->hasPersonalCalendarAccount($current_user->id);

            if ($hasPersonalAccount) {
                $log->info('[CalendarAccountViewEdit][preDisplay] User already has personal account, showing info message');
                SugarApplication::appendErrorMessage(translate('LBL_ALREADY_HAS_PERSONAL_ACCOUNT', 'CalendarAccount'));
            }
        }

        $isAdmin = $current_user->isAdmin() ? 'true' : 'false';
        echo "<script type='text/javascript'>SUGAR.userIsAdmin = $isAdmin;</script>";
    }

}