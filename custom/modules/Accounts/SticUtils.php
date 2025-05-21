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

require_once 'modules/Accounts/Account.php';

/**
 * Extend the Account class to add custom logic.
 * Due to the complexity and importance of the Account class and because it does not follow the standard SugarCRM structure of the SugarBean extension, this class is not used directly in the normal operation of SinergiaCRM
 */
class AccountsUtils
{
    /**
     * When the field Reason return mail takes the values "wrong_address, unknown, rejected"
     * will automatically generate a Call associated with the account, to address the reason for the return of the mail.
     * The reason for the return (field value) will be indicated in the subject of the call.
     *
     * @param Object $accountBean
     * @return void
     */
    public static function generateCallFromReturnMailReason($accountBean)
    {
        global $app_strings;
        $reasons = array('wrong_address', 'unknown', 'rejected');

        if (isset($accountBean->stic_postal_mail_return_reason_c) && in_array($accountBean->stic_postal_mail_return_reason_c, $reasons)) {
            global $current_user, $timedate, $app_strings;

            // Create the new call
            $callBean = BeanFactory::getBean('Calls');
            $callDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));
            $callBean->date_start = $timedate->asUser($callDate, $current_user);
            $callBean->name = $app_strings['LBL_STIC_MAIL_RETURN_REASON'] . ": " . $GLOBALS['app_list_strings']['stic_postal_mail_return_reasons_list'][$accountBean->stic_postal_mail_return_reason_c];
            $callBean->assigned_user_id = (empty($accountBean->assigned_user_id) ? $current_user->id : $accountBean->assigned_user_id);
            $callBean->direction = 'Outbound';
            $callBean->status = 'Planned';
            $callBean->parent_type = 'Accounts';
            $callBean->parent_id = $accountBean->id;
            $callBean->save();
        }
    }
}
