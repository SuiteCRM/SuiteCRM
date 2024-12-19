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

class ContactsUtils
{
    /**
     * This function is used mainly by the Scheduler to calculate daily the age of all the contacts in the system.
     * We place it in this file in order to be accessed by the controller as well.
     */
    public static function calculateContactsAge() {
        global $db;
        
        $updateContactsAge = "UPDATE
            contacts,
            contacts_cstm
        SET
            stic_age_c = IF(birthdate is not NULL,
            YEAR(CURDATE())-YEAR(birthdate)+ IF(DATE_FORMAT(CURDATE(), '%m-%d') >= DATE_FORMAT(birthdate, '%m-%d'), 0, -1), NULL)
        WHERE
            id = id_c";

        $resultUpdateContactsAge = $db->query($updateContactsAge); 
        if(!$resultUpdateContactsAge){
            $GLOBALS['log']->fatal("Error query: Scheduler function - calculateContactsAge - updateContactsAge");
            return false;
        }
        
        return true;
    }

    /**
     * Get the current age from the date of birth
     *
     * @param [string] $birthday (YYYY-MM-DD)
     * @return int
     */
    public static function getAge($birthday)
    {
        if (empty($birthday)) {
            $year_dif = '';
        } else {
            list($year, $month, $day) = explode("-", $birthday);
            $year_dif = date("Y") - $year;
            $month_dif = date("m") - $month;
            $day_dif = date("d") - $day;

            if (($day_dif < 0 && $month_dif == 0) || $month_dif < 0) {
                $year_dif--;
            }
        }

        return $year_dif;
    }

    /**
     * Automatic return of the mail: When the field Reason return mail takes the values "wrong_address, unknown, rejected"
     * will automatically generate a Call associated with the contact, to address the reason for the return of the mail.
     * The reason for the return (field value) will be indicated in the subject of the call.
     *
     * @param Object $contactBean
     * @return void
     */
    public static function generateCallFromReturnMailReason($contactBean)
    {
        $reasons = array('wrong_address', 'unknown', 'rejected');
        if (!empty($contactBean->stic_postal_mail_return_reason_c) && in_array($contactBean->stic_postal_mail_return_reason_c, $reasons)) {
            global $current_user, $timedate, $app_strings;

            // Create the new call
            $callBean = BeanFactory::getBean('Calls');
            $callDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));
            $callBean->date_start = $timedate->asUser($callDate, $current_user);
            $callBean->name = $app_strings['LBL_STIC_MAIL_RETURN_REASON'] . ": " . $GLOBALS['app_list_strings']['stic_postal_mail_return_reasons_list'][$contactBean->stic_postal_mail_return_reason_c];
            $callBean->assigned_user_id = (empty($contactBean->assigned_user_id) ? $current_user->id : $contactBean->assigned_user_id);
            $callBean->direction = 'Outbound';
            $callBean->status = 'Planned';
            $callBean->parent_type = 'Contacts';
            $callBean->parent_id = $contactBean->id;

            // STIC NOTE - $_REQUEST variable is not the same when saving a record from EDIT_VIEW as from INLINE_EDIT.
            // In order to generate the relationship between the contact and the call, it is necessary to enter the following properties ​​in $_REQUEST
            if ($_REQUEST['action'] == 'saveHTMLField') {
                $_REQUEST['relate_to'] = 'Contacts';
                $_REQUEST['relate_id'] = $contactBean->id;
            }

            $callBean->save();
        }
    }
}
