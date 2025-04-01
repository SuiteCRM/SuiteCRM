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

/**
 * Reminder_Invitee class
 *
 */
#[\AllowDynamicProperties]
class Reminder_Invitee extends Basic
{
    public $name;

    public $new_schema = true;
    public $module_dir = 'Reminders_Invitees';
    public $object_name = 'Reminder_Invitee';
    public $table_name = 'reminders_invitees';
    public $tracker_visibility = false;
    public $importable = false;
    public $disable_row_level_security = true;

    public $reminder_id;
    public $related_invitee_module;
    public $related_invitee_module_id;

    /**
     * Save multiple reminders invitees data.
     *
     * @param string $reminderId Related Reminder GUID
     * @param array $inviteesData Invitees Data
     */
    public static function saveRemindersInviteesData($reminderId, $inviteesData)
    {
        $savedInviteeIds = array();
        foreach ($inviteesData as $k => $inviteeData) {
            if (isset($_POST['isDuplicate']) && $_POST['isDuplicate']) {
                $inviteeData->id = '';
            }
            $reminderInviteeBean = BeanFactory::getBean('Reminders_Invitees', $inviteeData->id);
            $reminderInviteeBean->reminder_id = $reminderId;
            $reminderInviteeBean->related_invitee_module = $inviteeData->module;
            $reminderInviteeBean->related_invitee_module_id = $inviteeData->module_id;
            if (!$inviteeData->id) {
                $reminderInviteeBean->save();
                $savedInviteeIds[] = $reminderInviteeBean->id;
            } else {
                $addedInvitees = BeanFactory::getBean('Reminders_Invitees')->get_full_list("", "reminders_invitees.id != '{$inviteeData->id}' AND reminders_invitees.reminder_id = '{$reminderInviteeBean->reminder_id}' AND reminders_invitees.related_invitee_module = '{$reminderInviteeBean->related_invitee_module}' AND reminders_invitees.related_invitee_module_id = '{$reminderInviteeBean->related_invitee_module_id}'");
                if (!$addedInvitees) {
                    $reminderInviteeBean->save();
                    $savedInviteeIds[] = $reminderInviteeBean->id;
                } else {
                    $savedInviteeIds[] = $inviteeData->id;
                }
            }
        }
        self::deleteRemindersInviteesMultiple($reminderId, $savedInviteeIds);
    }

    /**
     * Load reminders invitees data.
     *
     * @param string $reminderId Related Reminder GUID
     * @return array Invitees data
     */
    public static function loadRemindersInviteesData($reminderId, $isDuplicate = false)
    {
        $ret = array();
        $reminderInviteeBeen = BeanFactory::newBean('Reminders_Invitees');
        $reminderInvitees = $reminderInviteeBeen->get_full_list("reminders_invitees.date_entered", "reminders_invitees.reminder_id = '$reminderId'");
        if ($reminderInvitees) {
            foreach ($reminderInvitees as $reminderInvitee) {
                $ret[] = array(
                    'id' => $isDuplicate ? null : $reminderInvitee->id,
                    'module' => $reminderInvitee->related_invitee_module,
                    'module_id' => $reminderInvitee->related_invitee_module_id,
                    'value' => self::getInviteeName($reminderInvitee->related_invitee_module, $reminderInvitee->related_invitee_module_id),
                );
            }
        }
        return $ret;
    }

    private static function getInviteeName($module, $moduleId)
    {
        $retValue = "unknown";

        $bean = BeanFactory::getBean($module, $moduleId);
        switch ($module) {
            case 'Users':
            case 'Contacts':
            case 'Leads':
            default:
                if (isset($bean->first_name) && isset($bean->last_name)) {
                    $retValue = "{$bean->first_name} {$bean->last_name}";
                } else {
                    if (isset($bean->name)) {
                        $retValue = $bean->name;
                    } else {
                        if (isset($bean->email)) {
                            $retValue = $bean->email;
                        }
                    }
                }
                if (!$retValue) {
                    $retValue = "$module ($moduleId)";
                }
                break;
        }
        return $retValue;
    }

    /**
     * Delete reminders invitees multiple.
     *
     * @param string $reminderId Related Reminder GUID
     * @param array $inviteeIds (optional) Exluded Invitees GUIDs, the invitee will not deleted if this argument contains that. Default is empty array.
     */
    public static function deleteRemindersInviteesMultiple($reminderId, $inviteeIds = array())
    {
        $invitees = BeanFactory::getBean('Reminders_Invitees')->get_full_list("", "reminders_invitees.reminder_id = '$reminderId'");
        if ($invitees) {
            foreach ($invitees as $invitee) {
                if (!in_array($invitee->id, $inviteeIds)) {
                    $invitee->mark_deleted($invitee->id);
                    $invitee->save();
                }
            }
        }
    }
}
