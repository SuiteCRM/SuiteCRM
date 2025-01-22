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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once("modules/Meetings/Meeting.php");
require_once("modules/Calls/Call.php");
require_once("modules/Users/User.php");
require_once("modules/Contacts/Contact.php");
require_once("modules/Leads/Lead.php");
require_once("include/utils.php");

/**
 * Class for sending email reminders of meetings and call to invitees
 *
 */
#[\AllowDynamicProperties]
class EmailReminder
{
    
    /**
     * string db datetime of now
     */
    protected $now;
    
    /**
     * string db datetime will be fetched till
     */
    protected $max;
    
    /**
     * constructor
     */
    public function __construct()
    {
        $max_time = 0;
        if (isset($GLOBALS['app_list_strings']['reminder_time_options'])) {
            foreach ($GLOBALS['app_list_strings']['reminder_time_options'] as $seconds => $value) {
                if ($seconds > $max_time) {
                    $max_time = $seconds;
                }
            }
        } else {
            $max_time = 8400;
        }
        $this->now = $GLOBALS['timedate']->nowDb();
        $this->max = $GLOBALS['timedate']->getNow()->modify("+{$max_time} seconds")->asDb();
    }
    
    /**
     * main method that runs reminding process
     * @return boolean
     */
    public function process()
    {
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();

        Reminder::sendEmailReminders($this, $admin);
        
        $meetings = $this->getMeetingsForRemind();
        foreach ($meetings as $id) {
            $recipients = $this->getRecipients($id, 'Meetings');
            $bean = BeanFactory::newBean('Meetings');
            $bean->retrieve($id);
            if ($this->sendReminders($bean, $admin, $recipients)) {
                $bean->email_reminder_sent = 1;
                $bean->save();
            }
        }
        
        $calls = $this->getCallsForRemind();
        foreach ($calls as $id) {
            $recipients = $this->getRecipients($id, 'Calls');
            $bean = BeanFactory::newBean('Calls');
            $bean->retrieve($id);
            if ($this->sendReminders($bean, $admin, $recipients)) {
                $bean->email_reminder_sent = 1;
                $bean->save();
            }
        }
        
        return true;
    }

    /**
     * send reminders
     * @param SugarBean $bean
     * @param Administration $admin
     * @param array $recipients
     * @return boolean
     */
    public function sendReminders(SugarBean $bean, Administration $admin, $recipients)
    {
        $current_language = get_current_language();

        if (!empty($bean->created_by)) {
            $user_id = $bean->created_by;
        } elseif (!empty($bean->assigned_user_id)) {
            $user_id = $bean->assigned_user_id;
        } else {
            $user_id = $GLOBALS['current_user']->id;
        }
        $user = BeanFactory::getBean('Users', $user_id);

        $OBCharset = $GLOBALS['locale']->getPrecedentPreference('default_email_charset');
        require_once __DIR__ . '/../../include/SugarPHPMailer.php';
        $mail = new SugarPHPMailer();
        $mail->setMailerForSystem();

        if (empty($admin->settings['notify_send_from_assigning_user'])) {
            $from_address = $admin->settings['notify_fromaddress'];
            $from_name = $admin->settings['notify_fromname'] ? '' : $admin->settings['notify_fromname'];
        } else {
            $from_address = (new SugarEmailAddress())->getReplyToAddress($user);
            $from_name = $user->full_name;
        }

        $mail->From = $from_address;
        isValidEmailAddress($mail->From);
        $mail->FromName = $from_name;

        $xtpl = new XTemplate(get_notify_template_file($current_language));
        $xtpl = $this->setReminderBody($xtpl, $bean, $user);

        $template_name = $GLOBALS['beanList'][$bean->module_dir] . 'Reminder';
        $xtpl->parse($template_name);
        $xtpl->parse($template_name . "_Subject");

        $tempBody = from_html(trim($xtpl->text($template_name)));
        $mail->msgHTML($tempBody);

        $tempBody = preg_replace('/<a href=([\"\']?)(.*?)\1>(.*?)<\/a>/', "\\3 [\\2]", (string) $tempBody);

        $mail->AltBody = strip_tags($tempBody);
        $mail->Subject = strip_tags(from_html($xtpl->text($template_name . "_Subject")));

        $oe = new OutboundEmail();
        $oe = $oe->getSystemMailerSettings();
        if (empty($oe->mail_smtpserver)) {
            $GLOBALS['log']->fatal("Email Reminder: error sending email, system smtp server is not set");

            return false;
        }

        foreach ($recipients as $r) {
            $mail->clearAddresses();
            $mail->addAddress($r['email'], $GLOBALS['locale']->translateCharsetMIME(trim($r['name']), 'UTF-8', $OBCharset));
            $mail->prepForOutbound();
            if (!$mail->send()) {
                $GLOBALS['log']->fatal("Email Reminder: error sending e-mail (method: {$mail->Mailer}), (error: {$mail->ErrorInfo})");
            }
        }

        return true;
    }
    
    /**
     * set reminder body
     * @param XTemplate $xtpl
     * @param SugarBean $bean
     * @param User $user
     * @return XTemplate
    */
    protected function setReminderBody(XTemplate $xtpl, SugarBean $bean, User $user)
    {
        $object = strtoupper($bean->object_name);

        $xtpl->assign("{$object}_SUBJECT", $bean->name);
        $date = $GLOBALS['timedate']->fromUser($bean->date_start, $GLOBALS['current_user']);
        $xtpl->assign("{$object}_STARTDATE", $GLOBALS['timedate']->asUser($date, $user)." ".TimeDate::userTimezoneSuffix($date, $user));
        if (isset($bean->location)) {
            $xtpl->assign("{$object}_LOCATION", $bean->location);
        }
        $xtpl->assign("{$object}_CREATED_BY", $user->full_name);
        $xtpl->assign("{$object}_DESCRIPTION", $bean->description);

        return $xtpl;
    }
    
    /**
     * get meeting ids list for remind
     * @return array
     */
    public function getMeetingsForRemind()
    {
        $db = DBManagerFactory::getInstance();
        $query = "
            SELECT id, date_start, email_reminder_time FROM meetings
            WHERE email_reminder_time != -1
            AND deleted = 0
            AND email_reminder_sent = 0
            AND status != 'Held'
            AND date_start >= '{$this->now}'
            AND date_start <= '{$this->max}'
        ";
        $re = $db->query($query);
        $meetings = array();
        while ($row = $db->fetchByAssoc($re)) {
            $remind_ts = $GLOBALS['timedate']->fromDb($db->fromConvert($row['date_start'], 'datetime'))->modify("-{$row['email_reminder_time']} seconds")->ts;
            $now_ts = $GLOBALS['timedate']->getNow()->ts;
            if ($now_ts >= $remind_ts) {
                $meetings[] = $row['id'];
            }
        }
        return $meetings;
    }
    
    /**
     * get calls ids list for remind
     * @return array
     */
    public function getCallsForRemind()
    {
        $db = DBManagerFactory::getInstance();
        $query = "
            SELECT id, date_start, email_reminder_time FROM calls
            WHERE email_reminder_time != -1
            AND deleted = 0
            AND email_reminder_sent = 0
            AND status != 'Held'
            AND date_start >= '{$this->now}'
            AND date_start <= '{$this->max}'
        ";
        $re = $db->query($query);
        $calls = array();
        while ($row = $db->fetchByAssoc($re)) {
            $remind_ts = $GLOBALS['timedate']->fromDb($db->fromConvert($row['date_start'], 'datetime'))->modify("-{$row['email_reminder_time']} seconds")->ts;
            $now_ts = $GLOBALS['timedate']->getNow()->ts;
            if ($now_ts >= $remind_ts) {
                $calls[] = $row['id'];
            }
        }
        return $calls;
    }
    
    /**
     * get recipients of reminding email for specific activity
     * @param string $id
     * @param string $module
     * @return array
     */
    protected function getRecipients($id, $module = "Meetings")
    {
        $db = DBManagerFactory::getInstance();
    
        switch ($module) {
            case "Meetings":
                $field_part = "meeting";
                break;
            case "Calls":
                $field_part = "call";
                break;
            default:
                return array();
        }
    
        $emails = array();
        // fetch users
        $query = "SELECT user_id FROM {$field_part}s_users WHERE {$field_part}_id = '{$id}' AND accept_status != 'decline' AND deleted = 0
        ";
        $re = $db->query($query);
        while ($row = $db->fetchByAssoc($re)) {
            $user = BeanFactory::newBean('Users');
            $user->retrieve($row['user_id']);
            if (!empty($user->email1) && $user->isEnabled() ) {
                $arr = array(
                    'type' => 'Users',
                    'name' => $user->full_name,
                    'email' => $user->email1,
                );
                $emails[] = $arr;
            }
        }
        // fetch contacts
        $query = "SELECT contact_id FROM {$field_part}s_contacts WHERE {$field_part}_id = '{$id}' AND accept_status != 'decline' AND deleted = 0";
        $re = $db->query($query);
        while ($row = $db->fetchByAssoc($re)) {
            $contact = BeanFactory::newBean('Contacts');
            $contact->retrieve($row['contact_id']);
            if (!empty($contact->email1)) {
                $arr = array(
                    'type' => 'Contacts',
                    'name' => $contact->full_name,
                    'email' => $contact->email1,
                );
                $emails[] = $arr;
            }
        }
        // fetch leads
        $query = "SELECT lead_id FROM {$field_part}s_leads WHERE {$field_part}_id = '{$id}' AND accept_status != 'decline' AND deleted = 0";
        $re = $db->query($query);
        while ($row = $db->fetchByAssoc($re)) {
            $lead = BeanFactory::newBean('Leads');
            $lead->retrieve($row['lead_id']);
            if (!empty($lead->email1)) {
                $arr = array(
                    'type' => 'Leads',
                    'name' => $lead->full_name,
                    'email' => $lead->email1,
                );
                $emails[] = $arr;
            }
        }
        return $emails;
    }
}
