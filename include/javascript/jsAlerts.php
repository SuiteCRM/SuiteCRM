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


require_once("include/utils/db_utils.php");

#[\AllowDynamicProperties]
class jsAlerts
{
    public $script;

    public function __construct()
    {
        global $app_strings;
        $this->script .= <<<EOQ
		if (!alertsTimeoutId) {
		    checkAlerts();
		}

EOQ;
        $this->addActivities();
        Reminder::addNotifications($this);
        if (!empty($GLOBALS['sugar_config']['enable_timeout_alerts'])) {
            $this->addAlert($app_strings['ERROR_JS_ALERT_SYSTEM_CLASS'], $app_strings['ERROR_JS_ALERT_TIMEOUT_TITLE'], '', $app_strings['ERROR_JS_ALERT_TIMEOUT_MSG_1'], (session_cache_expire() - 2) * 60);
            $this->addAlert($app_strings['ERROR_JS_ALERT_SYSTEM_CLASS'], $app_strings['ERROR_JS_ALERT_TIMEOUT_TITLE'], '', $app_strings['ERROR_JS_ALERT_TIMEOUT_MSG_2'], (session_cache_expire()) * 60, 'index.php');
        }
    }

    public function addAlert($type, $name, $subtitle, $description, $countdown, $redirect = '', $reminder_id = '')
    {
        if ($countdown < 0) {
            $countdown = 0;
        }
        $script = 'addAlert(' . json_encode($type) . ',' . json_encode($name) . ',' . json_encode($subtitle)
            . ',' . json_encode(str_replace(array("\r", "\n"), array('', '<br>'), $description))
            . ',' . $countdown . ',' . json_encode($redirect) . ',' . json_encode($reminder_id) . ');' . "\n";
        $this->script .= $script;
    }

    public function getScript()
    {
        return "<script>secondsSinceLoad = 0; alertList = [];" . $this->script . "</script>";
    }

    /*
     * To return the name of parent bean.
     * @param $parentType string parent type
     * @param $parentId string parent id
     */
    public function getRelatedName($parentType, $parentId)
    {
        if (!empty($parentType) && !empty($parentId)) {
            $parentBean = BeanFactory::getBean($parentType, $parentId);
            if (($parentBean instanceof SugarBean) && isset($parentBean->name)) {
                return $parentBean->name;
            }
        }
        return '';
    }

    public function addActivities()
    {
        global $app_list_strings, $timedate, $current_user, $app_strings;
        global $sugar_config;

        if (empty($current_user->id)) {
            return;
        }

        //Create separate variable to hold timedate value
        $alertDateTimeNow = $timedate->nowDb();

        // cn: get a boundary limiter
        $dateTimeMax = $timedate->getNow()->modify("+{$app_list_strings['reminder_max_time']} seconds")->asDb();
        $dateTimeNow = $timedate->getNow()->modify("-60 seconds")->asDb();

        $db = DBManagerFactory::getInstance();
        $dateTimeNow = $db->convert($db->quoted($dateTimeNow), 'datetime');
        $dateTimeMax = $db->convert($db->quoted($dateTimeMax), 'datetime');
        $desc = $db->convert("description", "text2char");
        if ($desc != "description") {
            $desc .= " description";
        }

        // Prep Meetings Query
        $selectMeetings = "SELECT meetings.id, name,reminder_time, $desc,location, status, parent_type, parent_id, date_start, assigned_user_id
			FROM meetings LEFT JOIN meetings_users ON meetings.id = meetings_users.meeting_id
			WHERE meetings_users.user_id ='".$current_user->id."'
				AND meetings_users.accept_status != 'decline'
				AND meetings.reminder_time != -1
				AND meetings_users.deleted != 1
				AND meetings.status = 'Planned'
			    AND date_start >= $dateTimeNow
			    AND date_start <= $dateTimeMax";
        $result = $db->query($selectMeetings);

        ///////////////////////////////////////////////////////////////////////
        ////	MEETING INTEGRATION
        $meetingIntegration = null;
        if (isset($sugar_config['meeting_integration']) && !empty($sugar_config['meeting_integration'])) {
            if (!class_exists($sugar_config['meeting_integration'])) {
                require_once("modules/{$sugar_config['meeting_integration']}/{$sugar_config['meeting_integration']}.php");
            }
            $meetingIntegration = new $sugar_config['meeting_integration']();
        }
        ////	END MEETING INTEGRATION
        ///////////////////////////////////////////////////////////////////////

        while ($row = $db->fetchByAssoc($result)) {
            // need to concatenate since GMT times can bridge two local days
            $timeStart = strtotime($db->fromConvert($row['date_start'], 'datetime'));
            $timeRemind = $row['reminder_time'];
            $timeStart -= $timeRemind;

            $url = 'index.php?action=DetailView&module=Meetings&record=' . $row['id'];
            $instructions = $app_strings['MSG_JS_ALERT_MTG_REMINDER_MEETING_MSG'];

            ///////////////////////////////////////////////////////////////////
            ////	MEETING INTEGRATION
            if (!empty($meetingIntegration) && $meetingIntegration->isIntegratedMeeting($row['id'])) {
                $url = $meetingIntegration->miUrlGetJsAlert($row);
                $instructions = $meetingIntegration->miGetJsAlertInstructions();
            }
            ////	END MEETING INTEGRATION
            ///////////////////////////////////////////////////////////////////

            $meetingName = from_html($row['name']);
            $desc1 = from_html($row['description']);
            $location = from_html($row['location']);

            $relatedToMeeting = $this->getRelatedName($row['parent_type'], $row['parent_id']);

            $description = empty($desc1) ? '' : $app_strings['MSG_JS_ALERT_MTG_REMINDER_AGENDA'].$desc1."\n";
            $description = $description  ."\n" .$app_strings['MSG_JS_ALERT_MTG_REMINDER_STATUS'] . $row['status'] ."\n". $app_strings['MSG_JS_ALERT_MTG_REMINDER_RELATED_TO']. $relatedToMeeting;


            // standard functionality
            $this->addAlert(
                $app_strings['MSG_JS_ALERT_MTG_REMINDER_MEETING'],
                $meetingName,
                $app_strings['MSG_JS_ALERT_MTG_REMINDER_TIME'].$timedate->to_display_date_time($db->fromConvert($row['date_start'], 'datetime')),
                $app_strings['MSG_JS_ALERT_MTG_REMINDER_LOC'].$location.
                $description.
                $instructions,
                $timeStart - strtotime($alertDateTimeNow),
                $url
            );
        }

        // Prep Calls Query
        $selectCalls = "
				SELECT calls.id, name, reminder_time, $desc, date_start, status, parent_type, parent_id
				FROM calls LEFT JOIN calls_users ON calls.id = calls_users.call_id
				WHERE calls_users.user_id ='".$current_user->id."'
				    AND calls_users.accept_status != 'decline'
				    AND calls.reminder_time != -1
					AND calls_users.deleted != 1
					AND calls.status = 'Planned'
				    AND date_start >= $dateTimeNow
				    AND date_start <= $dateTimeMax";

        $result = $db->query($selectCalls);

        while ($row = $db->fetchByAssoc($result)) {
            // need to concatenate since GMT times can bridge two local days
            $timeStart = strtotime($db->fromConvert($row['date_start'], 'datetime'));
            $timeRemind = $row['reminder_time'];
            $timeStart -= $timeRemind;
            $row['description'] = (isset($row['description'])) ? $row['description'] : '';

            $relatedToCall = $this->getRelatedName($row['parent_type'], $row['parent_id']);

            $callDescription = $row['description'] ."\n" .$app_strings['MSG_JS_ALERT_MTG_REMINDER_STATUS'] . $row['status'] ."\n". $app_strings['MSG_JS_ALERT_MTG_REMINDER_RELATED_TO']. $relatedToCall;


            $this->addAlert($app_strings['MSG_JS_ALERT_MTG_REMINDER_CALL'], $row['name'], $app_strings['MSG_JS_ALERT_MTG_REMINDER_TIME'].$timedate->to_display_date_time($db->fromConvert($row['date_start'], 'datetime')), $app_strings['MSG_JS_ALERT_MTG_REMINDER_DESC'].$callDescription, $timeStart - strtotime($alertDateTimeNow), 'index.php?action=DetailView&module=Calls&record=' . $row['id']);
        }
    }
}
