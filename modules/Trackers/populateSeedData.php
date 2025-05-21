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


if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}



require_once('modules/Trackers/TrackerUtility.php');

require_once('install/UserDemoData.php');

#[\AllowDynamicProperties]
class populateSeedData
{
    public $monitorIds = 500;
    public $user = 1;
    public $userDemoData;
    public $modules = array('Accounts', 'Calls', 'Contacts', 'Leads', 'Meetings', 'Notes', 'Opportunities', 'Users');
    public $actions = array('authenticate', 'detailview', 'editview', 'index', 'save', 'settimezone');
    public $db;
    public $beanIdMap = array();
    public $userSessions = array();
    public $trackerManager;

    public function start()
    {
        $this->db = DBManagerFactory::getInstance();
        $this->userDemoData = new UserDemoData($this->user, false);
        $this->trackerManager = TrackerManager::getInstance();

        foreach ($this->modules as $mod) {
            $query = "select id from $mod";
            $result = $this->db->limitQuery($query, 0, 50);
            $ids = array();
            while (($row = $this->db->fetchByAssoc($result))) {
                $ids[] = $row['id'];
            } //while
            $this->beanIdMap[$mod] = $ids;
        }

        while ($this->monitorIds-- > 0) {
            $this->monitorId = create_guid();
            $this->trackerManager->setMonitorId($this->monitorId);
            $this->user = $this->userDemoData->guids[array_rand($this->userDemoData->guids)];
            $this->module = $this->modules[array_rand($this->modules)];
            $this->action = $this->actions[array_rand($this->actions)];
            $this->date = $this->randomTimestamp();
            $this->populate_tracker();
            $this->trackerManager->save();
        }
    }

    public function populate_tracker()
    {
        if ($monitor = $this->trackerManager->getMonitor('tracker')) {
            $monitor->setValue('user_id', $this->user);
            $monitor->setValue('module_name', $this->module);
            $monitor->setValue('action', $this->action);
            $monitor->setValue('visible', (($monitor->action == 'detailview') || ($monitor->action == 'editview')) ? 1 : 0);
            $monitor->setValue('date_modified', $this->randomTimestamp());
            $monitor->setValue('session_id', $this->getSessionId());
            if ($this->action != 'settimezone' && isset($this->beanIdMap[$this->module][array_rand($this->beanIdMap[$this->module])])) {
                $monitor->setValue('item_id', $this->beanIdMap[$this->module][array_rand($this->beanIdMap[$this->module])]);
                $monitor->setValue('item_summary', 'random stuff'); //don't really need this
            }
        }
    }


    public function randomTimestamp()
    {
        global $timedate;
        // 1201852800 is 1 Feb 2008
        return $timedate->fromTimestamp(mt_rand(1201852800, $timedate->getNow()->ts))->asDb();
    }

    public function getSessionId()
    {
        if (isset($this->userSessions[$this->user])) {
            return $this->userSessions[$this->user];
        }
        $this->userSessions[$this->user] = $this->monitorId;
        return $this->monitorId;
    }
}

$test = new populateSeedData();
$test->start();
