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


#[\AllowDynamicProperties]
class SchedulersViewEdit extends ViewEdit
{
    protected static $xtDays = array(
                1 => 'MON',
                2 => 'TUE',
                3 => 'WED',
                4 => 'THU',
                5 => 'FRI',
                6 => 'SAT',
                0 => 'SUN');

    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = true;
        //$this->useModuleQuickCreateTemplate = true;
    }

    /**
     * @see SugarView::_getModuleTitleListParam()
     */
    protected function _getModuleTitleListParam($browserTitle = false)
    {
        global $mod_strings;

        return "<a href='index.php?module=Schedulers&action=index'>".$mod_strings['LBL_MODULE_TITLE']."</a>";
    }


    public function display()
    {
        global $mod_strings;
        global $app_list_strings;

        // job functions
        $this->bean->job_function = $this->bean->job;
        $this->ss->assign('JOB', $this->bean->job);
        if (substr((string) $this->bean->job, 0, 5) == "url::") {
            $this->bean->job_url = substr((string) $this->bean->job, 5);
            $this->ss->assign('JOB', 'url::');
        }
        // interval
        if (!empty($this->bean->job_interval)) {
            $exInterval = explode("::", $this->bean->job_interval);
        } else {
            $exInterval = array('*','*','*','*','*');
        }
        $this->ss->assign('mins', $exInterval[0]);
        $this->ss->assign('hours', $exInterval[1]);
        $this->ss->assign('day_of_month', $exInterval[2]);
        $this->ss->assign('months', $exInterval[3]);
        $this->ss->assign('day_of_week', $exInterval[4]);

        // Handle cron weekdays
        if ($exInterval[4] == '*') {
            $this->ss->assign('ALL', "CHECKED");
            foreach (self::$xtDays as $day) {
                $this->ss->assign($day, "CHECKED");
            }
        } elseif (strpos($exInterval[4], ',')) {
            // 1,2,4
            $exDays = explode(',', trim($exInterval[4]));
            foreach ($exDays as $days) {
                if (strpos($days, '-')) {
                    $exDaysRange = explode('-', $days);
                    for ($i=$exDaysRange[0]; $i<=$exDaysRange[1]; $i++) {
                        $this->ss->assign(self::$xtDays[$days], "CHECKED");
                    }
                } else {
                    $this->ss->assign(self::$xtDays[$days], "CHECKED");
                }
            }
        } elseif (strpos($exInterval[4], '-')) {
            $exDaysRange = explode('-', $exInterval[4]);
            for ($i=$exDaysRange[0]; $i<=$exDaysRange[1]; $i++) {
                $this->ss->assign(self::$xtDays[$i], "CHECKED");
            }
        } else {
            $this->ss->assign(self::$xtDays[$exInterval[4]], "CHECKED");
        }

        // Hours
        $ints = [];
        for ($i=1; $i<=30; $i++) {
            $ints[$i] = $i;
        }
        $this->bean->adv_interval = false;
        $this->ss->assign('basic_intervals', $ints);
        $this->ss->assign('basic_periods', $app_list_strings['scheduler_period_dom']);
        if ($exInterval[0] == '*' && $exInterval[1] == '*') {
            // hours
        } elseif (strpos($exInterval[1], '*/') !== false && $exInterval[0] == '0') {
            // we have a "BASIC" type of hour setting
            $exHours = explode('/', $exInterval[1]);
            $this->ss->assign('basic_interval', $exInterval[1]);
            $this->ss->assign('basic_period', 'hour');
        // Minutes
        } elseif (strpos($exInterval[0], '*/') !== false && $exInterval[1] == '*') {
            // we have a "BASIC" type of min setting
            $exMins = explode('/', $exInterval[0]);
            $this->ss->assign('basic_interval', $exMins[1]);
            $this->ss->assign('basic_period', 'min');
        // we've got an advanced time setting
        } else {
            $this->ss->assign('basic_interval', 12);
            $this->ss->assign('basic_period', 'hour');
            $this->bean->adv_interval = true;
        }
        if ($this->bean->time_from || $this->bean->time_to) {
            $this->bean->adv_interval = true;
        }

        $this->ss->assign("adv_interval", $this->bean->adv_interval?"true":"false");
        $this->ss->assign("adv_visibility", $this->bean->adv_interval?"":"display: none");
        $this->ss->assign("basic_visibility", $this->bean->adv_interval?"display: none":"");

        parent::display();
    }
}
