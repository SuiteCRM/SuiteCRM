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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'include/Dashlets/DashletGeneric.php';
require_once 'modules/stic_Bookings_Calendar/stic_Bookings_Calendar.php';
require_once 'SticInclude/Views.php';

class stic_Bookings_CalendarDashlet extends DashletGeneric
{
    public $configureTpl = '';

    public function __construct($id, $def = null)
    {
        parent::__construct($id);

        if (empty($def['title'])) {
            $this->title = translate('LBL_HOMEPAGE_TITLE', 'stic_Bookings_Calendar');
        }

        if (!empty($def['view'])) {
            $this->view = $def['view'];
        }

        // seedBean is need to set the calendar icon
        if ($this->seedBean = BeanFactory::newBean('stic_Bookings_Calendar')) {
            $this->seedBean->module_name = 'stic_Bookings_Calendar';
        } else {
            $GLOBALS['log']->warn('stic_Bookings_Calendar bean not created');
        }
    }

    public function display()
    {
        global $current_user, $sugar_config, $current_language;
        // In order to display the Calendar in the Dashlet, we need to retrieve the mod_strings manually from 
        // the module using this function
        $mod_strings = return_module_language($current_language, 'stic_Bookings_Calendar');
        ob_start();
        SticViews::display($this);

        $initialCalendarDate = $_REQUEST['start_date'] ? $_REQUEST['start_date'] : '';
        $initialCalendarDate = json_encode($initialCalendarDate);
        echo <<<SCRIPT
        <script>initialCalendarDate = $initialCalendarDate;</script>
    SCRIPT;

        // Define the default color for Calendar items in the calendar
        $defaultCalendarObjectColor = $sugar_config['stic_bookings_calendar_default_event_color'];
        $defaultCalendarObjectColor = json_encode($defaultCalendarObjectColor);
        echo <<<SCRIPT
        <script>defaultCalendarObjectColor = $defaultCalendarObjectColor;</script>
    SCRIPT;

        $lang = $_SESSION['authenticated_user_language'];
        $lang = explode('_', $lang);
        $lang = json_encode($lang[0]);
        echo <<<SCRIPT
        <script>lang = $lang;</script>
    SCRIPT;

        // Retriving user preferences
        $userBean = new UserPreference($current_user);
        $availabilityMode = $userBean->getPreference('stic_bookings_calendar_availability_mode') == "true" ? true : false;
        $availabilityModeJson = json_encode($availabilityMode);

        echo <<<SCRIPT
        <script>availabilityMode = $availabilityModeJson;</script>
    SCRIPT;

        // Retrieving user configuration for the Calendar View
        $calendarView = $userBean->getPreference('stic_bookings_calendar_view');
        $calendarViewJson = json_encode($calendarView);

        echo <<<SCRIPT
        <script>calendarView = $calendarViewJson;</script>
    SCRIPT;

        // Retriving resources and sending them to interface
        require_once "modules/stic_Bookings_Calendar/Utils.php";
        $resources = stic_Bookings_CalendarUtils::getAllResources();
        $resourcesArrayJson = json_encode($resources['resourcesArray']);

        $ss = new Sugar_Smarty();
        $ss->assign('APP', $GLOBALS['app_strings']);
        $ss->assign('APPLIST', $GLOBALS['app_list_strings']);
        $ss->assign('MOD', $mod_strings);
        echo '<br>';
        echo <<<SCRIPT
        <script>resourcesGroupArray = $resourcesArrayJson;</script>
    SCRIPT;

        echo getVersionedScript("SticInclude/vendor/fullcalendar/index.global.min.js");
        echo $ss->fetch("modules/stic_Bookings_Calendar/tpls/calendar.tpl");

        echo getVersionedScript("modules/stic_Bookings_Calendar/Utils.js");

        // Setting the language module into the JS Sugar variable manually for displaying 
        // the correct labels in the Bookings Calendar dashlet
        $modStrings = json_encode($mod_strings);
        echo "<script>SUGAR.language.setLanguage('stic_Bookings_Calendar', $modStrings)</script>";

        $str = ob_get_contents();
        ob_end_clean();

        return $str;
    }

    /**
     * It builds the filter menu
     */
    public function displayOptions()
    {
        global $current_language;
        // In order to display the Calendar in the Dashlet, we need to retrieve the mod_strings manually from 
        // the module using this function
        $mod_strings = return_module_language($current_language, 'stic_Bookings_Calendar');

        ob_start();
        $ss = new Sugar_Smarty();
        require_once "modules/stic_Bookings_Calendar/Utils.php";

        $resources = stic_Bookings_CalendarUtils::getAllResources();

        $ss->assign('RESOURCESGROUP', $resources['resourcesArrayByGroup']);
        $resourcesArrayJson = json_encode($resources['resourcesArray']);
        echo <<<SCRIPT
        <script>resourcesGroupArray = $resourcesArrayJson;</script>
    SCRIPT;
        echo getVersionedScript("modules/stic_Bookings_Calendar/Utils.js");
        $modStrings = json_encode($mod_strings);
        echo "<script>SUGAR.language.setLanguage('stic_Bookings_Calendar', $modStrings)</script>";
        $ss->assign('MOD', $mod_strings);

        $str = ob_get_contents();
        ob_end_clean();

        return parent::displayOptions() . $str .
        $ss->fetch('modules/stic_Bookings_Calendar/Dashlets/stic_Bookings_CalendarDashlet/stic_Bookings_CalendarDashletOptions.tpl');
    }

    public function displayScript()
    {
        return '';
    }
}
