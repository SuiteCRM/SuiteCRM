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
require_once 'include/MVC/View/views/view.list.php';
require_once 'SticInclude/Views.php';

class stic_Bookings_CalendarViewList extends ViewList
{
    public function __construct()
    {
        parent::__construct();

    }
    public function preDisplay()
    {
        parent::preDisplay();

        SticViews::preDisplay($this);

    }

    public function display()
    {
        global $mod_strings, $current_user, $app_strings, $sugar_config, $timedate;
        SticViews::display($this);

        $initialCalendarDate = $_REQUEST['start_date'] ?? '';
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

        // Define the language of the calendar
        $lang = $_SESSION['authenticated_user_language'];
        $lang = explode('_', $lang);
        $lang = json_encode($lang[0]);
        echo <<<SCRIPT
        <script>lang = $lang;</script>
    SCRIPT;
        $start_weekday = $GLOBALS['current_user']->get_first_day_of_week();
        echo <<<SCRIPT
        <script>start_weekday = $start_weekday;</script>
    SCRIPT;

        // Retrieve user configuration for the availability mode
        $userBean = new UserPreference($current_user);
        $availabilityMode = $userBean->getPreference('stic_bookings_calendar_availability_mode') == "true" ? true : false;
        $availabilityModeJson = json_encode($availabilityMode);

        echo <<<SCRIPT
        <script>availabilityMode = $availabilityModeJson;</script>
    SCRIPT;

        // Retrieve user configuration for the calendar view
        $calendarView = $userBean->getPreference('stic_bookings_calendar_view');
        $calendarViewJson = json_encode($calendarView);

        echo <<<SCRIPT
        <script>calendarView = $calendarViewJson;</script>
        SCRIPT;

        // Pass the resources to the template and javascript, to be used in the CSS and in the filters
        require_once "modules/stic_Bookings_Calendar/Utils.php";
        $resources = stic_Bookings_CalendarUtils::getAllResources();
        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        $this->ss->assign('RESOURCESGROUP', $resources['resourcesArrayByGroup']);

        $resourcesArrayJson = json_encode($resources['resourcesArrayNoPlaces']);
        echo <<<SCRIPT
        <script>resourcesGroupArray = $resourcesArrayJson;</script>
    SCRIPT;

        echo getVersionedScript("SticInclude/vendor/fullcalendar/index.global.min.js");
        echo getVersionedScript("modules/stic_Bookings_Calendar/Utils.js");

        $this->ss->display("modules/stic_Bookings_Calendar/tpls/filter.tpl");
        $this->ss->display("modules/stic_Bookings_Calendar/tpls/calendar.tpl");

    }
}
