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
require_once 'include/MVC/View/views/view.edit.php';
require_once 'SticInclude/Views.php';

#[\AllowDynamicProperties]
class stic_BookingsViewEdit extends ViewEdit
{

    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = true;
        $this->useModuleQuickCreateTemplate = true;
    }

    public function preDisplay()
    {
        global $timedate, $current_user;

        // If the Bookings' EditView is launched from the Bookings' Calendar, retrieve start and end dates from there
        if (isset($_REQUEST['return_module'], $_REQUEST['start'], $_REQUEST['end']) && $_REQUEST['return_module'] == 'stic_Bookings_Calendar' && $_REQUEST['start'] && $_REQUEST['end']) {
            // Parse the dates received from the calendar
            $startDate = new DateTime($_REQUEST['start']);
            $this->bean->start_date = $timedate->to_display_date_time(date_format($startDate, 'Y-m-d H:i:s'), false, false, $current_user);
            $endDate = new DateTime($_REQUEST['end']);
            $this->bean->end_date = $timedate->to_display_date_time(date_format($endDate, 'Y-m-d H:i:s'), false, false, $current_user);
            if ($_REQUEST['allDay'] == "true") {
                $this->bean->all_day = true;
            }
        } else {
            // If all_day is checked then remove the hours and minutes
            // and apply timezone to the dates
            if (isset($this->bean->all_day) && $this->bean->all_day == '1') {
                $startDate = explode(' ', $this->bean->fetched_row['start_date']);
                if ($startDate[1] > "12:00") {
                    $startDate = new DateTime($startDate[0]);
                    $startDate = $startDate->modify("next day");
                    $startDateDate = $timedate->asUserDate($startDate, false, $current_user);
                    $this->bean->start_date = $startDateDate . ' 00:00';
                } else {
                    $startDate = new DateTime($startDate[0]);
                    $startDate = $timedate->asUserDate($startDate, false, $current_user);
                    $this->bean->start_date = $startDate . ' 00:00';
                }

                $endDate = explode(' ', $this->bean->fetched_row['end_date']);
                if ($endDate[1] > "12:00") {
                    $endDate = new DateTime($endDate[0]);
                    $endDate = $endDate->modify("next day");
                    $endDate = $timedate->asUserDate($endDate, false, $current_user);
                    $this->bean->end_date = $endDate . ' 00:00';
                } else {
                    $endDate = new DateTime($endDate[0]);
                    $endDate = $timedate->asUserDate($endDate, false, $current_user);
                    $this->bean->end_date = $endDate . ' 00:00';
                }
            }
        }

	    parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here you custom code

    }

    public function display()
    {
        require_once 'SticInclude/Utils.php';

        // Add the resources template
        $this->ev->defs['templateMeta']['form']['footerTpl'] = 'modules/stic_Bookings/tpls/EditViewFooter.tpl';

        $relationshipName = 'stic_resources_stic_bookings';

        // If the Bookings editview is launched from the "new" button in the Resources detailview Bookings subpanel, 
        // then add the resource into the new booking. Notice that stic_resources_id is only available in that case,
        // not when Bookings editview is launched from the "edit" button in an already existing booking in the subpanel.
        if (($_REQUEST['return_module'] ?? null) == 'stic_Resources' && !empty($_REQUEST['stic_resources_id'] ?? '')){
            // When creating a record from a subpanel, the record in the detailview will be set as the parent record of the new one, 
            // ie, it will be assigned to the flex related field if there is any. In this case, the new booking would have a resource
            // as a parent record, what is nonsense. So let's remove the assignment from the $_REQUEST array.
            unset($_REQUEST['parent_type']);
            unset($_REQUEST['parent_name']);
            unset($_REQUEST['parent_id']);
            
            $resources[] = BeanFactory::getBean('stic_Resources', $_REQUEST['stic_resources_id']);
            $parsedResources = $this->parseResourceItems($resources);
            $parsedResourcesJson = json_encode($parsedResources);

            echo <<<SCRIPT
			<script>resources = $parsedResourcesJson;</script>
			SCRIPT;
        } else {
            // In any other case, check if there are currently related resources and load them into the Bookings editview
            if (!$this->bean->load_relationship($relationshipName)) {
                $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ': : Failed retrieving related resources data');
            } else {
                if ($resources = $this->bean->$relationshipName->getBeans()) {
                    $parsedResources = $this->parseResourceItems($resources);
                    $parsedResourcesJson = json_encode($parsedResources);

                    echo <<<SCRIPT
					<script>resources = $parsedResourcesJson;</script>
				SCRIPT;
                } else {
                    echo <<<SCRIPT
					<script>resources = [];</script>
				SCRIPT;
                }
            }
        }

        parent::display();

        SticViews::display($this);
        echo getVersionedScript("SticInclude/vendor/jqColorPicker/jqColorPicker.min.js");
        echo getVersionedScript("modules/stic_Bookings/Utils.js");
    }

    // Prepare resources data to be displayed in the editview
    public function parseResourceItems($resourcesBeanArray)
    {
        global $app_list_strings;

        $parsedResources = array();
        foreach ($resourcesBeanArray as $resourceBean) {
            $parsedResources[] = array(
                'resource_id' => $resourceBean->id,
                'resource_name' => $resourceBean->name,
                'resource_code' => $resourceBean->code,
                'resource_color' => $resourceBean->color,
                'resource_status' => $app_list_strings['stic_resources_status_list'][$resourceBean->status],
                'resource_type' => $app_list_strings['stic_resources_types_list'][$resourceBean->type],
                'resource_daily_rate' => self::formatNumberDec($resourceBean->daily_rate),
                'resource_hourly_rate' => self::formatNumberDec($resourceBean->hourly_rate),
            );
        }
        return $parsedResources;
    }

    public static function formatNumberDec($str)
    {
        $separator = get_number_separators();
        $thousandsSeparator = $separator[0];
        $decimalSeparator = $separator[1];
        // STIC Custom 20250206 JBL - Avoid Uncaught TypeError in number_format
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/477
        // return number_format($str, 2, $decimalSeparator, $thousandsSeparator);
        return number_format((float) $str, 2, $decimalSeparator, $thousandsSeparator);
        // End STIC Custom
    }
}
