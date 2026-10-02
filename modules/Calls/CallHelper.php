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
 * @param $focus
 * @param $field
 * @param $value
 * @param $view
 * @return string
 */
function getDurationMinutesOptions($focus, $field, $value, $view)
{
    if (isset($_REQUEST['duration_minutes'])) {
        $focus->duration_minutes = $_REQUEST['duration_minutes'];
    }
    
    if (!isset($focus->duration_minutes)) {
        $focus->duration_minutes = $focus->minutes_value_default;
    }
    
    global $timedate;
    //setting default date and time
    if (is_null($focus->date_start)) {
        $focus->date_start = $timedate->to_display_date(gmdate($timedate->get_date_time_format()));
    }
    if (is_null($focus->duration_hours)) {
        $focus->duration_hours = "0";
    }
    if (is_null($focus->duration_minutes)) {
        $focus->duration_minutes = "1";
    }
    
    if ($view == 'EditView' || $view == 'MassUpdate' || $view == "QuickCreate"
    ) {
        $html = '<select id="duration_minutes" ';
        if ($view != 'MassUpdate'
            ) {
            $html .= 'onchange="SugarWidgetScheduler.update_time();" ';
        }

        $html .=  'name="duration_minutes">';
        $html .= get_select_options_with_id($focus->minutes_values, (int)$focus->duration_minutes);
        $html .= '</select>';
        return $html;
    }

    return $focus->duration_minutes;
}

/**
 * @param $focus
 * @param $field
 * @param $value
 * @param $view
 * @return string
 *
 * @deprecated 6.5.0
 */
function getReminderTime($focus, $field, $value, $view)
{
    global $current_user, $app_list_strings;
    $reminder_t = -1;
    
    if (!empty($_REQUEST['full_form']) && !empty($_REQUEST['reminder_time'])) {
        $reminder_t = $_REQUEST['reminder_time'];
    } else {
        if (isset($focus->reminder_time)) {
            $reminder_t = $focus->reminder_time;
        } else {
            if (isset($value)) {
                $reminder_t = $value;
            }
        }
    }

    if ($view == 'EditView' || $view == 'MassUpdate' || $view == "SubpanelCreates" || $view == "QuickCreate"
    ) {
        global $app_list_strings;
        $html = '<select id="reminder_time" name="reminder_time">';
        $html .= get_select_options_with_id($app_list_strings['reminder_time_options'], $reminder_t);
        $html .= '</select>';
        return $html;
    }
 
    if ($reminder_t == -1) {
        return "";
    }
       
    return translate('reminder_time_options', '', $reminder_t);
}
