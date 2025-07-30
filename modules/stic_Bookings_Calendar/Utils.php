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
class stic_Bookings_CalendarUtils
{
    /**
     * Returns all the existing resources with the necessary information to display them in the calendar
     *
     * @return void
     */
    public static function getAllResources()
    {
        global $app_list_strings, $current_language, $sugar_config;
    
        $mod_strings = return_module_language($current_language, 'stic_Bookings_Calendar');
        $filteredResources = $GLOBALS['current_user']->getPreference('stic_bookings_calendar_filtered_resources');
    
        $resourcesBean = BeanFactory::getBean('stic_Resources');
        $resourcesBeans = $resourcesBean->get_full_list('name');
    
        $resourcesArrayByGroup = array();
        $resourcesArray = array();
        $resourcesArrayNoPlaces = array();
    
        foreach ($resourcesBeans as $resource) {
            $color = $resource->color ?: $sugar_config['stic_bookings_calendar_default_event_color'];
    
            $resourceObject = array(
                "id" => $resource->id,
                "name" => $resource->name,
                "color" => $color,
                "fontColor" => self::getContrastColor($color),
                "selected" => in_array($resource->id, $filteredResources ?: array()),
            );
    
            $type = $resource->type ?: 'no_type_assigned';
    
            $resourcesArray[] = $resourceObject;
    
            if ($type !== 'place') {
                $resourcesArrayNoPlaces[] = $resourceObject;
                $resourcesArrayByGroup[$type][] = $resourceObject;
            }
        }
    
        ksort($resourcesArrayByGroup);
    
        return array(
            'resourcesArray' => $resourcesArray,
            'resourcesArrayNoPlaces' => $resourcesArrayNoPlaces,
            'resourcesArrayByGroup' => $resourcesArrayByGroup,
        );
    }
    /**
     * This function requires a color and returns the most contrasted color, black or white. 
     * This is used to choose the font color for the resource in the calendar.
     *
     * @param String $hexColor
     * @return color
     */
    public static function getContrastColor($hexColor)
    {

        // hexColor RGB
        $R1 = hexdec(substr($hexColor, 1, 2));
        $G1 = hexdec(substr($hexColor, 3, 2));
        $B1 = hexdec(substr($hexColor, 5, 2));

        // Black RGB
        $blackColor = "#000000";
        $R2BlackColor = hexdec(substr($blackColor, 1, 2));
        $G2BlackColor = hexdec(substr($blackColor, 3, 2));
        $B2BlackColor = hexdec(substr($blackColor, 5, 2));

        // Calc contrast ratio
        $L1 = 0.2126 * pow($R1 / 255, 2.2) +
        0.7152 * pow($G1 / 255, 2.2) +
        0.0722 * pow($B1 / 255, 2.2);

        $L2 = 0.2126 * pow($R2BlackColor / 255, 2.2) +
        0.7152 * pow($G2BlackColor / 255, 2.2) +
        0.0722 * pow($B2BlackColor / 255, 2.2);

        $contrastRatio = 0;
        if ($L1 > $L2) {
            $contrastRatio = (int) (($L1 + 0.05) / ($L2 + 0.05));
        } else {
            $contrastRatio = (int) (($L2 + 0.05) / ($L1 + 0.05));
        }

        // If contrast is more than 5, return black color
        if ($contrastRatio > 5) {
            return '#000000';
        } else {
            // If not, return white color
            return '#FFFFFF';
        }
    }
}
