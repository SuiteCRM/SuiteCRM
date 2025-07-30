<?php

global $current_language;

$mod_strings = return_module_language($current_language, 'stic_Bookings');

return [
    'name' => $mod_strings['LBL_RESOURCES_NAME'],
    'code' => $mod_strings['LBL_RESOURCES_CODE'],
    'color' => $mod_strings['LBL_RESOURCES_COLOR'],
    'type' => $mod_strings['LBL_RESOURCES_TYPE'],
    'hourly_rate' => $mod_strings['LBL_RESOURCES_HOURLY_RATE'],
    'daily_rate' => $mod_strings['LBL_RESOURCES_DAILY_RATE'],
];


