<?php

global $current_language;

$mod_strings = return_module_language($current_language, 'stic_Resources');

return [
    'name' => $mod_strings['LBL_NAME'],
    'code' => $mod_strings['LBL_CODE'],
    'color' => $mod_strings['LBL_COLOR'],
    'type' => $mod_strings['LBL_TYPE'],
    'hourly_rate' => $mod_strings['LBL_HOURLY_RATE'],
    'daily_rate' => $mod_strings['LBL_DAILY_RATE'],
];


