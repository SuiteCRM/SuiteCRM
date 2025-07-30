<?php

global $current_language;

$mod_strings = return_module_language($current_language, 'stic_Bookings');

return [
    'name' => $mod_strings['LBL_RESOURCES_NAME'],
    'code' => $mod_strings['LBL_RESOURCES_CODE'],
    'user_type' => $mod_strings['LBL_RESOURCES_USER_TYPE'],
    'place_type' => $mod_strings['LBL_RESOURCES_PLACE_TYPE'],
    'gender' => $mod_strings['LBL_RESOURCES_GENDER'],
    'amount_day_occupied' => $mod_strings['LBL_RESOURCES_AMOUNT_DAY_OCCUPIED'],
    'amount_copayment' => $mod_strings['LBL_RESOURCES_AMOUNT_COPAYMENT'],
];


