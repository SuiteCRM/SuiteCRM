<?php

$hook_array['before_save'][] = array(
    30,
    'WIT VIN decode on lead save',
    'custom/modules/Leads/WITVinDecodeLeadHook.php',
    'WITVinDecodeLeadHook',
    'beforeSave'
);
