<?php

$hook_array['before_save'][] = array(
    20,
    'WIT insurance lead normalization',
    'custom/modules/Leads/WITInsuranceLeadHooks.php',
    'WITInsuranceLeadHooks',
    'beforeSave'
);

$hook_array['after_save'][] = array(
    20,
    'WIT insurance follow-up task creation',
    'custom/modules/Leads/WITInsuranceLeadHooks.php',
    'WITInsuranceLeadHooks',
    'afterSave'
);
