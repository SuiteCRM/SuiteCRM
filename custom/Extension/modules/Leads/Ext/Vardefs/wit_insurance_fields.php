<?php
// Upgrade-safe insurance fields for WIT lead intake.
// Run Admin > Repair > Quick Repair and Rebuild after deployment.

$dictionary['Lead']['fields']['policy_type_c'] = array(
    'name' => 'policy_type_c',
    'vname' => 'LBL_POLICY_TYPE',
    'type' => 'enum',
    'options' => 'wit_policy_type_list',
    'len' => 100,
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);

$dictionary['Lead']['fields']['coverage_limits_c'] = array(
    'name' => 'coverage_limits_c',
    'vname' => 'LBL_COVERAGE_LIMITS',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'rows' => 6,
    'cols' => 80,
);

$dictionary['Lead']['fields']['drivers_json_c'] = array(
    'name' => 'drivers_json_c',
    'vname' => 'LBL_DRIVERS_JSON',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => false,
    'rows' => 10,
    'cols' => 100,
);

$dictionary['Lead']['fields']['vehicles_json_c'] = array(
    'name' => 'vehicles_json_c',
    'vname' => 'LBL_VEHICLES_JSON',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => false,
    'rows' => 10,
    'cols' => 100,
);

$dictionary['Lead']['fields']['accidents_violations_json_c'] = array(
    'name' => 'accidents_violations_json_c',
    'vname' => 'LBL_ACCIDENTS_VIOLATIONS_JSON',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => false,
    'rows' => 10,
    'cols' => 100,
);

$dictionary['Lead']['fields']['next_action_c'] = array(
    'name' => 'next_action_c',
    'vname' => 'LBL_NEXT_ACTION',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'rows' => 5,
    'cols' => 80,
);

$dictionary['Lead']['fields']['next_follow_up_date_c'] = array(
    'name' => 'next_follow_up_date_c',
    'vname' => 'LBL_NEXT_FOLLOW_UP_DATE',
    'type' => 'datetimecombo',
    'dbType' => 'datetime',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'enable_range_search' => true,
    'options' => 'date_range_search_dom',
);

$dictionary['Lead']['fields']['call_summary_c'] = array(
    'name' => 'call_summary_c',
    'vname' => 'LBL_CALL_SUMMARY',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'rows' => 8,
    'cols' => 100,
);

$dictionary['Lead']['fields']['missing_info_c'] = array(
    'name' => 'missing_info_c',
    'vname' => 'LBL_MISSING_INFO',
    'type' => 'text',
    'dbType' => 'longtext',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'rows' => 5,
    'cols' => 80,
);

$dictionary['Lead']['fields']['last_violation_conviction_date_c'] = array(
    'name' => 'last_violation_conviction_date_c',
    'vname' => 'LBL_LAST_VIOLATION_CONVICTION_DATE',
    'type' => 'date',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'enable_range_search' => true,
    'options' => 'date_range_search_dom',
);

$dictionary['Lead']['fields']['violation_followup_date_c'] = array(
    'name' => 'violation_followup_date_c',
    'vname' => 'LBL_VIOLATION_FOLLOWUP_DATE',
    'type' => 'date',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
    'enable_range_search' => true,
    'options' => 'date_range_search_dom',
);

$dictionary['Lead']['fields']['auto_followup_required_c'] = array(
    'name' => 'auto_followup_required_c',
    'vname' => 'LBL_AUTO_FOLLOWUP_REQUIRED',
    'type' => 'bool',
    'default' => '0',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);

$dictionary['Lead']['fields']['vin_decode_status_c'] = array(
    'name' => 'vin_decode_status_c',
    'vname' => 'LBL_VIN_DECODE_STATUS',
    'type' => 'varchar',
    'len' => 255,
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);

$dictionary['Lead']['fields']['lead_source_email_id_c'] = array(
    'name' => 'lead_source_email_id_c',
    'vname' => 'LBL_LEAD_SOURCE_EMAIL_ID',
    'type' => 'varchar',
    'len' => 36,
    'source' => 'custom_fields',
    'audited' => false,
    'reportable' => true,
);

$dictionary['Lead']['fields']['email_parse_confidence_c'] = array(
    'name' => 'email_parse_confidence_c',
    'vname' => 'LBL_EMAIL_PARSE_CONFIDENCE',
    'type' => 'enum',
    'options' => 'wit_parse_confidence_list',
    'len' => 25,
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);
