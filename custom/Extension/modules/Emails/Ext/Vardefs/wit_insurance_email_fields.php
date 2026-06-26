<?php
// Tracks which imported lead emails have already been parsed into insurance leads.

$dictionary['Email']['fields']['wit_parsed_to_lead_c'] = array(
    'name' => 'wit_parsed_to_lead_c',
    'vname' => 'LBL_WIT_PARSED_TO_LEAD',
    'type' => 'bool',
    'default' => '0',
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);

$dictionary['Email']['fields']['wit_created_lead_id_c'] = array(
    'name' => 'wit_created_lead_id_c',
    'vname' => 'LBL_WIT_CREATED_LEAD_ID',
    'type' => 'varchar',
    'len' => 36,
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);

$dictionary['Email']['fields']['wit_email_parse_status_c'] = array(
    'name' => 'wit_email_parse_status_c',
    'vname' => 'LBL_WIT_EMAIL_PARSE_STATUS',
    'type' => 'varchar',
    'len' => 255,
    'source' => 'custom_fields',
    'audited' => true,
    'reportable' => true,
);
