<?php
$viewdefs['Leads'] = array(
    'DetailView' => array(
        'templateMeta' => array(
            'form' => array('buttons' => array(0 => 'EDIT', 1 => 'DUPLICATE', 2 => 'DELETE', 3 => 'FIND_DUPLICATES')),
            'maxColumns' => '2',
            'widths' => array(array('label' => '10', 'field' => '30'), array('label' => '10', 'field' => '30')),
            'useTabs' => true,
            'tabDefs' => array(
                'LBL_CONTACT_INFORMATION' => array('newTab' => true, 'panelDefault' => 'expanded'),
                'LBL_WIT_INSURANCE_REQUEST' => array('newTab' => true, 'panelDefault' => 'expanded'),
                'LBL_WIT_DRIVERS_VEHICLES' => array('newTab' => true, 'panelDefault' => 'expanded'),
                'LBL_WIT_ACCIDENTS_VIOLATIONS' => array('newTab' => true, 'panelDefault' => 'expanded'),
                'LBL_WIT_NEXT_ACTIONS' => array('newTab' => true, 'panelDefault' => 'expanded'),
                'LBL_PANEL_ASSIGNMENT' => array('newTab' => true, 'panelDefault' => 'expanded'),
            ),
        ),
        'panels' => array(
            'LBL_CONTACT_INFORMATION' => array(
                array(array('name' => 'full_name', 'label' => 'LBL_NAME'), 'phone_mobile'),
                array('email1', 'phone_work'),
                array('lead_source', 'status'),
                array(array('name' => 'primary_address_street', 'label' => 'LBL_PRIMARY_ADDRESS', 'type' => 'address', 'displayParams' => array('key' => 'primary'))),
            ),
            'LBL_WIT_INSURANCE_REQUEST' => array(
                array('policy_type_c', 'coverage_limits_c'),
                array('lead_source_email_id_c', 'email_parse_confidence_c'),
            ),
            'LBL_WIT_DRIVERS_VEHICLES' => array(
                array('drivers_json_c'),
                array('vehicles_json_c'),
                array('vin_decode_status_c'),
            ),
            'LBL_WIT_ACCIDENTS_VIOLATIONS' => array(
                array('accidents_violations_json_c'),
                array('last_violation_conviction_date_c', 'violation_followup_date_c'),
                array('auto_followup_required_c'),
            ),
            'LBL_WIT_NEXT_ACTIONS' => array(
                array('next_action_c', 'next_follow_up_date_c'),
                array('call_summary_c'),
                array('missing_info_c'),
                array('description'),
            ),
            'LBL_PANEL_ASSIGNMENT' => array(
                array(array('name' => 'assigned_user_name', 'label' => 'LBL_ASSIGNED_TO'), 'date_modified'),
                array('date_entered'),
            ),
        ),
    ),
);
