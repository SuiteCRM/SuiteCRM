<?php
$viewdefs['Leads'] = array(
    'EditView' => array(
        'templateMeta' => array(
            'form' => array(
                'hidden' => array(
                    0 => '<input type="hidden" name="prospect_id" value="{if isset($smarty.request.prospect_id)}{$smarty.request.prospect_id}{else}{$bean->prospect_id}{/if}">',
                    1 => '<input type="hidden" name="account_id" value="{if isset($smarty.request.account_id)}{$smarty.request.account_id}{else}{$bean->account_id}{/if}">',
                    2 => '<input type="hidden" name="contact_id" value="{if isset($smarty.request.contact_id)}{$smarty.request.contact_id}{else}{$bean->contact_id}{/if}">',
                    3 => '<input type="hidden" name="opportunity_id" value="{if isset($smarty.request.opportunity_id)}{$smarty.request.opportunity_id}{else}{$bean->opportunity_id}{/if}">',
                ),
                'buttons' => array('SAVE', 'CANCEL'),
            ),
            'maxColumns' => '2',
            'widths' => array(array('label' => '10', 'field' => '30'), array('label' => '10', 'field' => '30')),
            'includes' => array(array('file' => 'custom/modules/Leads/javascript/wit_insurance_lead.js')),
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
                array('first_name', 'last_name'),
                array('email1', 'phone_mobile'),
                array('phone_work', 'lead_source'),
                array(array('name' => 'primary_address_street', 'hideLabel' => true, 'type' => 'address', 'displayParams' => array('key' => 'primary', 'rows' => 2, 'cols' => 30, 'maxlength' => 150))),
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
                array(array('name' => 'assigned_user_name', 'label' => 'LBL_ASSIGNED_TO'), 'status'),
            ),
        ),
    ),
);
