<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$dictionary['PaymentBridge'] = array(
    'table' => 'payment_bridge',
    'audited' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'card_number_masked' => array(
            'name' => 'card_number_masked',
            'vname' => 'LBL_CARD_NUMBER_MASKED',
            'type' => 'varchar',
            'len' => '20',
            'audited' => true,
            'comment' => 'Masked credit card number'
        ),
        'validation_status' => array(
            'name' => 'validation_status',
            'vname' => 'LBL_VALIDATION_STATUS',
            'type' => 'enum',
            'options' => 'payment_validation_status_list',
            'len' => '20',
            'audited' => true,
            'comment' => 'Card validation status'
        ),
        'credit_limit' => array(
            'name' => 'credit_limit',
            'vname' => 'LBL_CREDIT_LIMIT',
            'type' => 'currency',
            'len' => '26,6',
            'audited' => true,
            'comment' => 'Credit limit from COBOL system'
        ),
        'current_balance' => array(
            'name' => 'current_balance',
            'vname' => 'LBL_CURRENT_BALANCE',
            'type' => 'currency',
            'len' => '26,6',
            'audited' => true,
            'comment' => 'Current balance from COBOL system'
        ),
        'apr' => array(
            'name' => 'apr',
            'vname' => 'LBL_APR',
            'type' => 'decimal',
            'len' => '5,2',
            'comment' => 'Annual Percentage Rate'
        ),
        'last_statement_date' => array(
            'name' => 'last_statement_date',
            'vname' => 'LBL_LAST_STATEMENT_DATE',
            'type' => 'date',
            'audited' => true,
            'comment' => 'Date of last statement generation'
        ),
        'last_validation_date' => array(
            'name' => 'last_validation_date',
            'vname' => 'LBL_LAST_VALIDATION_DATE',
            'type' => 'datetime',
            'audited' => true,
            'comment' => 'Last card validation timestamp'
        ),
        'payment_gateway_status' => array(
            'name' => 'payment_gateway_status',
            'vname' => 'LBL_PAYMENT_GATEWAY_STATUS',
            'type' => 'enum',
            'options' => 'payment_gateway_status_list',
            'len' => '20',
            'comment' => 'Status of payment gateway connection'
        ),
        'cobol_system_health' => array(
            'name' => 'cobol_system_health',
            'vname' => 'LBL_COBOL_SYSTEM_HEALTH',
            'type' => 'text',
            'comment' => 'JSON data of COBOL system health metrics'
        ),
        'interest_charge' => array(
            'name' => 'interest_charge',
            'vname' => 'LBL_INTEREST_CHARGE',
            'type' => 'currency',
            'len' => '26,6',
            'comment' => 'Monthly interest charge'
        ),
        'minimum_payment' => array(
            'name' => 'minimum_payment',
            'vname' => 'LBL_MINIMUM_PAYMENT',
            'type' => 'currency',
            'len' => '26,6',
            'comment' => 'Minimum payment due'
        ),
        'payment_plan_status' => array(
            'name' => 'payment_plan_status',
            'vname' => 'LBL_PAYMENT_PLAN_STATUS',
            'type' => 'enum',
            'options' => 'payment_plan_status_list',
            'len' => '20',
            'comment' => 'Status of payment plan'
        ),
        'account_id' => array(
            'name' => 'account_id',
            'vname' => 'LBL_ACCOUNT_ID',
            'type' => 'id',
            'comment' => 'Related Account ID'
        ),
        'account_name' => array(
            'name' => 'account_name',
            'vname' => 'LBL_ACCOUNT_NAME',
            'type' => 'relate',
            'table' => 'accounts',
            'module' => 'Accounts',
            'rname' => 'name',
            'id_name' => 'account_id',
            'link' => 'accounts',
            'isnull' => 'true'
        ),
        'accounts' => array(
            'name' => 'accounts',
            'type' => 'link',
            'relationship' => 'account_paymentbridge',
            'module' => 'Accounts',
            'bean_name' => 'Account',
            'source' => 'non-db',
            'vname' => 'LBL_ACCOUNTS',
        )
    ),
    'relationships' => array(
        'account_paymentbridge' => array(
            'lhs_module' => 'Accounts',
            'lhs_table' => 'accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'PaymentBridge',
            'rhs_table' => 'payment_bridge',
            'rhs_key' => 'account_id',
            'relationship_type' => 'one-to-many'
        )
    ),
    'indices' => array(
        array('name' => 'idx_payment_bridge_account', 'type' => 'index', 'fields' => array('account_id')),
        array('name' => 'idx_payment_bridge_card', 'type' => 'index', 'fields' => array('card_number_masked')),
        array('name' => 'idx_payment_bridge_status', 'type' => 'index', 'fields' => array('validation_status'))
    )
);