<?php
$popupMeta = [
    'moduleMain' => 'OutboundEmailAccount',
    'varName' => 'OutboundEmailAccount',
    'orderBy' => 'outboundemailaccount.name',
    'whereClauses' => [
        'mail_smtpuser' => 'outbound_email.mail_smtpuser',
        'mail_smtpserver' => 'outbound_email.mail_smtpserver',
    ],
    'searchInputs' => [
        4 => 'mail_smtpuser',
        5 => 'mail_smtpserver',
    ],
    'searchdefs' => [
        'mail_smtpuser' => [
            'type' => 'varchar',
            'label' => 'LBL_USERNAME',
            'width' => '10%',
            'name' => 'mail_smtpuser',
        ],
        'mail_smtpserver' => [
            'type' => 'varchar',
            'label' => 'LBL_SMTP_SERVERNAME',
            'width' => '10%',
            'name' => 'mail_smtpserver',
        ],
    ],
    'listviewdefs' => [
        'NAME' => [
            'width' => '40%',
            'label' => 'LBL_LIST_ACCOUNT_NAME',
            'link' => true,
            'default' => true,
            'name' => 'name',
        ],
        'MAIL_SMTPUSER' => [
            'type' => 'varchar',
            'label' => 'LBL_USERNAME',
            'width' => '10%',
            'default' => true,
            'link' => true,
            'name' => 'mail_smtpuser',
        ],
        'MAIL_SMTPSERVER' => [
            'type' => 'varchar',
            'label' => 'LBL_SMTP_SERVERNAME',
            'width' => '10%',
            'default' => true,
        ],
    ],
];
