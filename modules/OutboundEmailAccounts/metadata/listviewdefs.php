<?php
$module_name = 'OutboundEmailAccounts';
$listViewDefs [$module_name] = [
    'NAME' => [
        'width' => '32%',
        'label' => 'LBL_NAME',
        'default' => true,
        'link' => true,
    ],
    'MAIL_SMTPUSER' => [
        'type' => 'varchar',
        'label' => 'LBL_USERNAME',
        'width' => '10%',
        'default' => true,
    ],
    'TYPE' => [
        'label' => 'LBL_TYPE',
        'width' => '10%',
        'default' => true,
    ],
    'MAIL_SMTPSERVER' => [
        'type' => 'varchar',
        'label' => 'LBL_SMTP_SERVERNAME',
        'width' => '10%',
        'default' => true,
    ],
];
