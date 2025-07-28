<?php
$viewdefs ['OutboundEmailAccounts'] = [
    'EditView' => [
        'templateMeta' => [
            'maxColumns' => '2',
            'widths' => [
                [
                    'label' => '10',
                    'field' => '30',
                ],
                [
                    'label' => '10',
                    'field' => '30',
                ],
            ],
            'useTabs' => false,
            'tabDefs' => [
                'DEFAULT' => [
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ],
                'LBL_CONNECTION_CONFIGURATION' => [
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ],
                'LBL_OUTBOUND_CONFIGURATION' => [
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ],
            ],
            'form' => [
                'hidden' => [
                ],
            ],
            'javascript' => '
                <script type="text/javascript">
                    {literal}var userService = function() { return { isAdmin: function() { return {/literal}{if $is_admin}true{else}false{/if}{literal};}}}();{/literal}
                    {suite_combinescripts
                        files="modules/OutboundEmailAccounts/js/fields.js,
                               modules/OutboundEmailAccounts/js/ssl_port_set.js,
                               modules/OutboundEmailAccounts/js/panel_toggle.js,
                               modules/OutboundEmailAccounts/js/owner_toggle.js,
                               modules/OutboundEmailAccounts/js/auth_type_fields_toggle.js"}
                </script>
            ',
        ],
        'panels' => [
            'default' => [
                [
                    'name',
                    ''
                ],
                [
                    'type',
                    ''
                ],
                [
                    'owner_name',
                ],
            ],
            'lbl_connection_configuration' => [
                [
                    'auth_type',
                    'mail_smtpuser',
                ],
                [
                    'mail_smtpserver',
                    'mail_smtppass',
                ],
                [
                    'mail_smtpssl',
                    [
                        'name' => 'external_oauth_connection_name',
                        'displayParams' => [
                            'initial_filter' => '{if $fields.type.value === "system"}&type="+"group"+"{/if}',
                        ],
                    ],
                ],
                [
                    'mail_smtpport',
                    '',
                ],
                [
                    [
                        'name' => 'sent_test_email_btn',
                        'label' => 'LBL_SEND_TEST_EMAIL',
                    ],
                ],
            ],
            'lbl_outbound_configuration' => [
                [
                    'smtp_from_name',
                    'reply_to_name'
                ],
                [
                    'smtp_from_addr',
                    'reply_to_addr'
                ],
                [
                    'signature',
                    ''
                ]
            ],
        ],
    ],
];
