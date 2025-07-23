<?php
$viewdefs ['OutboundEmailAccounts'] = [
    'DetailView' => [
        'templateMeta' => [
            'form' => [
                'buttons' => [
                    'EDIT',
                    'DELETE',
                ],
            ],
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
                'LBL_EDITVIEW_PANEL1' => [
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ],
            ],
            'preForm' => '
                <script type="text/javascript">
                    {literal}var userService = function() { return { isAdmin: function() { return {/literal}{if $is_admin}true{else}false{/if}{literal};}}}();{/literal}
                    {suite_combinescripts
                        files="modules/OutboundEmailAccounts/js/fields.js,
                               modules/OutboundEmailAccounts/js/owner_toggle.js,
                               modules/OutboundEmailAccounts/js/panel_toggle.js,
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
                    'external_oauth_connection_name',
                ],
                [
                    'mail_smtpssl',
                    '',
                ],
                [
                    'mail_smtpport',
                    ''
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
