<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

$viewdefs['CalendarAccount'] = [
    'EditView' => [
        'templateMeta' => [
            'form' => [
                'buttons' => [
                    'SAVE',
                    'CANCEL',
                ]
            ],
            'maxColumns' => '2',
            'widths' => [
                ['label' => '10', 'field' => '30'],
                ['label' => '10', 'field' => '30']
            ],
            'javascript' => '
                <script type="text/javascript">
                    {suite_combinescripts
                        files="
                            modules/CalendarAccount/js/calendar_account_fields.js,
                            modules/CalendarAccount/js/auth_fields_visibility.js,
                            modules/CalendarAccount/js/sync_actions.js,
                            modules/CalendarAccount/js/editview-init.js
                        "}
                </script>
            ',
            'tabDefs' => [
                'DEFAULT' => [
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ],
                'LBL_SYNC_STATUS' => [
                    'newTab' => false,
                    'panelDefault' => 'expanded',
                ],
            ],
        ],
        'panels' => [
            'default' => [
                [
                    'name',
                    'source'
                ],
                [
                    'type',
                    'calendar_user_name'
                ],
                [
                    [
                        'name' => 'oauth_connection_name',
                        'displayParams' => [
                            'class' => 'auth-field oauth2-field',
                            'field_class' => 'auth-field oauth2-field'
                        ]
                    ]
                ],
                [
                    [
                        'name' => 'username',
                        'displayParams' => [
                            'class' => 'auth-field basic-field',
                            'field_class' => 'auth-field basic-field'
                        ]
                    ],
                    [
                        'name' => 'server_url',
                        'displayParams' => [
                            'class' => 'auth-field basic-field',
                            'field_class' => 'auth-field basic-field'
                        ]
                    ],
                ],
                [
                    [
                        'name' => 'password',
                        'displayParams' => [
                            'class' => 'auth-field basic-field',
                            'field_class' => 'auth-field basic-field'
                        ]
                    ],
                ],
                [
                    [
                        'name' => 'api_key',
                        'displayParams' => [
                            'class' => 'auth-field api-key-field',
                            'field_class' => 'auth-field api-key-field'
                        ]
                    ],
                    [
                        'name' => 'api_endpoint',
                        'displayParams' => [
                            'class' => 'auth-field api-key-field',
                            'field_class' => 'auth-field api-key-field'
                        ]
                    ]
                ],
                [
                    [
                        'name' => 'auth_buttons',
                        'customCode' => '
                            <div id="auth-buttons-container" style="padding: 10px 0;">
                                <input type="button" id="test-connection-btn" class="button auth-button"
                                       value="{$MOD.LBL_TEST_CONNECTION}" onclick="CalendarSync.testConnection()" style="display:none; margin-right: 10px;">
                            </div>
                        ',
                        'label' => 'LBL_AUTH_ACTIONS'
                    ]
                ],
            ],
            'LBL_SYNC_STATUS' => [
                [
                    [
                        'name' => 'last_connection_status',
                        'type' => 'readonly',
                    ],
                    [
                        'name' => 'last_connection_test',
                        'type' => 'readonly',
                    ],
                ],
                [
                    [
                        'name' => 'last_sync_attempt_status',
                        'type' => 'readonly',
                    ],
                    [
                        'name' => 'last_sync_attempt_date',
                        'type' => 'readonly',
                    ],
                ],
                [
                    [
                        'name' => 'last_sync_attempt_message',
                        'type' => 'readonly',
                    ],
                    [
                        'name' => 'last_sync_date',
                        'type' => 'readonly',
                    ],
                ],
            ],
        ],
    ]
];
