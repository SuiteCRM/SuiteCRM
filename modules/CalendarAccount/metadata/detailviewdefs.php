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

$module_name = 'CalendarAccount';
$viewdefs[$module_name]['DetailView'] = [
    'templateMeta' => [
        'form' => [
            'buttons' => [
                'EDIT',
                'DELETE',
                [
                    'customCode' => '<input type="button" name="sync_now" id="sync_now_button" class="button" value="{$MOD.LBL_SYNC_NOW}" title="{$MOD.LBL_SYNC_NOW_TITLE}" onclick="CalendarSync.sync(\'{$fields.id.value}\', \'{$fields.last_connection_status.value}\');" />',
                ],
            ]
        ],
        'maxColumns' => '2',
        'widths' => [
            ['label' => '10', 'field' => '30'],
            ['label' => '10', 'field' => '30']
        ],
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
        'javascript' => '
            <script type="text/javascript">
                {suite_combinescripts
                    files="
                        modules/CalendarAccount/js/calendar_account_fields.js,
                        modules/CalendarAccount/js/auth_fields_visibility.js,
                        modules/CalendarAccount/js/sync_actions.js,
                        modules/CalendarAccount/js/detailview_init.js
                    "}
            </script>
        ',
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
                'oauth_connection_name'
            ],
            [
                'username',
                'server_url',
            ],
            [
                'api_key',
                'api_endpoint'
            ],
            [
                'date_entered',
                'date_modified'
            ],
        ],
        'LBL_SYNC_STATUS' => [
            [
                'last_connection_status',
                'last_connection_test',
            ],
            [
                'last_sync_attempt_status',
                'last_sync_attempt_date',
            ],
            [
                'last_sync_attempt_message',
                'last_sync_date',
            ],
        ],
    ]
];
