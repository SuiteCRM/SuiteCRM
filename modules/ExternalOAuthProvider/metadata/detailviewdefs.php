<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

$module_name = 'ExternalOAuthProvider';
$viewdefs[$module_name]['DetailView'] = [
    'templateMeta' => [
        'form' => [
            'buttons' => [
                'EDIT',
                'DELETE',
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
            'LBL_EXTRA' => [
                'newTab' => false,
                'panelDefault' => 'expanded',
            ],
            'LBL_MAPPING' => [
                'newTab' => false,
                'panelDefault' => 'expanded',
            ],
            'LBL_OTHER' => [
                'newTab' => false,
                'panelDefault' => 'expanded',
            ],
        ],
    ],
    'panels' => [
        'default' => [
            [
                'type',
                'connector'
            ],
            [
                'name',
                'scope'
            ],
            [
                'client_id',
                ''
            ],
            [
                'url_authorize',
                'url_access_token'
            ],
            [
                'authorize_url_options',
                ''
            ],
            [
                'redirect_uri',
                'redirect_uri_type'
            ],
        ],
        'lbl_extra' => [
            [
                'extra_provider_params',
                ''
            ],
            [
                'get_token_request_grant',
                'get_token_request_options'
            ],
            [
                'refresh_token_request_grant',
                'refresh_token_request_options'
            ],
        ],
        'lbl_mapping' => [
            [
                'access_token_mapping',
                'expires_in_mapping'
            ],
            [
                'refresh_token_mapping',
                'token_type_mapping'
            ],
        ],
        'lbl_other' => [
            [
                [
                    'name' => 'date_entered',
                    'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
                    'label' => 'LBL_DATE_ENTERED',
                ],
                [
                    'name' => 'date_modified',
                    'customCode' => '{$fields.date_modified.value} {$APP.LBL_BY} {$fields.modified_by_name.value}',
                    'label' => 'LBL_DATE_MODIFIED',
                ],
            ],
            [
                [
                    'name' => 'created_by_name',
                    'customCode' => '{$fields.created_by_name.value}',
                    'label' => 'LBL_OWNER',
                ],
                ''
            ]
        ],
    ],
];
