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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$module_name = 'OAuth2Clients';

$viewdefs[$module_name]['EditView'] = [
    'templateMeta' => [
        'maxColumns' => '1',
        'widths' => [
            ['label' => '30', 'field' => '70'],
        ],
        'includes' => [
            [
                'file' => 'modules/OAuth2Clients/js/ClientCredentialsValidation.js'
            ]
        ],
    ],
    'panels' => [
        'default' =>
        [
            0 =>
            [
                'name' => 'name',
            ],
            1 =>
            [
                0 =>
                [
                    'name' => 'secret',
                    'label' => 'LBL_SECRET_HASHED',
                    'customCode' => '<input type="password" name="new_secret" id="new_secret" placeholder="{$MOD.LBL_LEAVE_BLANK}" size="30">'
                    . '<input type="hidden" name="allowed_grant_type" id="allowed_grant_type" value="authorization_code">'
                    . '<br /><span>{$MOD.LBL_REMEMBER_SECRET}</span>',
                ],
            ],
            2 =>
            [
                'name' => 'redirect_url'
            ],
            3 =>
            [
                'name' => 'is_confidential',
            ],
        ],
    ],
];
