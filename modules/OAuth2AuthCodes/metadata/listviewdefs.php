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

$module_name = 'OAuth2AuthCodes';

$viewdefs[$module_name]['ListView'] = [
    'templateMeta' => [
        'form' => [
            'actions' => [
                [
                    'customCode' => '<a href="javascript:void(0)" class="parent-dropdown-handler" id="delete_listview_top" onclick="return false;"><label class="selected-actions-label hidden-mobile">{$APP.LBL_BULK_ACTION_BUTTON_LABEL_MOBILE}</label><label class="selected-actions-label hidden-desktop">{$APP.LBL_BULK_ACTION_BUTTON_LABEL}<span class=\'suitepicon suitepicon-action-caret\'></span></label></a>',
                ],
                [
                    'customCode' => '<input class="button" type="button" id="delete_button" name="Delete" value="{$MOD.LBL_DELETE}" onclick="return sListView.send_mass_update(\'selected\',\'{$APP.LBL_LISTVIEW_NO_SELECTED}\', 1)">',
                ]
            ],
        ],
        'options' => [
            'hide_edit_link' => true,
        ]
    ]
];

$listViewDefs[$module_name] = [
    'oauth2client_name' => [
        'label' => 'LBL_CLIENT',
        'default' => true,
        'link' => false,
    ],
    'auto_authorize' => [
        'label' => 'LBL_AUTO_AUTHORIZE',
        'default' => true,
    ],
    'assigned_user_name' => [
        'label' => 'LBL_USER',
        'module' => 'Users',
        'id' => 'USER_ID',
        'default' => true,
        'sortable' => true,
    ],
    'date_entered' => [
        'label' => 'LBL_DATE_ENTERED',
        'default' => true,
        'sortable' => true,
    ],
    'auth_code_expires' => [
        'label' => 'LBL_AUTH_CODE_EXPIRES',
        'default' => true,
        'sortable' => true,
    ],
    'revoke_and_delete' => [
        'default' => true,
        'sortable' => false,
        'link' => true,
        'align' => 'right',
        'customCode' => '<button class="btn btn-sm btn-outline" style="padding:0; background-color: transparent; height: auto" onclick="if(confirm(\'{$MOD.LBL_DELETE_CONFIRMATION}\')) SUGAR.ajaxUI.go(\'index.php?module=OAuth2AuthCodes&action=Delete&record={$ID}&return_module=OAuth2AuthCodes\'); return false;" title="{$MOD.LBL_DELETE_BUTTON_LABEL}"><span class="suitepicon suitepicon-action-delete" style="font-size: 1.1em"> </span></button>'
    ],
];
