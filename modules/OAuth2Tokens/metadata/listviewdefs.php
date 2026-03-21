<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

$module_name = 'OAuth2Tokens';

$viewdefs[$module_name]['ListView'] = [
    'templateMeta' => [
        'form' => [
            'actions' => [
                [
                    'customCode' => '<a href="javascript:void(0)" class="parent-dropdown-handler" id="delete_listview_top" onclick="return false;"><label class="selected-actions-label hidden-mobile">{$APP.LBL_BULK_ACTION_BUTTON_LABEL_MOBILE}</label><label class="selected-actions-label hidden-desktop">{$APP.LBL_BULK_ACTION_BUTTON_LABEL}<span class=\'suitepicon suitepicon-action-caret\'></span></label></a>',
                ],
                [
                    'customCode' => '<a onclick="bulkRevokeTokens()" title="{$MOD.LBL_REVOKE_TOKENS}">{$MOD.LBL_REVOKE_TOKENS}</a>'
                ],
                [
                    'customCode' => '<input class="button" type="button" id="delete_button" name="Delete" value="{$MOD.LBL_REVOKE_AND_DELETE_BUTTON_LABEL}" onclick="return sListView.send_mass_update(\'selected\',\'{$APP.LBL_LISTVIEW_NO_SELECTED}\', 1)">',
                ]
            ],
        ],
        'includes' => [
            [
                'file' => 'modules/OAuth2Tokens/include/RevokeBulk.js',
            ],
        ],
        'options' => [
            'hide_edit_link' => true,
        ]
    ]
];

$listViewDefs[$module_name] = [
    'id' => [
        'label' => 'LBL_TOKEN_ID',
        'default' => true,
        'link' => true,
        'sortable' => true,
    ],
    'oauth2client_name' => [
        'label' => 'LBL_CLIENT',
        'module' => 'OAuth2Clients',
        'id' => 'CLIENT',
        'link' => true,
        'default' => true,
        'sortable' => true,
        'related_fields' => ['client']
    ],
    'assigned_user_name' => [
        'label' => 'LBL_USER',
        'module' => 'Users',
        'id' => 'USER_ID',
        'default' => true,
        'sortable' => true,
    ],
    'token_is_revoked' => [
        'label' => 'LBL_TOKEN_IS_REVOKED',
        'default' => true,
        'sortable' => true,
    ],
    'date_entered' => [
        'label' => 'LBL_DATE_ENTERED',
        'default' => true,
        'sortable' => true,
    ],
    'access_token_expires' => [
        'label' => 'LBL_ACCESS_TOKEN_EXPIRES',
        'default' => true,
        'sortable' => true,
    ],
    'refresh_token_expires' => [
        'label' => 'LBL_REFRESH_TOKEN_EXPIRES',
        'default' => true,
        'sortable' => true,
    ],
    'revoke_and_delete' => [
        'default' => true,
        'sortable' => false,
        'link' => true,
        'align' => 'right',
        'customCode' => '<button class="btn btn-sm btn-outline" style="padding:0; background-color: transparent; height: auto" onclick="if(confirm(\'{$MOD.LBL_DELETE_CONFIRMATION}\')) SUGAR.ajaxUI.go(\'index.php?module=OAuth2Tokens&action=Delete&record={$ID}&return_module=OAuth2Tokens\'); return false;" title="{$MOD.LBL_REVOKE_AND_DELETE_BUTTON_LABEL}"><span class="suitepicon suitepicon-action-delete" style="font-size: 1.1em"> </span></button>'
    ],
];
