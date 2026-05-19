<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
$module_name = 'stic_Messages';
$viewdefs[$module_name] =
array(
    'EditView' => array(
        'templateMeta' => array(
            'maxColumns' => '2',
            'widths' => array(
                0 => array(
                    'label' => '10',
                    'field' => '30',
                ),
                1 => array(
                    'label' => '10',
                    'field' => '30',
                ),
            ),
            'useTabs' => true,
            'tabDefs' => array(
                'LBL_DEFAULT_PANEL' => array(
                    'newTab' => true,
                    'panelDefault' => 'expanded',
                ),
            ),
            'syncDetailEditViews' => false,
        ),
        'panels' => array(
            'lbl_default_panel' => array(
                0 => array(
                    0 => 'name',
                    1 => 'assigned_user_name',
                ),
                1 => array(
                    0 => array(
                        'name' => 'parent_name',
                        'label' => 'LBL_LIST_RELATED_TO',
                    ),
                    1 => array (
                        'name' => 'phone',
                        'studio' => 'visible',
                        'label' => 'LBL_PHONE',
                    ),
                ),
                2 => array(
                    0 => array(
                        'name' => 'sender',
                        'comment' => 'Sender',
                        'label' => 'LBL_SENDER',
                    ),
                    1 => array(
                        'name' => 'template_id',
                        'studio' => 'visible',
                        'label' => 'LBL_TEMPLATE',
                    ),
                ),
                3 => array(
                    0 => array(
                        'name' => 'info',
                    ),
                ),
                4 => array(
                    0 => array(
                        'name' => 'message',
                        'comment' => 'Full text of the message',
                        'label' => 'LBL_MESSAGE',
                    ),
                ),
                5 => array(
                    0 => array(
                        'name' => 'type',
                        'studio' => 'visible',
                        'label' => 'LBL_TYPE',
                    ),
                    1 => array(
                        'name' => 'status',
                        'studio' => 'visible',
                        'label' => 'LBL_STATUS',
                    ),
                ),
                6 => array(
                    0 => array(
                        'name' => 'new_conversation',
                        'label' => 'LBL_NEW_CONVERSATION',
                    ),
                    1 => array(
                        'name' => 'stic_conversations_stic_messages_name',
                    ),
                ),
                7 => array(
                    0 => array(
                        'name' => 'stic_conversation_subject',
                    ),
                ),
            ),
        ),
    ),
);
