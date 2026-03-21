<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
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

$viewdefs ['AOW_WorkFlow'] =
    array(
        'DetailView' =>
        array(
            'templateMeta' =>
            array(
                'form' =>
                array(
                    'buttons' =>
                    array(
                        0 => 'EDIT',
                        1 => 'DUPLICATE',
                        2 => 'DELETE',
                        3 => 'FIND_DUPLICATES',
                    ),
                ),
                'maxColumns' => '2',
                'widths' =>
                array(
                    0 =>
                    array(
                        'label' => '10',
                        'field' => '30',
                    ),
                    1 =>
                    array(
                        'label' => '10',
                        'field' => '30',
                    ),
                ),
                'useTabs' => false,
                'tabDefs' =>
                array(
                    'DEFAULT' =>
                    array(
                        'newTab' => false,
                        'panelDefault' => 'expanded',
                    ),
                    'CONDITIONS' =>
                    array(
                        'newTab' => false,
                        'panelDefault' => 'expanded',
                    ),
                    'ACTIONS' =>
                    array(
                        'newTab' => false,
                        'panelDefault' => 'expanded',
                    ),
                ),
            ),
            'panels' =>
            array(
                'default' =>
                array(
                    0 =>
                    array(
                        0 => 'name',
                        1 => 'assigned_user_name',
                    ),
                    1 =>
                    array(
                        0 =>
                        array(
                            'name' => 'flow_module',
                            'studio' => 'visible',
                            'label' => 'LBL_FLOW_MODULE',
                        ),
                        1 =>
                        array(
                            'name' => 'status',
                            'studio' => 'visible',
                            'label' => 'LBL_STATUS',
                        ),
                    ),
                    2 =>
                    array(
                        0 =>
                        array(
                            'name' => 'run_when',
                            'label' => 'LBL_RUN_WHEN',
                        ),
                        1 =>
                            array(
                                'name' => 'flow_run_on',
                                'studio' => 'visible',
                                'label' => 'LBL_FLOW_RUN_ON',
                            ),
                    ),
                    3 =>
                    array(
                        0 =>
                        array(
                            'name' => 'multiple_runs',
                            'label' => 'LBL_MULTIPLE_RUNS',
                        ),
                        1 =>
                        array(
                            'name' => 'run_on_import',
                            'label' => 'LBL_RUN_ON_IMPORT',
                        ),
                    ),
                    4 =>
                    array(
                        0 => 'description',
                    ),
                    5 =>
                    array(
                        0 =>
                        array(
                            'name' => 'date_entered',
                            'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
                            'label' => 'LBL_DATE_ENTERED',
                        ),
                        1 =>
                        array(
                            'name' => 'date_modified',
                            'customCode' => '{$fields.date_modified.value} {$APP.LBL_BY} {$fields.modified_by_name.value}',
                            'label' => 'LBL_DATE_MODIFIED',
                        ),
                    ),
                ),
                'LBL_CONDITION_LINES' =>
                array(
                    0 =>
                    array(
                        0 => 'condition_lines',
                    ),
                ),
                'LBL_ACTION_LINES' =>
                array(
                    0 =>
                    array(
                        0 => 'action_lines',
                    ),
                ),
            ),
        ),
    );
