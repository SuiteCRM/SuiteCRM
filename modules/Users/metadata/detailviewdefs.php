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

$viewdefs['Users']['DetailView'] = array(
    'templateMeta' =>
        array(
            'form' =>
                array(
                    /**
                     * Actions for users are configured in modules/Users/views/view.detail.php
                     * This is to control security access to the actions based on the user and system preferences.
                     * To customise in an upgrade safe way, You need to create custom view instead.
                     * Then override UsersViewDetail::preDisplay().
                     */
                    'buttons' => array(),
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
            'useTabs' => true,
            'tabDefs' =>
                array(
                    'LBL_USER_INFORMATION' =>
                        array(
                            'newTab' => true,
                            'panelDefault' => 'expanded',
                        ),
                    'LBL_EMPLOYEE_INFORMATION' =>
                        array(
                            'newTab' => false,
                            'panelDefault' => 'collapsed',
                        ),
                ),
        ),
    'panels' =>
        array(
            'LBL_USER_INFORMATION' =>
                array(
                    0 =>
                        array(
                            0 => 'full_name',
                            1 => 'user_name',
                        ),
                    1 =>
                        array(
                            0 => 'status',
                            1 =>
                                array(
                                    'name' => 'UserType',
                                    'customCode' => '{if isset($USER_TYPE_READONLY)}{$USER_TYPE_READONLY}{/if}',
                                ),
                        ),
                    2 =>
                        array(
                            0 => 'photo',
                        ),
                ),
            'LBL_EMPLOYEE_INFORMATION' =>
                array(
                    0 =>
                        array(
                            0 => 'employee_status',
                            1 => 'show_on_employees',
                        ),
                    1 =>
                        array(
                            0 => 'title',
                            1 => 'phone_work',
                        ),
                    2 =>
                        array(
                            0 => 'department',
                            1 => 'phone_mobile',
                        ),
                    3 =>
                        array(
                            0 => 'reports_to_name',
                            1 => 'phone_other',
                        ),
                    4 =>
                        array(
                            0 => '',
                            1 => 'phone_fax',
                        ),
                    5 =>
                        array(
                            0 => '',
                            1 => 'phone_home',
                        ),
                    6 =>
                        array(
                            0 => 'messenger_type',
                            1 => 'messenger_id',
                        ),
                    7 =>
                        array(
                            0 => 'address_street',
                            1 => 'address_city',
                        ),
                    8 =>
                        array(
                            0 => 'address_state',
                            1 => 'address_postalcode',
                        ),
                    9 =>
                        array(
                            0 => 'address_country',
                        ),
                    10 =>
                        array(
                            0 => 'description',
                        ),
                ),
        ),
);
