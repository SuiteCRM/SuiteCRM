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

/*
 * Created on Aug 2, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
 $module_name = 'AOS_Invoices';
 $_module_name = 'aos_invoices';
  $searchdefs[$module_name] = array(
                    'templateMeta' => array(
                            'maxColumns' => '3',
                            'widths' => array('label' => '10', 'field' => '30'),
                           ),
                    'layout' => array(
                                'basic_search' =>
                        array(
                          'name' =>
                          array(
                            'name' => 'name',
                            'default' => true,
                            'width' => '10%',
                          ),
                          'current_user_only' =>
                          array(
                            'name' => 'current_user_only',
                            'label' => 'LBL_CURRENT_USER_FILTER',
                            'type' => 'bool',
                            'default' => true,
                            'width' => '10%',
                          ),

                            'favorites_only' =>  array('name' => 'favorites_only','label' => 'LBL_FAVORITES_FILTER','type' => 'bool',),
                          
                        ),
                        'advanced_search' => array(
                            'name',
                            'billing_contact',
                            'billing_account',
                            'number',
                            'total_amount',
                            'due_date',
                            'status',
                            array('name' => 'assigned_user_id', 'type' => 'enum', 'label' => 'LBL_ASSIGNED_TO', 'function' => array('name' => 'get_user_array', 'params' => array(false))),
                            
                            
                        ),
                    ),
               );
