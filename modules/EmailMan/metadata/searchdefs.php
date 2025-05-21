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

/*
 * Created on May 29, 2007
 *
 * To change the template for this generated file go to
 * Window - Preferences - PHPeclipse - PHP - Code Templates
 */
  $searchdefs['EmailMan'] = array(
                    'templateMeta' => array(
                            'maxColumns' => '3', 'maxColumnsBasic' => '4',
                            'widths' => array('label' => '10', 'field' => '30'),
                           ),
                    'layout' => array(
                        'basic_search' => array(
                            array('name'=>'campaign_name', 'label'=>'LBL_LIST_CAMPAIGN'),
                            array('name'=>'to_name', 'label'=>'LBL_LIST_RECIPIENT_NAME'),
                            array('name'=>'to_email', 'label'=>'LBL_LIST_RECIPIENT_EMAIL'),
                            array('name'=>'message_name', 'label'=>'LBL_LIST_MESSAGE_NAME'),
                            array('name'=>'send_date_time', 'label'=>'LBL_SEND_DATE_TIME'),                        
                            array('name'=>'send_attempts', 'label'=>'LBL_SEND_ATTEMPTS'),
                            array('name'=>'in_queue', 'label'=>'LBL_IN_QUEUE'),
                        ),
                        'advanced_search' => array(
                            array('name'=>'campaign_name', 'label'=>'LBL_LIST_CAMPAIGN'),
                            array('name'=>'to_name', 'label'=>'LBL_LIST_RECIPIENT_NAME'),
                            array('name'=>'to_email', 'label'=>'LBL_LIST_RECIPIENT_EMAIL'),
                            array('name'=>'message_name', 'label'=>'LBL_LIST_MESSAGE_NAME'),
                            array('name'=>'send_date_time', 'label'=>'LBL_SEND_DATE_TIME'),
                            array('name'=>'send_attempts', 'label'=>'LBL_SEND_ATTEMPTS'),
                            array('name'=>'in_queue', 'label'=>'LBL_IN_QUEUE'),
                            array('name'=>'current_user_only', 'label'=>'LBL_CURRENT_USER_FILTER', 'type'=>'bool'),

                            array('name' => 'date_entered', 'label' => 'LBL_DATE_ENTERED' ),
                            array('name' => 'date_modified', 'label' => 'LBL_DATE_MODIFIED' ),
                            array('name' => 'modified_user_id', 'label' => 'LBL_MODIFIED_USER'),
                            array('name' => 'user_id', 'label' => 'LBL_USER_ID' ),
                        ),
                    ),
               );
