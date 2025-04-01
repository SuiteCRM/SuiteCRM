<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

$dictionary['EmailMan'] =
array( 'table' => 'emailman', 'comment' => 'Email campaign queue', 'fields' => array(
    'date_entered' => array(
        'name' => 'date_entered',
        'vname' => 'LBL_DATE_ENTERED',
        'type' => 'datetime',
        'comment' => 'Date record created',
        'enable_range_search' => true,
        'options' => 'date_range_search_dom',
    ),
    'date_modified' => array(
        'name' => 'date_modified',
        'vname' => 'LBL_DATE_MODIFIED',
        'type' => 'datetime',
        'comment' => 'Date record last modified',
        'enable_range_search' => true,
        'options' => 'date_range_search_dom',
    ),
    'user_id' => array(
        'name' => 'user_id',
        'vname' => 'LBL_USER_ID',
        'type' => 'id','len' => '36',
        'reportable' =>false,
        'comment' => 'User ID representing assigned-to user',
    ),
    'id' =>
    array(
        'name' => 'id',
        'vname' => 'LBL_ID',
        'type' => 'int',
        'len' => '11',
        'auto_increment'=>true,
        'comment' => 'Unique identifier',
    ),
    'campaign_id' => array(
        'name' => 'campaign_id',
        'vname' => 'LBL_CAMPAIGN_ID',
        'type' => 'id',
        'reportable' =>false,
        'comment' => 'ID of related campaign',
    ),
    'marketing_id' => array(
        'name' => 'marketing_id',
        'vname' => 'LBL_MARKETING_ID',
        'type' => 'id',
        'reportable' =>false,
        'comment' => '',
    ),
    'list_id' => array(
        'name' => 'list_id',
        'vname' => 'LBL_LIST_ID',
        'type' => 'id',
        'reportable' =>false,
        'len' => '36',
        'comment' => 'Associated list',
    ),
    'send_date_time' => array(
        'name' => 'send_date_time' ,
        'vname' => 'LBL_SEND_DATE_TIME',
        'type' => 'datetime',
    ),
    'modified_user_id' => array(
        'name' => 'modified_user_id',
        'vname' => 'LBL_MODIFIED_USER_ID',
        'type' => 'id',
        'reportable' =>false,
        'len' => '36',
        'comment' => 'User ID who last modified record',
    ),
    'in_queue' => array(
        'name' => 'in_queue',
        'vname' => 'LBL_IN_QUEUE',
        'type' => 'bool',
        'default' => '0',
        'comment' => 'Flag indicating if item still in queue',
    ),
    'in_queue_date' => array(
        'name' => 'in_queue_date',
        'vname' => 'LBL_IN_QUEUE_DATE',
        'type' => 'datetime',
        'comment' => 'Datetime in which item entered queue',
    ),
    'send_attempts' => array(
        'name' => 'send_attempts',
        'vname' => 'LBL_SEND_ATTEMPTS',
        'type' => 'int',
        'default' => '0',
        'comment' => 'Number of attempts made to send this item',
    ),
    'deleted' => array(
        'name' => 'deleted',
        'vname' => 'LBL_DELETED',
        'type' => 'bool',
        'reportable' =>false,
        'comment' => 'Record deletion indicator',
                'default' => '0',
    ),
    'related_id' => array(
        'name' => 'related_id',
        'vname' => 'LBL_RELATED_ID',
        'type' => 'id',
        'reportable' =>false,
        'comment' => 'ID of Sugar object to which this item is related',
    ),
    'related_type' => array(
        'name' => 'related_type' ,
        'vname' => 'LBL_RELATED_TYPE',
        'type' => 'varchar',
        'len' => '100',
        'comment' => 'Descriptor of the Sugar object indicated by related_id',
    ),
    
                'related_confirm_opt_in' => array(
                    'name' => 'related_confirm_opt_in',
                    'vname' => 'LBL_RELATED_CONFIRM_OPT_IN',
                    'type' => 'bool',
                    'default' => 0,
                    'reportable' => false,
                    'comment' => '',
                ),
    
                'recipient_name' => array(
        'name' => 'recipient_name',
        'type' => 'varchar',
        'len' => '255',
        'source'=>'non-db',
    ),
    'recipient_email' => array(
        'name' => 'recipient_email',
        'type' => 'varchar',
        'len' => '255',
        'source'=>'non-db',
    ),
    'message_name' => array(
        'name' => 'message_name',
        'type' => 'varchar',
        'len' => '255',
        'source'=>'non-db',
    ),
    'campaign_name' => array(
        'name' => 'campaign_name',
        'vname' => 'LBL_LIST_CAMPAIGN',
        'type' => 'varchar',
        'len' => '50',
        'source'=>'non-db',
    ),

), 'indices' => array(
                    array('name' => 'emailmanpk', 'type' => 'primary', 'fields' => array('id')),
                    array('name' => 'idx_eman_list', 'type' => 'index', 'fields' => array('list_id','user_id','deleted')),
                    array('name' => 'idx_eman_campaign_id', 'type' => 'index', 'fields' => array('campaign_id')),
                    array('name' => 'idx_eman_relid_reltype_id', 'type' => 'index', 'fields'=> array('related_id', 'related_type', 'campaign_id')),
                    )
);
