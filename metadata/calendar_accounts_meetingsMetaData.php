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

$dictionary['calendar_account_meetings'] = array(
    'true_relationship_type' => 'many-to-many',
    'relationships' => array(
        'calendar_account_meetings' => array(
            'lhs_module' => 'CalendarAccount',
            'lhs_table' => 'calendar_accounts',
            'lhs_key' => 'id',
            'rhs_module' => 'Meetings',
            'rhs_table' => 'meetings',
            'rhs_key' => 'id',
            'relationship_type' => 'one-to-many',
            'join_table' => 'calendar_account_meetings',
            'join_key_lhs' => 'calendar_account_id',
            'join_key_rhs' => 'meeting_id',
        ),
    ),
    'table' => 'calendar_account_meetings',
    'fields' => array(
        0 => array(
            'name' => 'id',
            'type' => 'varchar',
            'len' => '36'
        ),
        1 => array(
            'name' => 'calendar_account_id',
            'type' => 'varchar',
            'len' => '36',
        ),
        2 => array(
            'name' => 'meeting_id',
            'type' => 'varchar',
            'len' => '36',
        ),
        3 => array(
            'name' => 'calendar_account_source',
            'type' => 'varchar',
            'len' => '255',
            'default' => null,
            'required' => false
        ),
        4 => array(
            'name' => 'external_event_id',
            'type' => 'varchar',
            'len' => '255',
            'default' => null,
            'required' => false
        ),
        5 => array(
            'name' => 'last_sync',
            'type' => 'datetime',
            'default' => null,
            'required' => false
        ),
        6 => array(
            'name' => 'date_modified',
            'type' => 'datetime'
        ),
        7 => array(
            'name' => 'deleted',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            'required' => false
        ),
    ),
    'indices' => array(
        0 => array(
            'name' => 'calendar_accounts_meetingspk',
            'type' => 'primary',
            'fields' => array(0 => 'id'),
        ),
        1 => array(
            'name' => 'idx_cal_acc_mtg_cal',
            'type' => 'index',
            'fields' => array(0 => 'calendar_account_id'),
        ),
        2 => array(
            'name' => 'idx_cal_acc_mtg_mtg',
            'type' => 'index',
            'fields' => array(0 => 'meeting_id'),
        ),
        3 => array(
            'name' => 'idx_calendar_account_meeting',
            'type' => 'unique',
            'fields' => array(0 => 'calendar_account_id', 1 => 'meeting_id'),
        ),
    ),
);
