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

$dictionary['Tracker'] = array(
    'table' => 'tracker',
    'fields' => array(
        'id'=>array(
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'int',
            'len' => '11',
            'isnull' => 'false',
            'auto_increment' => true,
            'reportable'=>true,
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'monitor_id'=>array(
            'name' => 'monitor_id',
            'vname' => 'LBL_MONITOR_ID',
            'type' => 'id',
            'required'=>true,
            'reportable'=>false,
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'user_id'=>array(
            'name' => 'user_id',
            'vname' => 'LBL_USER_ID',
            'type' => 'varchar',
            'len' => '36',
            'isnull' => 'false',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'module' => 'Users',
            'id' => 'user_id',
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'module_name'=>array(
            'name' => 'module_name',
            'vname' => 'LBL_MODULE_NAME',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // 'type' => 'varchar',
            // We add the properties, since in extension does not pick them up and then they disappear 
            'type' => 'enum',
            'options' => 'moduleList',
            'inline_edit' => 0,
            // END STIC-Custom
            'len' => '255',
            'isnull' => 'false',
        ),
        'item_id'=>array(
            'name' => 'item_id',
            'vname' => 'LBL_ITEM_ID',
            'type' => 'varchar',
            'len' => '36',
            'isnull' => 'false',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'item_summary'=>array(
            'name' => 'item_summary',
            'vname' => 'LBL_ITEM_SUMMARY',
            'len' => '255',
            'isnull' => 'false',
            // STIC-Custom 20250613 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // 'type' => 'varchar',
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            'type' => 'html',
            // END STIC-Custom
        ),
        'date_modified'=>array(
            'name' => 'date_modified',
            'vname' => 'LBL_DATE_LAST_ACTION',
            'type' => 'datetime',
            'isnull' => 'false',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'options' => 'date_range_search_dom',
            'enable_range_search' => true,
            'dbType' => 'datetimecombo',
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'action'=>array(
            'name' => 'action',
            'vname' => 'LBL_ACTION',
            'len' => '255',
            'isnull' => 'false',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // 'type' => 'varchar',
            // We add the properties, since in extension does not pick them up and then they disappear 
            'type' => 'enum',
            'options' => 'trackers_actions_list',
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'session_id'=>array(
            'name' => 'session_id',
            'vname' => 'LBL_SESSION_ID',
            'type' => 'varchar',
            'len' => '36',
            'isnull' => 'true',
            'exportable' => false,
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'visible'=>array(
            'name' => 'visible',
            'vname' => 'LBL_VISIBLE',
            'type' => 'bool',
            'len' => '1',
            'default' => '0',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'deleted' =>array(
            'name' => 'deleted',
            'vname' => 'LBL_DELETED',
            'type' => 'bool',
            'default' => '0',
            'reportable'=>false,
            'comment' => 'Record deletion indicator',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'assigned_user_link'=>array(
            'name' => 'assigned_user_link',
            'type' => 'link',
            'relationship' => 'tracker_user_id',
            'vname' => 'LBL_ASSIGNED_TO_USER',
            'link_type' => 'one',
            'module'=>'Users',
            'bean_name'=>'User',
            'source'=>'non-db',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
        'monitor_id_link'=>array(
            'name' => 'monitor_id_link',
            'type' => 'link',
            'relationship' => 'tracker_monitor_id',
            'vname' => 'LBL_MONITOR_ID',
            'link_type' => 'one',
            'module'=>'TrackerPerfs',
            'bean_name'=>'TrackerPerf',
            'source'=>'non-db',
            // STIC-Custom 20241016 ART - Tracker Module
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/211
            // We add the properties, since in extension does not pick them up and then they disappear 
            'inline_edit' => 0,
            // END STIC-Custom
        ),
    ) ,

    //indices
    'indices' => array(
        array(
            'name' => 'tracker_pk',
            'type' => 'primary',
            'fields' => array(
                'id'
            )
        ) ,
        array(
            'name' => 'idx_tracker_iid',
            'type' => 'index',
            'fields' => array(
                'item_id',
            ),
        ),
        array(
            // shortened name to comply with Oracle length restriction
            'name' => 'idx_tracker_userid_vis_id',
            'type' => 'index',
            'fields' => array(
                'user_id',
                'visible',
                'id',
            ),
        ),
        array(
            // shortened name to comply with Oracle length restriction
            'name' => 'idx_tracker_userid_itemid_vis',
            'type' => 'index',
            'fields' => array(
                'user_id',
                'item_id',
                'visible'
            ),
        ),
        array(
            'name' => 'idx_tracker_monitor_id',
            'type' => 'index',
            'fields' => array(
                'monitor_id',
            ),
        ),
        array(
            'name' => 'idx_tracker_date_modified',
            'type' => 'index',
            'fields' => array(
                'date_modified',
            ),
        ),
    ),

    //relationships
    'relationships' => array(
      'tracker_monitor_id' =>
           array(
                'lhs_module'=> 'TrackerPerfs', 'lhs_table'=> 'tracker_perf', 'lhs_key' => 'monitor_id',
                'rhs_module'=> 'Trackers', 'rhs_table'=> 'tracker', 'rhs_key' => 'monitor_id',
                'relationship_type'=>'one-to-one'
           )
    ),
);

// STIC-Custom 20241029 ART - Tracker Module
// https://github.com/SinergiaTIC/SinergiaCRM/pull/211
// Set special values for SuiteCRM base fields
VardefManager::createVardef('Trackers', 'Tracker', array('assignable'));
$dictionary['Tracker']['fields']['assigned_user_id']['inline_edit'] = 0; // Assigned user can not edit inline in this module
$dictionary['Tracker']['fields']['assigned_user_name']['inline_edit'] = 0; // Assigned user can not edit inline in this module
// END STIC-Custom