<?php
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

/**
 * STIC-Custom AAM 20241015 
 * https://github.com/SinergiaTIC/SinergiaCRM/pull/442
 * The defaultDashlet array is transformed to contain the default column number. 
 * To accomplish this we overwrite the index.php file of this module in custom, placing the new version in the custom folder.
 * And we customize the default dashlet list.
 */
// $defaultDashlets = array(
//                         'MessageDashlet' => 'Home',
//                         'MyCallsDashlet'=>'Calls',
//                         'MyMeetingsDashlet'=>'Meetings',
//                         'MyOpportunitiesDashlet'=>'Opportunities',
//                         'MyAccountsDashlet'=>'Accounts',
//                         'MyLeadsDashlet'=>'Leads',
//                          );

$defaultDashlets = array(
    'SticNewsDashlet'=> array(
        'module' => 'Home',
        'column' => 1,
    ),
    'MyCallsDashlet'=> array(
        'module' => 'Calls',
        'column' => 0,
    ),
    'MyMeetingsDashlet'=> array(
        'module' => 'Meetings',
        'column' => 0,
    ),
    'MyTasksDashlet'=> array(
        'module' => 'Tasks',
        'column' => 0,
    ),
);

// END STIC-Custom

if (is_file('custom/modules/Home/dashlets.php')) {
    include_once('custom/modules/Home/dashlets.php');
}
