<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2024 SalesAgility Ltd.
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

$dictionary['AOR_Scheduled_Reports'] = array(
    'table' => 'aor_scheduled_reports',
    'audited' => false,
    'duplicate_merge' => false,
    'fields' => array(
        'schedule' => array(
            'required' => true,
            'name' => 'schedule',
            'vname' => 'LBL_SCHEDULE',
            'type' => 'CronSchedule',
            'dbType' => 'varchar',
            'massupdate' => 0,
            'no_default' => false,
            'comments' => '',
            'help' => '',
            'importable' => 'true',
            'duplicate_merge' => 'disabled',
            'duplicate_merge_dom_value' => '0',
            'audited' => false,
            'reportable' => true,
            'unified_search' => false,
            'merge_filter' => 'disabled',
            'len' => '100',
            'size' => '20',
        ),
        'last_run' => array(
            'name' => 'last_run',
            'vname' => 'LBL_LAST_RUN',
            'dbtype' => 'datetime',
            'type' => 'readonly',
            'required' => false,
            'reportable' => false,
        ),
        'status' => array(
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'type' => 'enum',
            'options' => 'aor_scheduled_reports_status_dom',
            'len' => 100,
            'editable' => false,
            'required' => false,
            'reportable' => false,
            'importable' => 'required',
        ),
        'email_recipients' =>
            array(
                'required' => false,
                'name' => 'email_recipients',
                'vname' => 'LBL_EMAIL_RECIPIENTS',
                'type' => 'longtext',
                'massupdate' => 0,
                'importable' => 'false',
                'duplicate_merge' => 'disabled',
                'duplicate_merge_dom_value' => 0,
                'audited' => false,
                'reportable' => false,
                'inline_edit' => false,
                'function' =>
                    array(
                        'name' => 'display_email_lines',
                        'returns' => 'html',
                        'onListView' => true,
                        'include' => 'modules/AOR_Scheduled_Reports/emailRecipients.php'
                    ),
            ),
        "aor_report" => array(
            'name' => 'aor_report',
            'type' => 'link',
            'relationship' => 'aor_scheduled_reports_aor_reports',
            'module' => 'AOR_Reports',
            'bean_name' => 'AOR_Report',
            'link_type' => 'one',
            'source' => 'non-db',
            'vname' => 'LBL_AOR_REPORT',
            'side' => 'left',
            'id_name' => 'aor_report_id',
        ),
        "aor_report_name" => array(
            'name' => 'aor_report_name',
            'type' => 'relate',
            'source' => 'non-db',
            'vname' => 'LBL_AOR_REPORT_NAME',
            'required' => true,
            'save' => true,
            'id_name' => 'aor_report_id',
            'link' => 'aor_report',
            'table' => 'aor_reports',
            'module' => 'AOR_Reports',
            'rname' => 'name',
        ),
        "aor_report_id" => array(
            'name' => 'aor_report_id',
            'type' => 'id',
            'reportable' => false,
            'vname' => 'LBL_AOR_REPORT_ID',
        ),


    ),
    'relationships' => array(
    ),
    'optimistic_locking' => true,
    'unified_search' => false,
);
if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('AOR_Scheduled_Reports', 'AOR_Scheduled_Reports', ['basic', 'security_groups']);
