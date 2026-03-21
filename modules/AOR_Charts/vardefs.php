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

$dictionary['AOR_Chart'] = array(
    'table'=>'aor_charts',
    'audited'=>false,
    'duplicate_merge'=>true,
    'fields'=>array(
        "aor_report" => array(
            'name' => 'aor_report',
            'type' => 'link',
            'relationship' => 'aor_charts_aor_reports',
            'module'=>'AOR_Reports',
            'bean_name'=>'AOR_Report',
            'link_type'=>'one',
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
            'save' => true,
            'id_name' => 'aor_report_id',
            'link' => 'aor_charts_aor_reports',
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
        'type' =>
            array(
                'required' => false,
                'name' => 'type',
                'vname' => 'LBL_TYPE',
                'type' => 'enum',
                'massupdate' => 0,
                'len' => 100,
                'size' => '20',
                'options' => 'aor_chart_types',
            ),
        'x_field' =>
            array(
                'required' => false,
                'name' => 'x_field',
                'vname' => 'LBL_X_FIELD',
                'type' => 'int',
            ),
        'y_field' =>
            array(
                'required' => false,
                'name' => 'y_field',
                'vname' => 'LBL_Y_FIELD',
                'type' => 'int',
            ),
    ),
    'relationships'=>array(
        "aor_charts_aor_reports" => array(
            'lhs_module'=> 'AOR_Reports',
            'lhs_table'=> 'aor_reports',
            'lhs_key' => 'id',
            'rhs_module'=> 'AOR_Charts',
            'rhs_table'=> 'aor_charts',
            'rhs_key' => 'aor_report_id',
            'relationship_type'=>'one-to-many',
        ),
    ),
    'optimistic_locking'=>true,
    'unified_search'=>true,
);

if (!class_exists('VardefManager')) {
    require_once('include/SugarObjects/VardefManager.php');
}
VardefManager::createVardef('AOR_Charts', 'AOR_Chart', array('basic'));
