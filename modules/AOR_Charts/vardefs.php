<?php
/**
 * Advanced OpenReports, SugarCRM Reporting.
 * @package Advanced OpenReports for SugarCRM
 * @copyright SalesAgility Ltd http://www.salesagility.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 *
 * @author SalesAgility <info@salesagility.com>
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
