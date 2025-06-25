<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 * Advanced OpenWorkflow, Automating SugarCRM.
 * @package Advanced OpenWorkflow for SugarCRM
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


$subpanel_layout = array(
    'top_buttons' => array(),

    'where' => '',

    'list_fields' => array(
        'custom_parent_name'=>array(
            'vname' => 'LBL_BEAN',
            'target_record_key' => 'parent_id',
            'target_module_key'=>'parent_type',
            'widget_class' => 'SubPanelDynamicParentNameLink',
            'sortable'=>false,
            'width' => '15%',
        ),
        'status'=>array(
            'vname' => 'LBL_STATUS',
            'width' => '15%',
        ),
        'date_entered'=>array(
            'vname' => 'LBL_DATE_ENTERED',
            'width' => '15%',
        ),
        'date_modified'=>array(
            'vname' => 'LBL_DATE_MODIFIED',
            'width' => '15%',
        ),
        'parent_id'=>array(
            'usage'=>'query_only',
        ),
        'parent_type'=>array(
            'usage'=>'query_only',
        ),
    ),
);
