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
 
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


$subpanel_layout = array(
    'top_buttons' => array(),

    'where' => '',

    'list_fields' => array(
        'parent_name'=>array(
            'vname' => 'LBL_BEAN',
            'target_record_key' => 'parent_id',
            'target_module_key'=>'parent_type',
            'widget_class' => 'SubPanelDetailViewLink',
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
