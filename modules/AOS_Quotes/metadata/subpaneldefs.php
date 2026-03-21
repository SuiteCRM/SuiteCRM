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


$layout_defs['AOS_Quotes'] = array(
    'subpanel_setup' => array(
        'aos_quotes_aos_contracts' =>
        array(
            'order' => 100,
            'module' => 'AOS_Contracts',
            'subpanel_name' => 'default',
            'sort_order' => 'asc',
            'sort_by' => 'id',
            'title_key' => 'AOS_Contracts',
            'get_subpanel_data' => 'aos_quotes_aos_contracts',
            'top_buttons' =>
            array(
                0 =>
                array(
                    'widget_class' => 'SubPanelTopCreateButton',
                ),
                    1 =>
                    array(
                    'widget_class' => 'SubPanelTopSelectButton',
                    'popup_module' => 'AOS_Contracts',
                    'mode' => 'MultiSelect',
                ),
            ),
        ),
    
    'aos_quotes_aos_invoices' =>
    array(
        'order' => 100,
        'module' => 'AOS_Invoices',
        'subpanel_name' => 'default',
        'sort_order' => 'asc',
        'sort_by' => 'id',
        'title_key' => 'AOS_Invoices',
        'get_subpanel_data' => 'aos_quotes_aos_invoices',
        'top_buttons' =>
        array(
                0 =>
             array(
                'widget_class' => 'SubPanelTopCreateButton',
            ),
            1 =>
            array(
                'widget_class' => 'SubPanelTopSelectButton',
                'popup_module' => 'AOS_Invoices',
                'mode' => 'MultiSelect',
            ),
        ),
    ),
    
    'aos_quotes_project' =>
    array(
        'order' => 100,
        'module' => 'Project',
        'subpanel_name' => 'default',
        'sort_order' => 'asc',
        'sort_by' => 'id',
        'title_key' => 'Project',
        'get_subpanel_data' => 'aos_quotes_project',
        'top_buttons' =>
        array(
            0 =>
            array(
                'widget_class' => 'SubPanelTopCreateButton',
            ),
            1 =>
            array(
                'widget_class' => 'SubPanelTopSelectButton',
                'popup_module' => 'Accounts',
                'mode' => 'MultiSelect',
                ),
        ),
    ),
),
);
