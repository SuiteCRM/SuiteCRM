<?php
 if (!defined('sugarEntry') || !sugarEntry) {
     die('Not A Valid Entry Point');
 }
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
$searchFields['AOS_Quotes'] =
    array(
        'name' => array( 'query_type'=>'default'),
        'account_type'=> array('query_type'=>'default', 'options' => 'account_type_dom', 'template_var' => 'ACCOUNT_TYPE_OPTIONS'),
        'industry'=> array('query_type'=>'default', 'options' => 'industry_dom', 'template_var' => 'INDUSTRY_OPTIONS'),
        'annual_revenue'=> array('query_type'=>'default'),
        'address_street'=> array('query_type'=>'default','db_field'=>array('billing_address_street','shipping_address_street')),
        'address_city'=> array('query_type'=>'default','db_field'=>array('billing_address_city','shipping_address_city')),
        'address_state'=> array('query_type'=>'default','db_field'=>array('billing_address_state','shipping_address_state')),
        'address_postalcode'=> array('query_type'=>'default','db_field'=>array('billing_address_postalcode','shipping_address_postalcode')),
        'address_country'=> array('query_type'=>'default','db_field'=>array('billing_address_country','shipping_address_country')),
        'rating'=> array('query_type'=>'default'),
        'phone'=> array('query_type'=>'default','db_field'=>array('phone_office')),
        'email'=> array('query_type'=>'default','db_field'=>array('email1','email2')),
        'website'=> array('query_type'=>'default'),
        'ownership'=> array('query_type'=>'default'),
        'employees'=> array('query_type'=>'default'),
        'ticker_symbol'=> array('query_type'=>'default'),
        'current_user_only'=> array('query_type'=>'default','db_field'=>array('assigned_user_id'),'my_items'=>true, 'vname' => 'LBL_CURRENT_USER_FILTER', 'type' => 'bool'),
        'assigned_user_id'=> array('query_type'=>'default'),
        'favorites_only' => array(
            'query_type'=>'format',
            'operator' => 'subquery',
            'checked_only' => true,
            'subquery' => "SELECT favorites.parent_id FROM favorites
			                    WHERE favorites.deleted = 0
			                        and favorites.parent_type = 'AOS_Quotes'
			                        and favorites.assigned_user_id = '{1}'",
            'db_field'=>array('id')),

        //Range Search Support
        'range_total_amount' => array('query_type' => 'default', 'enable_range_search' => true),
        'start_range_total_amount' => array('query_type' => 'default',  'enable_range_search' => true),
        'end_range_total_amount' => array('query_type' => 'default', 'enable_range_search' => true),
        'range_expiration' => array('query_type' => 'default', 'enable_range_search' => true, 'is_date_field' => true),
        'start_range_expiration' => array('query_type' => 'default',  'enable_range_search' => true, 'is_date_field' => true),
        'end_range_expiration' => array('query_type' => 'default', 'enable_range_search' => true, 'is_date_field' => true),
    );
