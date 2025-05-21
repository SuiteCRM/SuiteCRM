<?php
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

require_once('modules/AOS_Line_Item_Groups/AOS_Line_Item_Groups_sugar.php');

#[\AllowDynamicProperties]
class AOS_Line_Item_Groups extends AOS_Line_Item_Groups_sugar
{
    public function __construct()
    {
        parent::__construct();
    }




    public function save_groups($post_data, $parent, $key = '')
    {
        $groups = array();
        $group_count = isset($post_data[$key . 'group_number']) ? count($post_data[$key . 'group_number']) : 0;
        $j = 0;
        for ($i = 0; $i < $group_count; ++$i) {
            $postData = null;
            if (isset($post_data[$key . 'deleted'][$i])) {
                $postData = $post_data[$key . 'deleted'][$i];
            } else {
                LoggerManager::getLogger()->warn('AOS Line Item Group deleted field is not set in requested POST data at key: ' . $key . '['. $i .']');
            }

            if ($postData == 1) {
                $this->mark_deleted($post_data[$key . 'id'][$i]);
            } else {
                $product_quote_group = BeanFactory::newBean('AOS_Line_Item_Groups');
                foreach ($this->field_defs as $field_def) {
                    $field_name = $field_def['name'];
                    if (isset($post_data[$key . $field_name][$i])) {
                        $product_quote_group->$field_name = $post_data[$key . $field_name][$i];
                    }
                }
                $product_quote_group->number = ++$j;
                $product_quote_group->assigned_user_id = $parent->assigned_user_id;

                $parentCurrencyId = null;
                if (isset($parent->currency_id)) {
                    $parentCurrencyId = $parent->currency_id;
                } else {
                    LoggerManager::getLogger()->warn('AOS Line Item Group trying to save group nut Parent currency ID is not set');
                }

                $product_quote_group->currency_id = $parentCurrencyId;
                $product_quote_group->parent_id = $parent->id;
                $product_quote_group->parent_type = $parent->object_name;
                $product_quote_group->save();
                $post_data[$key . 'id'][$i] = $product_quote_group->id;

                if (isset($post_data[$key . 'group_number'][$i])) {
                    $groups[$post_data[$key . 'group_number'][$i]] = $product_quote_group->id;
                }
            }
        }

        require_once('modules/AOS_Products_Quotes/AOS_Products_Quotes.php');
        $productQuote = BeanFactory::newBean('AOS_Products_Quotes');
        $productQuote->save_lines($post_data, $parent, $groups, 'product_');
        $productQuote->save_lines($post_data, $parent, $groups, 'service_');
    }

    public function save($check_notify = false)
    {
        require_once('modules/AOS_Products_Quotes/AOS_Utils.php');
        perform_aos_save($this);
        return parent::save($check_notify);
    }
}
