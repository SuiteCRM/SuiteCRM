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

require_once('modules/AOS_Products_Quotes/AOS_Products_Quotes_sugar.php');

#[\AllowDynamicProperties]
class AOS_Products_Quotes extends AOS_Products_Quotes_sugar
{
    public function __construct()
    {
        parent::__construct();
    }




    public function save_lines($post_data, $parent, $groups = array(), $key = '')
    {
        $line_count = isset($post_data[$key . 'name']) ? count($post_data[$key . 'name']) : 0;
        $j = 0;
        for ($i = 0; $i < $line_count; ++$i) {
            if (isset($post_data[$key . 'deleted'][$i]) && $post_data[$key . 'deleted'][$i] == 1) {
                $this->mark_deleted($post_data[$key . 'id'][$i]);
            } else {
                if (!isset($post_data[$key . 'id'][$i])) {
                    LoggerManager::getLogger()->warn('Post date has no key id');
                    $postDataKeyIdI = null;
                } else {
                    $postDataKeyIdI = $post_data[$key . 'id'][$i];
                }

                $product_quote = BeanFactory::getBean('AOS_Products_Quotes', $postDataKeyIdI);
                if (!$product_quote) {
                    $product_quote = BeanFactory::newBean('AOS_Products_Quotes');
                }
                foreach ($this->field_defs as $field_def) {
                    $field_name = $field_def['name'];
                    if (isset($post_data[$key . $field_name][$i])) {
                        $product_quote->$field_name = $post_data[$key . $field_name][$i];
                    }
                }
                if (isset($post_data[$key . 'group_number'][$i])) {
                    if (!isset($post_data[$key . 'group_number'][$i])) {
                        LoggerManager::getLogger()->warn('AOS Product Quotes error: Group number at post data key index is undefined in groups. Key and index was: ' . $key . ', ' . $i);
                        $groupIndex = null;
                    } else {
                        $groupIndex = $post_data[$key . 'group_number'][$i];
                    }
                    if (!isset($groups[$groupIndex])) {
                        LoggerManager::getLogger()->warn('AOS Product Quotes error: Group index was: ' . $groupIndex);
                        $product_quote->group_id = null;
                    } else {
                        $product_quote->group_id = $groups[$post_data[$key . 'group_number'][$i]];
                    }
                }
                if (trim($product_quote->product_id) != '' && trim($product_quote->name) != '' && trim($product_quote->product_unit_price) != '') {
                    $product_quote->number = ++$j;
                    $product_quote->assigned_user_id = $parent->assigned_user_id;
                    $product_quote->parent_id = $parent->id;

                    if (!isset($parent->currency_id)) {
                        LoggerManager::getLogger()->warn('Paren Currency ID is not defined for AOD Product Quotes / save lines.');
                        $parentCurrencyId = null;
                    } else {
                        $parentCurrencyId = $parent->currency_id;
                    }

                    $product_quote->currency_id = $parentCurrencyId;
                    $product_quote->parent_type = $parent->object_name;
                    $product_quote->save();
                    $_POST[$key . 'id'][$i] = $product_quote->id;
                }
            }
        }
    }

    public function save($check_notify = false)
    {
        require_once('modules/AOS_Products_Quotes/AOS_Utils.php');
        perform_aos_save($this);
        return parent::save($check_notify);
    }

    /**
     * @param $parent SugarBean
     */
    public function mark_lines_deleted($parent)
    {
        require_once('modules/Relationships/Relationship.php');
        $product_quotes = $parent->get_linked_beans('aos_products_quotes', $this->object_name);
        foreach ($product_quotes as $product_quote) {
            $product_quote->mark_deleted($product_quote->id);
        }
    }
}
