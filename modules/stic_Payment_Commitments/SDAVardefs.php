<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */

/**
 * Array of virtual fields for SinergiaDA (Sinergia Data Analytics).
 *
 * This array defines various virtual fields that can be used in SinergiaDA for different purposes.
 *
 * Each element of the array represents a virtual field.
 *
 * General structure of each element:
 *
 * @key string The key is the virtual field name without the 'LBL_' prefix
 *
 * @param string label          Label of the virtual field (includes the 'LBL_' prefix)
 * @param string description    Description of the virtual field (includes the 'LBL_' prefix)
 * @param string type           Data type of the virtual field (e.g., 'numeric', 'text', 'date')
 * @param int    precision      Precision for numeric fields (number of decimal places)
 * @param string aggregations   Comma-separated list of supported aggregations (e.g., 'sum,avg,max,min,count,count_distinct,none')
 * @param int    hidden         Visibility flag (0 = visible to all, 1 = visible only to administrators in SDA)
 * @param string expression     SQL expression to calculate the virtual field value
 *

 */

$SDAVirtualFields = array(
    'SDA_EXPECTED_PAYMENTS_MONTH_1' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_1',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_1_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(m.expected_payments_detail, '|', 1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_2' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_2',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_2_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 2), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_3' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_3',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_3_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 3), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_4' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_4',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_4_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 4), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_5' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_5',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_5_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 5), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_6' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_6',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_6_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 6), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_7' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_7',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_7_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 7), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_8' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_8',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_8_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 8), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_9' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_9',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_9_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 9), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_10' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_10',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_10_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 10), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_11' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_11',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_11_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 11), '|', -1)",
    ),
    'SDA_EXPECTED_PAYMENTS_MONTH_12' => array(
        'label' => 'LBL_EXPECTED_PAYMENTS_MONTH_12',
        'description' => 'LBL_EXPECTED_PAYMENTS_MONTH_12_DESCRIPTION',
        'type' => 'numeric',
        'precision' => 2,
        'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
        'hidden' => 0,
        'expression' => "SUBSTRING_INDEX(SUBSTRING_INDEX(m.expected_payments_detail, '|', 12), '|', -1)",
    ),
);
