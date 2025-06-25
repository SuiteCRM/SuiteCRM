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
    'SDA_ACTIVE_CURRENT_YEAR_MINUS_0' => array(
        'label' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_0',
        'description' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_0_DESCRIPTION',
        'type' => 'text',
        'precision' => 0,
        'aggregations' => 'count,count_distinct,none',
        'hidden' => 0,
        'expression' => "CASE
            WHEN (m.start_date IS NULL OR m.start_date <= LAST_DAY(CONCAT(YEAR(CURDATE()),'-12-31')))
            AND (m.end_date IS NULL OR m.end_date >= CONCAT(YEAR(CURDATE()),'-01-01'))
            THEN '1'
            ELSE '0'
        END",
    ),
    'SDA_ACTIVE_CURRENT_YEAR_MINUS_1' => array(
        'label' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_1',
        'description' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_1_DESCRIPTION',
        'type' => 'text',
        'precision' => 0,
        'aggregations' => 'count,count_distinct,none',
        'hidden' => 0,
        'expression' => "CASE
            WHEN (m.start_date IS NULL OR m.start_date <= LAST_DAY(CONCAT(YEAR(CURDATE())-1,'-12-31')))
            AND (m.end_date IS NULL OR m.end_date >= CONCAT(YEAR(CURDATE())-1,'-01-01'))
            THEN '1'
            ELSE '0'
        END",
    ),
    'SDA_ACTIVE_CURRENT_YEAR_MINUS_2' => array(
        'label' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_2',
        'description' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_2_DESCRIPTION',
        'type' => 'text',
        'precision' => 0,
        'aggregations' => 'count,count_distinct,none',
        'hidden' => 0,
        'expression' => "CASE
            WHEN (m.start_date IS NULL OR m.start_date <= LAST_DAY(CONCAT(YEAR(CURDATE())-2,'-12-31')))
            AND (m.end_date IS NULL OR m.end_date >= CONCAT(YEAR(CURDATE())-2,'-01-01'))
            THEN '1'
            ELSE '0'
        END",
    ),
    'SDA_ACTIVE_CURRENT_YEAR_MINUS_3' => array(
        'label' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_3',
        'description' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_3_DESCRIPTION',
        'type' => 'text',
        'precision' => 0,
        'aggregations' => 'count,count_distinct,none',
        'hidden' => 0,
        'expression' => "CASE
            WHEN (m.start_date IS NULL OR m.start_date <= LAST_DAY(CONCAT(YEAR(CURDATE())-3,'-12-31')))
            AND (m.end_date IS NULL OR m.end_date >= CONCAT(YEAR(CURDATE())-3,'-01-01'))
            THEN '1'
            ELSE '0'
        END",
    ),
    'SDA_ACTIVE_CURRENT_YEAR_MINUS_4' => array(
        'label' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_4',
        'description' => 'LBL_SDA_ACTIVE_CURRENT_YEAR_MINUS_4_DESCRIPTION',
        'type' => 'text',
        'precision' => 0,
        'aggregations' => 'count,count_distinct,none',
        'hidden' => 0,
        'expression' => "CASE
            WHEN (m.start_date IS NULL OR m.start_date <= LAST_DAY(CONCAT(YEAR(CURDATE())-4,'-12-31')))
            AND (m.end_date IS NULL OR m.end_date >= CONCAT(YEAR(CURDATE())-4,'-01-01'))
            THEN '1'
            ELSE '0'
        END",
    ),
);
