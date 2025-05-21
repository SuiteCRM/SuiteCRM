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

class AOS_QuotesLogicHooks
{

    public function after_save(&$bean, $event, $arguments)
    {
        // If mass duplicate
        if (!empty($_REQUEST['mass_duplicate'])) {
            global $db, $sugar_config;
            include_once 'SticInclude/Utils.php';

            // *****************************************
            // 1) Duplicate related AOS_Line_Item_Groups records.
            // The query should ensure that we always retrieve some records in case there are no defined line groups (UNION SELECT NULL).
            $lineGroupsQuery = "SELECT
                                    id
                                FROM
                                    aos_line_item_groups
                                WHERE
                                    deleted = 0
                                    AND parent_type = 'AOS_Quotes' AND parent_id='{$bean->fromId}'
                                UNION SELECT NULL";

            $resultsLines = $db->query($lineGroupsQuery);

            while ($rowLines = $db->fetchByAssoc($resultsLines)) {

                // Change the parent_id field to point to the new bean id
                $lineChanges = ['parent_id' => $bean->id];

                // line group is duplicated only if the configuration is enabled.
                if ($sugar_config['aos']['lineItems']['enableGroups'] == true) {
                    $lineId = SticUtils::duplicateBeanRecord('AOS_Line_Item_Groups', $rowLines['id'], $lineChanges);
                } 

                // *****************************************
                // 2) For each Line Group, duplicate related AOS_Products_Quotes records
                $productQuery = "SELECT
                                    id
                                FROM
                                    aos_products_quotes
                                WHERE
                                    deleted = 0
                                    AND parent_type = 'AOS_Quotes'
                                    AND parent_id = '{$bean->fromId}'
                                    AND (group_id = '{$rowLines['id']}' || group_id IS null)
                                    ";

                $resultsProducts = $db->query($productQuery);

                while ($rowProduct = $db->fetchByAssoc($resultsProducts)) {
                    // Change the parent_id field to point to the new bean id
                    $productChanges = ['parent_id' => $bean->id, 'group_id' => $lineId];
                    $productId = SticUtils::duplicateBeanRecord('AOS_Products_Quotes', $rowProduct['id'], $productChanges);
                }
            }
        }
    }
}
