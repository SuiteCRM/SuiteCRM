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

if (!defined('sugarEntry') || !sugarEntry) {  
    die('Not A Valid Entry Point');  
}

/**
 * This file contains functions used for PDF generation and electronic signature processes.
 * These functions are kept separate from SticGeneratePdf.php to allow for reuse in other contexts.
 */

/**
 * Populates group lines in a given text template with data from line item groups.
 *
 * Renders once (and only once) every table that contains article group variables
 * ($aos_line_item_groups_*). Inside those tables the header rows (rows without any
 * $aos_* variable) are rendered once, and the content rows are rendered for every
 * group. Each content row is rendered according to the variables it contains:
 * product rows are rendered for every product line item of the group, service rows
 * for every service line item and mixed rows (both product and service variables)
 * for every line item, emptying the variables that do not belong to that line item.
 * This way duplicated products/services are never dropped and mixed rows no longer
 * turn the product variables into $aos_null. All rows remain inside a single
 * <table>, which keeps the header and the content columns aligned and prevents the
 * header from being repeated.
 *
 * Templates without group variables keep the previous behaviour: the whole text is
 * populated with the individual line items.
 *
 * @param string $text The text template to populate.
 * @param array $lineItemsGroups An array of line item groups.
 * @param array $lineItems An array of individual line items.
 * @param string $element The HTML element name used for grouping (e.g., 'table').
 * @return string The populated text template.
 */
function stic_populate_group_lines($text, $lineItemsGroups, $lineItems, $element = 'table')
{
    $startElement = '<' . $element;
    $endElement = '</' . $element . '>';

    $groups = BeanFactory::newBean('AOS_Line_Item_Groups');
    $groupVarNames = array();
    foreach ($groups->field_defs as $name => $arr) {
        if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || $arr['type'] == 'id' || $arr['type'] == 'link')) {
            $groupVarNames[] = '$aos_line_item_groups_' . $name;
        }
    }

    // Check if any group variable exists in the text
    $hasGroupVars = false;
    foreach ($groupVarNames as $var) {
        if (strpos((string) $text, $var) !== false) {
            $hasGroupVars = true;
            break;
        }
    }

    if (empty($groupVarNames) || !$hasGroupVars) {
        $text = stic_populate_product_lines($text, $lineItems);
        $text = stic_populate_service_lines($text, $lineItems);
        return $text;
    }

    // Find all table blocks and process each one that contains group variables
    $aosVarPattern = '/\$aos_[a-z0-9_]+/i';
    $processedText = '';
    $remainingText = $text;

    while (($tableStartPos = strpos($remainingText, $startElement)) !== false) {
        // Find the end of this table
        $tableEndPos = strpos($remainingText, $endElement, $tableStartPos);
        if ($tableEndPos === false) {
            break;
        }
        $tableEndPos += strlen($endElement);

        // Extract text before this table
        $beforeTable = substr($remainingText, 0, $tableStartPos);
        $tableBlock = substr($remainingText, $tableStartPos, $tableEndPos - $tableStartPos);

        // Check if this table contains group variables
        $tableHasGroupVars = false;
        foreach ($groupVarNames as $var) {
            if (strpos($tableBlock, $var) !== false) {
                $tableHasGroupVars = true;
                break;
            }
        }

        if ($tableHasGroupVars && (is_countable($lineItemsGroups) ? count($lineItemsGroups) : 0) != 0) {
            // Extract all <tr>...</tr> blocks from this table
            $rows = array();
            $searchPos = 0;
            while (($rowStart = strpos($tableBlock, '<tr', $searchPos)) !== false) {
                $rowEnd = strpos($tableBlock, '</tr>', $rowStart);
                if ($rowEnd === false) {
                    break;
                }
                $rowEnd += strlen('</tr>');
                $rows[] = substr($tableBlock, $rowStart, $rowEnd - $rowStart);
                $searchPos = $rowEnd;
            }

            // Separate header rows (no $aos_* variables) from content rows
            $headerRows = array();
            $contentRows = array();
            foreach ($rows as $row) {
                if (!preg_match($aosVarPattern, $row)) {
                    $headerRows[] = $row;
                } else {
                    $contentRows[] = $row;
                }
            }

            // Extract the opening <table...> tag once
            $tableOpenTag = '';
            if (preg_match('/(<table[^>]*>)/i', $tableBlock, $matches)) {
                $tableOpenTag = $matches[1];
            }

            if ($tableOpenTag === '') {
                // No opening tag, cannot rebuild the table, keep it as is
                $processedText .= $beforeTable . $tableBlock;
                $remainingText = substr($remainingText, $tableEndPos);
                continue;
            }

            // Build a single table so the header and the group content share the same column widths.
            // The header rows (no $aos_* variables) are rendered only once at the beginning of the
            // <tbody>; the content rows are rendered once per group. Keeping both in the same <table>
            // keeps the columns aligned, and the header is never repeated (not per group, not per page).
            $tableHtml = $tableOpenTag . '<tbody>' . implode('', $headerRows);

            foreach ($lineItemsGroups as $group_id => $groupLineItems) {
                foreach ($contentRows as $row) {
                    $tableHtml .= stic_render_group_row($row, $groupLineItems, $group_id);
                }
            }

            $tableHtml .= '</tbody></table>';

            // Append text before table and the rendered table once
            $processedText .= $beforeTable;
            $processedText .= templateParser::parse_template($tableHtml, array());
        } else {
            // Table without group variables - keep as is
            $processedText .= $beforeTable . $tableBlock;
        }

        $remainingText = substr($remainingText, $tableEndPos);
    }

    // Append any remaining text after the last table
    $processedText .= $remainingText;

    return $processedText;
}

/**
 * Renders a single content row of a grouped table for one line item group.
 *
 * A row may contain only group variables (group name, totals...), only product
 * variables, only service variables, or a mix of product and service variables.
 * Product variables are rendered once per product line item and service variables
 * once per service line item of the group. When a row mixes both families, the
 * variables that do not belong to the current line item are emptied instead of
 * being rewritten to $aos_null, which was causing the products to disappear.
 *
 * @param string $row The <tr>...</tr> block to render.
 * @param array $groupLineItems Line items of the group (line item id => product id).
 * @param string $group_id The group id used to populate the group variables.
 * @return string The rendered row (empty when the group has no matching line items).
 */
function stic_render_group_row($row, $groupLineItems, $group_id)
{
    $productVarPattern = '/\$aos_products_quotes_|\$aos_products_/i';
    $serviceVarPattern = '/\$aos_services_/i';

    $hasProductVars = (bool) preg_match($productVarPattern, $row);
    $hasServiceVars = (bool) preg_match($serviceVarPattern, $row);

    // Rows without product/service variables belong to the group (name, totals...)
    if (!$hasProductVars && !$hasServiceVars) {
        return templateParser::parse_template($row, array('AOS_Line_Item_Groups' => $group_id));
    }

    $html = '';
    foreach ((array) $groupLineItems as $lineItemId => $productId) {
        $isProduct = ($productId != null && $productId != '0');

        if ($isProduct && !$hasProductVars) {
            continue;
        }
        if (!$isProduct && !$hasServiceVars) {
            continue;
        }

        if ($isProduct) {
            // Products: empty the service variables and fill the product ones.
            $rowHtml = preg_replace('/\$aos_services[a-z0-9_]*/i', '', $row);
            $beanArr = array(
                'AOS_Products_Quotes' => $lineItemId,
                'AOS_Products' => $productId,
                'AOS_Line_Item_Groups' => $group_id,
            );
        } else {
            // Services: empty the product variables and map the service ones to the
            // AOS_Products_Quotes fields (services are stored as product quote lines).
            $rowHtml = preg_replace('/\$aos_products[a-z0-9_]*/i', '', $row);
            $rowHtml = str_replace('$aos_services_quotes_service', '$aos_services_quotes_product', $rowHtml);
            $rowHtml = str_replace('$aos_services', '$aos_products', $rowHtml);
            $beanArr = array(
                'AOS_Products_Quotes' => $lineItemId,
                'AOS_Line_Item_Groups' => $group_id,
            );
        }

        $html .= templateParser::parse_template($rowHtml, $beanArr);
    }

    return $html;
}

/**
 * Populates product lines in a given text template with data from line items.
 *
 * @param string $text The text template to populate.
 * @param array $lineItems An array of line items, where keys are IDs and values are product IDs.
 * @param string $element The HTML element name used for lines (e.g., 'tr').
 * @return string The populated text template.
 */
function stic_populate_product_lines($text, $lineItems, $element = 'tr')
{
    $firstValue = '';
    $firstNum = 0;

    $lastValue = '';
    $lastNum = 0;

    $startElement = '<' . $element;
    $endElement = '</' . $element . '>';

    // Find first and last valid line values for products quotes
    $product_quote = BeanFactory::newBean('AOS_Products_Quotes');
    foreach ($product_quote->field_defs as $name => $arr) {
        if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || $arr['type'] == 'id' || $arr['type'] == 'link')) {
            $curNum = strpos((string) $text, '$aos_products_quotes_' . $name);

            if ($curNum) {
                if ($curNum < $firstNum || $firstNum == 0) {
                    $firstValue = '$aos_products_quotes_' . $name;
                    $firstNum = $curNum;
                }
                if ($curNum > $lastNum) {
                    $lastValue = '$aos_products_quotes_' . $name;
                    $lastNum = $curNum;
                }
            }
        }
    }

    // Find first and last valid line values for products
    $product = BeanFactory::newBean('AOS_Products');
    foreach ($product->field_defs as $name => $arr) {
        if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || $arr['type'] == 'id' || $arr['type'] == 'link')) {
            $curNum = strpos((string) $text, '$aos_products_' . $name);
            if ($curNum) {
                if ($curNum < $firstNum || $firstNum == 0) {
                    $firstValue = '$aos_products_' . $name;
                    $firstNum = $curNum;
                }
                if ($curNum > $lastNum) {
                    $lastValue = '$aos_products_' . $name;
                    $lastNum = $curNum;
                }
            }
        }
    }

    if ($firstValue !== '' && $lastValue !== '') {
        // Converting Text
        $tparts = explode($firstValue, $text);
        $temp = $tparts[0];

        // Check if there is only one line item
        if ($firstNum === $lastNum) {
            $linePart = $firstValue;
        } else {
            $tparts = explode($lastValue, $tparts[1]);
            $linePart = $firstValue . $tparts[0] . $lastValue;
        }

        $tcount = strrpos($temp, $startElement);
        $lsValue = substr($temp, $tcount);
        $tcount = strpos($lsValue, ">") + 1;
        $lsValue = substr($lsValue, 0, $tcount);

        // Read line end values
        $tcount = strpos($tparts[1], $endElement) + strlen($endElement);
        $leValue = substr($tparts[1], 0, $tcount);
        $tdTemp = explode($lsValue, $temp);

        $linePart = $lsValue . $tdTemp[(is_countable($tdTemp) ? count($tdTemp) : 0) - 1] . $linePart . $leValue;
        $parts = explode($linePart, $text);
        $text = $parts[0];

        // Converting Line Items
        if ((is_countable($lineItems) ? count($lineItems) : 0) != 0) {
            foreach ($lineItems as $id => $productId) {
                if ($productId != null && $productId != '0') {
                    $obb['AOS_Products_Quotes'] = $id;
                    $obb['AOS_Products'] = $productId;
                    $text .= templateParser::parse_template($linePart, $obb);
                }
            }
        }
        $partsCount = is_countable($parts) ? count($parts) : 0;

        for ($i = 1; $i < $partsCount; $i++) {
            $text .= $parts[$i];
        }
    }
    return $text;
}

/**
 * Populates service lines in a given text template with data from line items.
 *
 * @param string $text The text template to populate.
 * @param array $lineItems An array of line items, where keys are IDs and values are product IDs.
 * @param string $element The HTML element name used for lines (e.g., 'tr').
 * @return string The populated text template.
 */
function stic_populate_service_lines($text, $lineItems, $element = 'tr')
{
    $firstValue = '';
    $firstNum = 0;

    $lastValue = '';
    $lastNum = 0;

    $startElement = '<' . $element;
    $endElement = '</' . $element . '>';

    $text = str_replace("\$aos_services_quotes_service", "\$aos_services_quotes_product", (string) $text);

    // Find first and last valid line values for products quotes
    $product_quote = BeanFactory::newBean('AOS_Products_Quotes');
    foreach ($product_quote->field_defs as $name => $arr) {
        if (!((isset($arr['dbType']) && strtolower($arr['dbType']) == 'id') || $arr['type'] == 'id' || $arr['type'] == 'link')) {
            $curNum = strpos($text, '$aos_services_quotes_' . $name);
            if ($curNum) {
                if ($curNum < $firstNum || $firstNum == 0) {
                    $firstValue = '$aos_products_quotes_' . $name;
                    $firstNum = $curNum;
                }
                if ($curNum > $lastNum) {
                    $lastValue = '$aos_products_quotes_' . $name;
                    $lastNum = $curNum;
                }
            }
        }
    }
    if ($firstValue !== '' && $lastValue !== '') {
        $text = str_replace("\$aos_products", "\$aos_null", $text);
        $text = str_replace("\$aos_services", "\$aos_products", $text);

        // Converting Text
        $tparts = explode($firstValue, $text);
        $temp = $tparts[0];

        // Check if there is only one line item
        if ($firstNum === $lastNum) {
            $linePart = $firstValue;
        } else {
            $tparts = explode($lastValue, $tparts[1]);
            $linePart = $firstValue . $tparts[0] . $lastValue;
        }

        $tcount = strrpos($temp, $startElement);
        $lsValue = substr($temp, $tcount);
        $tcount = strpos($lsValue, ">") + 1;
        $lsValue = substr($lsValue, 0, $tcount);

        // Read line end values
        $tcount = strpos($tparts[1], $endElement) + strlen($endElement);
        $leValue = substr($tparts[1], 0, $tcount);
        $tdTemp = explode($lsValue, $temp);

        $linePart = $lsValue . $tdTemp[(is_countable($tdTemp) ? count($tdTemp) : 0) - 1] . $linePart . $leValue;
        $parts = explode($linePart, $text);
        $text = $parts[0];

        // Converting Line Items
        if ((is_countable($lineItems) ? count($lineItems) : 0) != 0) {
            foreach ($lineItems as $id => $productId) {
                if ($productId == null || $productId == '0') {
                    $obb['AOS_Products_Quotes'] = $id;
                    $text .= templateParser::parse_template($linePart, $obb);
                }
            }
        }
        $partsCount = is_countable($parts) ? count($parts) : 0;

        for ($i = 1; $i < $partsCount; $i++) {
            $text .= $parts[$i];
        }
    }
    return $text;
}
