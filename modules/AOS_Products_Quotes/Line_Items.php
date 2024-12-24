<?php
/**
 * Advanced OpenSales, Advanced, robust set of sales modules.
 * @package Advanced OpenSales for SugarCRM
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

function display_lines($focus, $field, $value, $view)
{
    global $sugar_config, $locale, $app_list_strings, $mod_strings;

    $enable_groups = (int)$sugar_config['aos']['lineItems']['enableGroups'];
    $total_tax = (int)$sugar_config['aos']['lineItems']['totalTax'];

    $html = '';

    if ($view == 'EditView') {
        $html .= '<script src="modules/AOS_Products_Quotes/line_items.js"></script>';
        if (file_exists('custom/modules/AOS_Products_Quotes/line_items.js')) {
            $html .= '<script src="custom/modules/AOS_Products_Quotes/line_items.js"></script>';
        }
        $html .= '<script language="javascript">var sig_digits = '.$locale->getPrecision().';';
        $html .= 'var module_sugar_grp1 = "'.$focus->module_dir.'";';
        $html .= 'var enable_groups = '.$enable_groups.';';
        $html .= 'var total_tax = '.$total_tax.';';
        $html .= '</script>';

        $html .= "<table border='0' cellspacing='4' id='lineItems'></table>";

        if ($enable_groups) {
            $html .= "<div style='padding-top: 10px; padding-bottom:10px;'>";
            $html .= "<input type=\"button\" tabindex=\"116\" class=\"button\" value=\"".$mod_strings['LBL_ADD_GROUP']."\" id=\"addGroup\" onclick=\"insertGroup(0)\" />";
            $html .= "</div>";
        }
        $html .= '<input type="hidden" name="vathidden" id="vathidden" value="'.get_select_options_with_id($app_list_strings['vat_list'], '').'">
				  <input type="hidden" name="discounthidden" id="discounthidden" value="'.get_select_options_with_id($app_list_strings['discount_list'], '').'">';
        if ($focus->id != '') {
            require_once('modules/AOS_Products_Quotes/AOS_Products_Quotes.php');
            require_once('modules/AOS_Line_Item_Groups/AOS_Line_Item_Groups.php');

            $sql = "SELECT pg.id, pg.group_id FROM aos_products_quotes pg LEFT JOIN aos_line_item_groups lig ON pg.group_id = lig.id WHERE pg.parent_type = '" . $focus->object_name . "' AND pg.parent_id = '" . $focus->id . "' AND pg.deleted = 0 ORDER BY lig.number ASC, pg.number ASC";

            $result = $focus->db->query($sql);
            $html .= "<script>
                if(typeof sqs_objects == 'undefined'){var sqs_objects = new Array;}
                </script>";

            while ($row = $focus->db->fetchByAssoc($result)) {
                $line_item = BeanFactory::newBean('AOS_Products_Quotes');
                $line_item->retrieve($row['id'], false);
                $line_item = json_encode($line_item->toArray());

                $group_item = 'null';
                if ($row['group_id'] != null) {
                    $group_item = BeanFactory::newBean('AOS_Line_Item_Groups');
                    $group_item->retrieve($row['group_id'], false);
                    $group_item = json_encode($group_item->toArray());
                }
                $html .= "<script>
                        insertLineItems(" . $line_item . "," . $group_item . ");
                    </script>";
            }
        }
        if (!$enable_groups) {
            $html .= '<script>insertGroup();</script>';
        }
    } elseif ($view == 'DetailView') {
        $params = array('currency_id' => $focus->currency_id);

        $sql = "SELECT pg.id, pg.group_id FROM aos_products_quotes pg LEFT JOIN aos_line_item_groups lig ON pg.group_id = lig.id WHERE pg.parent_type = '".$focus->object_name."' AND pg.parent_id = '".$focus->id."' AND pg.deleted = 0 ORDER BY lig.number ASC, pg.number ASC";

        $result = $focus->db->query($sql);
        $sep = get_number_separators();

        $html .= "<table border='0' width='100%' cellpadding='0' cellspacing='0'>";

        $i = 0;
        $productCount = 0;
        $serviceCount = 0;
        $group_id = '';
        $groupStart = '';
        $groupEnd = '';
        $product = '';
        $service = '';

        while ($row = $focus->db->fetchByAssoc($result)) {
            $line_item = BeanFactory::newBean('AOS_Products_Quotes');
            $line_item->retrieve($row['id']);


            if ($enable_groups && ($group_id != $row['group_id'] || $i == 0)) {
                $html .= $groupStart.$product.$service.$groupEnd;
                if ($i != 0) {
                    $html .= "<tr><td colspan='9' nowrap='nowrap'><br></td></tr>";
                }
                $groupStart = '';
                $groupEnd = '';
                $product = '';
                $service = '';
                $i = 1;
                $productCount = 0;
                $serviceCount = 0;
                $group_id = $row['group_id'];

                $group_item = BeanFactory::newBean('AOS_Line_Item_Groups');
                $group_item->retrieve($row['group_id']);

                $groupStart .= "<tr>";
                $groupStart .= "<td class='tabDetailViewDL' style='text-align: left;padding:2px;' scope='row'>&nbsp;</td>";
                $groupStart .= "<td class='tabDetailViewDL' style='text-align: left;padding:2px;' scope='row'>".$mod_strings['LBL_GROUP_NAME'].":</td>";
                $groupStart .= "<td class='tabDetailViewDL' colspan='7' style='text-align: left;padding:2px;'>".$group_item->name."</td>";
                $groupStart .= "</tr>";

                $groupEnd = "<tr><td colspan='9' nowrap='nowrap'><br></td></tr>";
                $groupEnd .= "<tr>";
                $groupEnd .= "<td class='tabDetailViewDL' colspan='8' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_TOTAL_AMT'].":&nbsp;&nbsp;</td>";
                $groupEnd .= "<td class='tabDetailViewDL' style='text-align: right;padding:2px;'>".currency_format_number($group_item->total_amt, $params)."</td>";
                $groupEnd .= "</tr>";
                $groupEnd .= "<tr>";
                $groupEnd .= "<td class='tabDetailViewDL' colspan='8' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_DISCOUNT_AMOUNT'].":&nbsp;&nbsp;</td>";
                $groupEnd .= "<td class='tabDetailViewDL' style='text-align: right;padding:2px;'>".currency_format_number($group_item->discount_amount, $params)."</td>";
                $groupEnd .= "</tr>";
                $groupEnd .= "<tr>";
                $groupEnd .= "<td class='tabDetailViewDL' colspan='8' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_SUBTOTAL_AMOUNT'].":&nbsp;&nbsp;</td>";
                $groupEnd .= "<td class='tabDetailViewDL' style='text-align: right;padding:2px;'>".currency_format_number($group_item->subtotal_amount, $params)."</td>";
                $groupEnd .= "</tr>";
                $groupEnd .= "<tr>";
                $groupEnd .= "<td class='tabDetailViewDL' colspan='8' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_TAX_AMOUNT'].":&nbsp;&nbsp;</td>";
                $groupEnd .= "<td class='tabDetailViewDL' style='text-align: right;padding:2px;'>".currency_format_number($group_item->tax_amount, $params)."</td>";
                $groupEnd .= "</tr>";
                $groupEnd .= "<tr>";
                $groupEnd .= "<td class='tabDetailViewDL' colspan='8' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_GRAND_TOTAL'].":&nbsp;&nbsp;</td>";
                $groupEnd .= "<td class='tabDetailViewDL' style='text-align: right;padding:2px;'>".currency_format_number($group_item->total_amount, $params)."</td>";
                $groupEnd .= "</tr>";
            }
            if ($line_item->product_id != '0' && $line_item->product_id != null) {
                if ($productCount == 0) {
                    $product .= "<tr>";
                    $product .= "<td width='5%' class='tabDetailViewDL' style='text-align: left;padding:2px;' scope='row'>&nbsp;</td>";
                    $product .= "<td width='10%' class='tabDetailViewDL' style='text-align: left;padding:2px;' scope='row'>".$mod_strings['LBL_PRODUCT_QUANITY']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: left;padding:2px;' scope='row'>".$mod_strings['LBL_PRODUCT_NAME']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_LIST_PRICE']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_DISCOUNT_AMT']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_UNIT_PRICE']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_VAT']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_VAT_AMT']."</td>";
                    $product .= "<td width='12%' class='tabDetailViewDL' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_TOTAL_PRICE']."</td>";
                    $product .= "</tr>";
                }

                $product .= "<tr>";
                $product_note = wordwrap($line_item->description, 40, "<br />\n");
                $product .= "<td class='tabDetailViewDF' style='text-align: left; padding:2px;'>".++$productCount."</td>";
                $product .= "<td class='tabDetailViewDF' style='padding:2px;'>".stripDecimalPointsAndTrailingZeroes(format_number($line_item->product_qty), $sep[1])."</td>";

                $product .= "<td class='tabDetailViewDF' style='padding:2px;'><a href='index.php?module=AOS_Products&action=DetailView&record=".$line_item->product_id."' class='tabDetailViewDFLink'>".$line_item->name."</a><br />".$product_note."</td>";
                $product .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->product_list_price, $params)."</td>";

                $product .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".get_discount_string($line_item->discount, $line_item->product_discount, $params, $locale, $sep)."</td>";

                $product .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->product_unit_price, $params)."</td>";
                $product .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".display_tax_detail_view($locale, $line_item->vat, $sep)."</td>";
                $product .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->vat_amt, $params)."</td>";
                $product .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->product_total_price, $params)."</td>";
                $product .= "</tr>";
            } else {
                if ($serviceCount == 0) {
                    $service .= "<tr>";
                    $service .= "<td width='5%' class='tabDetailViewDL' style='text-align: left;padding:2px;' scope='row'>&nbsp;</td>";
                    $service .= "<td width='22%' class='dataLabel' style='text-align: left;padding:2px;' colspan='2' scope='row'>".$mod_strings['LBL_SERVICE_NAME']."</td>";
                    $service .= "<td width='12%' class='dataLabel' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_SERVICE_LIST_PRICE']."</td>";
                    $service .= "<td width='12%' class='dataLabel' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_SERVICE_DISCOUNT']."</td>";
                    $service .= "<td width='12%' class='dataLabel' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_SERVICE_PRICE']."</td>";
                    $service .= "<td width='12%' class='dataLabel' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_VAT']."</td>";
                    $service .= "<td width='12%' class='dataLabel' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_VAT_AMT']."</td>";
                    $service .= "<td width='12%' class='dataLabel' style='text-align: right;padding:2px;' scope='row'>".$mod_strings['LBL_TOTAL_PRICE']."</td>";
                    $service .= "</tr>";
                }

                $service .= "<tr>";
                $service .= "<td class='tabDetailViewDF' style='text-align: left; padding:2px;'>".++$serviceCount."</td>";
                $service .= "<td class='tabDetailViewDF' style='padding:2px;' colspan='2'>".$line_item->name."</td>";
                $service .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->product_list_price, $params)."</td>";

                $service .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".get_discount_string($line_item->discount, $line_item->product_discount, $params, $locale, $sep)."</td>";
                $service .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->product_unit_price, $params)."</td>";
                $service .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".display_tax_detail_view($locale, $line_item->vat, $sep)."</td>";
                $service .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->vat_amt, $params)."</td>";
                $service .= "<td class='tabDetailViewDF' style='text-align: right; padding:2px;'>".currency_format_number($line_item->product_total_price, $params)."</td>";
                $service .= "</tr>";
            }
        }
        $html .= $groupStart.$product.$service.$groupEnd;
        $html .= "</table>";
    }
    return $html;
}

//Bug #598
//The original approach to trimming the characters was rtrim(rtrim(format_number($line_item->product_qty), '0'),$sep[1])
//This however had the unwanted side-effect of turning 1000 (or 10 or 100) into 1 when the Currency Significant Digits
//field was 0.
//The approach below will strip off the fractional part if it is only zeroes (and in this case the decimal separator
//will also be stripped off) The custom decimal separator is passed in to the function from the locale settings
function stripDecimalPointsAndTrailingZeroes($inputString, $decimalSeparator)
{
    return preg_replace('/'.preg_quote((string) $decimalSeparator).'[0]+$/', '', (string) $inputString);
}

function get_discount_string($type, $amount, $params, $locale, $sep)
{
    if ($amount != '' && $amount != '0.00') {
        if ($type == 'Amount') {
            return currency_format_number($amount, $params)."</td>";
        } elseif ($locale->getPrecision()) {
            return rtrim(rtrim(format_number($amount), '0'), $sep[1])."%";
        }
        return format_number($amount)."%";
    }
    return "-";
}

function display_shipping_vat($focus, $field, $value, $view)
{
    if ($view == 'EditView') {
        global $app_list_strings;

        if ($value != '') {
            $value = format_number($value);
        }

        $html = "<input id='shipping_tax_amt' type='text' tabindex='0' title='' value='".$value."' maxlength='26,6' size='22' name='shipping_tax_amt' onblur='calculateTotal(\"lineItems\");'>";
        $html .= "<select name='shipping_tax' id='shipping_tax' onchange='calculateTotal(\"lineItems\");' >".get_select_options_with_id($app_list_strings['vat_list'], (isset($focus->shipping_tax) ? $focus->shipping_tax : ''))."</select>";

        return $html;
    }
    return format_number($value);
}

/**
 * Returns formatted value for the tax field;
 * @param $locale
 * @param $value
 * @param $sep
 * @return string
 */
function display_tax_detail_view($locale, $value, $sep): string
{
    global $app_strings;

    if ($locale->getPrecision()) {
        $value = rtrim(rtrim(format_number($value), '0'), $sep[1]) . $app_strings['LBL_PERCENTAGE_SYMBOL'];
    } else {
        $value = format_number($value) . $app_strings['LBL_PERCENTAGE_SYMBOL'];
    }

    return $value;
}
