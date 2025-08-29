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

    if (!(ACLController::checkAccess('AOS_Invoices', 'edit', true))) {
        ACLController::displayNoAccess();
        die;
    }

    require_once('modules/AOS_Quotes/AOS_Quotes.php');
    require_once('modules/AOS_Invoices/AOS_Invoices.php');
    require_once('modules/AOS_Products_Quotes/AOS_Products_Quotes.php');

    global $timedate;
    //Setting values in Quotes
    $quote = BeanFactory::newBean('AOS_Quotes');
    $quote->retrieve($_REQUEST['record']);
    $quote->invoice_status = 'Invoiced';
    $quote->total_amt = format_number($quote->total_amt);
    $quote->discount_amount = format_number($quote->discount_amount);
    $quote->subtotal_amount = format_number($quote->subtotal_amount);
    $quote->tax_amount = format_number($quote->tax_amount);
    if ($quote->shipping_amount != null) {
        $quote->shipping_amount = format_number($quote->shipping_amount);
    }
    $quote->total_amount = format_number($quote->total_amount);
    $quote->save();

    //Setting Invoice Values
    $invoice = BeanFactory::newBean('AOS_Invoices');
    $rawRow = $quote->fetched_row;
    $rawRow['id'] = '';
    $rawRow['template_ddown_c'] = ' ';
    $rawRow['quote_number'] = $rawRow['number'];
    $rawRow['number'] = '';
    $dt = explode(' ', $rawRow['date_entered']);
    $rawRow['quote_date'] = $dt[0];
    $rawRow['invoice_date'] = date('Y-m-d');
    $rawRow['total_amt'] = format_number($rawRow['total_amt']);
    $rawRow['discount_amount'] = format_number($rawRow['discount_amount']);
    $rawRow['subtotal_amount'] = format_number($rawRow['subtotal_amount']);
    $rawRow['tax_amount'] = format_number($rawRow['tax_amount']);
    $rawRow['date_entered'] = '';
    $rawRow['date_modified'] = '';
    if ($rawRow['shipping_amount'] != null) {
        $rawRow['shipping_amount'] = format_number($rawRow['shipping_amount']);
    }
    $rawRow['total_amount'] = format_number($rawRow['total_amount']);
    $invoice->populateFromRow($rawRow);
    $invoice->populateDefaultValues();
    $invoice->process_save_dates =false;
    $invoice->save();

    //Setting invoice quote relationship
    require_once('modules/Relationships/Relationship.php');
    $key = Relationship::retrieve_by_modules('AOS_Quotes', 'AOS_Invoices', $GLOBALS['db']);
    if (!empty($key)) {
        $quote->load_relationship($key);
        $quote->$key->add($invoice->id);
    }

    //Setting Group Line Items
    $sql = "SELECT * FROM aos_line_item_groups WHERE parent_type = 'AOS_Quotes' AND parent_id = '".$quote->id."' AND deleted = 0";
    $result = $this->bean->db->query($sql);
    $quoteToInvoiceGroupIds = array();
    while ($row = $this->bean->db->fetchByAssoc($result)) {
        $quoteGroupId = $row['id'];
        $row['id'] = '';
        $row['parent_id'] = $invoice->id;
        $row['parent_type'] = 'AOS_Invoices';
        if ($row['total_amt'] != null) {
            $row['total_amt'] = format_number($row['total_amt']);
        }
        if ($row['discount_amount'] != null) {
            $row['discount_amount'] = format_number($row['discount_amount']);
        }
        if ($row['subtotal_amount'] != null) {
            $row['subtotal_amount'] = format_number($row['subtotal_amount']);
        }
        if ($row['tax_amount'] != null) {
            $row['tax_amount'] = format_number($row['tax_amount']);
        }
        if ($row['subtotal_tax_amount'] != null) {
            $row['subtotal_tax_amount'] = format_number($row['subtotal_tax_amount']);
        }
        if ($row['total_amount'] != null) {
            $row['total_amount'] = format_number($row['total_amount']);
        }
        $group_invoice = BeanFactory::newBean('AOS_Line_Item_Groups');
        $group_invoice->populateFromRow($row);
        $group_invoice->save();
        $quoteToInvoiceGroupIds[$quoteGroupId] = $group_invoice->id;
    }

    //Setting Line Items
    $sql = "SELECT * FROM aos_products_quotes WHERE parent_type = 'AOS_Quotes' AND parent_id = '".$quote->id."' AND deleted = 0";
    $result = $this->bean->db->query($sql);
    while ($row = $this->bean->db->fetchByAssoc($result)) {
        $row['id'] = '';
        $row['parent_id'] = $invoice->id;
        $row['parent_type'] = 'AOS_Invoices';
        $row['group_id'] = $quoteToInvoiceGroupIds[$row['group_id']];
        if ($row['product_cost_price'] != null) {
            $row['product_cost_price'] = format_number($row['product_cost_price']);
        }
        $row['product_list_price'] = format_number($row['product_list_price']);
        if ($row['product_discount'] != null) {
            $row['product_discount'] = format_number($row['product_discount']);
            $row['product_discount_amount'] = format_number($row['product_discount_amount']);
        }
        $row['product_unit_price'] = format_number($row['product_unit_price']);
        $row['vat_amt'] = format_number($row['vat_amt']);
        $row['product_total_price'] = format_number($row['product_total_price']);
        $row['product_qty'] = format_number($row['product_qty']);
        $prod_invoice = BeanFactory::newBean('AOS_Products_Quotes');
        $prod_invoice->populateFromRow($row);
        $prod_invoice->save();
    }
    ob_clean();
    header('Location: index.php?module=AOS_Invoices&action=EditView&record='.$invoice->id);
