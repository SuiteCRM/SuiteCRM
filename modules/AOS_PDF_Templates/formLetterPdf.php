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

use SuiteCRM\PDF\Exceptions\PDFException;
use SuiteCRM\PDF\PDFWrapper;

require_once('modules/AOS_PDF_Templates/templateParser.php');
require_once('modules/AOS_PDF_Templates/AOS_PDF_Templates.php');
// STIC-Custom 20260813 ART - Repeated Headers in PDF Templates with Invoices
// https://github.com/SinergiaTIC/SinergiaCRM/pull/1381
require_once('custom/modules/AOS_PDF_Templates/SticGeneratePdfFunctions.php');
// END STIC-Custom

global $sugar_config, $current_user;

$bean = BeanFactory::getBean($_REQUEST['module']);

if (!$bean) {
    sugar_die("Invalid Module");
}

$recordIds = array();

if (isset($_REQUEST['current_post']) && $_REQUEST['current_post'] != '') {
    $order_by = '';
    require_once('include/MassUpdate.php');
    $mass = new MassUpdate();
    $mass->generateSearchWhere($_REQUEST['module'], $_REQUEST['current_post']);
    $ret_array = create_export_query_relate_link_patch($_REQUEST['module'], $mass->searchFields, $mass->where_clauses);

    // STIC-Custom 20260112 EPS - Adding SG check as in export_utils.php
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/928
    // $query = $bean->create_export_query($order_by, $ret_array['where'], $ret_array['join']);
    $where = $ret_array['where'];

    if ($bean->bean_implements('ACL')) {
        if (!ACLController::checkAccess($bean->module_dir, 'export', true)) {
            ACLController::displayNoAccess();
            sugar_die('');
        }

        $accessWhere = $bean->buildAccessWhere('export');
        if (!empty($accessWhere)) {
            $where .= empty($where) ? $accessWhere : ' AND ' . $accessWhere;
        }
    }
    
    
    $query = $bean->create_export_query($order_by, $where, $ret_array['join']);
    // END STIC
    $result = DBManagerFactory::getInstance()->query($query, true);
    $uids = array();
    while ($val = DBManagerFactory::getInstance()->fetchByAssoc($result, false)) {
        $recordIds[] = $val['id'];
    }
} else {
    $recordIds = explode(',', $_REQUEST['uid']);
}


$template = BeanFactory::getBean('AOS_PDF_Templates', $_REQUEST['templateID']);

if (!$template) {
    sugar_die("Invalid Template");
}

$file_name = str_replace(" ", "_", (string) $template->name) . ".pdf";

$pdfConfig = [
    'mode' => 'en',
    'page_size' => $template->page_size,
    'font' => 'DejaVuSansCondensed',
    'margin_left' => $template->margin_left,
    'margin_right' => $template->margin_right,
    'margin_top' => $template->margin_top,
    'margin_bottom' => $template->margin_bottom,
    'margin_header' => $template->margin_header,
    'margin_footer' => $template->margin_footer,
    'orientation' => $template->orientation
];

try {
    $pdf = PDFWrapper::getPDFEngine();
    $pdf->configurePDF($pdfConfig);
} catch (PDFException $e) {
    LoggerManager::getLogger()->warn('PDFException: ' . $e->getMessage());
}
$count = 0;
foreach ($recordIds as $recordId) {
    $bean->retrieve($recordId);

    try {
        $pdfHistory = PDFWrapper::getPDFEngine();
        $pdfHistory->configurePDF($pdfConfig);
    } catch (PDFException $e) {
        LoggerManager::getLogger()->warn('PDFException: ' . $e->getMessage());
    }

    $object_arr = array();
    $object_arr[$bean->module_dir] = $bean->id;

    if ($bean->module_dir === 'Contacts') {
        $object_arr['Accounts'] = $bean->account_id;
    }

    $search = array(
        '@<script[^>]*?>.*?</script>@si',        // Strip out javascript
        '@<[\/\!]*?[^<>]*?>@si',        // Strip out HTML tags
        '@([\r\n])[\s]+@',            // Strip out white space
        '@&(quot|#34);@i',            // Replace HTML entities
        '@&(amp|#38);@i',
        '@&(lt|#60);@i',
        '@&(gt|#62);@i',
        '@&(nbsp|#160);@i',
        '@&(iexcl|#161);@i',
        '@<address[^>]*?>@si'
    );

    $replace = array(
        '',
        '',
        '\1',
        '"',
        '&',
        '<',
        '>',
        ' ',
        chr(161),
        '<br>'
    );

    $text = preg_replace($search, $replace, (string) $template->description);
    $text = preg_replace_callback(
        '/{DATE\s+(.*?)}/',
        function ($matches) {
            return date($matches[1]);
        },
        $text
    );
    // STIC-Custom 20240125 JBL - Product line items in pdf
    // https://github.com/SinergiaTIC/SinergiaCRM/pull/76
    if (str_starts_with($_REQUEST['module'], "AOS_")) {
        $variableName = strtolower($bean->module_dir);
        $lineItemsGroups = array();
        $lineItems = array();

        $sql = "SELECT pg.id, pg.product_id, pg.group_id FROM aos_products_quotes pg LEFT JOIN aos_line_item_groups lig ON pg.group_id = lig.id WHERE pg.parent_type = '" . $bean->object_name . "' AND pg.parent_id = '" . $bean->id . "' AND pg.deleted = 0 ORDER BY lig.number ASC, pg.number ASC";
        $res = $bean->db->query($sql);
        while ($row = $bean->db->fetchByAssoc($res)) {
            $lineItemsGroups[$row['group_id']][$row['id']] = $row['product_id'];
            $lineItems[$row['id']] = $row['product_id'];
        }

        //backward compatibility
        if (isset($bean->billing_account_id)) {
            $object_arr['Accounts'] = $bean->billing_account_id;
        }
        if (isset($bean->billing_contact_id)) {
            $object_arr['Contacts'] = $bean->billing_contact_id;
        }
        if (isset($bean->assigned_user_id)) {
            $object_arr['Users'] = $bean->assigned_user_id;
        }
        if (isset($bean->currency_id)) {
            $object_arr['Currencies'] = $bean->currency_id;
        }

        $text = str_replace("\$aos_quotes", "\$" . $variableName, $text);
        $text = str_replace("\$aos_invoices", "\$" . $variableName, $text);
        $text = str_replace("\$total_amt", "\$" . $variableName . "_total_amt", $text);
        $text = str_replace("\$discount_amount", "\$" . $variableName . "_discount_amount", $text);
        $text = str_replace("\$subtotal_amount", "\$" . $variableName . "_subtotal_amount", $text);
        $text = str_replace("\$tax_amount", "\$" . $variableName . "_tax_amount", $text);
        $text = str_replace("\$shipping_amount", "\$" . $variableName . "_shipping_amount", $text);
        $text = str_replace("\$total_amount", "\$" . $variableName . "_total_amount", $text);

        // STIC-Custom 20260813 ART - Repeated Headers in PDF Templates with Invoices
        // https://github.com/SinergiaTIC/SinergiaCRM/pull/1381
        $text = stic_populate_group_lines($text, $lineItemsGroups, $lineItems);
        // END STIC-Custom
    }
    // END STIC-Custom 20240125

    $header = preg_replace($search, $replace, (string) $template->pdfheader);
    $footer = preg_replace($search, $replace, (string) $template->pdffooter);

    $converted = templateParser::parse_template($text, $object_arr);
    $header = templateParser::parse_template($header, $object_arr);
    $footer = templateParser::parse_template($footer, $object_arr);

    $printable = str_replace("\n", "<br />", (string) $converted);

    try {
        $note = BeanFactory::newBean('Notes');
        $note->modified_user_id = $current_user->id;
        $note->created_by = $current_user->id;
        $note->name = $file_name;
        $note->parent_type = $bean->module_dir;
        $note->parent_id = $bean->id;
        $note->file_mime_type = 'application/pdf';
        $note->filename = $file_name;
        if ($bean->module_dir == 'Contacts') {
            $note->contact_id = $bean->id;
            $note->parent_type = 'Accounts';
            $note->parent_id = $bean->account_id;
        }
        $note->save();

        $fp = fopen($sugar_config['upload_dir'] . 'nfile.pdf', 'wb');
        fclose($fp);

        $pdfHistory->writeHeader($header);
        $pdfHistory->writeFooter($footer);
        $pdfHistory->writeHTML($printable);
        $pdfHistory->outputPDF($sugar_config['upload_dir'] . 'nfile.pdf', 'F', $note->name);

        if ($count > 0) {
            $pdf->writeBlankPage();
        }
        $pdf->writeHeader($header);
        $pdf->writeFooter($footer);
        $pdf->writeHTML($printable);

        rename($sugar_config['upload_dir'] . 'nfile.pdf', $sugar_config['upload_dir'] . $note->id);
    } catch (PDFException $e) {
        LoggerManager::getLogger()->warn('PDFException: ' . $e->getMessage());
    }
    ++$count;
}

$pdf->outputPDF($file_name, 'D');

// STIC-Custom 20260813 ART - Repeated Headers in PDF Templates with Invoices
// https://github.com/SinergiaTIC/SinergiaCRM/pull/1381
// populate_group_lines(), populate_product_lines() and populate_service_lines() were moved to
// custom/modules/AOS_PDF_Templates/SticGeneratePdfFunctions.php as stic_populate_group_lines(),
// stic_populate_product_lines() and stic_populate_service_lines().
// END STIC-Custom
