<?php
/**
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

/**
 * Handles the generation and finalization of a signed PDF document
 * by retrieving template data, inserting signature/acceptance images,
 * adding an audit page (if configured), and saving the resulting PDF file.
 */
class sticGenerateSignedPdf
{
    /**
     * Generates a signed PDF for a given signer ID and saves it to the file system.
     * The generated PDF file name is stored in the 'pdf_document' field of the signer record.
     *
     * @param string $signedMode The mode of signing, either 'handwritten' (for drawn signature) or 'button' (for acceptance).
     * Defaults to 'handwritten'.
     * @return string The file path of the generated PDF document.
     */
    public static function generateSignaturePdf($signedMode = 'handwritten')
    {
        global $sugar_config, $app_list_strings, $app_strings;
        
        $mod_strings = return_module_language($GLOBALS['current_language'], 'stic_Signatures');
        
        // Required utility and function files
        require_once 'custom/modules/AOS_PDF_Templates/SticGeneratePdfFunctions.php';
        require_once 'modules/stic_Signatures/Utils.php';
        require_once 'modules/stic_Signers/Utils.php';

        // Check required parameters
        if (!isset($_REQUEST['signerId']) || empty($_REQUEST['signerId'])) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " No signerId received");
            sugar_die('No signerId received');
        }

        // Retrieve the signer bean
        $signerBean = BeanFactory::getBean('stic_Signers', $_REQUEST['signerId']);
        if (!$signerBean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Invalid Signer ID: {$_REQUEST['signerId']}");
            sugar_die("Invalid Signer");
        }

        // Retrieve the signature bean
        $signatureBean = BeanFactory::getBean('stic_Signatures', $signerBean->stic_signatures_stic_signersstic_signatures_ida);
        if (!$signatureBean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Invalid Signature ID: {$signerBean->signature_id} for Signer ID: {$signerBean->id}");
            sugar_die("Invalid Signature");
        }

        $sourceModule = $signatureBean->main_module;
        $sourceId = $signerBean->record_id;
        // Check that the signature has an associated record
        if (empty($sourceModule) || empty($sourceId)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Signature ID: {$signatureBean->id} has no associated record");
            $errMsg = translate('LBL_NO_SIGNATURE_FOR_SIGNER', 'stic_Signatures');
            SugarApplication::appendErrorMessage("<p class='label label-warning'>{$errMsg}</p>");
            SugarApplication::redirect('index.php?module=stic_Signers&action=DetailView&record=' . $signerBean->id);
            sugar_die("Signature has no associated record");
        }

        // Retrieve the PDF template bean
        $templateBean = BeanFactory::getBean('AOS_PDF_Templates', $signatureBean->pdftemplate_id_c);

        if (!$templateBean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Invalid Template ID: {$signatureBean->pdftemplate_id_c} for Signature ID: {$signatureBean->id}");
            sugar_die("Invalid Template");
        }

        $file_name = str_replace(" ", "_", (string) $templateBean->name) . ".pdf";

        // PDF Configuration based on the template bean
        $pdfConfig = [
            'mode' => 'en',
            'page_size' => $templateBean->page_size,
            'font' => 'DejaVuSansCondensed',
            'margin_left' => $templateBean->margin_left,
            'margin_right' => $templateBean->margin_right,
            'margin_top' => $templateBean->margin_top,
            'margin_bottom' => $templateBean->margin_bottom,
            'margin_header' => $templateBean->margin_header,
            'margin_footer' => $templateBean->margin_footer,
            'orientation' => $templateBean->orientation,
        ];

        try {
            $pdf = PDFWrapper::getPDFEngine();
            $pdf->configurePDF($pdfConfig);
        } catch (PDFException $e) {
            LoggerManager::getLogger()->warn('PDFException: ' . $e->getMessage());
        }

        // Retrieve the record bean (source of data for the PDF)
        $sourceBean = BeanFactory::getBean($sourceModule, $sourceId);
        if (!$sourceBean) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " Invalid Record ID: {$sourceId} for Module: {$sourceModule}");
            sugar_die("Invalid Record");
        }

        // PDF History (unused here, but configured)
        try {
            $pdfHistory = PDFWrapper::getPDFEngine();
            $pdfHistory->configurePDF($pdfConfig);
        } catch (PDFException $e) {
            LoggerManager::getLogger()->warn('PDFException: ' . $e->getMessage());
        }

        // Set time in user format and UTC for use later in audit/acceptance
        $userTime = (new DateTime())->format('Y-m-d H:i:s (\U\T\C P)');
        $utcTime = (new DateTime('UTC'))->format('Y-m-d H:i:s P');

        // If 'pdf_audit_page' is enabled, append an audit page to the PDF content
        if (!empty($signatureBean->pdf_audit_page) && $signatureBean->pdf_audit_page && $signedMode != 'unsigned') {

            // Get logs related to the signer
            require_once 'modules/stic_Signature_Log/Utils.php';
            $signerLog = stic_SignatureLogUtils::getSignatureLogActions($signerBean->id, 'SIGNER', ['OPEN_PORTAL_BEFORE_SIGN']);

            $sugar_smarty = new Sugar_Smarty();

            // Assign variables for the audit page template
            $sugar_smarty->assign('DOCUMENT_NAME', $templateBean->name);
            $sugar_smarty->assign('SIGNER_NAME', $signerBean->parent_name);
            $sugar_smarty->assign('SIGNER_EMAIL', $signerBean->email_address);
            $sugar_smarty->assign('SIGNER_PHONE', $signerBean->phone);
            $sugar_smarty->assign('SIGNER_USER_TIME', $userTime);
            $sugar_smarty->assign('SIGNER_MODE', $app_list_strings['stic_signatures_modes_list'][$signedMode]);
            $sugar_smarty->assign('SIGNER_STATUS', $app_list_strings['stic_signers_status_list'][$signerBean->status]);

            // If signing on behalf of someone else, include that information
            if ($signerBean->parent_id != $signerBean->contact_id_c) {
                $behalfName = BeanFactory::getBean('Contacts', $signerBean->contact_id_c)->full_name;
                $sugar_smarty->assign('SIGNER_ON_BEHALF_OF', $behalfName);
            }

            $sugar_smarty->assign('MOD_STRINGS', return_module_language($GLOBALS['current_language'], 'stic_Signatures'));
            $sugar_smarty->assign('APP_STRINGS', $app_strings);

            $sugar_smarty->assign('SIGNER_LOG', $signerLog);

            // Construct the audit HTML content
            $auditHtml = '<p style="page-break-before: always;">&nbsp;</p>';
            $auditHtml .= $sugar_smarty->fetch('modules/stic_Signatures/AuditPageTemplate.tpl');

            // Append the audit page HTML (encoded) to the template description
            $templateBean->description .= htmlspecialchars($auditHtml);
        }

        // Determine the signature image HTML based on signed mode. The signature
        // marker is normalized to a plain-text token inside
        // stic_SignaturesUtils::getParsedTemplate() (before the HTML cleaning
        // pipeline), so this replacement is applied AFTER the template is parsed.
        // Note: no closing </div> is appended, as an unmatched </div> makes TCPDF
        // drop the rest of the document when the signature is inside a table cell.
        $replaceWith = '';
        switch ($signedMode) {
            case 'handwritten':
                // Use the drawn signature image URL from the signer bean
                $replaceWith = '<img class="signature" src="' . $signerBean->signature_image . '" width="200">';
                break;
            case 'button':
                // Generate an acceptance image with signer details and timestamp
                $textArray = [
                    $mod_strings['LBL_PORTAL_DOCUMENT_ACCEPTED_BY'],
                    $signerBean->parent_name,
                    $signerBean->email_address,
                    $userTime,
                ];
                $acceptImage = stic_SignaturesUtils::generateAcceptImage($textArray);
                $replaceWith = '<img class="signature" src="' . $acceptImage . '" width="200">';
                break;
            default:
                // Default case, no replacement
                break;
        }

        // Final template parsing using the utility function, which handles
        // advanced placeholders.
        $parsedText = stic_SignaturesUtils::getParsedTemplate($signerBean->id);
        $converted = $parsedText['converted'];
        $header = $parsedText['header'];
        $footer = $parsedText['footer'];

        // Replace the token with the real image. Re-normalize first as a fallback
        // for residual placeholders introduced after getParsedTemplate().
        $converted = str_replace(stic_SignaturesUtils::SIGNATURE_TOKEN, $replaceWith, stic_SignaturesUtils::normalizeSignatureMarkers($converted));
        $header = str_replace(stic_SignaturesUtils::SIGNATURE_TOKEN, $replaceWith, stic_SignaturesUtils::normalizeSignatureMarkers($header));
        $footer = str_replace(stic_SignaturesUtils::SIGNATURE_TOKEN, $replaceWith, stic_SignaturesUtils::normalizeSignatureMarkers($footer));

        // Replace newlines with HTML line breaks for PDF generation
        $printable = str_replace("\n", "<br />", (string) $converted);

        try {

            // Define the file name and path for the generated PDF
            if ($signedMode == 'unsigned') {
                $fileName = "{$signerBean->id}_draft.pdf";
            } else {
                $fileName = "{$signerBean->id}_signed.pdf";
            }

            $filePath = $sugar_config['upload_dir'] . $fileName;

            // Generate the PDF and save it to the file system
            $pdf->writeHeader($header);
            $pdf->writeFooter($footer);
            ob_clean(); // avoid debug messages breaking the PDF
            $pdf->writeHTML($printable);
            $pdf->outputPDF($filePath, 'F'); // 'F' parameter saves to a local file

            // Store the file reference in the signer bean's 'pdf_document' field
            if ($signedMode != 'unsigned') {
                $signerBean->pdf_document = $fileName;
                $signerBean->save();
            }

            return $filePath;

        } catch (PDFException $e) {

            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . " PDF generation failed for Signer ID: {$signerBean->id} - " . $e->getMessage());
            sugar_die('PDF generation failed. Please contact the system administrator.');
        }
    }
}
