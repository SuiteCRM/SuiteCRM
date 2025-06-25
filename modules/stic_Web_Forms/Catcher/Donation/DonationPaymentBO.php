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

require_once __DIR__ . "/../Include/Payment/PaymentBO.php";

#[AllowDynamicProperties]
class DonationPaymentBO extends PaymentBO
{
    /**
     * Overload the method of generating payment to carry out subsequent actions of donations only
     * @see PaymentBO::newPayment()
     */
    public function newPayment($namePrefix = '')
    {
        $payment = parent::newPayment();
        if ($payment != null) {
            $fp = $this->getLastPC();

            // Assign the campaign id
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Linking payment method to the campaign [{$this->campaign->id}]");
            $fp->stic_payment_commitments_campaignscampaigns_ida = $this->campaign->id;
            // Save the payment commitment with "false" param to avoid the CRM sending assigned records notifications
            $fp->save(false);

            // Create the attached documents and relationships with the person or organization
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":  Generating the attached documents and relationships.");
            $this->createAttachedDocumentsAndRelationships($this->payer, $fp->name, $this->campaign->name);
        }
        return $payment;
    }

    /**
     * Method that creates the attached documents and relations with the person or organization
     */
    protected function createAttachedDocumentsAndRelationships($objLink, $paymentName, $campaignName)
    {
        // Manage attached files
        require_once 'modules/Documents/Document.php'; // Call to the Document class
        $total_files = isset($_FILES["documents"]["name"]) ? count($_FILES["documents"]["name"]) : 0; // Assignment of the number of attachments received from the form
        $objLink->load_relationship('documents'); // Loading of the relationship between the contacts/organizations module and the document module

        // For each document received from the form
        for ($i = 0; $i < $total_files; $i++) {
            // If the document exists
            if ($_FILES["documents"]['name'][$i] != "") {
                // Create the document
                $document = new Document();
                $document->document_name = $_FILES["documents"]['name'][$i];
                $document->filename = $_FILES["documents"]['name'][$i];
                $document->doc_type = "Sugar";
                $document->status_id = "Active";
                $document->revision = 1;
                $document->assigned_user_id = $this->actionDefParams['assigned_user_id'];
                $document->set_created_by = false;
                $document->created_by = $this->actionDefParams['assigned_user_id'];
                $document->update_modified_by = false;
                $document->modified_user_id = $document->created_by;
                $document->description = $this->getMsgString('LBL_DONATION_ATTACHMENT_DESCRIPTION') . $campaignName . ' (' . $this->getMsgString('LBL_DONATION_ATTACHMENT_DESCRIPTION2') . $paymentName . ')';
                $document->save();

                // Version 1 of the document that has just been generated is added
                $revision = new DocumentRevision();
                $revision->id = $document->document_revision_id;
                $revision->set_created_by = false;
                $revision->created_by = $this->actionDefParams['assigned_user_id'];
                $revision->update_modified_by = false;
                $revision->modified_user_id = $revision->created_by;
                $revision->not_use_rel_in_req = true;
                $revision->new_rel_id = $document->id;
                $revision->new_rel_relname = 'Documents';
                $revision->change_log = translate('DEF_CREATE_LOG', 'Documents');
                $revision->revision = 1;
                $revision->document_id = $document->id;
                $revision->doc_type = "Sugar";
                $revision->filename = $_FILES["documents"]['name'][$i];
                $revision->file_ext = pathinfo($_FILES["documents"]['name'][$i], PATHINFO_EXTENSION);
                $revision->file_mime_type = mime_content_type($_FILES["documents"]['tmp_name'][$i]);
                $revision->save();

                move_uploaded_file($_FILES["documents"]['tmp_name'][$i], "upload/$revision->id");
                chmod("upload/" . $revision->id, 0777);

                // Finally, the uploaded document is linked to the donor contact/organization
                $objLink->documents->add($document->id);

                unset($document);
                unset($revision);
            }
        }
    }
}
