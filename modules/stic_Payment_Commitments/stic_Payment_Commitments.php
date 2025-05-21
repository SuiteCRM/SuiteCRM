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


#[\AllowDynamicProperties]
class stic_Payment_Commitments extends Basic
{
    public $new_schema = true;
    public $module_dir = 'stic_Payment_Commitments';
    public $object_name = 'stic_Payment_Commitments';
    public $table_name = 'stic_payment_commitments';
    public $importable = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;
    public $SecurityGroups;
    public $payment_type;
    public $bank_account;
    public $channel;
    public $periodicity;
    public $amount;
    public $first_payment_date;
    public $end_date;
    public $payment_method;
    public $transaction_type;
    public $signature_date;
    public $mandate;
    public $annualized_fee;
    public $segmentation;
    public $in_kind_donation;
    public $banking_concept;
    public $destination;
	
    public function bean_implements($interface)
    {
        switch($interface)
        {
            case 'ACL':
                return true;
        }

        return false;
    }

    /**
     *  Overriding SugarBean save function to introduce additional logic:
     *  1) On Payments subpanel changes, set the proper contact/account in payments
     *
     * @param boolean $check_notify
     * @return void
     */
    public function save($check_notify = true) {

        // Call the generic save() function from the SugarBean class
        parent::save($check_notify);
        
        // If things are happening in Payments subpanel...
        if (!empty($_REQUEST['child_field']) && $_REQUEST['child_field'] == 'stic_payments_stic_payment_commitments') {
            // For multiple payments selection from popup
            if (is_array($_REQUEST['subpanel_id'])) {
                foreach($_REQUEST['subpanel_id'] as $paymentId) {
                    $this->setContactOrAccountToPayment($paymentId);
                }
            } else { // Single payment selection
                $this->setContactOrAccountToPayment($_REQUEST['subpanel_id']);
            }
            
        }
    }
    
    protected function setContactOrAccountToPayment($paymentId) {
        // Get payment bean
        $paymentBean = BeanFactory::getBean('stic_Payments', $paymentId);
	    
        // At this point, the payment bean could be saved by code and the payment:save() function
        // would be called without need of aditional code below. However, in that case, it is been
        // verified that the payment bean is still attached to the old payment commitment, so the
        // relationship with the new account/contact wouldn't be properly created. That's why there's
        // no such call to payment:save() and there's need for the code below.
	    
        // Get payment commmitment related contact (usual case)            
        if ($contactId = $this->stic_payment_commitments_contactscontacts_ida) {
            // Remove previous relationship with an account, if any 
            // (a payment can only be related with a single contact or account, not both)
            $paymentBean->load_relationship('stic_payments_accounts');
            foreach($paymentBean->stic_payments_accounts->getBeans() as $accountBean){
                $paymentBean->stic_payments_accounts->delete($accountBean->id);
            }
            // Set the relationship between payment and contact
            $paymentBean->load_relationship('stic_payments_contacts');
            $paymentBean->stic_payments_contacts->add($contactId);
        } else {
            // Remove previous relationship with a contact, if any 
            // (a payment can only be related with a single contact or account, not both)
            $paymentBean->load_relationship('stic_payments_contacts');
            foreach($paymentBean->stic_payments_contacts->getBeans() as $contactBean){
                $paymentBean->stic_payments_contacts->delete($contactBean->id);
            }
            // Get payment commitment related account
            if ($accountId = $this->stic_payment_commitments_accountsaccounts_ida) {
                // Set the relationship between payment and account
                $paymentBean->load_relationship('stic_payments_accounts');
                $paymentBean->stic_payments_accounts->add($accountId);
            }
        }
    }
}
