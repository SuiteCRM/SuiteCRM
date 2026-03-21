<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

require_once('include/MVC/Controller/SugarController.php');

#[\AllowDynamicProperties]
class AOS_ContractsController extends SugarController
{
    public function action_editview()
    {
        global $mod_string;

        $this->view = 'edit';
        $GLOBALS['view'] = $this->view;

        if (isset($_REQUEST['aos_quotes_id'])) {
            $query = "SELECT * FROM aos_quotes WHERE id = '?'";
            $result = $this->bean->db->pquery($query, [$_REQUEST['aos_quotes_id']]);
            $row = $this->bean->db->fetchByAssoc($result);
            $this->bean->name = $row['name'];
            $this->bean->total_contract_value = $row['total_amount'];

            if (isset($row['billing_account_id'])) {
                $_REQUEST['account_id'] = $row['billing_account_id'];
            }

            if (isset($row['billing_contact_id'])) {
                $_REQUEST['contact_id'] = $row['billing_contact_id'];
            }

            if (isset($row['opportunity_id'])) {
                $_REQUEST['opportunity_id'] = $row['opportunity_id'];
            }
        }

        if (isset($_REQUEST['account_id'])) {
            $query = "SELECT id,name FROM accounts WHERE id = '?'";
            $result = $this->bean->db->pquery($query, [$_REQUEST['account_id']]);
            $row = $this->bean->db->fetchByAssoc($result);
            if ($row){
                $this->bean->contract_account = $row['name'];
                $this->bean->contract_account_id = $row['id'];
            }
        }

        if (isset($_REQUEST['contact_id'])) {
            $contact = BeanFactory::newBean('Contacts');
            $contact->retrieve($_REQUEST['contact_id']);
            $this->bean->contact = $contact->name;
            $this->bean->contact_id = $contact->id;
        }

        if (isset($_REQUEST['opportunity_id'])) {
            $query = "SELECT id,name FROM opportunities WHERE id = '?'";
            $result = $this->bean->db->pquery($query, [$_REQUEST['opportunity_id']]);
            $row = $this->bean->db->fetchByAssoc($result);
            $this->bean->opportunity = $row['name'];
            $this->bean->opportunity_id = $row['id'];
        }
    }
}
