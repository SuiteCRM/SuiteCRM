<?php
/**
 * Products, Quotations & Invoices modules.
 * Extensions to SugarCRM
 * @package Advanced OpenSales for SugarCRM
 * @subpackage Products
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
 * @author SalesAgility Ltd <support@salesagility.com>
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
