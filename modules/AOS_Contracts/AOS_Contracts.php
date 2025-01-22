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

/**
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/AOS_Contracts/AOS_Contracts_sugar.php');

#[\AllowDynamicProperties]
class AOS_Contracts extends AOS_Contracts_sugar
{
    public function __construct()
    {
        parent::__construct();

        //Process the default reminder date setting
        if ($this->id == null && $this->renewal_reminder_date == null) {
            global $sugar_config, $timedate;

            $default_time = "12:00:00";

            $period = empty($sugar_config['aos'])?false:(int)$sugar_config['aos']['contracts']['renewalReminderPeriod'];

            //Calculate renewal date from end_date minus $period days and format this.
            if ($period && !empty($this->end_date)) {
                $renewal_date = $timedate->fromUserDate($this->end_date);

                $renewal_date->modify("-$period days");
                $time_value = $timedate->fromString($default_time);
                $renewal_date->setTime($time_value->hour, $time_value->min, $time_value->sec);

                $renewal_date = $renewal_date->format($timedate->get_date_time_format());
                $this->renewal_reminder_date = $renewal_date;
            }
        }
    }




    public function save($check_notify = false)
    {
        if (empty($this->id) || (isset($_POST['duplicateSave']) && $_POST['duplicateSave'] == 'true')) {
            unset($_POST['group_id']);
            unset($_POST['product_id']);
            unset($_POST['service_id']);
        }

        if (isset($_POST['renewal_reminder_date']) && !empty($_POST['renewal_reminder_date'])) {
            $this->createReminder();
        }

        require_once('modules/AOS_Products_Quotes/AOS_Utils.php');

        perform_aos_save($this);

        $return_id = parent::save($check_notify);

        require_once('modules/AOS_Line_Item_Groups/AOS_Line_Item_Groups.php');
        $productQuoteGroup = BeanFactory::newBean('AOS_Line_Item_Groups');
        $productQuoteGroup->save_groups($_POST, $this, 'group_');

        if (isset($_POST['renewal_reminder_date']) && !empty($_POST['renewal_reminder_date'])) {
            $this->createLink();
        }
        return $return_id;
    }

    public function mark_deleted($id)
    {
        $productQuote = BeanFactory::newBean('AOS_Products_Quotes');
        $productQuote->mark_lines_deleted($this);
        $this->deleteCall();
        parent::mark_deleted($id);
    }

    public function createReminder()
    {
        global $mod_strings;
        require_once('modules/Calls/Call.php');
        $call = new call();

        if ($this->renewal_reminder_date != 0) {
            if (!isset($this->call_id)) {
                LoggerManager::getLogger()->warn('Call is not set for reminder creation.');
                $call->id = null;
            } else {
                $call->id = $this->call_id;
            }
            $call->parent_id = $this->id;
            $call->parent_type = 'AOS_Contracts';
            $call->date_start = $this->renewal_reminder_date;
            $call->name = $this->name . $mod_strings['LBL_RENEWAL_REMINDER'];
            $call->assigned_user_id = $this->assigned_user_id;
            $call->status = 'Planned';
            $call->direction = 'Outbound';
            $call->reminder_time = 60;
            $call->duration_hours = 0;
            $call->duration_minutes = 30;
            $call->deleted = 0;
            $call->save();
            $this->call_id = $call->id;
        }
    }

    public function createLink()
    {
        require_once('modules/Calls/Call.php');
        $call = new call();

        if ($this->renewal_reminder_date != 0) {
            $call->id = $this->call_id;

            if (!isset($this->contract_account_id)) {
                LoggerManager::getLogger()->warn('Contract Account ID not defined for AOS Contracts / create link.');
                $contractAccountId = null;
            } else {
                $contractAccountId = $this->contract_account_id;
            }
            $call->parent_id = $contractAccountId;
            $call->parent_type = 'Accounts';
            $call->reminder_time = 60;
            $call->save();
        }
    }

    public function deleteCall()
    {
        require_once('modules/Calls/Call.php');
        $call = new call();

        if (!isset($this->call_id)) {
            LoggerManager::getLogger()->warn('Call ID not found for AOS Contract / delete call.');
            $callId = null;
        } else {
            $callId = $this->call_id;
        }

        if ($callId != null) {
            $call->id = $this->call_id;
            $call->mark_deleted($call->id);
        }
    }
}
