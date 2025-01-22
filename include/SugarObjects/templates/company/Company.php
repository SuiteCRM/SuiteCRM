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

require_once 'include/SugarObjects/templates/basic/Basic.php';

class Company extends Basic
{
    public $emailAddress;
    public $email1;

    /**
     * Constructor
     */
    public function __construct()
    {
        parent::__construct();
        $this->emailAddress = new SugarEmailAddress();
    }



    /**
     * @see parent::save()
     *
     * @param bool $check_notify
     *
     * @return string
     */
    public function save($check_notify = false)
    {
        if (!empty($GLOBALS['resavingRelatedBeans'])) {
            return parent::save($check_notify);
        }
        $this->add_address_streets('billing_address_street');
        $this->add_address_streets('shipping_address_street');
        $ori_in_workflow = empty($this->in_workflow) ? false : true;
	$this->emailAddress->handleLegacySave($this);

        if (empty($this->id)) {
	    $this->id = create_guid();
            $this->new_with_id = true;
	}

        $override_email = array();
        if (!empty($this->email1_set_in_workflow)) {
            $override_email['emailAddress0'] = $this->email1_set_in_workflow;
        }
        if (!empty($this->email2_set_in_workflow)) {
            $override_email['emailAddress1'] = $this->email2_set_in_workflow;
        }
        if (!isset($this->in_workflow)) {
            $this->in_workflow = false;
        }
        if ($ori_in_workflow === false || !empty($override_email)) {
            $this->emailAddress->saveEmail(
                $this->id,
                $this->module_dir,
                $override_email,
                '',
                '',
                '',
                '',
                $this->in_workflow,
                isset($_REQUEST['shouldSaveOptInFlag']) && $_REQUEST['shouldSaveOptInFlag'] ? true : null
            );
        }

	$record_id = parent::save($check_notify);
        return $record_id;
    }

    /**
     * Populate email address fields here instead of retrieve() so that they are properly available for logic hooks
     *
     * @see parent::fill_in_relationship_fields()
     */
    public function fill_in_relationship_fields()
    {
        parent::fill_in_relationship_fields();
        $this->emailAddress->handleLegacyRetrieve($this);
    }

    /**
     * @see parent::get_list_view_data()
     */
    public function get_list_view_data()
    {
        global $current_user;

        $temp_array = $this->get_list_view_array();

        $temp_array['EMAIL1'] = $this->emailAddress->getPrimaryAddress($this);

        $this->email1 = $temp_array['EMAIL1'];

        $temp_array['EMAIL1_LINK'] = $current_user->getEmailLink('email1', $this, '', '', 'ListView');

        return $temp_array;
    }

    /**
     * Default export query for Company based modules
     * used to pick all mails (primary and non-primary)
     *
     * @see SugarBean::create_export_query()
     *
     * @param string $order_by
     * @param string $where
     * @param string $relate_link_join
     *
     * @return string
     */
    public function create_export_query($order_by, $where, $relate_link_join = '')
    {
        $custom_join = $this->custom_fields->getJOIN(true, true, $where);

        // For easier code reading, reused plenty of time
        $table = $this->table_name;

        if ($custom_join) {
            $custom_join['join'] .= $relate_link_join;
        }
        $query = "SELECT
					$table.*,
					email_addresses.email_address email_address,
					'' email_addresses_non_primary, " .
                 // email_addresses_non_primary needed for get_field_order_mapping()
                 'users.user_name as assigned_user_name ';
        if ($custom_join) {
            $query .= $custom_join['select'];
        }

        $query .= " FROM $table ";

        $query .= "LEFT JOIN users
					ON $table.assigned_user_id=users.id ";

        //Join email address table too.
        $query .= " LEFT JOIN email_addr_bean_rel on $table.id = email_addr_bean_rel.bean_id and email_addr_bean_rel.bean_module = '" .
                  $this->module_dir .
                  "' and email_addr_bean_rel.deleted = 0 and email_addr_bean_rel.primary_address = 1";
        $query .= ' LEFT JOIN email_addresses on email_addresses.id = email_addr_bean_rel.email_address_id ';

        if ($custom_join) {
            $query .= $custom_join['join'];
        }

        $where_auto = " $table.deleted=0 ";

        if (!empty($where)) {
            $query .= "WHERE ($where) AND " . $where_auto;
        } else {
            $query .= 'WHERE ' . $where_auto;
        }

        $order_by = $this->process_order_by($order_by);
        if (!empty($order_by)) {
            $query .= ' ORDER BY ' . $order_by;
        }

        return $query;
    }
}
