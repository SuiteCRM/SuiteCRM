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

class Person extends Basic
{
    public $photo;
    public $first_name;
    public $last_name;
    public $full_name;
    public $salutation;
    public $title;
    public $email1;
    public $phone_fax;
    public $phone_work;
    public $phone_other;
    public $lawful_basis;
    public $date_reviewed;
    public $lawful_basis_source;


    /**
     * @var bool controls whether or not to invoke the getLocalFormattedName method with title and salutation
     */
    public $createLocaleFormattedName = true;

    /**
     * @var Link2
     */
    public $email_addresses;

    /**
     * @var SugarEmailAddress
     */
    public $emailAddress;

    /**
     * Person constructor.
     */
    public function __construct()
    {
        parent::__construct();
        $this->emailAddress = new SugarEmailAddress();
    }



    /**
     * need to override to have a name field created for this class
     *
     * @see parent::retrieve()
     *
     * @param int $id
     * @param bool $encode
     * @param bool $deleted
     *
     * @return SugarBean
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        $ret_val = parent::retrieve($id, $encode, $deleted);
        $this->_create_proper_name_field();

        return $ret_val;
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
     * This function helps generate the name and full_name member field variables from the salutation, title,
     * first_name and last_name fields. It takes into account the locale format settings as well as ACL settings if
     * supported.
     */
    public function _create_proper_name_field()
    {
        global $locale, $app_list_strings;

        // Bug# 46125 - make first name, last name, salutation and title of Contacts respect field level ACLs
        $salutation = '';

        // first name has at least read access
        $firstName = $this->first_name;

        // last name has at least read access
        $lastName = $this->last_name;

        // salutation has at least read access
        if (isset($this->field_defs['salutation']['options']) &&
            isset($app_list_strings[$this->field_defs['salutation']['options']]) &&
            isset($app_list_strings[$this->field_defs['salutation']['options']][$this->salutation])
        ) {
            $salutation = $app_list_strings[$this->field_defs['salutation']['options']][$this->salutation];
        } // if

        // last name has at least read access
        $title = $this->title;

        // Corner Case:
        // Both first name and last name cannot be empty, at least one must be shown
        // In that case, we can ignore field level ACL and just display last name...
        // In the ACL field level access settings, last_name cannot be set to "none"
        if (empty($firstName) && empty($lastName)) {
            $full_name = $locale->getLocaleFormattedName('', $lastName, $salutation, $title);
        } else {
            if ($this->createLocaleFormattedName) {
                $full_name = $locale->getLocaleFormattedName($firstName, $lastName, $salutation, $title);
            } else {
                $full_name = $locale->getLocaleFormattedName($firstName, $lastName);
            }
        }

        $this->name = $full_name;
        $this->full_name = $full_name; //used by campaigns
    }

    /**
     * @see parent::save()
     */
    public function save($check_notify = false)
    {
        // If we are saving due to relationship changes, don't bother trying to update the emails
        if (!empty($GLOBALS['resavingRelatedBeans'])) {
            parent::save($check_notify);

            return $this->id;
        }
        $this->add_address_streets('primary_address_street');
        $this->add_address_streets('alt_address_street');
        $ori_in_workflow = empty($this->in_workflow) ? false : true;
	$this->emailAddress->handleLegacySave($this);

        if (empty($this->id)) {
            $this->id = create_guid();
            $this->new_with_id = true;
        }

        // bug #39188 - store emails state before workflow make any changes
        $this->emailAddress->stash($this->id, $this->module_dir);
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

	parent::save($check_notify);
        return $this->id;
    }

    /**
     * @see parent::get_summary_text()
     */
    public function get_summary_text()
    {
        $this->_create_proper_name_field();

        return $this->name;
    }

    /**
     * @see parent::get_list_view_data()
     */
    public function get_list_view_data()
    {
        global $current_user;

        $this->_create_proper_name_field();
        $temp_array = $this->get_list_view_array();

        $temp_array['NAME'] = $this->name;
        $temp_array['ENCODED_NAME'] = $this->full_name;
        $temp_array['FULL_NAME'] = $this->full_name;

        $temp_array['EMAIL1'] = $this->emailAddress->getPrimaryAddress($this);

        $this->email1 = $temp_array['EMAIL1'];
        $temp_array['EMAIL1_LINK'] = $current_user->getEmailLink('email1', $this, '', '', 'ListView');

        return $temp_array;
    }

    /**
     * @see SugarBean::populateRelatedBean()
     */
    public function populateRelatedBean(
        SugarBean $newBean
    ) {
        parent::populateRelatedBean($newBean);

        if ($newBean instanceof Company) {
            $newBean->phone_fax = $this->phone_fax;
            $newBean->phone_office = $this->phone_work;
            $newBean->phone_alternate = $this->phone_other;
            $newBean->email1 = $this->email1;
            $this->add_address_streets('primary_address_street');
            $newBean->billing_address_street = $this->primary_address_street;
            $newBean->billing_address_city = $this->primary_address_city;
            $newBean->billing_address_state = $this->primary_address_state;
            $newBean->billing_address_postalcode = $this->primary_address_postalcode;
            $newBean->billing_address_country = $this->primary_address_country;
            $this->add_address_streets('alt_address_street');
            $newBean->shipping_address_street = $this->alt_address_street;
            $newBean->shipping_address_city = $this->alt_address_city;
            $newBean->shipping_address_state = $this->alt_address_state;
            $newBean->shipping_address_postalcode = $this->alt_address_postalcode;
            $newBean->shipping_address_country = $this->alt_address_country;
        }
    }

    /**
     * Default export query for Person based modules
     * used to pick all mails (primary and non-primary)
     *
     * @see SugarBean::create_export_query()
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

    /**
     * Set Lawful Basis
     * @param string $basis
     * @param string $source
     * @return int
     * @throws InvalidArgumentException
     */
    public function setLawfulBasis($basis, $source)
    {
        global $app_list_strings,$timedate;
        /**
         * This function will update the lawful basis, source and date of the change.
         * Will take the parameters of email id and possible the module?
         */

        if (!is_string($basis)) {
            throw new InvalidArgumentException('basis must be a string');
        }

        if (!array_key_exists($basis, $app_list_strings['lawful_basis_dom'])) {
            throw new InvalidArgumentException('invalid lawful basis');
        }

        if (!is_string($source)) {
            throw new InvalidArgumentException('source for lawful basis must be a string');
        }

        if (!array_key_exists($source, $app_list_strings['lawful_basis_source_dom'])) {
            throw new InvalidArgumentException('invalid lawful basis source');
        }

        //Set lawful basis, lawful basis source and date reviewed
        $this->lawful_basis = '^'.$basis.'^';
        $this->lawful_basis_source = $source;
        $date = TimeDate::getInstance()->nowDb();
        $date_test = $timedate->to_display_date($date, false);
        $this->date_reviewed = $date_test;

        return (bool)$this->save();
    }
}
