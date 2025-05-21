<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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







// Contact is used to store customer information.
#[\AllowDynamicProperties]
class ContactOpportunityRelationship extends SugarBean
{
    // Stored fields
    public $id;
    public $contact_id;
    public $contact_role;
    public $opportunity_id;

    // Related fields
    public $contact_name;
    public $opportunity_name;

    public $table_name = "opportunities_contacts";
    public $object_name = "ContactOpportunityRelationship";
    public $column_fields = array("id"
        ,"contact_id"
        ,"opportunity_id"
        ,"contact_role"
        ,'date_modified'
        );

    public $new_schema = true;

    public $additional_column_fields = array();
    public $field_defs = array(
       'id'=>array('name' =>'id', 'type' =>'char', 'len'=>'36', 'default'=>'')
      , 'contact_id'=>array('name' =>'contact_id', 'type' =>'char', 'len'=>'36', )
      , 'opportunity_id'=>array('name' =>'opportunity_id', 'type' =>'char', 'len'=>'36',)
      , 'contact_role'=>array('name' =>'contact_role', 'type' =>'char', 'len'=>'50')
      , 'date_modified'=>array('name' => 'date_modified','type' => 'datetime')
      , 'deleted'=>array('name' =>'deleted', 'type' =>'bool', 'len'=>'1', 'default'=>'0', 'required'=>true)
      );
    public function __construct()
    {
        parent::__construct();
        $this->db = DBManagerFactory::getInstance();
        $this->dbManager = DBManagerFactory::getInstance();

        $this->disable_row_level_security =true;
    }




    public function fill_in_additional_detail_fields()
    {
        global $locale;
        if (isset($this->contact_id) && $this->contact_id != "") {
            $query = "SELECT first_name, last_name from contacts where id='$this->contact_id' AND deleted=0";
            $result =$this->db->query($query, true, " Error filling in additional detail fields: ");
            // Get the id and the name.
            $row = $this->db->fetchByAssoc($result);

            if ($row != null) {
                $this->contact_name = $locale->getLocaleFormattedName($row['first_name'], $row['last_name']);
            }
        }

        if (isset($this->opportunity_id) && $this->opportunity_id != "") {
            $query = "SELECT name from opportunities where id='$this->opportunity_id' AND deleted=0";
            $result =$this->db->query($query, true, " Error filling in additional detail fields: ");
            // Get the id and the name.
            $row = $this->db->fetchByAssoc($result);

            if ($row != null) {
                $this->opportunity_name = $row['name'];
            }
        }
    }
}
