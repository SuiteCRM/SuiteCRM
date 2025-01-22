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

/**

 * Description:
 */







#[\AllowDynamicProperties]
class Release extends SugarBean
{
    // Stored fields
    public $id;
    public $deleted;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $created_by_name;
    public $modified_by_name;
    public $name;
    public $status;

    public $table_name = "releases";

    public $object_name = "Release";
    public $module_dir = 'Releases';
    public $new_schema = true;

    // This is used to retrieve related fields from form posts.
    public $additional_column_fields = array();

    public function __construct()
    {
        parent::__construct();
    }




    public function get_summary_text()
    {
        return (string)$this->name;
    }

    public function get_releases($add_blank=false, $status='Active', $where='')
    {
        if ($where!='') {
            $query = "SELECT id, name FROM $this->table_name where ". $where ." and deleted=0 ";
        } else {
            $query = "SELECT id, name FROM $this->table_name where deleted=0 ";
        }
        if ($status=='Active') {
            $query .= " and status='Active' ";
        } elseif ($status=='Hidden') {
            $query .= " and status='Hidden' ";
        } elseif ($status=='All') {
        }
        $query .= " order by list_order asc";
        $result = $this->db->query($query, false);
        $GLOBALS['log']->debug("get_releases: result is ".var_export($result, true));

        $list = array();
        if ($add_blank) {
            $list['']='';
        }
        //if($this->db->getRowCount($result) > 0){
        // We have some data.
        while (($row = $this->db->fetchByAssoc($result)) != null) {
            //while ($row = $this->db->fetchByAssoc($result)) {
            $list[$row['id']] = $row['name'];
            $GLOBALS['log']->debug("row id is:".$row['id']);
            $GLOBALS['log']->debug("row name is:".$row['name']);
        }
        //}
        return $list;
    }

    public function fill_in_additional_list_fields()
    {
        $this->fill_in_additional_detail_fields();
    }

    public function fill_in_additional_detail_fields()
    {
    }

    public function get_list_view_data()
    {
        global $app_list_strings;
        $temp_array = $this->get_list_view_array();
        $temp_array["ENCODED_NAME"]=$this->name;

        if (!isset($app_list_strings['release_status_dom'][$this->status])) {
            LoggerManager::getLogger()->warn('Release get_list_view_data: Undefined index: "' . $this->status . '"');
            $appListStringReleaseStatusDomThisStatus = null;
        } else {
            $appListStringReleaseStatusDomThisStatus = $app_list_strings['release_status_dom'][$this->status];
        }

        $temp_array['ENCODED_STATUS'] = $appListStringReleaseStatusDomThisStatus;
        //	$temp_array["ENCODED_NAME"]=htmlspecialchars($this->name, ENT_QUOTES);
        return $temp_array;
    }
    /**
    	builds a generic search based on the query string using or
    	do not include any $this-> because this is called on without having the class instantiated
    */
    public function build_generic_where_clause($the_query_string)
    {
        $where_clauses = array();
        $the_query_string = DBManagerFactory::getInstance()->quote($the_query_string);
        array_push($where_clauses, "name like '$the_query_string%'");

        $the_where = "";
        foreach ($where_clauses as $clause) {
            if ($the_where != "") {
                $the_where .= " or ";
            }
            $the_where .= $clause;
        }


        return $the_where;
    }
}
