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
class Role extends SugarBean
{
    public $field_name_map;

    public $id;
    public $deleted;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $created_by;
    public $name;
    public $description;
    public $modules;
    public $disable_row_level_security = true;

    public $table_name = 'roles';
    public $rel_module_table = 'roles_modules';
    public $object_name = 'Role';
    public $module_dir = 'Roles';
    public $new_schema = true;

    public function __construct()
    {
        parent::__construct();
    }




    public function get_summary_text()
    {
        return $this->name;
    }

    public function create_export_query($order_by, $where)
    {
        return $this->create_new_list_query($order_by, $where);
    }

    public function query_modules($allow = 1)
    {
        $query = "SELECT module_id FROM roles_modules WHERE ";
        $query .= "role_id = '$this->id' AND allow = '$allow' AND deleted=0";
        $result = $this->db->query($query);

        $return_array = array();

        while ($row = $this->db->fetchByAssoc($result)) {
            array_push($return_array, $row['module_id']);
        }

        return $return_array;
    }
    public function set_module_relationship($role_id, &$mod_ids, $allow)
    {
        foreach ($mod_ids as $mod_id) {
            if ($mod_id != '') {
                $this->set_relationship('roles_modules', array( 'module_id'=>$mod_id, 'role_id'=>$role_id, 'allow'=>$allow ));
            }
        }
    }

    public function clear_module_relationship($role_id)
    {
        $query = "DELETE FROM roles_modules WHERE role_id='$role_id'";
        $this->db->query($query);
    }

    public function set_user_relationship($role_id, &$user_ids)
    {
        foreach ($user_ids as $user_id) {
            if ($user_id != '') {
                $this->set_relationship('roles_users', array( 'user_id'=>$user_id, 'role_id'=>$role_id ));
            }
        }
    }

    public function clear_user_relationship($role_id, $user_id)
    {
        $query = "DELETE FROM roles_users WHERE role_id='$role_id' AND user_id='$user_id'";
        $this->db->query($query);
    }

    public function query_user_allowed_modules($user_id)
    {
        $userArray = array();
        global $app_list_strings;



        $sql = "SELECT role_id FROM roles_users WHERE user_id='$user_id'";

        $result = $this->db->query($sql);

        while ($row = $this->db->fetchByAssoc($result)) {
            $role_id = $row["role_id"];
            $sql = "SELECT module_id FROM roles_modules WHERE role_id='$role_id' AND allow='1'";
            $res = $this->db->query($sql);

            while ($col = $this->db->fetchByAssoc($res)) {
                $key = $col['module_id'];
                if (!(array_key_exists($key, $userArray))) {
                    $userArray[$key] = $app_list_strings['moduleList'][$key];
                }
            }
        }

        return $userArray;
    }

    public function query_user_disallowed_modules($user_id, &$allowed)
    {
        global $moduleList;

        $returnArray = array();

        foreach ($moduleList as $key=>$val) {
            if (array_key_exists($val, $allowed)) {
                continue;
            }
            $returnArray[$val] = $val;
        }

        return $returnArray;
    }

    public function get_users()
    {
        // First, get the list of IDs.



        $query = "SELECT user_id as id FROM roles_users WHERE role_id='$this->id' AND deleted=0";

        $user =  BeanFactory::newBean('Users');
        return $this->build_related_list($query, $user);
    }

    public function check_user_role_count($user_id)
    {
        $query =  "SELECT count(*) AS num FROM roles_users WHERE ";
        $query .= "user_id='$user_id' AND deleted=0";
        $result = $this->db->query($query);

        $row = $this->db->fetchByAssoc($result);

        return $row['num'];
    }
}
