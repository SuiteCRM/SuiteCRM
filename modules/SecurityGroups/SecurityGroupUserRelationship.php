<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


require_once('data/SugarBean.php');

// Contact is used to store customer information.
#[\AllowDynamicProperties]
class SecurityGroupUserRelationship extends SugarBean
{
    // Stored fields
    public $id;
    public $securitygroup_id;
    public $securitygroup_noninheritable;
    public $user_id;
    public $noninheritable;
    public $primary_group;

    // Related fields
    public $securitygroup_name;
    public $user_name;

    public $table_name = "securitygroups_users";
    public $object_name = "SecurityGroupUserRelationship";
    public $column_fields = array("id"
        ,"securitygroup_id"
        ,"user_id"
        ,"noninheritable"
        ,"primary_group"
        ,'date_modified'
        );

    public $new_schema = true;

    public $additional_column_fields = array();
    public $field_defs = array(
       'id'=>array('name' =>'id', 'type' =>'char', 'len'=>'36', 'default'=>'')
      , 'securitygroup_id'=>array('name' =>'securitygroup_id', 'type' =>'char', 'len'=>'36', )
      , 'user_id'=>array('name' =>'user_id', 'type' =>'char', 'len'=>'36',)
      , 'noninheritable'=>array('name' =>'noninheritable', 'type' =>'bool', 'len'=>'1')
      , 'primary_group'=>array('name' =>'primary_group', 'type' =>'bool', 'len'=>'1')
      , 'date_modified'=>array('name' => 'date_modified','type' => 'datetime')
      , 'deleted'=>array('name' =>'deleted', 'type' =>'bool', 'len'=>'1', 'default'=>'0', 'required'=>true)
      );
    public function __construct()
    {
        $this->db = DBManagerFactory::getInstance();
        $this->dbManager = DBManagerFactory::getInstance();

        $this->disable_row_level_security =true;
    }




    public function fill_in_additional_detail_fields()
    {
        if (isset($this->securitygroup_id) && $this->securitygroup_id != "") {
            $query = "SELECT name from securitygroups where id='$this->securitygroup_id' AND deleted=0";
            $result =$this->db->query($query, true, " Error filling in additional detail fields: ");
            // Get the id and the name.
            $row = $this->db->fetchByAssoc($result);

            if ($row != null) {
                $this->securitygroup_name = $row['name'];
            }
        }

        if (isset($this->user_id) && $this->user_id != "") {
            $query = "SELECT user_name from users where id='$this->user_id' AND deleted=0";
            $result =$this->db->query($query, true, " Error filling in additional detail fields: ");
            // Get the id and the name.
            $row = $this->db->fetchByAssoc($result);

            if ($row != null) {
                $this->user_name = $row['user_name'];
            }
        }
    }

    public function create_list_query(&$order_by, &$where)
    {
        $query = "SELECT id, first_name, last_name, user_name FROM users ";
        $where_auto = "deleted=0";

        if ($where != "") {
            $query .= "where $where AND ".$where_auto;
        } else {
            $query .= "where ".$where_auto;
        }

        $query .= " ORDER BY last_name, first_name";

        return $query;
    }
}
