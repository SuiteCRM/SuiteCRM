<?php


#[\AllowDynamicProperties]
class SecurityGroup_sugar extends Basic
{
    public $new_schema = true;
    public $module_dir = 'SecurityGroups';
    public $object_name = 'SecurityGroup';
    public $table_name = 'securitygroups';
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





    public function __construct()
    {
        parent::__construct();
    }




    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL': return true;
        }
        return false;
    }
}
