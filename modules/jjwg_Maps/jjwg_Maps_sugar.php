<?php

#[\AllowDynamicProperties]
class jjwg_Maps_sugar extends Basic
{
    public $new_schema = true;
    public $module_dir = 'jjwg_Maps';
    public $object_name = 'jjwg_Maps';
    public $table_name = 'jjwg_maps';
    public $importable = true;
    public $disable_row_level_security = true;
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
    public $distance;
    public $unit_type;
    public $module_type;
    public $parent_name;
    public $parent_type;
    public $parent_id;

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
