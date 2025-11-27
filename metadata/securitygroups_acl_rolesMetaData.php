<?php
$dictionary['securitygroups_acl_roles'] = array(
    'table' => 'securitygroups_acl_roles',
    'fields' => array(
        array('name' =>'id', 'type' =>'char', 'len'=>'36', 'required'=>true, 'default'=>'')
      , array('name' =>'securitygroup_id', 'type' =>'char', 'len'=>'36')
      , array('name' =>'role_id', 'type' =>'char', 'len'=>'36')
      , array('name' =>'date_modified','type' => 'datetime')
      , array('name' =>'deleted', 'type' =>'bool', 'len'=>'1', 'required'=>true, 'default'=>'0')
    ),
    'indices' => array(
       array('name' =>'securitygroups_acl_rolespk', 'type' =>'primary', 'fields'=>array('id')),
       array('name' => 'idx_SG_roles', 'type' => 'index', 'fields' => array('securitygroup_id', 'role_id')),
    ),
    'relationships' => array(
    
        'securitygroups_acl_roles' => array(
            'lhs_module'=> 'SecurityGroups', 'lhs_table'=> 'securitygroups', 'lhs_key' => 'id',
            'rhs_module'=> 'ACLRoles', 'rhs_table'=> 'acl_roles', 'rhs_key' => 'id',
            'relationship_type'=>'many-to-many',
            'join_table'=> 'securitygroups_acl_roles', 'join_key_lhs'=>'securitygroup_id', 'join_key_rhs'=>'role_id',
        ),
    )
);
