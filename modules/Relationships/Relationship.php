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




#[\AllowDynamicProperties]
class Relationship extends SugarBean
{
    public $object_name='Relationship';
    public $module_dir = 'Relationships';
    public $new_schema = true;
    public $table_name = 'relationships';

    public $id;
    public $relationship_name;
    public $lhs_module;
    public $lhs_table;
    public $lhs_key;
    public $rhs_module;
    public $rhs_table;
    public $rhs_key;
    public $join_table;
    public $join_key_lhs;
    public $join_key_rhs;
    public $relationship_type;
    public $relationship_role_column;
    public $relationship_role_column_value;
    public $reverse;

    public $_self_referencing;

    public function __construct()
    {
        parent::__construct();
    }




    /*returns true if the relationship is self referencing. equality check is performed for both table and
     * key names.
     */
    public function is_self_referencing()
    {
        if (empty($this->_self_referencing)) {
            $this->_self_referencing=false;

            //is it self referencing, both table and key name from lhs and rhs should  be equal.
            if ($this->lhs_table == $this->rhs_table && $this->lhs_key == $this->rhs_key) {
                $this->_self_referencing=true;
            }
        }
        return $this->_self_referencing;
    }

    /*returns true if a relationship with provided name exists*/
    public static function exists($relationship_name, $db)
    {
        if ($db instanceof DBManager) {
            $query = "SELECT relationship_name FROM relationships WHERE deleted=0 AND relationship_name = '" . $relationship_name . "'";
            $result = $db->query($query, true, 'Error searching relationships table..');
            $row = $db->fetchByAssoc($result);
            if ($row != null) {
                return true;
            }
        } else {
            $GLOBALS['log']->fatal('Invalid Argument: Argument 2 should be a DBManager');
        }

        return false;
    }

    public static function delete($relationship_name, $db)
    {
        if ($db instanceof DBManager) {
            $query = "UPDATE relationships SET deleted=1 WHERE deleted=0 AND relationship_name = '" . $relationship_name . "'";
            $db->query($query, true, " Error updating relationships table for " . $relationship_name);
        } else {
            $GLOBALS['log']->fatal('Invalid Argument: Argument 2 should be a DBManager');
        }
    }


    public function get_other_module($relationship_name, $base_module, &$db)
    {
        //give it the relationship_name and base module
        //it will return the module name on the other side of the relationship

        $query = "SELECT relationship_name, rhs_module, lhs_module FROM relationships WHERE deleted=0 AND relationship_name = '".$relationship_name."'";
        $result = $db->query($query, true, " Error searching relationships table..");
        $row  =  $db->fetchByAssoc($result);
        if ($row != null) {
            if ($row['rhs_module']==$base_module) {
                return $row['lhs_module'];
            }
            if ($row['lhs_module']==$base_module) {
                return $row['rhs_module'];
            }
        }

        return false;


        //end function get_other_module
    }

    public function retrieve_by_sides($lhs_module, $rhs_module, &$db)
    {
        //give it the relationship_name and base module
        //it will return the module name on the other side of the relationship

        $query = "SELECT * FROM relationships WHERE deleted=0 AND lhs_module = '".$lhs_module."' AND rhs_module = '".$rhs_module."'";
        $result = $db->query($query, true, " Error searching relationships table..");
        $row  =  $db->fetchByAssoc($result);
        if ($row != null) {
            return $row;
        }

        return null;


        //end function retrieve_by_sides
    }

    public static function retrieve_by_modules($lhs_module, $rhs_module, &$db, $type ='')
    {
        //give it the relationship_name and base module
        //it will return the module name on the other side of the relationship

        $query = "	SELECT * FROM relationships
					WHERE deleted=0
					AND (
					(lhs_module = '".$lhs_module."' AND rhs_module = '".$rhs_module."')
					OR
					(lhs_module = '".$rhs_module."' AND rhs_module = '".$lhs_module."')
					)
					";
        if (!empty($type)) {
            $query .= " AND relationship_type='$type'";
        }
        $result = $db->query($query, true, " Error searching relationships table..");
        $row  =  $db->fetchByAssoc($result);
        if ($row != null) {
            return $row['relationship_name'];
        }

        return null;


        //end function retrieve_by_sides
    }


    public function retrieve_by_name($relationship_name)
    {
        if (empty($GLOBALS['relationships'])) {
            $this->load_relationship_meta();
        }

        if (array_key_exists($relationship_name, $GLOBALS['relationships'])) {
            foreach ($GLOBALS['relationships'][$relationship_name] as $field=>$value) {
                $this->$field = $value;
            }
        } else {
            $GLOBALS['log']->fatal('Error fetching relationship from cache '.$relationship_name);
            return false;
        }
    }

    public function load_relationship_meta()
    {
        if (!file_exists(Relationship::cache_file_dir().'/'.Relationship::cache_file_name_only())) {
            $this->build_relationship_cache();
        }
        include(Relationship::cache_file_dir().'/'.Relationship::cache_file_name_only());
        $GLOBALS['relationships']=$relationships;
    }

    public function build_relationship_cache()
    {
        $query="SELECT * from relationships where deleted=0";
        $result=$this->db->query($query);

        $relationships = [];

        while (($row=$this->db->fetchByAssoc($result))!=null) {
            $relationships[$row['relationship_name']] = $row;
        }

        sugar_mkdir(static::cache_file_dir(), null, true);
        $out = "<?php \n \$relationships = " . var_export($relationships, true) . ";";
        sugar_file_put_contents_atomic(Relationship::cache_file_dir() . '/' . Relationship::cache_file_name_only(), $out);

        require_once("data/Relationships/RelationshipFactory.php");
        SugarRelationshipFactory::deleteCache();
    }


    public static function cache_file_dir()
    {
        return sugar_cached("modules/Relationships");
    }
    public static function cache_file_name_only()
    {
        return 'relationships.cache.php';
    }

    public static function delete_cache()
    {
        $filename=Relationship::cache_file_dir().'/'.Relationship::cache_file_name_only();
        if (file_exists($filename)) {
            unlink($filename);
        }
        require_once("data/Relationships/RelationshipFactory.php");
        SugarRelationshipFactory::deleteCache();
    }


    public function trace_relationship_module($base_module, $rel_module1_name, $rel_module2_name="")
    {
        global $beanList;
        global $dictionary;

        $temp_module = get_module_info($base_module);

        $rel_attribute1_name = $temp_module->field_defs[strtolower($rel_module1_name)]['relationship'];
        $rel_module1 = $this->get_other_module($rel_attribute1_name, $base_module, $temp_module->db);
        $rel_module1_bean = get_module_info($rel_module1);

        if ($rel_module2_name!="") {
            if ($rel_module2_name == 'ProjectTask') {
                $rel_module2_name = strtolower($rel_module2_name);
            }
            $rel_attribute2_name = $rel_module1_bean->field_defs[strtolower($rel_module2_name)]['relationship'];
            $rel_module2 = $this->get_other_module($rel_attribute2_name, $rel_module1_bean->module_dir, $rel_module1_bean->db);
            $rel_module2_bean = get_module_info($rel_module2);
            return $rel_module2_bean;
        } else {
            //no rel_module2, so return rel_module2 bean
            return $rel_module1_bean;
        }

        //end function trace_relationship_module
    }
}
