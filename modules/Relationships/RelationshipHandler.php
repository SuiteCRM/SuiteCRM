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









class RelationshipHandler extends Relationship
{
    public $db;							//Database link by reference

    public $base_module;					//name of module
    public $base_bean;						//actual object
    public $base_vardef_field;				//base's vardef field name of relationship with rel1

    public $rel1_module;					//name of related module
    public $rel1_bean;						//actual related object
    public $rel1_relationship_name;		//Relationship name between base and rel1
    public $rel1_vardef_field;				//rel1's vardef field name of relationship with rel2
    public $rel1_vardef_field_base;		//rel1's vardef field name of relationship with base

    public $rel2_module;					//name of related related module
    public $rel2_bean;						//actual related related object
    public $rel2_relationship_name;		//Relationship name between rel1 and rel2
    public $rel2_vardef_field;				//rel2's vardef field name of relationship with rel1


    public $base_array;					//Info array
    public $rel1_array;					//Info array
    public $rel2_array;					//Info array


    /*

    info arrays contain:

        'slabel' ->		singular module name in correct language
        'plabel' ->  	plural module name in correct language



    */


    ///////////////////////////Setup and populate functions//////////////////////////////

    public function __construct(& $db, $base_module="")
    {
        $this->db = $db;
        $this->base_module = $base_module;

        //end function RelationshipHandler
    }

    public function set_rel_vardef_fields($base_vardef_field, $rel1_vardef_field="")
    {
        $this->base_vardef_field = $base_vardef_field;
        $this->rel1_vardef_field = $rel1_vardef_field;

        //end function set_rel_vardef_fields
    }


    public function set_rel_relationship_names($build_rel2=false)
    {
        $this->rel1_relationship_name = $this->base_bean->field_defs[$this->base_vardef_field]['relationship'];

        if ($build_rel2==true) {
            $this->rel2_relationship_name = $this->rel1_bean->field_defs[$this->rel1_vardef_field]['relationship'];
        }

        //end function set_rel_relationship_names
    }




    ///////////////////////////////END Setup and populate functions/////////////////////


    /*
    set the build_rel2 to true if you want the rel2 info array as well
    This function will build all the relationship info it can based on values set in the setup functions
    When you use the info arrays (rel1_array) or (rel2_array), make sure you always check for empty values
    */
    public function build_info($build_rel2=false)
    {
        if ($this->base_bean == null) {
            $this->base_bean = get_module_info($this->base_module);
        }

        if (empty($this->rel1_bean)) {
            $this->build_rel1_info();
            $this->rel1_module = $this->rel1_bean->module_dir;
        }

        if ($build_rel2==true && $this->rel2_bean=="") {
            $this->build_rel2_info();
            $this->rel2_module = $this->rel2_bean->module_dir;
        }

        //translate the module titles to the proper language
        $this->build_module_labels($build_rel2);

        //end function build_info
    }

    public function build_rel1_info()
    {
        $this->rel1_bean = $this->trace_relationship_module($this->base_module, $this->base_vardef_field);

        //end function build_rel1_info
    }

    public function build_rel2_info()
    {
        $this->rel2_bean = $this->trace_relationship_module($this->base_module, $this->base_vardef_field, $this->rel1_vardef_field);

        //end function build_rel1_info
    }

    /*
    Translates the module names to their singular and plural label and puts them in
    the info arrays.  Does it for base, rel1, and rel2 if specified
    */

    public function build_module_labels($build_rel2=false)
    {
        global $app_list_strings;

        ///Base Module Labels
        if (!empty($app_list_strings['moduleList'][$this->base_bean->module_dir])) {
            $this->base_array['plabel'] = $app_list_strings['moduleList'][$this->base_bean->module_dir];
        } else {
            $this->base_array['plabel'] = $this->base_bean->module_dir;
        }
        if (!empty($app_list_strings['moduleListSingular'][$this->base_bean->module_dir])) {
            $this->base_array['slabel'] = $app_list_strings['moduleListSingular'][$this->base_bean->module_dir];
        } else {
            $this->base_array['slabel'] = $this->base_bean->object_name;
        }

        ///Rel1 Module Labels
        if (!empty($app_list_strings['moduleList'][$this->rel1_bean->module_dir])) {
            $this->rel1_array['plabel'] = $app_list_strings['moduleList'][$this->rel1_bean->module_dir];
        } else {
            $this->rel1_array['plabel'] = $this->rel1_bean->module_dir;
        }

        if (!empty($app_list_strings['moduleListSingular'][$this->rel1_bean->module_dir])) {
            $this->rel1_array['slabel'] = $app_list_strings['moduleListSingular'][$this->rel1_bean->module_dir];
        } else {
            $this->rel1_array['slabel'] = $this->rel1_bean->object_name;
        }


        //Rel2 Module Labels
        if ($build_rel2==true) {
            if (!empty($app_list_strings['moduleList'][$this->rel2_bean->module_dir])) {
                $this->rel2_array['plabel'] = $app_list_strings['moduleList'][$this->rel2_bean->module_dir];
            } else {
                $this->rel2_array['plabel'] = $this->rel2_bean->module_dir;
            }
            if (!empty($app_list_strings['moduleListSingular'][$this->rel2_bean->module_dir])) {
                $this->rel2_array['slabel'] = $app_list_strings['moduleListSingular'][$this->rel2_bean->module_dir];
            } else {
                $this->rel2_array['slabel'] = $this->rel2_bean->module_dir;
            }
            //end if build_rel2 is true
        }

        //end function buld_module_lables
    }





    public function build_related_list($query = 'base', &$template, $row_offset = 0, $limit = -1)
    {
        $type = $query;
        //type can be base, rel1

        $target_list = "";

        if ($type=="base") {
            $target_list = $this->base_bean->get_linked_beans($this->base_vardef_field, $this->rel1_bean->object_name);
            //Possibility exists that this is a new relationship, so capture via relationship fields
            if (empty($target_list)) {
                $target_list = search_filter_rel_info($this->base_bean, $this->rel1_bean->module_dir, $this->base_vardef_field);
                //end if the target list is empty
            }
        }

        if ($type=="rel1") {
            $target_list = $this->rel1_bean->get_linked_beans($this->rel1_vardef_field, $this->rel2_bean->object_name);

            //Possibility exists that this is a new relationship, so capture via relationship fields
            if (empty($target_list)) {
                $target_list = search_filter_rel_info($this->rel1_bean, $this->rel2_bean->module_dir, $this->rel1_vardef_field);
                //end if the target list is empty
            }
        }

        return $target_list;

        //end function build_related_list
    }




    ///////BEGIN Functions to find relationships/////////////////////////////////

    public function get_relationship_information(& $target_bean, $get_upstream_rel_field_name = false)
    {
        $target_module_name = $target_bean->module_dir;
        $current_module_name = $this->base_module;

        //Look for downstream connection
        $rel_array = $this->retrieve_by_sides($current_module_name, $target_module_name, $this->db);


        //Does a downstream relationship exist
        if ($rel_array!=null) {
            if ($rel_array['relationship_type']=="many-to-many") {
                $joinKeyLHS = $rel_array['join_key_lhs'];
                $target_bean->$joinKeyLHS = $this->base_bean->id;
                if ($rel_array['relationship_role_column']!="") {
                    $relRole = $rel_array['relationship_role_column'];
                    $target_bean->$relRole = $rel_array['relationship_role_column_value'];
                }
                //end if many-to-many
            }

            if ($rel_array['relationship_type']=="one-to-many") {
                $RHSKey = $rel_array['rhs_key'];
                $target_bean->$RHSKey = $this->base_bean->id;
                if ($rel_array['relationship_role_column']!="") {
                    $relRole = $rel_array['relationship_role_column'];
                    $target_bean->$relRole = $rel_array['relationship_role_column_value'];
                }
                //end if one-to-many
            }

            return;
            //end if downstream relationship exists
        }



        //Look for upstream connection
        $rel_array = $this->retrieve_by_sides($target_module_name, $current_module_name, $this->db);

        //Does an upstream relationship exist
        if ($rel_array!=null) {
            if ($rel_array['relationship_type']=="many-to-many") {
                $joinKeyRHS = $rel_array['join_key_rhs'];
                $target_bean->$joinKeyRHS = $this->base_bean->id;
                if ($rel_array['relationship_role_column']!="") {
                    $relRole = $rel_array['relationship_role_column'];
                    $target_bean->$relRole = $rel_array['relationship_role_column_value'];
                }
                //end if many-to-many
            }

            if ($rel_array['relationship_type']=="one-to-many") {
                $RHSKey = $rel_array['rhs_key'];
                $target_bean->$RHSKey = $this->base_bean->id;
                if ($rel_array['relationship_role_column']!="") {
                    $relRole = $rel_array['relationship_role_column'];
                    $target_bean->$relRole = $rel_array['relationship_role_column_value'];
                }
                //end if one-to-many
            }


            ///Fill additional id field if necessary
            if (($id_name = $this->traverse_rel_meta($current_module_name, $target_bean, $rel_array['relationship_name'])) != null) {
                $target_bean->$id_name = $this->base_bean->id;
                if ($get_upstream_rel_field_name) {
                    $target_bean->new_rel_relname = $id_name;
                    $target_bean->new_rel_id = $this->base_bean->id;
                }
            }

            //end if an upstream relationship exists
        }

        //end function get_relationship_information
    }

    public function traverse_rel_meta($base_module, & $target_bean, $target_rel_name)
    {
        $id_name = null;

        //returns name of variable to store id in
        //if none exists, then returns null
        foreach ($target_bean->field_defs as $field_array) {
            if (!empty($field_array['relationship']) && $field_array['relationship']==$target_rel_name) {
                $id_name = $this->get_id_name($target_bean, $field_array['name']);
                return $id_name;
                //end if rel name match
            }

            //end foreach field def
        }

        return null;

        //end function traverse_rel_meta
    }


    public function get_id_name(& $target_bean, $field_name)
    {
        foreach ($target_bean->relationship_fields as $target_id => $rel_name) {
            if ($rel_name == $field_name) {
                //relationship id found
                return $target_id;
                //end if match
            }
            //end foreach
        }

        return null;
        //end function get_id_name
    }

    ///////////////////////////END functions to find relationships //////////////////////


    public function process_by_rel_bean($rel1_module)
    {
        $this->rel1_relationship_name = $this::retrieve_by_modules($this->base_module, $rel1_module, $this->db);
        $this->rel1_module = $rel1_module;
        $this->rel1_bean = get_module_info($this->rel1_module);

        //end function process_by_rel_bean
    }


    public function get_rel1_vardef_field_base($field_defs)
    {
        foreach ($field_defs as $field_array) {
            if (!empty($field_array['relationship']) && $field_array['relationship']==$this->rel1_relationship_name) {
                $this->rel1_vardef_field_base = $field_array['name'];
                //end if rel name match
            }

            //end foreach field def
        }

        return null;


        //end get_rel1_vardef_field_base
    }


    public function get_farthest_reach()
    {
        if ($this->rel1_vardef_field!="") {
            //the farthest reach is rel2
            $this->build_info(true);
            return $this->rel2_bean;
        }

        //the farthest reach is rel1
        $this->build_info(false);
        return $this->rel1_bean;

        //end function get_farthest_reach
    }

    //end class RelationshipHandler
}
