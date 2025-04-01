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
class FieldsMetaData extends SugarBean
{
    // database table columns
    public $id;
    public $name;
    public $vname;
    public $custom_module;
    public $type;
    public $len;
    public $required;
    public $default_value;
    public $deleted;
    public $ext1;
    public $ext2;
    public $ext3;
    public $audited;
    public $inline_edit;
    public $duplicate_merge;
    public $reportable;
    public $required_fields =  array("name"=>1, "date_start"=>2, "time_start"=>3,);

    public $table_name = 'fields_meta_data';
    public $object_name = 'FieldsMetaData';
    public $module_dir = 'DynamicFields';
    public $column_fields = array(
        'id',
        'name',
        'vname',
        'custom_module',
        'type',
        'len',
        'required',
        'default_value',
        'deleted',
        'ext1',
        'ext2',
        'ext3',
        'audited',
        'inline_edit',
        'massupdate',
        'duplicate_merge',
        'reportable',
    );

    public $list_fields = array(
        'id',
        'name',
        'vname',
        'type',
        'len',
        'required',
        'default_value',
        'audited',
        'inline_edit',
        'massupdate',
        'duplicate_merge',
        'reportable',
    );

    public $field_name_map;
    public $new_schema = true;

    //////////////////////////////////////////////////////////////////
    // METHODS
    //////////////////////////////////////////////////////////////////

    public function __construct()
    {
        parent::__construct();
        $this->disable_row_level_security = true;
    }




    public function mark_deleted($id)
    {
        $query = "DELETE FROM $this->table_name WHERE  id='$id'";
        $this->db->query($query, true, "Error deleting record: ");
        $this->mark_relationships_deleted($id);
    }

    public function get_list_view_data()
    {
        $data = parent::get_list_view_data();
        $data['VNAME'] = translate($this->vname, $this->custom_module);
        $data['NAMELINK'] = '<input class="checkbox" type="checkbox" name="remove[]" value="' . $this->id . '">&nbsp;&nbsp;<a href="index.php?module=Studio&action=wizard&wizard=EditCustomFieldsWizard&option=EditCustomField&record=' . $this->id . '" >';
        return $data;
    }


    public function get_summary_text()
    {
        return $this->name;
    }
}
