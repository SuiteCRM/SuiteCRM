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

$GLOBALS['studioReadOnlyFields'] = array('date_entered'=>1, 'date_modified'=>1, 'created_by'=>1, 'id'=>1, 'modified_user_id'=>1);
#[\AllowDynamicProperties]
class TemplateField
{
    /*
    	The view is the context this field will be used in
    	-edit
    	-list
    	-detail
    	-search
    	*/
    public $view = 'edit';
    public $name = '';
    public $vname = '';
    public $label = '';
    public $id = '';
    public $size = '20';
    public $len = '255';
    public $required = false;
    public $default = null;
    public $default_value = null;
    public $type = 'varchar';
    public $comment = '';
    public $bean;
    public $ext1 = '';
    public $ext2 = '';
    public $ext3 = '';
    public $ext4 = '';
    public $audited= 0;
    public $inline_edit = 1;
    public $massupdate = 0;
    public $importable = 'true' ;
    public $duplicate_merge=0;
    public $new_field_definition;
    public $reportable = true;
    public $label_value = '';
    public $help = '';
    public $formula = '';
    public $unified_search = 0;
    public $supports_unified_search = false;
    public $vardef_map = array(
        'name'=>'name',
        'label'=>'vname',
    // bug 15801 - need to ALWAYS keep default and default_value consistent as some methods/classes use one, some use another...
        'default_value'=>'default',
        'default'=>'default_value',
        'display_default'=>'default_value',
    //		'default_value'=>'default_value',
    //		'default'=>'default_value',
        'len'=>'len',
        'required'=>'required',
        'type'=>'type',
        'audited'=>'audited',
        'inline_edit'=>'inline_edit',
        'massupdate'=>'massupdate',
        'options'=>'ext1',
        'help'=>'help',
        'comments'=>'comment',
        'importable'=>'importable',
        'duplicate_merge'=>'duplicate_merge',
        'duplicate_merge_dom_value'=>'duplicate_merge_dom_value', //bug #14897
        'merge_filter'=>'merge_filter',
        'reportable' => 'reportable',
        'ext2'=>'ext2',
        'ext4'=>'ext4',
        'ext3'=>'ext3',
        'labelValue' => 'label_value',
        'unified_search'=>'unified_search',
        'full_text_search'=>'full_text_search',
    );
    // Bug #48826
    // fields to decode from post request
    public $decode_from_request_fields_map = array('formula', 'dependency');

    public function __construct()
    {
    }

    /*
    	HTML FUNCTIONS
    	*/
    public function get_html()
    {
        $view = $this->view;
        if (!empty($GLOBALS['studioReadOnlyFields'][$this->name])) {
            $view = 'detail';
        }
        switch ($view) {
            case 'search':return $this->get_html_search();
            case 'edit': return $this->get_html_edit();
            case 'list': return $this->get_html_list();
            case 'detail': return $this->get_html_detail();

        }
    }
    public function set($values)
    {
        foreach ($values as $name=>$value) {
            $this->$name = $value;
        }
    }

    public function get_html_edit()
    {
        return 'not implemented';
    }

    public function get_html_list()
    {
        return $this->get_html_detail();
    }

    public function get_html_detail()
    {
        return 'not implemented';
    }

    public function get_html_search()
    {
        return $this->get_html_edit();
    }
    public function get_html_label()
    {
        $label =  "{MOD." .$this->vname . "}";
        if (!empty($GLOBALS['app_strings'][$this->vname])) {
            $label = "{APP." .$this->label . "}";
        }
        if ($this->view == 'edit' && $this->is_required()) {
            $label .= '<span class="required">*</span>';
        }
        if ($this->view == 'list') {
            if (isset($this->bean)) {
                if (!empty($this->id)) {
                    $name = $this->bean->table_name . '_cstm.'. $this->name;
                    $arrow = $this->bean->table_name . '_cstm_'. $this->name;
                } else {
                    $name = $this->bean->table_name . '.'. $this->name;
                    $arrow = $this->bean->table_name . '_'. $this->name;
                }
            } else {
                $name = $this->name;
                $arrow = $name;
            }
            $label = "<a href='{ORDER_BY}$name' class='listViewThLinkS1'>{MOD.$this->label}{arrow_start}{".$arrow."_arrow}{arrow_end}</a>";
        }
        return $label;
    }

    /*
    	XTPL FUNCTIONS
    	*/

    public function get_xtpl($bean = false)
    {
        if ($bean) {
            $this->bean = $bean;
        }
        $view = $this->view;
        if (!empty($GLOBALS['studioReadOnlyFields'][$this->name])) {
            $view = 'detail';
        }
        switch ($view) {
            case 'search':return $this->get_xtpl_search();
            case 'edit': return $this->get_xtpl_edit();
            case 'list': return $this->get_xtpl_list();
            case 'detail': return $this->get_xtpl_detail();

        }
    }

    public function get_xtpl_edit()
    {
        return '/*not implemented*/';
    }

    public function get_xtpl_list()
    {
        return get_xtpl_detail();
    }

    public function get_xtpl_detail()
    {
        return '/*not implemented*/';
    }

    public function get_xtpl_search()
    {
        //return get_xtpl_edit();
    }

    public function is_required()
    {
        if ($this->required) {
            return true;
        }
        return false;
    }




    /*
    	DB FUNCTIONS
    	*/

    public function get_db_type()
    {
        if (!empty($this->type)) {
            $type = DBManagerFactory::getInstance()->getColumnType($this->type);
        }
        if (!empty($type)) {
            return " $type";
        }
        $type = DBManagerFactory::getInstance()->getColumnType("varchar");
        return " $type({$this->len})";
    }

    public function get_db_default($modify=false)
    {
        $GLOBALS['log']->debug('get_db_default(): default_value='.$this->default_value);
        if (!$modify || empty($this->new_field_definition['default_value']) || $this->new_field_definition['default_value'] != $this->default_value) {
            if (!is_null($this->default_value)) { // add a default value if it is not null - we want to set a default even if default_value is '0', which is not null, but which is empty()
                if (null == trim($this->default_value)) {
                    return " DEFAULT NULL";
                } else {
                    return " DEFAULT '$this->default_value'";
                }
            } else {
                return '';
            }
        }
    }

    /*
     * Return the required clause for this field
     * Confusingly, when modifying an existing field ($modify=true) there are two exactly opposite cases:
     * 1. if called by Studio, only $this->required is set. If set, we return "NOT NULL" otherwise we return "NULL"
     * 2. if not called by Studio, $this->required holds the OLD value of required, and new_field_definition['required'] is the NEW
     * So if not called by Studio we want to return NULL if required=true (because we are changing FROM this setting)
     */

    public function get_db_required($modify=false)
    {
        //		$GLOBALS['log']->debug('get_db_required required='.$this->required." and ".(($modify)?"true":"false")." and ".print_r($this->new_field_definition,true));
        $req = "";

        if ($modify) {
            if (!empty($this->new_field_definition['required'])) {
                if ($this->required && $this->new_field_definition['required'] != $this->required) {
                    $req = " NULL ";
                }
            } else {
                $req = ($this->required) ? " NOT NULL " : ''; // bug 17184 tyoung - set required correctly when modifying custom field in Studio
            }
        } else {
            if (empty($this->new_field_definition['required']) || $this->new_field_definition['required'] != $this->required) {
                if (!empty($this->required) && $this->required) {
                    $req = " NOT NULL";
                }
            }
        }

        return $req;
    }

    /*	function get_db_required($modify=false){
    	$GLOBALS['log']->debug('get_db_required required='.$this->required." and ".(($modify)?"true":"false")." and ".print_r($this->new_field_definition,true));
    	if ($modify) {
    	if (!empty($this->new_field_definition['required'])) {
    	if ($this->required and $this->new_field_definition['required'] != $this->required) {
    	return " null ";
    	}
    	return "";
    	}
    	}
    	if (empty($this->new_field_definition['required']) or $this->new_field_definition['required'] != $this->required ) {
    	if(!empty($this->required) && $this->required){
    	return " NOT NULL";
    	}
    	}
    	return '';
    	}
    	*/
    /**
     * Oracle Support: do not set required constraint if no default value is supplied.
     * In this case the default value will be handled by the application/sugarbean.
     */
    public function get_db_add_alter_table($table)
    {
        return DBManagerFactory::getInstance()->getHelper()->addColumnSQL($table, $this->get_field_def(), true);
    }

    public function get_db_delete_alter_table($table)
    {
        return DBManagerFactory::getInstance()->getHelper()->dropColumnSQL(
            $table,
            $this->get_field_def()
        );
    }

    /**
     * mysql requires the datatype caluse in the alter statment.it will be no-op anyway.
     */
    public function get_db_modify_alter_table($table)
    {
        return DBManagerFactory::getInstance()->alterColumnSQL($table, $this->get_field_def());
    }


    /*
     * BEAN FUNCTIONS
     *
     */
    public function get_field_def()
    {
        $array =  array(
            'required'=>$this->convertBooleanValue($this->required),
            'source'=>'custom_fields',
            'name'=>$this->name,
            'vname'=>$this->vname,
            'type'=>$this->type,
            'massupdate'=>$this->massupdate,
            'default'=>$this->default,
            'no_default'=> !empty($this->no_default),
            'comments'=> (isset($this->comments)) ? $this->comments : '',
            'help'=> (isset($this->help)) ?  $this->help : '',
            'importable'=>$this->importable,
            'duplicate_merge'=>$this->duplicate_merge,
            'duplicate_merge_dom_value'=> $this->getDupMergeDomValue(),
            'audited'=>$this->convertBooleanValue($this->audited),
            'inline_edit'=>$this->convertBooleanValue($this->inline_edit),
            'reportable'=>$this->convertBooleanValue($this->reportable),
            'unified_search'=>$this->convertBooleanValue($this->unified_search),
            'merge_filter' => empty($this->merge_filter) ? "disabled" : $this->merge_filter
        );
        if (isset($this->full_text_search)) {
            $array['full_text_search'] = $this->full_text_search;
        }
        if (!empty($this->len)) {
            $array['len'] = $this->len;
        }
        if (!empty($this->size)) {
            $array['size'] = $this->size;
        }

        $this->get_dup_merge_def($array);

        return $array;
    }

    protected function convertBooleanValue($value)
    {
        if ($value === 'true' || $value === '1' || $value === 1) {
            return  true;
        } else {
            if ($value === 'false' || $value === '0' || $value === 0) {
                return  false;
            } else {
                return $value;
            }
        }
    }


    /* if the field is duplicate merge enabled this function will return the vardef entry for the same.
     */
    public function get_dup_merge_def(&$def)
    {
        switch ($def['duplicate_merge_dom_value']) {
            case 0:
                $def['duplicate_merge']='disabled';
                $def['merge_filter']='disabled';
                break;
            case 1:
                $def['duplicate_merge']='enabled';
                $def['merge_filter']='disabled';
                break;
            case 2:
                $def['merge_filter']='enabled';
                $def['duplicate_merge']='enabled';
                break;
            case 3:
                $def['merge_filter']='selected';
                $def['duplicate_merge']='enabled';
                break;
            case 4:
                $def['merge_filter']='enabled';
                $def['duplicate_merge']='disabled';
                break;
        }
    }

    /**
     * duplicate_merge_dom_value drives the dropdown in the studio editor. This dropdown drives two fields though,
     * duplicate_merge and merge_filter. When duplicate_merge_dom_value is not set, we need to derive it from the values
     * of those two fields. Also, when studio sends this value down to be read in PopulateFromPost, it is set to
     * duplicate_merge rather than duplicate_merge_dom_value, so we must check if duplicate_merge is a number rather
     * than a string as well.
     * @return int
     */
    public function getDupMergeDomValue()
    {
        if (isset($this->duplicate_merge_dom_value)) {
            return $this->duplicate_merge_dom_value;
        }

        //If duplicate merge is numeric rather than a string, it is probably what duplicate_merge_dom_value was set to.
        if (is_numeric($this->duplicate_merge)) {
            return $this->duplicate_merge;
        }


        //Figure out the duplicate_merge_dom_value based on the values of merge filter and duplicate merge
        if (empty($this->merge_filter) || $this->merge_filter === 'disabled') {
            if (empty($this->duplicate_merge) || $this->duplicate_merge === 'disabled') {
                $this->duplicate_merge_dom_value = 0;
            } else {
                $this->duplicate_merge_dom_value = 1;
            }
        } else {
            if ($this->merge_filter === "selected") {
                $this->duplicate_merge_dom_value = 3;
            } else {
                if (empty($this->duplicate_merge) || $this->duplicate_merge === 'disabled') {
                    $this->duplicate_merge_dom_value = 4;
                } else {
                    $this->duplicate_merge_dom_value = 2;
                }
            }
        }

        return $this->duplicate_merge_dom_value;
    }

    /*
    	HELPER FUNCTIONS
    	*/


    public function prepare()
    {
        if (empty($this->id)) {
            $this->id = $this->name;
        }
    }

    /**
     * populateFromRow
     * This function supports setting the values of all TemplateField instances.
     * @param $row The Array key/value pairs from fields_meta_data table
     */
    public function populateFromRow($row=array())
    {
        $fmd_to_dyn_map = array('comments' => 'comment', 'require_option' => 'required', 'label' => 'vname',
                                'mass_update' => 'massupdate', 'max_size' => 'len', 'default_value' => 'default', 'id_name' => 'ext3');
        if (!is_array($row)) {
            $GLOBALS['log']->error("Error: TemplateField->populateFromRow expecting Array");
        }
        //Bug 24189: Copy fields from FMD format to Field objects and vice versa
        foreach ($fmd_to_dyn_map as $fmd_key => $dyn_key) {
            if (isset($row[$dyn_key])) {
                $this->$fmd_key = $row[$dyn_key];
            }
            if (isset($row[$fmd_key])) {
                $this->$dyn_key = $row[$fmd_key];
            }
        }
        foreach ($row as	$key=>$value) {
            $this->$key = $value;
        }
    }

    public function populateFromPost()
    {
        foreach ($this->vardef_map as $vardef=>$field) {
            if (isset($_REQUEST[$vardef])) {
                $this->$vardef = $_REQUEST[$vardef];

                //  Bug #48826. Some fields are allowed to have special characters and must be decoded from the request
                // Bug 49774, 49775: Strip html tags from 'formula' and 'dependency'.
                if (is_string($this->$vardef) && in_array($vardef, $this->decode_from_request_fields_map)) {
                    $this->$vardef = html_entity_decode(strip_tags(from_html($this->$vardef)));
                }


                //Remove potential xss code from help field
                if ($field == 'help' && !empty($this->$vardef)) {
                    $help = htmlspecialchars_decode((string) $this->$vardef, ENT_QUOTES);

                    // Fix for issue #1170 - text in studio can't accept the special language characters.
                    //$this->$vardef = htmlentities(remove_xss($help));
                    $this->$vardef = htmlspecialchars((string) remove_xss($help));
                }


                if ($vardef != $field) {
                    $this->$field = $this->$vardef;
                }
            }
        }
        $this->applyVardefRules();
        $GLOBALS['log']->debug('populate: '.print_r($this, true));
    }

    protected function applyVardefRules()
    {
    }

    public function get_additional_defs()
    {
        return array();
    }

    public function delete($df)
    {
        $df->deleteField($this);
    }

    /**
     * get_field_name
     *
     * This is a helper function to return a field's proper name.  It checks to see if an instance of the module can
     * be created and then attempts to retrieve the field's name based on the name lookup skey supplied to the method.
     *
     * @param String $module The name of the module
     * @param String $name The field name key
     * @return The field name for the module
     */
    protected function get_field_name($module, $name)
    {
        $bean = loadBean($module);
        if (empty($bean) || is_null($bean)) {
            return $name;
        }

        $field_defs = $bean->field_defs;
        return isset($field_defs[$name]['name']) ? $field_defs[$name]['name'] : $name;
    }

    /**
     * save
     *
     * This function says the field template by calling the DynamicField addFieldObject function.  It then
     * checks to see if updates are needed for the SearchFields.php file.  In the event that the unified_search
     * member variable is set to true, a search field definition is updated/created to the SearchFields.php file.
     *
     * @param DynamicField $df
     */
    public function save($df)
    {
        //	    $GLOBALS['log']->debug('saving field: '.print_r($this,true));
        $df->addFieldObject($this);

        require_once('modules/ModuleBuilder/parsers/parser.searchfields.php');
        $searchFieldParser = new ParserSearchFields($df->getModuleName(), $df->getPackageName()) ;
        //If unified_search is enabled for this field, then create the SearchFields entry
        $fieldName = $this->get_field_name($df->getModuleName(), $this->name);
        if ($this->unified_search && !isset($searchFieldParser->searchFields[$df->getModuleName()][$fieldName])) {
            $searchFieldParser->addSearchField($fieldName, array('query_type'=>'default'));
            $searchFieldParser->saveSearchFields($searchFieldParser->searchFields);
        }
    }
}
