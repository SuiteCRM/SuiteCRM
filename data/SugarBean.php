<?php
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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

require_once 'modules/DynamicFields/DynamicField.php';
require_once "data/Relationships/RelationshipFactory.php";


/**
 * SugarBean is the base class for all business objects in Sugar.  It implements
 * the primary functionality needed for manipulating business objects: create,
 * retrieve, update, delete.  It allows for searching and retrieving list of records.
 * It allows for retrieving related objects (e.g. contacts related to a specific account).
 *
 * In the current implementation, there can only be one bean per folder.
 * Naming convention has the bean name be the same as the module and folder name.
 * All bean names should be singular (e.g. Contact).  The primary table name for
 * a bean should be plural (e.g. contacts).
 * @api
 */
#[\AllowDynamicProperties]
class SugarBean
{
    /**
     * Blowfish encryption key
     * @var string $field_key
     */
    protected static $field_key;
    /**
     * Cache of fields which can contain files
     *
     * @var array $fileFields
     */
    protected static $fileFields = array();
    /**
     * A pointer to the database object
     *
     * @var DBManager $db
     */
    public $db;
    /**
     * Unique object identifier
     *
     * @var string $id
     */
    public $id;
    /**
     * When creating a bean, you can specify a value in the id column as
     * long as that value is unique.  During save, if the system finds an
     * id, it assumes it is an update.  Setting new_with_id to true will
     * make sure the system performs an insert instead of an update.
     *
     * @var boolean $new_with_id -- default false
     */
    public $new_with_id = false;
    /**
     * Disable vardefs.  This should be set to true only for beans that do not have vardefs.  Tracker is an example
     *
     * @var boolean $disable_vardefs -- default false
     */
    public $disable_vardefs = false;
    /**
     * holds the full name of the user that an item is assigned to.  Only used if notifications
     * are turned on and going to be sent out.
     *
     * @var string $new_assigned_user_name
     */
    public $new_assigned_user_name;
    /**
     * An array of bool.  This array is cleared out when data is loaded.
     * As date/times are converted, a "1" is placed under the key, the field is converted.
     *
     * @var array $processed_dates_times array of bool
     */
    public $processed_dates_times = array();
    /**
     * Whether to process date/time fields for storage in the database in GMT
     *
     * @var boolean $process_save_dates
     */
    public $process_save_dates = true;
    /**
     * This signals to the bean that it is being saved in a mass mode.
     * Examples of this kind of save are import and mass update.
     * We turn off notifications of this is the case to make things more efficient.
     *
     * @var boolean $save_from_post
     */
    public $save_from_post = true;
    /**
     * When running a query on related items using the method: retrieve_by_string_fields
     * this value will be set to true if more than one item matches the search criteria.
     *
     * @var boolean $duplicates_found
     */
    public $duplicates_found = false;
    /**
     * true if this bean has been deleted, false otherwise.
     *
     * @var integer $deleted - 0 === false, 1 === true
     */
    public $deleted = 0;
    /**
     * Should the date modified column of the bean be updated during save?
     * This is used for admin level functionality that should not be updating
     * the date modified.  This is only used by sync to allow for updates to be
     * replicated in a way that will not cause them to be replicated back.
     *
     * @var boolean $update_date_modified
     */
    public $update_date_modified = true;
    /**
     * Should the modified by column of the bean be updated during save?
     * This is used for admin level functionality that should not be updating
     * the modified by column.  This is only used by sync to allow for updates to be
     * replicated in a way that will not cause them to be replicated back.
     *
     * @var boolean $update_modified_by
     */
    public $update_modified_by = true;
    /**
     * Setting this to true allows for updates to overwrite the date_entered
     *
     * @var boolean $update_date_entered
     */
    public $update_date_entered = false;
    /**
     * This allows for seed data to be created without using the current user to set the id.
     * This should be replaced by altering the current user before the call to save.
     *
     * @var boolean $set_created_by
     */
    public $set_created_by = true;
    /**
     * The database table where records of this Bean are stored.
     *
     * @var string $table_name
     */
    public $table_name = '';
    /**
     * This is the singular name of the bean.  (i.e. Contact).
     *
     * @var string $object_name
     */
    public $object_name = '';

    /** Set this to true if you query contains a sub-select and bean is converting both select statements
     * into count queries.
     * @var boolean $ungreedy_count
     */
    public $ungreedy_count = false;

    /**
     * The name of the module folder for this type of bean.
     *
     * @var string $module_dir
     */
    public $module_dir = '';

    /**
     * @var string $module_name
     */
    public $module_name = '';

    /**
     * @var array $field_name_map
     */
    public $field_name_map;

    /**
     * @var array $field_defs
     */
    public $field_defs;

    /**
     * @var DynamicField $custom_fields
     */
    public $custom_fields;

    /**
     * @var array $column_fields
     */
    public $column_fields = array();

    /**
     * @var array $list_fields
     */
    public $list_fields = array();

    /**
     * @var array $additional_column_fields
     */
    public $additional_column_fields = array();

    /**
     * @var array $relationship_fields
     */
    public $relationship_fields = array();

    /**
     * @var bool $current_notify_user
     */
    public $current_notify_user;

    /**
     * @var bool|array $fetched_row
     */
    public $fetched_row = false;
    /**
     * @var array $fetched_rel_row
     */
    public $fetched_rel_row = array();

    /**
     * @var array $layout_def
     */
    public $layout_def;

    /**
     * @var bool $force_load_details
     */
    public $force_load_details = false;

    /**
     * @var bool $optimistic_lock
     */
    public $optimistic_lock = false;

    /**
     * @var bool $disable_custom_fields
     */
    public $disable_custom_fields = false;

    /**
     * @var bool $number_formatting_done
     */
    public $number_formatting_done = false;

    /**
     * @var bool $process_field_encrypted
     */
    public $process_field_encrypted = false;

    /**
     * @var string $acltype
     */
    public $acltype = 'module';

    /**
     * @var array $additional_meta_fields
     */
    public $additional_meta_fields = array();

    /**
     * @var bool $notify_inworkflow
     */
    public $notify_inworkflow;

    /**
     * @var string $name
     */
    public $name;

    /**
     * @var string $description
     */
    public $description;

    /**
     * @var string $data_entered
     */
    public $date_entered;

    /**
     * @var string $date_modified
     */
    public $date_modified;

    /**
     * @var string $modified_user_id
     */
    public $modified_user_id;

    /**
     * @var string $assigned_user_id
     */
    public $assigned_user_id;

    /**
     * @var string $created_by
     */
    public $created_by;

    /**
     * @var string $created_by_name
     */
    public $created_by_name;

    /**
     * @var string $modified_by_name
     */
    public $modified_by_name;

    /**
     * @var boolean $importable Set to true in the child beans if the module supports importing
     */
    public $importable = false;

    /**
     * @var boolean $special_notification Set to true in the child beans if the module use the
     * special notification template
     */
    public $special_notification = false;

    /**
     * @var boolean $in_workflow Set to true if the bean is being dealt with in a workflow
     */
    public $in_workflow = false;

    /**
     *
     * @var boolean $tracker_visibility By default it will be true but if any module is to be kept non visible
     * to tracker, then its value needs to be overridden in that particular module to false.
     *
     */
    public $tracker_visibility = true;

    /**
     * @var array $listview_inner_join Used to pass inner join string to ListView Data.
     */
    public $listview_inner_join = array();

    /**
     * @var boolean $in_import Set to true in <modules>/Import/views/view.step4.php if a module is being imported
     */
    public $in_import = false;

    /**
     * @var boolean $in_save
     */
    public $in_save;

    /**
     * @var array $bean_fields_to_save
     */
    public $bean_fields_to_save;

    /**
     * @var integer $logicHookDepth
     */
    public $logicHookDepth;

    /**
     * @var int $max_logic_depth  How deep logic hooks can go
     */
    protected $max_logic_depth = 10;

    /**
     * A way to keep track of the loaded relationships so when we clone the object we can unset them.
     *
     * @var array
     */
    protected $loaded_relationships = array();

    /**
     * @var boolean $is_updated_dependent_fields set to true if dependent fields updated
     */
    protected $is_updated_dependent_fields = false;

    /**
     * Returns the ACL category for this module; defaults to the SugarBean::$acl_category if defined
     * otherwise it is SugarBean::$module_dir
     *
     * @var null|string
     */
    public $acl_category;

    /**
     * @var string $old_modified_by_name
     */
    public $old_modified_by_name;

    /**
     * @var bool $createdAuditRecords
     */
    public $createdAuditRecords;

    /**
     * Keeps track of emails sent to notify_user ids to avoid duplicate emails
     * @var array $sentAssignmentNotifications
     */
    public $sentAssignmentNotifications = array();

    /**
     * SugarBean constructor.
     * Performs following tasks:
     *
     * 1. Initialized a database connections
     * 2. Load the vardefs for the module implementing the class. cache the entries
     *    if needed
     * 3. Setup row-level security preference
     * All implementing classes  must call this constructor using the parent::SugarBean() class.
     *
     */
    public function __construct()
    {
        global $dictionary;
        static $loaded_definitions = array();
        $this->db = DBManagerFactory::getInstance();
        if (empty($this->module_name)) {
            $this->module_name = $this->module_dir;
        }
        if ((!$this->disable_vardefs && empty($loaded_definitions[$this->object_name]))
            || !empty($GLOBALS['reload_vardefs'])) {
            VardefManager::loadVardef($this->module_dir, $this->object_name);

            // build $this->column_fields from the field_defs if they exist
            if (!empty($dictionary[$this->object_name]['fields'])) {
                foreach ($dictionary[$this->object_name]['fields'] as $key => $value_array) {
                    $column_fields[] = $key;
                    if (!empty($value_array['required']) && !empty($value_array['name'])) {
                        $this->required_fields[$value_array['name']] = 1;
                    }
                }
                $this->column_fields = $column_fields;
            }

            //setup custom fields
            if (!isset($this->custom_fields) &&
                empty($this->disable_custom_fields)
            ) {
                $this->setupCustomFields($this->module_dir);
            }

            if (isset($GLOBALS['dictionary'][$this->object_name]) && !$this->disable_vardefs) {
                $this->field_name_map = isset($dictionary[$this->object_name]['fields']) ? $dictionary[$this->object_name]['fields'] : null;

                if (!isset($dictionary[$this->object_name]['fields'])) {
                    LoggerManager::getLogger()->warn('SugarBean constructor error: Object has not fields in dictionary. Object name was: ' . $this->object_name);
                    $this->field_defs = null;
                } else {
                    $this->field_defs = $dictionary[$this->object_name]['fields'];
                }

                if (!empty($dictionary[$this->object_name]['optimistic_locking'])) {
                    $this->optimistic_lock = true;
                }
            }
            $loaded_definitions[$this->object_name]['column_fields'] =& $this->column_fields;
            $loaded_definitions[$this->object_name]['list_fields'] =& $this->list_fields;
            $loaded_definitions[$this->object_name]['required_fields'] =& $this->required_fields;
            $loaded_definitions[$this->object_name]['field_name_map'] =& $this->field_name_map;
            $loaded_definitions[$this->object_name]['field_defs'] =& $this->field_defs;
        } else {
            $this->column_fields =& $loaded_definitions[$this->object_name]['column_fields'];
            $this->list_fields =& $loaded_definitions[$this->object_name]['list_fields'];
            $this->required_fields =& $loaded_definitions[$this->object_name]['required_fields'];
            $this->field_name_map =& $loaded_definitions[$this->object_name]['field_name_map'];
            $this->field_defs =& $loaded_definitions[$this->object_name]['field_defs'];
            $this->added_custom_field_defs = true;

            if (!isset($this->custom_fields) &&
                empty($this->disable_custom_fields)
            ) {
                $this->setupCustomFields($this->module_dir);
            }
            if (!empty($dictionary[$this->object_name]['optimistic_locking'])) {
                $this->optimistic_lock = true;
            }
        }

        if ($this->bean_implements('ACL') && !empty($GLOBALS['current_user'])) {
            $this->acl_fields = !(isset($dictionary[$this->object_name]['acl_fields'])
                && $dictionary[$this->object_name]['acl_fields'] === false);
        }
        $this->populateDefaultValues();
    }

    /**
     * Loads the definition of custom fields defined for the module.
     * Local file system cache is created as needed.
     *
     * @param string $module_name setting up custom fields for this module.
     */
    public function setupCustomFields($module_name)
    {
        $this->custom_fields = new DynamicField($module_name);
        $this->custom_fields->setup($this);
    }

    /**
     * @param $interface
     *
     * @return bool
     */
    public function bean_implements($interface)
    {
        return false;
    }

    /**
     * @param bool $force
     */
    public function populateDefaultValues($force = false)
    {
        if (!is_array($this->field_defs)) {
            $GLOBALS['log']->warn($this->module_name.'::populateDefaultValues $field_defs should be an array');
            return;
        }
        foreach ($this->field_defs as $field => $value) {
            if ((isset($value['default']) || !empty($value['display_default'])) && ($force || empty($this->$field))) {
                if (!isset($value['type'])) {
                    $GLOBALS['log']->warn('Undefined index: type');
                    $type = null;
                } else {
                    $type = $value['type'];
                }

                switch ($type) {
                    case 'date':
                        if (!empty($value['display_default'])) {
                            $this->$field = $this->parseDateDefault($value['display_default']);
                        }
                        break;
                    case 'datetime':
                    case 'datetimecombo':
                        if (!empty($value['display_default'])) {
                            $this->$field = $this->parseDateDefault($value['display_default'], true);
                        }
                        break;
                    case 'multienum':
                        if (empty($value['default']) && !empty($value['display_default'])) {
                            $this->$field = $value['display_default'];
                        } else {
                            $this->$field = $value['default'];
                        }
                        break;
                    case 'bool':
                        if (isset($this->$field)) {
                            break;
                        }
                    // no break
                    default:
                        if (isset($value['default']) && $value['default'] !== '') {
                            $this->$field = htmlentities((string) $value['default'], ENT_QUOTES, 'UTF-8');
                        } else {
                            $this->$field = '';
                        }
                } //switch
            }
            // refact info:
            // may we should htmlentities on field:
            // $this->field = htmlentities($this->$field, ENT_QUOTES, 'UTF-8');
        } //foreach
    }

    /**
     * Create date string from default value
     * like '+1 month'
     * @param string $value
     * @param bool $time Should be expect time set too?
     * @return string
     *
     * @throws \Exception
     */
    protected function parseDateDefault($value, $time = false)
    {
        $results = false;
        global $timedate;
        if ($time) {
            $dtAry = explode('&', $value, 2);
            $now = $timedate->getNow(true);
            $dateValue = $now->modify($dtAry[0]);
            if ($dateValue === false) {
                $GLOBALS['log']->fatal('Invalid modifier for DateTime::modify(): ' . $dtAry[0]);
            }
            if (!empty($dtAry[1])) {
                $timeValue = $timedate->fromString($dtAry[1]);
                $dateValue->setTime($timeValue->hour, $timeValue->min, $timeValue->sec);
            }
            if (is_bool($dateValue)) {
                $GLOBALS['log']->fatal('Type Error: Argument 1 passed to TimeDate::asUser() ' .
                    'must be an instance of DateTime, boolean given');
                return false;
            }
            return $timedate->asUser($dateValue);
        }
        $now = $timedate->getNow(true);
        try {
            $results = $now->modify($value);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('DateTime error: ' . $e->getMessage());
        }
        if (is_bool($results)) {
            $GLOBALS['log']->fatal('Type Error: Argument 1 passed to TimeDate::asUser() ' .
                'must be an instance of DateTime, boolean given');
            return false;
        }
        return $timedate->asUserDate($results);
    }

    /**
     * Removes relationship metadata cache.
     *
     * Every module that has relationships defined with other modules, has this meta data cached.  The cache is
     * stores in 2 locations: relationships table and file system. This method clears the cache from both locations.
     *
     * @param string $key module whose meta cache is to be cleared.
     * @param string $db database handle.
     * @param string $tablename table name
     * @param array $dictionary vardef for the module
     * @param string $module_dir name of subdirectory where module is installed.
     *
     * @static
     *
     * Internal function, do not override.
     */
    public static function removeRelationshipMeta($key, $db, $tablename, $dictionary, $module_dir)
    {
        //load the module dictionary if not supplied.
        if ((!isset($dictionary) || empty($dictionary)) && !empty($module_dir)) {
            $filename = 'modules/' . $module_dir . '/vardefs.php';
            if (file_exists($filename)) {
                include($filename);
            }
        }
        if (!is_array($dictionary) || !array_key_exists($key, $dictionary)) {
            $GLOBALS['log']->fatal("removeRelationshipMeta: Metadata for table " . $tablename . " does not exist");
            display_notice("meta data absent for table " . $tablename . " keyed to $key ");
        } else {
            if (isset($dictionary[$key]['relationships'])) {
                $RelationshipDefs = $dictionary[$key]['relationships'];
                if (!is_array($RelationshipDefs)) {
                    $GLOBALS['log']->fatal('Relationship definitions should be an array');
                    $RelationshipDefs = (array)$RelationshipDefs;
                }
                foreach ($RelationshipDefs as $rel_name => $rel_def) {
                    Relationship::delete($rel_name, $db);
                }
            }
        }
    }

    /**
     * Populates the relationship meta for a module.
     *
     * It is called during setup/install. It is used statically to create relationship meta data
     * for many-to-many tables.
     *
     * @param string $key name of the object.
     * @param object $db database handle.
     * @param string $tablename table, meta data is being populated for.
     * @param array $dictionary vardef dictionary for the object.     *
     * @param string $module_dir name of subdirectory where module is installed.
     * @param bool $is_custom Optional,set to true if module is installed in a custom directory. Default value is false.
     * @static
     *
     *  Internal function, do not override.
     */
    public static function createRelationshipMeta($key, $db, $tablename, array $dictionary, $module_dir, $is_custom = false)
    {
        //load the module dictionary if not supplied.
        if (empty($dictionary) && !empty($module_dir)) {
            if ($is_custom) {
                $filename = 'custom/modules/' . $module_dir . '/Ext/Vardefs/vardefs.ext.php';
            } else {
                if ($key == 'User') {
                    // a very special case for the Employees module
                    // this must be done because the Employees/vardefs.php does an include_once on
                    // Users/vardefs.php
                    $filename = 'modules/Users/vardefs.php';
                } else {
                    $filename = 'modules/' . $module_dir . '/vardefs.php';
                }
            }

            if (file_exists($filename)) {
                include($filename);
                // cn: bug 7679 - dictionary entries defined as $GLOBALS['name'] not found
                if (empty($dictionary) || !empty($GLOBALS['dictionary'][$key])) {
                    $dictionary = $GLOBALS['dictionary'];
                }
            } else {
                $GLOBALS['log']->debug('createRelationshipMeta: no metadata file found' . $filename);

                return;
            }
        }

        if (!is_array($dictionary) || !array_key_exists($key, $dictionary)) {
            $GLOBALS['log']->fatal("createRelationshipMeta: Metadata for table " . $tablename . " does not exist");
            display_notice("meta data absent for table " . $tablename . " keyed to $key ");
        } else {
            if (isset($dictionary[$key]['relationships'])) {
                $RelationshipDefs = $dictionary[$key]['relationships'];

                global $beanList;
                $beanList_ucase = array_change_key_case($beanList, CASE_UPPER);
                foreach ($RelationshipDefs as $rel_name => $rel_def) {
                    if (isset($rel_def['lhs_module']) && !isset($beanList_ucase[strtoupper($rel_def['lhs_module'])])) {
                        $GLOBALS['log']->debug('skipping orphaned relationship record ' .
                            $rel_name . ' lhs module is missing ' . $rel_def['lhs_module']);
                        continue;
                    }
                    if (isset($rel_def['rhs_module']) && !isset($beanList_ucase[strtoupper($rel_def['rhs_module'])])) {
                        $GLOBALS['log']->debug('skipping orphaned relationship record ' .
                            $rel_name . ' rhs module is missing ' . $rel_def['rhs_module']);
                        continue;
                    }


                    //check whether relationship exists or not first.
                    if (Relationship::exists($rel_name, $db)) {
                        $GLOBALS['log']->debug('Skipping, relationship already exists ' . $rel_name);
                    } else {
                        $seed = BeanFactory::getBean("Relationships");
                        $keys = array_keys($seed->field_defs);
                        $toInsert = array();
                        foreach ($keys as $key) {
                            if ($key == "id") {
                                $toInsert[$key] = create_guid();
                            } elseif ($key == "relationship_name") {
                                $toInsert[$key] = $rel_name;
                            } elseif (isset($rel_def[$key])) {
                                $toInsert[$key] = $rel_def[$key];
                            }
                            //todo specify defaults if meta not defined.
                        }


                        $column_list = implode(",", array_keys($toInsert));
                        $value_list = "'" . implode("','", array_values($toInsert)) . "'";

                        //create the record. todo add error check.
                        $insert_string = "INSERT into relationships (" . $column_list . ") " .
                            "values (" . $value_list . ")";
                        if ($db instanceof DBManager) {
                            $db->query($insert_string, true);
                        } else {
                            $GLOBALS['log']->fatal('Invalid Argument: Argument 2 should be a DBManager');
                        }
                    }
                }
            } else {
                $GLOBALS['log']->info('No relationships meta set for '.$module_dir);
            }
        }
    }

    /**
     * Constructs a query to fetch data for subpanels and list views
     *
     * It constructs union queries for activities subpanel.
     *
     * @param SugarBean $parentbean constructing queries for link attributes in this bean
     * @param string $order_by Optional, order by clause
     * @param string $sort_order Optional, sort order
     * @param string $where Optional, additional where clause
     * @param int $row_offset
     * @param int $limit
     * @param int $max
     * @param int $show_deleted
     * @param aSubPanel $subpanel_def
     *
     * @return array
     *
     * Internal Function, do not override.
     */
    public static function get_union_related_list(
        $parentbean,
        $order_by = '',
        $sort_order = '',
        $where = '',
        $row_offset = 0,
        $limit = -1,
        $max = -1,
        $show_deleted = 0,
        $subpanel_def= null
    ) {
        if (is_null($subpanel_def)) {
            $GLOBALS['log']->fatal('subpanel_def is null');
        }
        $secondary_queries = array();
        global $layout_edit_mode;

        $final_query = '';
        $final_query_rows = '';
        $subpanel_list = array();
        if (method_exists($subpanel_def ?? '', 'isCollection')) {
            if ($subpanel_def->isCollection()) {
                if ($subpanel_def->load_sub_subpanels() === false) {
                    $subpanel_list = array();
                } else {
                    $subpanel_list = $subpanel_def->sub_subpanels;
                }
            } else {
                $subpanel_list[] = $subpanel_def;
            }
        } else {
            $GLOBALS['log']->fatal('Subpanel definition should be an aSubPanel');
        }


        $first = true;

        //Breaking the building process into two loops. The first loop gets a list of all the sub-queries.
        //The second loop merges the queries and forces them to select the same number of columns
        //All columns in a sub-subpanel group must have the same aliases
        //If the subpanel is a datasource function, it can't be a collection
        // so we just poll that function for the and return that
        foreach ($subpanel_list as $this_subpanel) {
            if ($this_subpanel->isDatasourceFunction()
                && empty($this_subpanel->_instance_properties['generate_select'])) {
                $shortcut_function_name = $this_subpanel->get_data_source_name();
                $parameters = $this_subpanel->get_function_parameters();
                if (!empty($parameters)) {
                    //if the import file function is set, then import the file to call the custom function from
                    if (is_array($parameters) && isset($parameters['import_function_file'])) {
                        //this call may happen multiple times, so only require if function does not exist
                        if (!function_exists($shortcut_function_name)) {
                            require_once($parameters['import_function_file']);
                        }
                        //call function from required file
                        $tmp_final_query = $shortcut_function_name($parameters);
                    } else {
                        //call function from parent bean
                        $tmp_final_query = $parentbean->$shortcut_function_name($parameters);
                    }
                } else {
                    $tmp_final_query = $parentbean->$shortcut_function_name();
                }
                if (!$first) {
                    $final_query_rows .= ' UNION ALL ( '
                        . $parentbean->create_list_count_query($tmp_final_query, $parameters) . ' )';
                    $final_query .= ' UNION ALL ( ' . $tmp_final_query . ' )';
                } else {
                    $final_query_rows = '(' . $parentbean->create_list_count_query($tmp_final_query, $parameters) . ')';
                    $final_query = '(' . $tmp_final_query . ')';
                    $first = false;
                }
            }
        }
        //If final_query is still empty, its time to build the sub-queries
        if (empty($final_query)) {
            $subqueries = SugarBean::build_sub_queries_for_union($subpanel_list, $subpanel_def, $parentbean, $order_by);
            $all_fields = array();
            foreach ($subqueries as $i => $subquery) {
                $query_fields = DBManagerFactory::getInstance()->getSelectFieldsFromQuery($subquery['select']);
                foreach ($query_fields as $field => $select) {
                    if (!in_array($field, $all_fields)) {
                        $all_fields[] = $field;
                    }
                }
                $subqueries[$i]['query_fields'] = $query_fields;
            }
            $first = true;
            //Now ensure the queries have the same set of fields in the same order.
            foreach ($subqueries as $subquery) {
                $subquery['select'] = "SELECT";
                foreach ($all_fields as $field) {
                    if (!isset($subquery['query_fields'][$field])) {
                        $subquery['select'] .= " NULL $field,";
                    } else {
                        $subquery['select'] .= " {$subquery['query_fields'][$field]},";
                    }
                }
                $subquery['select'] = substr($subquery['select'], 0, strlen($subquery['select']) - 1);

                // Find related email address for sub panel ordering
                if ($order_by && isset($subpanel_def->panel_definition['list_fields'][$order_by]['widget_class']) &&
                    $subpanel_def->panel_definition['list_fields'][$order_by]['widget_class'] === 'SubPanelEmailLink' &&
                    !array_key_exists($order_by, $subquery['query_fields'])) {
                    $relatedBeanTable = $subpanel_def->table_name;
                    $relatedBeanModule = $subpanel_def->get_module_name();
                    $subquery['select'] .= ",
                    (SELECT email_addresses.email_address
                     FROM email_addr_bean_rel
                     JOIN email_addresses ON email_addresses.id = email_addr_bean_rel.email_address_id
                     WHERE
                        email_addr_bean_rel.primary_address = 1 AND
                        email_addr_bean_rel.deleted = 0 AND
                        email_addr_bean_rel.bean_id = $relatedBeanTable.id AND
                        email_addr_bean_rel.bean_module = '$relatedBeanModule') as $order_by";
                }

                //Put the query into the final_query
                $query = $subquery['select'] . " " . $subquery['from'] . " " . $subquery['where'];
                if (!$first) {
                    $query = ' UNION ALL ( ' . $query . ' )';
                    $final_query_rows .= " UNION ALL ";
                } else {
                    $query = '(' . $query . ')';
                    $first = false;
                }
                $query_array = $subquery['query_array'];
                $select_position = strpos((string) $query_array['select'], "SELECT");
                $distinct_position = strpos((string) $query_array['select'], "DISTINCT");
                if (!empty($subquery['params']['distinct']) && !empty($subpanel_def->table_name)) {
                    $query_rows = "( SELECT count(DISTINCT " . $subpanel_def->table_name . ".id)"
                        . $subquery['from_min'] . $query_array['join'] . $subquery['where'] . ' )';
                } elseif ($select_position !== false && $distinct_position !== false) {
                    $replacement = substr_replace($query_array['select'], "SELECT count(", $select_position, 6);
                    $query_rows = "( " . $replacement . ")" . $subquery['from_min']
                        . $query_array['join'] . $subquery['where'] . ' )';
                } else {
                    //resort to default behavior.
                    $query_rows = "( SELECT count(*)" . $subquery['from_min']
                        . $query_array['join'] . $subquery['where'] . ' )';
                }
                if (!empty($subquery['secondary_select'])) {
                    $subquerystring = $subquery['secondary_select'] . $subquery['secondary_from']
                        . $query_array['join'] . $subquery['where'];
                    if (!empty($subquery['secondary_where'])) {
                        if (empty($subquery['where'])) {
                            $subquerystring .= " WHERE " . $subquery['secondary_where'];
                        } else {
                            $subquerystring .= " AND " . $subquery['secondary_where'];
                        }
                    }
                    $secondary_queries[] = $subquerystring;
                }
                $final_query .= $query;
                $final_query_rows .= $query_rows;
            }
        }

        if (!empty($order_by)) {
            $isCollection = $subpanel_def->isCollection();
            if ($isCollection) {
                /** @var aSubPanel $header */
                $header = $subpanel_def->get_header_panel_def();
                $submodule = $header->template_instance;
                $suppress_table_name = true;
            } else {
                $submodule = $subpanel_def->template_instance;
                $suppress_table_name = false;
            }

            if (!empty($sort_order)) {
                $order_by .= ' ' . $sort_order;
            }

            $order_by = $parentbean->process_order_by($order_by, $submodule, $suppress_table_name);
            if (!empty($order_by)) {
                $final_query .= ' ORDER BY ' . $order_by;
            }
        }


        if (isset($layout_edit_mode) && $layout_edit_mode) {
            $response = array();
            if (!empty($submodule)) {
                $submodule->assign_display_fields($submodule->module_dir);
                $response['list'] = array($submodule);
            } else {
                $response['list'] = array();
            }
            $response['parent_data'] = array();
            $response['row_count'] = 1;
            $response['next_offset'] = 0;
            $response['previous_offset'] = 0;

            return $response;
        }

        if (method_exists($parentbean ?? '', 'process_union_list_query')) {
            return $parentbean->process_union_list_query(
                $parentbean,
                $final_query,
                $row_offset,
                $limit,
                $max,
                '',
                $subpanel_def,
                $final_query_rows,
                $secondary_queries
            );
        }
        $GLOBALS['log']->fatal('Parent bean should be a SugarBean');
        return null;
    }

    /**
     * @param $subpanel_list
     * @param $subpanel_def
     * @param $parentbean
     * @param $order_by
     *
     * @return array
     */
    protected static function build_sub_queries_for_union($subpanel_list, $subpanel_def, $parentbean, $order_by)
    {
        global $beanList;
        $subqueries = array();

        if (!is_array($subpanel_list) || is_object($subpanel_list)) {
            $GLOBALS['log']->fatal('Invalid Argument: Subpanel list should be an array.');
            $subpanel_list = (array) $subpanel_list;
        }

        foreach ($subpanel_list as $this_subpanel) {
            if (
                is_object($this_subpanel) &&
                method_exists($this_subpanel, 'isDatasourceFunction')
            ) {
                if (!$this_subpanel->isDatasourceFunction() || ($this_subpanel->isDatasourceFunction()
                        && isset($this_subpanel->_instance_properties['generate_select'])
                        && $this_subpanel->_instance_properties['generate_select'])
                ) {
                    //the custom query function must return an array with
                    if ($this_subpanel->isDatasourceFunction()) {
                        $shortcut_function_name = $this_subpanel->get_data_source_name();
                        $parameters = $this_subpanel->get_function_parameters();
                        $query_array = [];
                        if (!empty($parameters)) {
                            //if the import file function is set, then import the file to call the custom function from
                            if (is_array($parameters) && isset($parameters['import_function_file'])) {
                                //this call may happen multiple times, so only require if function does not exist
                                if (!function_exists($shortcut_function_name)) {
                                    require_once($parameters['import_function_file']);
                                }
                                //call function from required file
                                $query_array = $shortcut_function_name($parameters);
                            } elseif (!empty($parentbean)) {
                                //call function from parent bean
                                $query_array = $parentbean->$shortcut_function_name($parameters);
                            }
                        } elseif (!empty($parentbean)) {
                            $query_array = $parentbean->$shortcut_function_name();
                        }
                    } else {
                        $related_field_name = $this_subpanel->get_data_source_name();
                        if (!method_exists($parentbean ?? '', 'load_relationship')) {
                            $GLOBALS['log']->fatal('Fatal error:  Call to a member function load_relationship() ' .
                                'on an invalid object');
                        } else {
                            if (!$parentbean->load_relationship($related_field_name)) {
                                if (isset($parentbean->$related_field_name)) {
                                    unset($parentbean->$related_field_name);
                                }
                                continue;
                            }
                            $query_array = $parentbean->$related_field_name->getSubpanelQuery(array(), true);
                        }
                    }
                    $table_where = preg_replace('/^\s*WHERE/i', '', (string) $this_subpanel->get_where());
                    $queryArrayWhere = '';
                    if (isset($query_array)) {
                        $queryArrayWhere = $query_array['where'];
                    } else {
                        $GLOBALS['log']->fatal('Undefined variable: query_array');
                    }
                    $where_definition = preg_replace('/^\s*WHERE/i', '', (string) $queryArrayWhere);

                    if (!empty($table_where)) {
                        if (empty($where_definition)) {
                            $where_definition = $table_where;
                        } else {
                            $where_definition .= ' AND ' . $table_where;
                        }
                    }

                    if (isset($this_subpanel->_instance_properties['module'])) {
                        $submodulename = $this_subpanel->_instance_properties['module'];
                    } else {
                        $GLOBALS['log']->fatal('Undefined index: module');
                        $submodulename = '';
                    }
                    if (isset($beanList[$submodulename])) {
                        $submoduleclass = $beanList[$submodulename];
                    } else {
                        $GLOBALS['log']->fatal('Undefined index: ' . $submodulename);
                        $submoduleclass = null;
                    }

                    /** @var SugarBean $submodule */
                    if (class_exists($submoduleclass)) {
                        $submodule = new $submoduleclass();
                    } else {
                        $GLOBALS['log']->fatal('Class name must be a valid object or a string');
                        $submodule = null;
                    }
                    $subwhere = $where_definition;


                    $list_fields = $this_subpanel->get_list_fields();
                    foreach ($list_fields as $list_key => $list_field) {
                        if (isset($list_field['usage']) && $list_field['usage'] == 'display_only') {
                            unset($list_fields[$list_key]);
                        }
                    }


                    if (!method_exists($subpanel_def ?? '', 'isCollection')) {
                        $GLOBALS['log']->fatal('Call to a member function isCollection() on an invalid object');
                    }
                    if (
                        method_exists($subpanel_def ?? '', 'isCollection') &&
                        !$subpanel_def->isCollection() &&
                        isset($list_fields[$order_by]) &&
                        isset($submodule->field_defs[$order_by]) &&
                        (!isset($submodule->field_defs[$order_by]['source'])
                            || $submodule->field_defs[$order_by]['source'] == 'db')
                    ) {
                        $order_by = $submodule->table_name . '.' . $order_by;
                    }
                    $panel_name = $this_subpanel->name;
                    $params = array();
                    $params['distinct'] = $this_subpanel->distinct_query();

                    $params['joined_tables'] = isset($query_array['join_tables']) ? $query_array['join_tables'] : null;
                    $params['include_custom_fields'] = method_exists($subpanel_def ?? '', 'isCollection')
                        ? !$subpanel_def->isCollection() : null;
                    $params['collection_list'] = method_exists($subpanel_def ?? '', 'get_inst_prop_value')
                        ? $subpanel_def->get_inst_prop_value('collection_list') : null;

                    // use single select in case when sorting by relate field
                    $singleSelect = method_exists($submodule ?? '', 'is_relate_field')
                        ? $submodule->is_relate_field($order_by) : null;

                    $subquery = method_exists($submodule ?? '', 'create_new_list_query')
                        ? $submodule->create_new_list_query(
                            '',
                            $subwhere,
                            $list_fields,
                            $params,
                            0,
                            '',
                            true,
                            $parentbean,
                            $singleSelect
                        ) : null;

                    if (isset($subquery['select'])) {
                        $subquery['select'] .= " , '$panel_name' panel_name ";
                    } else {
                        $subquery['select'] = " , '$panel_name' panel_name ";
                    }
                    if (isset($query_array)) {
                        $subquery['from'] .= $query_array['join'];
                        $subquery['query_array'] = $query_array;
                    } else {
                        $subquery['query_array'] = null;
                    }
                    $subquery['params'] = $params;

                    $subqueries[] = $subquery;
                }
            } else {
                $GLOBALS['log']->fatal('isDatasourceFunction() is not implemented.');
            }
        }
        return $subqueries;
    }

    /**
     * Applies pagination window to union queries used by list view and subpanels,
     * executes the query and returns fetched data.
     *
     * Internal function, do not override.
     * @param object $parent_bean
     * @param string $query query to be processed.
     * @param int $row_offset
     * @param int $limit optional, default -1
     * @param int $max_per_page Optional, default -1
     * @param string $where Custom where clause.
     * @param aSubPanel $subpanel_def definition of sub-panel to be processed
     * @param string $query_row_count
     * @param array $secondary_queries
     * @return array $fetched data.
     */
    public function process_union_list_query(
        $parent_bean,
        $query,
        $row_offset,
        $limit = -1,
        $max_per_page = -1,
        $where = '',
        $subpanel_def= null,
        $query_row_count = '',
        $secondary_queries = array()
    ) {
        $current_bean = null;
        if (is_null($subpanel_def)) {
            $GLOBALS['log']->fatal('subpanel_def is null');
        }

        $db = DBManagerFactory::getInstance('listviews');
        /**
         * if the row_offset is set to 'end' go to the end of the list
         */
        $toEnd = (string)$row_offset == 'end';
        global $sugar_config;
        $use_count_query = false;
        if (!method_exists($subpanel_def, 'isCollection')) {
            $GLOBALS['log']->fatal('Call to a member function isCollection() on an invalid object');
            $processing_collection = null;
        } else {
            $processing_collection = $subpanel_def->isCollection();
        }

        $GLOBALS['log']->debug("process_union_list_query: " . $query);
        if ($max_per_page == -1) {
            $max_per_page = $sugar_config['list_max_entries_per_subpanel'];
        }
        if (empty($query_row_count)) {
            $query_row_count = $query;
        }
        $distinct_position = strpos($query_row_count, "DISTINCT");

        if ($distinct_position !== false) {
            $use_count_query = true;
        }
        $performSecondQuery = true;
        if (empty($sugar_config['disable_count_query']) || $toEnd) {
            $rows_found = $this->_get_num_rows_in_query($query_row_count, $use_count_query);
            if ($rows_found < 1) {
                $performSecondQuery = false;
            }
            if (!empty($rows_found) && (empty($limit) || $limit == -1)) {
                $limit = $max_per_page;
            }
            if ($toEnd) {
                $row_offset = (floor(($rows_found - 1) / $limit)) * $limit;
            }
        } else {
            if ((empty($limit) || $limit == -1)) {
                $limit = $max_per_page + 1;
                $max_per_page = $limit;
            }
        }

        if (empty($row_offset)) {
            $row_offset = 0;
        }
        $list = array();
        $previous_offset = $row_offset - $max_per_page;
        $next_offset = $row_offset + $max_per_page;

        if ($performSecondQuery) {
            if (!empty($limit) && $limit != -1 && $limit != -99) {
                if (empty($parent_bean)) {
                    $objectName = '[empty parent bean]';
                } else {
                    $objectName = $parent_bean->object_name;
                }
                $result = $db->limitQuery($query, $row_offset, $limit, true, "Error retrieving $objectName list: ");
            } else {
                $result = $db->query($query, true, "Error retrieving $this->object_name list: ");
            }
            //use -99 to return all

            // get the current row
            $index = $row_offset;
            $row = $db->fetchByAssoc($result);

            $post_retrieve = array();
            $isFirstTime = true;
            while ($row) {
                $function_fields = array();
                if (($index < $row_offset + $max_per_page || $max_per_page == -99)) {
                    if ($processing_collection) {
                        if (!isset($row['panel_name'])) {
                            $GLOBALS['log']->fatal('"panel_name" is not set');
                            $row['panel_name'] = null;
                        }
                        if (
                            !isset($subpanel_def->sub_subpanels) ||
                            !isset($subpanel_def->sub_subpanels[$row['panel_name']]) ||
                            !isset($subpanel_def->sub_subpanels[$row['panel_name']]->template_instance)) {
                            $current_bean = new stdClass();
                        } else {
                            $current_bean = $subpanel_def->sub_subpanels[$row['panel_name']]->template_instance;
                        }
                        if (!$isFirstTime) {
                            $class = get_class($subpanel_def->sub_subpanels[$row['panel_name']]->template_instance);
                            $current_bean = new $class();
                        }
                    } else {
                        if (!is_object($subpanel_def)) {
                            $GLOBALS['log']->fatal('Subpanel Definition is not an object');
                        } elseif (!isset($subpanel_def->template_instance)) {
                            $GLOBALS['log']->fatal('Undefined template instance');
                        } else {
                            $current_bean = $subpanel_def->template_instance;
                            if (!$isFirstTime) {
                                $class = get_class($subpanel_def->template_instance);
                                $current_bean = new $class();
                            }
                        }
                    }
                    $isFirstTime = false;
                    //set the panel name in the bean instance.
                    if (isset($row['panel_name'])) {
                        $current_bean->panel_name = $row['panel_name'];
                    }

                    $fieldDefs = array();
                    if (isset($current_bean) && is_object($current_bean)) {
                        if (!isset($current_bean->field_defs)) {
                            $GLOBALS['log']->fatal('Trying to get property of non-object');
                        } else {
                            if (!is_array($current_bean->field_defs)) {
                                $GLOBALS['log']->fatal(
                                    'SugarBean::process_union_list_query $field_defs should be an array'
                                );
                                $fieldDefs = (array)$current_bean->field_defs;
                            } else {
                                $fieldDefs = $current_bean->field_defs;
                            }
                        }
                    } else {
                        $GLOBALS['log']->fatal('Unresolved current bean');
                        $current_bean = new stdClass();
                    }

                    foreach ($fieldDefs as $field => $value) {
                        if (isset($row[$field])) {
                            $current_bean->$field = $this->convertField($row[$field], $value);
                            unset($row[$field]);
                        } elseif (isset($row[$this->table_name . '.' . $field])) {
                            $current_bean->$field = $this->convertField($row[$this->table_name . '.' . $field], $value);
                            unset($row[$this->table_name . '.' . $field]);
                        } else {
                            $current_bean->$field = "";
                            unset($row[$field]);
                        }
                        if (isset($value['source']) && $value['source'] == 'function') {
                            $function_fields[] = $field;
                        }
                    }
                    foreach ($row as $key => $value) {
                        $current_bean->$key = $value;
                    }
                    foreach ($function_fields as $function_field) {
                        $value = $current_bean->field_defs[$function_field];
                        $can_execute = true;
                        $execute_params = array();
                        $execute_function = array();
                        if (!empty($value['function_class'])) {
                            $execute_function[] = $value['function_class'];
                            $execute_function[] = $value['function_name'];
                        } else {
                            $execute_function = $value['function_name'];
                        }
                        foreach ($value['function_params'] as $param) {
                            if (empty($value['function_params_source'])
                                || $value['function_params_source'] == 'parent') {
                                if (empty($this->$param)) {
                                    $can_execute = false;
                                } elseif ($param == '$this') {
                                    $execute_params[] = $this;
                                } else {
                                    $execute_params[] = $this->$param;
                                }
                            } elseif ($value['function_params_source'] == 'this') {
                                if (empty($current_bean->$param)) {
                                    $can_execute = false;
                                } elseif ($param == '$this') {
                                    $execute_params[] = $current_bean;
                                } else {
                                    $execute_params[] = $current_bean->$param;
                                }
                            } else {
                                $can_execute = false;
                            }
                        }
                        if ($can_execute) {
                            if (!empty($value['function_require'])) {
                                require_once($value['function_require']);
                            }
                            $current_bean->$function_field = call_user_func_array($execute_function, $execute_params);
                        }
                    }
                    if (!empty($current_bean->parent_type) && !empty($current_bean->parent_id)) {
                        if (!isset($post_retrieve[$current_bean->parent_type])) {
                            $post_retrieve[$current_bean->parent_type] = array();
                        }
                        $post_retrieve[$current_bean->parent_type][] = array(
                            'child_id' => $current_bean->id,
                            'parent_id' => $current_bean->parent_id,
                            'parent_type' => $current_bean->parent_type,
                            'type' => 'parent'
                        );
                    }
                    if (!isset($current_bean->id)) {
                        $current_bean->id = null;
                    }
                    $list[$current_bean->id] = $current_bean;
                }
                // go to the next row
                $index++;
                $row = $db->fetchByAssoc($result);
            }
            //now handle retrieving many-to-many relationships
            if (!empty($list)) {
                foreach ($secondary_queries as $query2) {
                    $result2 = $db->query($query2);

                    $row2 = $db->fetchByAssoc($result2);
                    while ($row2) {
                        $id_ref = $row2['ref_id'];

                        if (isset($list[$id_ref])) {
                            foreach ($row2 as $r2key => $r2value) {
                                if ($r2key != 'ref_id') {
                                    $list[$id_ref]->$r2key = $r2value;
                                }
                            }
                        }
                        $row2 = $db->fetchByAssoc($result2);
                    }
                }
            }

            if (isset($post_retrieve)) {
                $parent_fields = $this->retrieve_parent_fields($post_retrieve);
            } else {
                $parent_fields = array();
            }
            if (!empty($sugar_config['disable_count_query']) && !empty($limit)) {
                //C.L. Bug 43535 - Use the $index value to set the $rows_found value here
                $rows_found = isset($index) ? $index : $row_offset + count($list);

                if (!$toEnd) {
                    if (count($list) >= $limit) {
                        array_pop($list);
                    }
                    $next_offset--;
                    $previous_offset++;
                }
            }
        } else {
            $parent_fields = array();
        }
        $response = array();
        $response['list'] = $list;
        $response['parent_data'] = $parent_fields;
        $response['row_count'] = $rows_found;
        $response['next_offset'] = $next_offset;
        $response['previous_offset'] = $previous_offset;
        $response['current_offset'] = $row_offset;
        $response['query'] = $query;

        return $response;
    }

    /**
     * Returns the number of rows that the given SQL query should produce
     *
     * Internal function, do not override.
     * @param string $query valid select  query
     * @param bool $is_count_query Optional, Default false, set to true if passed query is a count query.
     * @return int count of rows found
     */
    public function _get_num_rows_in_query($query, $is_count_query = false)
    {
        $num_rows_in_query = 0;
        if (!$is_count_query) {
            $count_query = $this->create_list_count_query($query);
        } else {
            $count_query = $query;
        }

        $result = $this->db->query($count_query, true, "Error running count query for $this->object_name List: ");
        while ($row = $this->db->fetchByAssoc($result, true)) {
            $num_rows_in_query += current($row);
        }

        return (int)$num_rows_in_query;
    }

    /**
     * Returns parent record data for objects that store relationship information
     *
     * @param array $type_info
     * @return array
     *
     * Internal function, do not override.
     */
    public function retrieve_parent_fields($type_info)
    {
        $queries = array();
        global $beanList, $beanFiles;
        $templates = array();
        $parent_child_map = array();

        if (!is_array($type_info)) {
            $GLOBALS['log']->warn('Type info is not an array');
        }
        foreach ((array)$type_info as $children_info) {
            if (!is_array($children_info)) {
                $GLOBALS['log']->warn('Children info is not an array');
            }
            foreach ((array)$children_info as $child_info) {
                if (is_array($child_info) && $child_info['type'] == 'parent') {
                    if (!isset($child_info['parent_type'])) {
                        $GLOBALS['log']->fatal('"parent_type" is not set');
                    }
                    if (!isset($child_info['parent_type']) || empty($templates[$child_info['parent_type']])) {
                        //Test emails will have an invalid parent_type, don't try to load the non-existent parent bean
                        if (isset($child_info['parent_type'])) {
                            if ($child_info['parent_type'] == 'test') {
                                continue;
                            }
                            if (isset($beanList[$child_info['parent_type']])) {
                                $class = $beanList[$child_info['parent_type']];
                            } else {
                                $GLOBALS['log']->fatal('Parent type is not a valid bean: '
                                    . $child_info['parent_type']);
                                $class = null;
                            }
                        }
                        // Added to avoid error below; just silently fail and write message to log
                        if (isset($class)) {
                            if (empty($beanFiles[$class])) {
                                $GLOBALS['log']->error($this->object_name . '::retrieve_parent_fields() - ' .
                                    'cannot load class "' . $class . '", skip loading.');
                                continue;
                            }
                            require_once($beanFiles[$class]);
                            $templates[$child_info['parent_type']] = new $class();
                        }
                    }

                    if (isset($child_info['parent_type']) && empty($queries[$child_info['parent_type']])) {
                        $queries[$child_info['parent_type']] = "SELECT id ";
                        if (isset($templates[$child_info['parent_type']])) {
                            $field_def = $templates[$child_info['parent_type']]->field_defs['name'];
                        } else {
                            $GLOBALS['log']->fatal('No template for Parent type: ' . $child_info['parent_type']);
                            $field_def = null;
                        }
                        if (isset($field_def['db_concat_fields'])) {
                            $queries[$child_info['parent_type']] .= ' , '
                                . $this->db->concat(
                                    $templates[$child_info['parent_type']]->table_name,
                                    $field_def['db_concat_fields']
                                ) . ' parent_name';
                        } else {
                            $queries[$child_info['parent_type']] .= ' , name parent_name';
                        }
                        if (isset($templates[$child_info['parent_type']]->field_defs['assigned_user_id'])) {
                            $queries[$child_info['parent_type']] .= ", assigned_user_id parent_name_owner , " .
                                "'{$child_info['parent_type']}' parent_name_mod";
                        } elseif (isset($templates[$child_info['parent_type']]->field_defs['created_by'])) {
                            $queries[$child_info['parent_type']] .= ", created_by parent_name_owner, " .
                                "'{$child_info['parent_type']}' parent_name_mod";
                        }
                        if (isset($templates[$child_info['parent_type']])) {
                            if (isset($child_info['parent_id'])) {
                                $childInfoParentId = $child_info['parent_id'];
                            } else {
                                $GLOBALS['log']->fatal('"parent_id" is not set');
                                $childInfoParentId = null;
                            }
                            $queries[$child_info['parent_type']] .=
                                " FROM " . $templates[$child_info['parent_type']]->table_name .
                                " WHERE id IN ('$childInfoParentId'";
                        }
                    } else {
                        if (isset($child_info['parent_id']) && empty($parent_child_map[$child_info['parent_id']]) && isset($child_info['parent_type'])) {
                            $queries[$child_info['parent_type']] .= " ,'{$child_info['parent_id']}'";
                        }
                    }

                    if (isset($child_info['parent_id'])) {
                        if (isset($child_info['child_id'])) {
                            $parent_child_map[$child_info['parent_id']][] = $child_info['child_id'];
                        } else {
                            $GLOBALS['log']->fatal('"child_id" is not set');
                            $parent_child_map[$child_info['parent_id']][] = null;
                        }
                    }
                }
            }
        }
        $results = array();
        foreach ($queries as $query) {
            $result = $this->db->query($query . ')');
            while ($row = $this->db->fetchByAssoc($result)) {
                $results[$row['id']] = $row;
            }
        }

        $child_results = array();
        foreach ($parent_child_map as $parent_key => $parent_child) {
            foreach ($parent_child as $child) {
                if (isset($results[$parent_key])) {
                    $child_results[$child] = $results[$parent_key];
                }
            }
        }
        return $child_results;
    }

    /**
     * Returns a list of fields with their definitions that have the audited property set to true.
     * Before calling this function, check whether audit has been enabled for the table/module or not.
     * You would set the audit flag in the implementing module's vardef file.
     *
     * @return array
     * @see is_AuditEnabled
     *
     * Internal function, do not override.
     */
    public function getAuditEnabledFieldDefinitions()
    {
        if (!isset($this->audit_enabled_fields)) {
            $this->audit_enabled_fields = array();
            if (!isset($this->field_defs)) {
                $GLOBALS['log']->fatal('Field definition is not set.');
            } elseif (!is_array($this->field_defs)) {
                $GLOBALS['log']->fatal('Field definition is not an array.');
            } else {
                foreach ($this->field_defs as $field => $properties) {
                    if (
                    (
                        !empty($properties['Audited']) || !empty($properties['audited'])
                    )
                    ) {
                        $this->audit_enabled_fields[$field] = $properties;
                    }
                }
            }
        }
        return $this->audit_enabled_fields;
    }

    /**
     * Returns true of false if the user_id passed is the owner
     *
     * @param string $user_id GUID
     * @return bool
     */
    public function isOwner($user_id)
    {
        //if we don't have an id we must be the owner as we are creating it
        if (!isset($this->id)  || $this->id == "[SELECT_ID_LIST]") {
            return true;
        }
        //if there is an assigned_user that is the owner
        if (!empty($this->fetched_row['assigned_user_id'])) {
            if ($this->fetched_row['assigned_user_id'] == $user_id) {
                return true;
            }
            return false;
        } elseif (!empty($this->assigned_user_id)) {
            if ($this->assigned_user_id == $user_id) {
                return true;
            }
            return false;
        }
        //other wise if there is a created_by that is the owner
        if (!empty($this->created_by) && $this->created_by == $user_id) {
            return true;
        }

        return false;
    }

    /**
     * Returns the name of the custom table.
     * Custom table's name is based on implementing class' table name.
     *
     * @return String Custom table name.
     *
     * Internal function, do not override.
     */
    public function get_custom_table_name()
    {
        return $this->getTableName() . '_cstm';
    }

    /**
     * Returns the implementing class' table name.
     *
     * All implementing classes set a value for the table_name variable. This value is returned as the
     * table name. If not set, table name is extracted from the implementing module's vardef.
     *
     * @return String Table name.
     *
     * Internal function, do not override.
     */
    public function getTableName()
    {
        if (isset($this->table_name)) {
            return $this->table_name;
        }
        global $dictionary;
        if (!isset($dictionary[$this->getObjectName()])) {
            $GLOBALS['log']->fatal('Dictionary doesn\'t contains an index: ' . $this->getObjectName());
            return null;
        }
        return $dictionary[$this->getObjectName()]['table'];
    }

    /**
     * Returns the object name. If object_name is not set, table_name is returned.
     *
     * All implementing classes must set a value for the object_name variable.
     *
     * @return  string
     *
     */
    public function getObjectName()
    {
        if (!isset($this->object_name)) {
            $GLOBALS['log']->fatal('"object_name" is not set');
            return null;
        }
        if ($this->object_name) {
            return $this->object_name;
        }

        // This is a quick way out. The generated metadata files have the table name
        // as the key. The correct way to do this is to override this function
        // in bean and return the object name. That requires changing all the beans
        // as well as put the object name in the generator.
        if (!isset($this->table_name)) {
            $GLOBALS['log']->fatal('"table_name" is not set');
            return null;
        }
        return $this->table_name;
    }

    /**
     * Returns index definitions for the implementing module.
     *
     * The definitions were loaded in the constructor.
     *
     * @return array Index definitions.
     *
     * Internal function, do not override.
     */
    public function getIndices()
    {
        global $dictionary;
        if (isset($dictionary[$this->getObjectName()]['indices'])) {
            return $dictionary[$this->getObjectName()]['indices'];
        }
        return array();
    }

    /**
     * Returns definition for the id field name.
     *
     * The definitions were loaded in the constructor.
     *
     * @return array Field properties.
     *
     * Internal function, do not override.
     */
    public function getPrimaryFieldDefinition()
    {
        $def = $this->getFieldDefinition("id");
        if (empty($def)) {
            $def = $this->getFieldDefinition(0);
        }
        if (empty($def)) {
            if (!is_array($this->field_defs) && !is_object($this->field_defs)) {
                $GLOBALS['log']->fatal('SugarBean::getPrimaryFieldDefinition $field_defs should be an array');
            } else {
                $defs = (array)$this->field_defs;
                reset($defs);
                $def = current($defs);
            }
        }
        return $def;
    }

    /**
     * Returns field definition for the requested field name.
     *
     * The definitions were loaded in the constructor.
     *
     * @param string $name ,
     * @return mixed Field properties or bool false if the field doesn't exist
     *
     * Internal function, do not override.
     */
    public function getFieldDefinition($name)
    {
        if (!isset($this->field_defs[$name])) {
            return false;
        }

        return $this->field_defs[$name];
    }

    /**
     * Returns the value for the requested field.
     *
     * When a row of data is fetched using the bean, all fields are created as variables in the context
     * of the bean and then fetched values are set in these variables.
     *
     * @param string $name ,
     * @return mixed.
     *
     * Internal function, do not override.
     */
    public function getFieldValue($name)
    {
        if (!isset($this->$name)) {
            return false;
        }
        if ($this->$name === true) {
            return 1;
        }
        if ($this->$name === false) {
            return 0;
        }
        return $this->$name;
    }

    /**
     * Basically undoes the effects of SugarBean::populateDefaultValues(); this method is best called right after object
     * initialization.
     */
    public function unPopulateDefaultValues()
    {
        if (!is_array($this->field_defs)) {
            return;
        }

        foreach ($this->field_defs as $field => $value) {
            if (!empty($this->$field)
                && ((isset($value['default']) && $this->$field == $value['default'])
                    || (!empty($value['display_default']) && $this->$field == $value['display_default']))
            ) {
                $this->$field = null;
                continue;
            }
            if (
                !empty($this->$field) &&
                !empty($value['display_default']) &&
                in_array($value['type'], array('date', 'datetime', 'datetimecombo')) &&
                $this->$field == $this->parseDateDefault($value['display_default'], ($value['type'] != 'date'))
            ) {
                $this->$field = null;
            }
        }
    }

    /**
     * Handle the following when a SugarBean object is cloned
     *
     * Currently all this does it unset any relationships that were created prior to cloning the object
     *
     * @api
     */
    public function __clone()
    {
        if (!empty($this->loaded_relationships)) {
            foreach ($this->loaded_relationships as $rel) {
                unset($this->$rel);
            }
        }
    }

    /**
     * Loads all attributes of type link.
     *
     * DO NOT CALL THIS FUNCTION IF YOU CAN AVOID IT. Please use load_relationship directly instead.
     *
     * Method searches the implementing module's vardef file for attributes of type link, and for each attribute
     * create a similarly named variable and load the relationship definition.
     *
     * Internal function, do not override.
     */
    public function load_relationships()
    {
        $GLOBALS['log']->debug("SugarBean.load_relationships, Loading all relationships of type link.");
        $linked_fields = $this->get_linked_fields();
        foreach ($linked_fields as $name => $properties) {
            $this->load_relationship($name);
        }
    }

    /**
     * Returns an array of fields that are of type link.
     *
     * @return array List of fields.
     *
     * Internal function, do not override.
     */
    public function get_linked_fields()
    {
        $linked_fields = array();

        $fieldDefs = $this->getFieldDefinitions();

        //find all definitions of type link.
        if (!empty($fieldDefs)) {
            foreach ($fieldDefs as $name => $properties) {
                if (!is_array($properties)) {
                    $GLOBALS['log']->fatal('array_search() expects parameter 2 to be array, ' .
                        gettype($properties) . ' given');
                } elseif (array_search('link', $properties, true) === 'type') {
                    $linked_fields[$name] = $properties;
                }
            }
        }

        return $linked_fields;
    }

    /**
     * Returns field definitions for the implementing module.
     *
     * The definitions were loaded in the constructor.
     *
     * @return array Field definitions.
     *
     * Internal function, do not override.
     */
    public function getFieldDefinitions()
    {
        return $this->field_defs;
    }

    /**
     * Loads the request relationship.
     * This method should be called before performing any operations on the related data.
     *
     * This method searches the vardef array for the requested attribute's definition. If the attribute is of the type
     * link then it creates a similarly named variable and loads the relationship definition.
     *
     * @param string $rel_name relationship/attribute name.
     * @return bool.
     */
    public function load_relationship($rel_name)
    {
        $GLOBALS['log']->debug("SugarBean[{$this->object_name}].load_relationships, Loading relationship (" .
            $rel_name . ").");

        if (empty($rel_name)) {
            $GLOBALS['log']->error("SugarBean.load_relationships, Null relationship name passed.");
            return false;
        }
        $fieldDefs = $this->getFieldDefinitions();

        //find all definitions of type link.
        if (!empty($fieldDefs[$rel_name])) {
            //initialize a variable of type Link
            require_once('data/Link2.php');
            $class = load_link_class($fieldDefs[$rel_name]);
            if (isset($this->$rel_name) && $this->$rel_name instanceof $class) {
                return true;
            }
            //if rel_name is provided, search the fieldDef array keys by name.
            if (isset($fieldDefs[$rel_name]['type']) && $fieldDefs[$rel_name]['type'] == 'link') {
                if ($class == "Link2") {
                    $this->$rel_name = new $class($rel_name, $this);
                } else {
                    if (!class_exists($class)) {
                        $GLOBALS['log']->fatal('Class not found: ' . $class);
                    } else {
                        if (!isset($fieldDefs[$rel_name]['relationship'])) {
                            $GLOBALS['log']->fatal('Relationship not found');
                        } else {
                            $this->$rel_name = new $class(
                                $fieldDefs[$rel_name]['relationship'],
                                $this,
                                $fieldDefs[$rel_name]
                            );
                        }
                    }
                }

                if (empty($this->$rel_name) ||
                    (method_exists($this->$rel_name, "loadedSuccesfully") && !$this->$rel_name->loadedSuccesfully())
                ) {
                    unset($this->$rel_name);
                    return false;
                }
                // keep track of the loaded relationships
                $this->loaded_relationships[] = $rel_name;
                return true;
            }
        }
        $GLOBALS['log']->debug("SugarBean.load_relationships, failed Loading relationship (" . $rel_name . ")");
        return false;
    }

    /**
     * Returns an array of beans of related data.
     *
     * For instance, if an account is related to 10 contacts , this function will return an array of contacts beans (10)
     * with each bean representing a contact record.
     * Method will load the relationship if not done so already.
     *
     * @param string $field_name relationship to be loaded.
     * @param string $bean_name  class name of the related bean.legacy
     * @param string $order_by , Optional, default empty.
     * @param int $begin_index Optional, default 0, unused.
     * @param int $end_index Optional, default -1
     * @param int $deleted Optional, Default 0, 0  adds deleted=0 filter, 1  adds deleted=1 filter.
     * @param string $optional_where , Optional, default empty.
     * @return SugarBean[]
     *
     * Internal function, do not override.
     */
    public function get_linked_beans(
        $field_name,
        $bean_name = '',
        $order_by = '',
        $begin_index = 0,
        $end_index = -1,
        $deleted = 0,
        $optional_where = ""
    ) {
        //if bean_name is Case then use aCase
        if ($bean_name == "Case") {
            $bean_name = "aCase";
        }

        if ($this->load_relationship($field_name)) {
            if ($this->$field_name instanceof Link) {
                // some classes are still based on Link, e.g. TeamSetLink
                return array_values($this->$field_name->getBeans(
                    new $bean_name(),
                    $order_by,
                    $begin_index,
                    $end_index,
                    $deleted,
                    $optional_where
                ));
            }
            // Link2 style
            if ($begin_index != 0 || $end_index != -1 || !empty($deleted) || !empty($optional_where) || !empty($order_by)) {
                return array_values($this->$field_name->getBeans(array(
                    'where' => $optional_where,
                    'deleted' => $deleted,
                    'offset' => $begin_index,
                    'limit' => ($end_index - $begin_index),
                    'order_by' => $order_by
                )));
            }
            return array_values($this->$field_name->getBeans());
        }
        return array();
    }

    /**
     * Returns an array of fields that are required for import
     *
     * @return array
     */
    public function get_import_required_fields()
    {
        $importable_fields = $this->get_importable_fields();
        $required_fields = array();

        foreach ($importable_fields as $name => $properties) {
            if (isset($properties['importable']) && is_string($properties['importable'])
                && $properties['importable'] == 'required') {
                $required_fields[$name] = $properties;
            }
        }

        return $required_fields;
    }

    /**
     * Returns an array of fields that are able to be Imported into
     * i.e. 'importable' not set to 'false'
     *
     * @return array List of fields.
     *
     * Internal function, do not override.
     */
    public function get_importable_fields()
    {
        $importableFields = array();

        $fieldDefs = $this->getFieldDefinitions();

        if (!empty($fieldDefs)) {
            foreach ($fieldDefs as $key => $value_array) {
                if ((isset($value_array['importable'])
                        && (is_string($value_array['importable']) && $value_array['importable'] == 'false'
                            || is_bool($value_array['importable']) && !$value_array['importable']))
                    || (isset($value_array['type']) && $value_array['type'] == 'link')
                    || (isset($value_array['auto_increment'])
                        && ($value_array['type'] || $value_array['type'] == 'true'))
                ) {
                    // only allow import if we force it
                    if (isset($value_array['importable'])
                        && (is_string($value_array['importable']) && $value_array['importable'] == 'true'
                            || is_bool($value_array['importable']) && $value_array['importable'])
                    ) {
                        $importableFields[$key] = $value_array;
                    }
                } else {

                    //Expose the corresponding id field of a relate field if it is only defined as a link
                    // so that users can relate records by id during import
                    if (isset($value_array['type']) && ($value_array['type'] == 'relate')
                        && isset($value_array['id_name'])) {
                        $idField = $value_array['id_name'];
                        if (isset($fieldDefs[$idField]) && isset($fieldDefs[$idField]['type'])
                            && $fieldDefs[$idField]['type'] == 'link') {
                            $tmpFieldDefs = $fieldDefs[$idField];
                            $tmpFieldDefs['vname'] = translate($value_array['vname'], $this->module_dir)
                                . " " . $GLOBALS['app_strings']['LBL_ID'];
                            $importableFields[$idField] = $tmpFieldDefs;
                        }
                    }

                    $importableFields[$key] = $value_array;
                }
            }
        }

        return $importableFields;
    }

    /**
     * Creates tables for the module implementing the class.
     * If you override this function make sure that your code can handles table creation.
     *
     */
    public function create_tables()
    {
        global $dictionary;

        $key = $this->getObjectName();
        if (!array_key_exists($key, $dictionary)) {
            $GLOBALS['log']->fatal("create_tables: Metadata for table " . $this->table_name . " does not exist");
            display_notice("meta data absent for table " . $this->table_name . " keyed to $key ");
        } else {
            if (!$this->db->tableExists($this->table_name)) {
                $this->db->createTable($this);
                if ($this->bean_implements('ACL')) {
                    if (!empty($this->acltype)) {
                        ACLAction::addActions($this->getACLCategory(), $this->acltype);
                    } else {
                        ACLAction::addActions($this->getACLCategory());
                    }
                }
            } else {
                echo "Table already exists : $this->table_name<br>";
            }
            if ($this->is_AuditEnabled() && !$this->db->tableExists($this->get_audit_table_name())) {
                $this->create_audit_table();
            }
        }
    }

    /**
     * Returns the ACL category for this module; defaults to the SugarBean::$acl_category if defined
     * otherwise it is SugarBean::$module_dir
     *
     * @return string
     */
    public function getACLCategory()
    {
        return !empty($this->acl_category) ? $this->acl_category : $this->module_dir;
    }

    /**
     * Return true if auditing is enabled for this object
     * You would set the audit flag in the implementing module's vardef file.
     *
     * @return bool
     *
     * Internal function, do not override.
     */
    public function is_AuditEnabled()
    {
        global $dictionary;
        if (isset($dictionary[$this->getObjectName()]['audited'])) {
            return $dictionary[$this->getObjectName()]['audited'];
        }
        return false;
    }

    /**
     * Returns the name of the audit table.
     * Audit table's name is based on implementing class' table name.
     *
     * @return String Audit table name.
     *
     * Internal function, do not override.
     */
    public function get_audit_table_name()
    {
        return $this->getTableName() . '_audit';
    }

    /**
     * If auditing is enabled, create the audit table.
     *
     * Function is used by the install scripts and a repair utility in the admin panel.
     *
     * Internal function, do not override.
     */
    public function create_audit_table()
    {
        global $dictionary;
        $table_name = $this->get_audit_table_name();

        require('metadata/audit_templateMetaData.php');

        // Bug: 52583 Need ability to customize template for audit tables
        $custom = 'custom/metadata/audit_templateMetaData_' . $this->getTableName() . '.php';
        if (file_exists($custom)) {
            require($custom);
        }

        $fieldDefs = $dictionary['audit']['fields'];
        $indices = $dictionary['audit']['indices'];

        // Renaming template indexes to fit the particular audit table (removed the brittle hard coding)
        foreach ($indices as $nr => $properties) {
            $indices[$nr]['name'] = 'idx_' . strtolower($this->getTableName()) . '_' . $properties['name'];
        }

        $engine = null;
        if (isset($dictionary['audit']['engine'])) {
            $engine = $dictionary['audit']['engine'];
        } elseif (isset($dictionary[$this->getObjectName()]['engine'])) {
            $engine = $dictionary[$this->getObjectName()]['engine'];
        }

        $this->db->createTableParams($table_name, $fieldDefs, $indices, $engine);
    }

    /**
     * Delete the primary table for the module implementing the class.
     * If custom fields were added to this table/module, the custom table will be removed too, along with the cache
     * entries that define the custom fields.
     *
     */
    public function drop_tables()
    {
        global $dictionary;
        $key = $this->getObjectName();
        if (!array_key_exists($key, $dictionary)) {
            $GLOBALS['log']->fatal("drop_tables: Metadata for table " . $this->table_name . " does not exist");
            echo "meta data absent for table " . $this->table_name . "<br>\n";
        } else {
            if (empty($this->table_name)) {
                return;
            }
            if ($this->db->tableExists($this->table_name)) {
                $this->db->dropTable($this);
            }
            if ($this->db->tableExists($this->table_name . '_cstm')) {
                $this->db->dropTableName($this->table_name . '_cstm');
                DynamicField::deleteCache();
            }
            if ($this->db->tableExists($this->get_audit_table_name())) {
                $this->db->dropTableName($this->get_audit_table_name());
            }
        }
    }

    /**
     * Implements a generic insert and update logic for any SugarBean
     * This method only works for subclasses that implement the same variable names.
     * This method uses the presence of an id field that is not null to signify and update.
     * The id field should not be set otherwise.
     *
     * @param bool $check_notify Optional, default false, if set to true assignee of the record is notified via email.
     * @return string ID
     * @todo Add support for field type validation and encoding of parameters.
     */
    public function save($check_notify = false)
    {
        $this->in_save = true;
        // cn: SECURITY - strip XSS potential vectors
        $this->cleanBean();
        // This is used so custom/3rd-party code can be upgraded with fewer issues,
        // this will be removed in a future release
        $this->fixUpFormatting();
        global $current_user, $action;

        $isUpdate = true;
        if (empty($this->id)) {
            $isUpdate = false;
        }

        if ($this->new_with_id) {
            $isUpdate = false;
        }
        if (empty($this->date_modified) || $this->update_date_modified) {
            $this->date_modified = $GLOBALS['timedate']->nowDb();
        }

        $this->_checkOptimisticLocking($action, $isUpdate);

        if (!empty($this->modified_by_name)) {
            $this->old_modified_by_name = $this->modified_by_name;
        }
        if ($this->update_modified_by) {
            $this->modified_user_id = 1;

            if (!empty($current_user)) {
                $this->modified_user_id = $current_user->id;
                $this->modified_by_name = $current_user->user_name;
            }
        }
        if ($this->deleted != 1) {
            $this->deleted = 0;
        }
        if (!$isUpdate) {
            if (empty($this->date_entered)) {
                $this->date_entered = $this->date_modified;
            }
            if ($this->set_created_by) {
                // created by should always be this user
                $this->created_by = (isset($current_user)) ? $current_user->id : "";
            }
            if (!$this->new_with_id) {
                $this->id = create_guid();
            }
        }


        require_once("data/BeanFactory.php");
        BeanFactory::registerBean($this->module_name, $this);

        if (empty($GLOBALS['updating_relationships']) && empty($GLOBALS['saving_relationships'])
            && empty($GLOBALS['resavingRelatedBeans'])) {
            $GLOBALS['saving_relationships'] = true;
            // let subclasses save related field changes
            $this->save_relationship_changes($isUpdate);
            $GLOBALS['saving_relationships'] = false;
        }
        if ($isUpdate && !$this->update_date_entered) {
            unset($this->date_entered);
        }
        // call the custom business logic
        $custom_logic_arguments = [];
        $custom_logic_arguments['check_notify'] = $check_notify;


        $this->call_custom_logic("before_save", $custom_logic_arguments);
        unset($custom_logic_arguments);

        // If we're importing back semi-colon separated non-primary emails
        if ($this->hasEmails() && !empty($this->email_addresses_non_primary)
            && is_array($this->email_addresses_non_primary)) {
            // Add each mail to the account
            if (!isset($this->emailAddress)) {
                $GLOBALS['log']->fatal('Undefined property: SugarBeanMock::$emailAddress');
            } else {
                foreach ($this->email_addresses_non_primary as $mail) {
                    $this->emailAddress->addAddress($mail);
                }
                $this->emailAddress->saveEmail(
                    $this->id,
                    $this->module_dir,
                    '',
                    '',
                    '',
                    '',
                    '',
                    $this->in_workflow
                );
            }
        }

        if (isset($this->custom_fields)) {
            $this->custom_fields->bean = $this;
            $this->custom_fields->save($isUpdate);
        }

        $this->_sendNotifications($check_notify);

        if ($isUpdate) {
            $this->db->update($this);
        } else {
            $this->db->insert($this);
        }

        if (empty($GLOBALS['resavingRelatedBeans'])) {
            SugarRelationship::resaveRelatedBeans();
        }

        /* BEGIN - SECURITY GROUPS - inheritance */
        require_once('modules/SecurityGroups/SecurityGroup.php');
        SecurityGroup::inherit($this, $isUpdate);
        /* END - SECURITY GROUPS */
        //If we aren't in setup mode and we have a current user and module, then we track
        if (isset($GLOBALS['current_user']) && isset($this->module_dir)) {
            $this->track_view($current_user->id, $this->module_dir, 'save');
        }

        $this->call_custom_logic('after_save', '');

        $this->auditBean($isUpdate);

        //Now that the record has been saved, we don't want to insert again on further saves
        $this->new_with_id = false;
        $this->in_save = false;
        return $this->id;
    }

    /**
     * Saves only the listed fields. Does not create record, existing records only.
     * @param array $fieldToSave
     * @return void
     */
    public function saveFields(array $fieldToSave): void
    {
        global $current_user, $action, $timedate;

        if (empty($this->id) || $this->new_with_id || empty($fieldToSave)) {
            return;
        }

        $this->in_save = true;

        // cn: SECURITY - strip XSS potential vectors
        $this->cleanBean();

        // This is used so custom/3rd-party code can be upgraded with fewer issues,
        // this will be removed in a future release
        $this->fixUpFormatting();

        $isUpdate = true;

        $this->bean_fields_to_save = $fieldToSave;

        if (empty($this->date_modified) || $this->update_date_modified) {
            $this->date_modified = $timedate->nowDb();
            $this->bean_fields_to_save[] = 'date_modified';
        }

        $this->_checkOptimisticLocking($action, $isUpdate);

        if (!empty($this->modified_by_name)) {
            $this->old_modified_by_name = $this->modified_by_name;
        }

        if ($this->update_modified_by) {
            $this->modified_user_id = 1;
            $this->bean_fields_to_save[] = 'modified_user_id';

            if (!empty($current_user)) {
                $this->modified_user_id = $current_user->id;

                $this->modified_by_name = $current_user->user_name;
                $this->bean_fields_to_save[] = 'modified_by_name';
            }
        }

        if ($this->deleted != 1) {
            $this->deleted = 0;
        }

        if (isset($this->custom_fields)) {
            $this->custom_fields->bean = $this;
            $this->custom_fields->save($isUpdate);
        }

        $this->db->update($this);

        $this->bean_fields_to_save = null;
        $this->in_save = false;
    }

    /**
     * Cleans char, varchar, text, etc. fields of XSS type materials
     */
    public function cleanBean()
    {
        if (!is_array($this->field_defs) && !is_object($this->field_defs)) {
            $GLOBALS['log']->fatal('SugarBean::$filed_defs should be an array');
        } else {
            foreach ((array)$this->field_defs as $key => $def) {
                $type = '';
                if (isset($def['type'])) {
                    $type = $def['type'];
                }
                if (isset($def['dbType'])) {
                    $type .= $def['dbType'];
                }

                // Trim name & varchar type values on save when the value is not null
                if (isset($def['type']) && in_array($def['type'], ['name', 'varchar']) && !is_null($this->$key)) {
                    $this->$key = trim($this->$key);
                }

                if (isset($def['type']) && ($def['type'] == 'html' || $def['type'] == 'longhtml')) {
                    $this->$key = purify_html($this->$key, ['HTML.ForbiddenElements' => ['iframe' => true]]);
                } elseif (
                    (strpos((string) $type, 'char') !== false || strpos((string) $type, 'text') !== false || $type == 'enum') &&
                    !empty($this->$key)
                ) {
                    $this->$key = purify_html($this->$key, ['HTML.ForbiddenElements' => ['iframe' => true]]);
                }
            }
        }
    }

    /**
     * Function corrects any bad formatting done by 3rd party/custom code
     *
     * This function will be removed in a future release, it is only here to assist upgrading existing code
     * that expects formatted data in the bean
     */
    public function fixUpFormatting()
    {
        global $timedate;
        static $bool_false_values = array('off', 'false', '0', 'no');

        if (!is_array($this->field_defs) && !is_object($this->field_defs)) {
            $GLOBALS['log']->fatal('SugarBean::fixUpFormatting $field_defs should be an array');
        } else {
            foreach ((array)$this->field_defs as $field => $def) {
                if (!isset($this->$field)) {
                    continue;
                }
                if ((isset($def['source']) && $def['source'] == 'non-db') || $field == 'deleted') {
                    continue;
                }
                if (isset($this->fetched_row[$field]) && $this->$field == $this->fetched_row[$field]) {
                    // Don't hand out warnings because the field was untouched between retrieval and saving,
                    // most database drivers hand pretty much everything back as strings.
                    continue;
                }
                $reformatted = false;
                if (isset($def['type']) && $def['type']) {
                    switch ($def['type']) {
                        case 'datetime':
                        case 'datetimecombo':
                            if (empty($this->$field) || $this->$field == 'NULL') {
                                $this->$field = '';
                                break;
                            }
                            if (!preg_match('/^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$/',
                                (string) $this->$field)) {
                                $this->$field = $timedate->to_db($this->$field);
                                $reformatted = true;
                            }
                            break;
                        case 'date':
                            if (empty($this->$field) || $this->$field == 'NULL') {
                                $this->$field = '';
                                break;
                            }
                            if (!preg_match('/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/', (string) $this->$field)) {
                                $this->$field = $timedate->to_db_date($this->$field, false);
                                $reformatted = true;
                            }
                            break;
                        case 'time':
                            if (empty($this->$field) || $this->$field == 'NULL') {
                                $this->$field = '';
                                break;
                            }
                            if (preg_match('/(am|pm)/i', (string) $this->$field)) {
                                $fromUserTime = $timedate->fromUserTime($this->$field);
                                if (is_object($fromUserTime) && method_exists($fromUserTime, 'format')) {
                                    $this->$field = $fromUserTime->format(TimeDate::DB_TIME_FORMAT);
                                    $reformatted = true;
                                } else {
                                    $GLOBALS['log']->fatal('Get DateTime from user time string is failed.');
                                }
                            }
                            break;
                        case 'double':
                        case 'decimal':
                        case 'currency':
                        case 'float':
                            if ($this->$field === '' || $this->$field == null || $this->$field == 'NULL') {
                                break;
                            }
                            if (is_string($this->$field)) {
                                $this->$field = (float)unformat_number($this->$field);
                                $reformatted = true;
                            }
                            break;
                        case 'uint':
                        case 'ulong':
                        case 'long':
                        case 'short':
                        case 'tinyint':
                        case 'int':
                            if ($this->$field === '' || $this->$field == null || $this->$field == 'NULL') {
                                break;
                            }
                            if (is_string($this->$field)) {
                                $this->$field = (int)unformat_number($this->$field);
                                $reformatted = true;
                            }
                            break;
                        case 'bool':
                            if (empty($this->$field) || in_array((string)$this->$field, $bool_false_values)) {
                                $this->$field = false;
                            } elseif (true === $this->$field || 1 == $this->$field) {
                                $this->$field = true;
                            } else {
                                $this->$field = true;
                                $reformatted = true;
                            }
                            break;
                        case 'encrypt':
                            $this->$field = $this->encrpyt_before_save($this->$field);
                            break;
                        default:
                            //do nothing
                    }


                    if ($def['type'] !== 'encrypt' && isTrue($def['db_encrypted'] ?? false)) {
                        $this->$field = $this->encrpyt_before_save($this->$field);
                    }
                }
                if ($reformatted) {
                    $GLOBALS['log']->info('Formatting correction: ' . $this->module_dir . '->' . $field .
                        ' had formatting automatically corrected.');
                }
            }
        }
    }

    /**
     * Encrypt and base64 encode an 'encrypt' field type in the bean using Blowfish.
     * The default system key is stored in cache/Blowfish/{keytype}
     * @param string $value -plain text value of the bean field.
     * @return string
     */
    public function encrpyt_before_save($value)
    {
        require_once("include/utils/encryption_utils.php");
        return blowfishEncode($this->getEncryptKey(), $value);
    }

    /**
     * @return string
     */
    protected function getEncryptKey()
    {
        if (empty(static::$field_key)) {
            static::$field_key = blowfishGetKey('encrypt_field');
        }
        return static::$field_key;
    }

    /**
     * Moved from save() method, functionality is the same, but this is intended to handle
     * Optimistic locking functionality.
     *
     * @param string $action
     * @param bool $isUpdate
     *
     */
    private function _checkOptimisticLocking($action, $isUpdate)
    {
        if ($this->optimistic_lock && !isset($_SESSION['o_lock_fs'])) {
            if (isset($_SESSION['o_lock_id']) && $_SESSION['o_lock_id'] == $this->id
                && $_SESSION['o_lock_on'] == $this->object_name) {
                if ($action == 'Save' && $isUpdate && isset($this->modified_user_id)
                    && $this->has_been_modified_since($_SESSION['o_lock_dm'], $this->modified_user_id)) {
                    $_SESSION['o_lock_class'] = get_class($this);
                    $_SESSION['o_lock_module'] = $this->module_dir;
                    $_SESSION['o_lock_object'] = $this->toArray();
                    $saveform = "<form name='save' id='save' method='POST'>";
                    foreach ($_POST as $key => $arg) {
                        $saveform .= "<input type='hidden' name='" . addslashes($key)
                            . "' value='" . addslashes($arg) . "'>";
                    }
                    $saveform .= "</form><script>document.getElementById('save').submit();</script>";
                    $_SESSION['o_lock_save'] = $saveform;
                    header('Location: index.php?module=OptimisticLock&action=LockResolve');
                    die();
                }
                unset($_SESSION['o_lock_object']);
                unset($_SESSION['o_lock_id']);
                unset($_SESSION['o_lock_dm']);
            }
        } else {
            if (isset($_SESSION['o_lock_object'])) {
                unset($_SESSION['o_lock_object']);
            }
            if (isset($_SESSION['o_lock_id'])) {
                unset($_SESSION['o_lock_id']);
            }
            if (isset($_SESSION['o_lock_dm'])) {
                unset($_SESSION['o_lock_dm']);
            }
            if (isset($_SESSION['o_lock_fs'])) {
                unset($_SESSION['o_lock_fs']);
            }
            if (isset($_SESSION['o_lock_save'])) {
                unset($_SESSION['o_lock_save']);
            }
        }
    }

    /**
     * Performs a check if the record has been modified since the specified date
     *
     * @param Datetime $date Datetime for verification
     * @param string $modified_user_id User modified by
     * @return bool
     */
    public function has_been_modified_since($date, $modified_user_id)
    {
        global $current_user;
        $date = $this->db->convert($this->db->quoted($date), 'datetime');
        if (isset($current_user)) {
            $query = "SELECT date_modified FROM $this->table_name WHERE id='$this->id' AND modified_user_id != " .
                "'$current_user->id' AND (modified_user_id != '$modified_user_id' OR date_modified > $date)";
            $result = $this->db->query($query);

            if ($this->db->fetchByAssoc($result)) {
                return true;
            }
        }
        return false;
    }

    /**
     * returns this bean as an array
     *
     * @param bool $dbOnly
     * @param bool $stringOnly
     * @param bool $upperKeys
     * @return array of fields with id, name, access and category
     */
    public function toArray($dbOnly = false, $stringOnly = false, $upperKeys = false)
    {
        static $cache = array();
        $arr = array();

        foreach ($this->field_defs as $field => $data) {
            if (!$dbOnly || !isset($data['source']) || $data['source'] == 'db') {
                if (!$stringOnly || is_string($this->$field)) {
                    if ($upperKeys) {
                        if (!isset($cache[$field])) {
                            $cache[$field] = strtoupper($field);
                        }
                        $arr[$cache[$field]] = $this->$field;
                    } else {
                        if (isset($this->$field)) {
                            $arr[$field] = $this->$field;
                        } else {
                            $arr[$field] = '';
                        }
                    }
                }
            }
        }
        return $arr;
    }

    /**
     * This function is a good location to save changes that have been made to a relationship.
     * This should be overridden in subclasses that have something to save.
     *
     * @param bool $is_update true if this save is an update.
     * @param array $exclude a way to exclude relationships
     */
    public function save_relationship_changes($is_update, $exclude = array())
    {
        list($new_rel_id, $new_rel_link) = $this->set_relationship_info($exclude);

        $new_rel_id = $this->handle_preset_relationships($new_rel_id, $new_rel_link, $exclude);

        $this->handle_remaining_relate_fields($exclude);

        $this->update_parent_relationships($exclude);

        $this->handle_request_relate($new_rel_id, $new_rel_link);
    }

    /**
     * Look in the bean for the new relationship_id and relationship_name if $this->not_use_rel_in_req is set to true,
     * otherwise check the $_REQUEST param for a relate_id and relate_to field.  Once we have that make sure that it's
     * not excluded from the passed in array of relationships to exclude
     *
     * @param array $exclude any relationship's to exclude
     * @return array                The relationship_id and relationship_name in an array
     */
    protected function set_relationship_info($exclude = array())
    {
        $new_rel_id = false;
        $new_rel_link = false;
        // check incoming data
        if (isset($this->not_use_rel_in_req) && $this->not_use_rel_in_req) {
            // if we should use relation data from properties (for REQUEST-independent calls)
            $rel_id = isset($this->new_rel_id) ? $this->new_rel_id : '';
            $rel_link = isset($this->new_rel_relname) ? $this->new_rel_relname : '';
        } else {
            // if we should use relation data from REQUEST
            $rel_id = isset($_REQUEST['relate_id']) ? $_REQUEST['relate_id'] : '';
            $rel_link = isset($_REQUEST['relate_to']) ? $_REQUEST['relate_to'] : '';
        }

        // filter relation data
        if ($rel_id && $rel_link && !in_array($rel_link, $exclude) && $rel_id != $this->id) {
            $new_rel_id = $rel_id;
            $new_rel_link = $rel_link;
            // Bug #53223 : wrong relationship from subpanel create button
            // if LHSModule and RHSModule are same module use left link to add new item b/s of:
            // $rel_id and $rel_link are not empty - request is from subpanel
            // $rel_link contains relationship name - checked by call load_relationship
            $isRelationshipLoaded = $this->load_relationship($rel_link);
            if ($isRelationshipLoaded && !empty($this->$rel_link) && $this->$rel_link->getRelationshipObject()
                && $this->$rel_link->getRelationshipObject()->getLHSModule()
                == $this->$rel_link->getRelationshipObject()->getRHSModule()) {
                $new_rel_link = $this->$rel_link->getRelationshipObject()->getLHSLink();
            } else {
                //Try to find the link in this bean based on the relationship
                foreach ($this->field_defs as $key => $def) {
                    if (isset($def['type']) && $def['type'] == 'link' && isset($def['relationship'])
                        && $def['relationship'] == $rel_link) {
                        $new_rel_link = $key;
                    }
                }
            }
        }

        return array($new_rel_id, $new_rel_link);
    }

    /**
     * Handle the preset fields listed in the fixed relationship_fields array hardcoded into the OOB beans
     *
     * TODO: remove this mechanism and replace with mechanism exclusively based on the vardefs
     *
     * @api
     * @see save_relationship_changes
     * @param string|bool $new_rel_id String of the ID to add
     * @param string                        Relationship Name
     * @param array $exclude any relationship's to exclude
     * @return string|bool               Return the new_rel_id if it was not used.  False if it was used.
     */
    protected function handle_preset_relationships($new_rel_id, $new_rel_link, $exclude = array())
    {
        if (isset($this->relationship_fields) && is_array($this->relationship_fields)) {
            foreach ($this->relationship_fields as $id => $rel_name) {
                if (in_array($id, $exclude)) {
                    continue;
                }

                if (!empty($this->$id)) {
                    // Bug #44930 We do not need to update main related field if it is changed from sub-panel.
                    if ($rel_name == $new_rel_link && $this->$id != $new_rel_id) {
                        $new_rel_id = '';
                    }
                    $GLOBALS['log']->debug('save_relationship_changes(): From relationship_field array - ' .
                        'adding a relationship record: ' . $rel_name . ' = ' . $this->$id);
                    //already related the new relationship id so let's set it to false so we don't add it again
                    // using the _REQUEST['relate_i'] mechanism in a later block
                    $this->load_relationship($rel_name);
                    $rel_add = $this->$rel_name->add($this->$id);
                    // move this around to only take out the id if it was save successfully
                    if ($this->$id == $new_rel_id && $rel_add) {
                        $new_rel_id = false;
                    }
                } else {
                    //if before value is not empty then attempt to delete relationship
                    if (!empty($this->rel_fields_before_value[$id])) {
                        $GLOBALS['log']->debug('save_relationship_changes(): From relationship_field array - ' .
                            'attempting to remove the relationship record, using relationship attribute' . $rel_name);
                        $this->load_relationship($rel_name);
                        $this->$rel_name->delete($this->id, $this->rel_fields_before_value[$id]);
                    }
                }
            }
        }

        return $new_rel_id;
    }

    /**
     * Next, we'll attempt to update all of the remaining relate fields in the vardefs that have 'save' set in their
     * field_def
     * Only the 'save' fields should be saved as some vardef entries today are not for display only purposes
     * and break the application if saved
     * If the vardef has entries for field <a> of type relate, where a->id_name = <b> and field <b> of type link
     * then we receive a value for b from the MVC in the _REQUEST, and it should be set in the bean as $this->$b
     *
     * @api
     * @see save_relationship_changes
     * @param array $exclude any relationship's to exclude
     * @return array the list of relationships that were added or removed successfully or if they were a failure
     */
    protected function handle_remaining_relate_fields($exclude = array())
    {
        $modified_relationships = array(
            'add' => array('success' => array(), 'failure' => array()),
            'remove' => array('success' => array(), 'failure' => array()),
        );
        if (!is_array($this->field_defs) && !is_object($this->field_defs)) {
            $GLOBALS['log']->fatal('SugarBean::handle_remaining_relate_fields $field_defs should be an array');
        } else {
            foreach ((array)$this->field_defs as $def) {
                if ((isset($def['type']) && $def ['type'] == 'relate') && isset($def ['id_name'])
                    && isset($def ['link']) && isset($def['save'])) {
                    if (in_array($def['id_name'], $exclude) || in_array($def['id_name'], $this->relationship_fields)) {
                        // continue to honor the exclude array and exclude any relationships that will be handled
                        // by the relationship_fields mechanism
                        continue;
                    }

                    $linkField = $def['link'];
                    if (isset($this->field_defs[$linkField])) {
                        if ($this->load_relationship($linkField)) {
                            $idName = $def['id_name'];

                            if (!empty($this->rel_fields_before_value[$idName]) && empty($this->$idName)) {
                                //if before value is not empty then attempt to delete relationship
                                $GLOBALS['log']->debug("save_relationship_changes(): From field_defs - attempting to " .
                                    "remove the relationship record: {$linkField} = " .
                                    $this->rel_fields_before_value[$idName]);
                                $success = $this->$linkField->delete(
                                    $this->id,
                                    $this->rel_fields_before_value[$idName]
                                );
                                // just need to make sure it's true and not an array as it's possible to return an array
                                if ($success) {
                                    $modified_relationships['remove']['success'][] = $linkField;
                                } else {
                                    $modified_relationships['remove']['failure'][] = $linkField;
                                }
                                $GLOBALS['log']->debug("save_relationship_changes(): From field_defs - attempting to " .
                                    "remove the relationship record returned " . var_export($success, true));
                            }

                            if (!empty($this->$idName) && is_string($this->$idName)) {
                                $GLOBALS['log']->debug("save_relationship_changes(): From field_defs - attempting to " .
                                    "add a relationship record - {$linkField} = {$this->$idName}");

                                $success = $this->$linkField->add($this->$idName);

                                // just need to make sure it's true and not an array as it's possible to return an array
                                if ($success) {
                                    $modified_relationships['add']['success'][] = $linkField;
                                } else {
                                    $modified_relationships['add']['failure'][] = $linkField;
                                }

                                $GLOBALS['log']->debug("save_relationship_changes(): From field_defs - add a " .
                                    "relationship record returned " . var_export($success, true));
                            }
                        } else {
                            $logFunction = 'fatal';
                            if (isset($this->field_defs[$linkField]['source'])
                                && $this->field_defs[$linkField]['source'] === 'non-db') {
                                $logFunction = 'warn';
                            }
                            $GLOBALS['log']->$logFunction("Failed to load relationship {$linkField} while " .
                                "saving {$this->module_dir}");
                        }
                    }
                }
            }
        }

        return $modified_relationships;
    }

    /**
     * Updates relationships based on changes to fields of type 'parent' which
     * may or may not have links associated with them
     *
     * @param array $exclude
     */
    protected function update_parent_relationships($exclude = array())
    {
        if (!is_array($this->field_defs) && !is_object($this->field_defs)) {
            $GLOBALS['log']->fatal('SugarBean::update_parent_relationships $field_defs should be an array');
        } else {
            foreach ($this->field_defs as $def) {
                if (!empty($def['type']) && $def['type'] == "parent") {
                    if (empty($def['type_name']) || empty($def['id_name'])) {
                        continue;
                    }
                    $typeField = $def['type_name'];
                    $idField = $def['id_name'];
                    if (in_array($idField, $exclude)) {
                        continue;
                    }
                    //Determine if the parent field has changed.
                    if (
                        //First check if the fetched row parent existed and now we no longer have one
                        (
                            !empty($this->fetched_row[$typeField]) && !empty($this->fetched_row[$idField])
                            && (empty($this->$typeField) || empty($this->$idField))
                        ) ||
                        //Next check if we have one now that doesn't match the fetch row
                        (
                            !empty($this->$typeField) && !empty($this->$idField) &&
                            (empty($this->fetched_row[$typeField]) || empty($this->fetched_row[$idField])
                                || $this->fetched_row[$idField] != $this->$idField)
                        ) ||
                        // Check if we are deleting the bean, should remove the bean from any relationships
                        $this->deleted == 1
                    ) {
                        $parentLinks = array();
                        //Correlate links to parent field module types
                        foreach ($this->field_defs as $ldef) {
                            if (!empty($ldef['type']) && $ldef['type'] == "link" && !empty($ldef['relationship'])) {
                                $relDef = SugarRelationshipFactory::getInstance()->getRelationshipDef(
                                    $ldef['relationship']
                                );
                                if (!empty($relDef['relationship_role_column'])
                                    && $relDef['relationship_role_column'] == $typeField) {
                                    $parentLinks[$relDef['lhs_module']] = $ldef;
                                }
                            }
                        }

                        // Save $this->$idField, because it can be reset in case of link->delete() call
                        $idFieldVal = $this->$idField;

                        //If we used to have a parent, call remove on that relationship
                        if (!empty($this->fetched_row[$typeField]) && !empty($this->fetched_row[$idField])
                            && !empty($parentLinks[$this->fetched_row[$typeField]])
                            && ($this->fetched_row[$idField] != $this->$idField)
                        ) {
                            $oldParentLink = $parentLinks[$this->fetched_row[$typeField]]['name'];
                            //Load the relationship
                            if ($this->load_relationship($oldParentLink)) {
                                $this->$oldParentLink->delete($this->fetched_row[$idField]);
                                // Should re-save the old parent
                                SugarRelationship::addToResaveList(BeanFactory::getBean(
                                    $this->fetched_row[$typeField],
                                    $this->fetched_row[$idField]
                                ));
                            }
                        }

                        // If both parent type and parent id are set, save it unless the bean is being deleted
                        if (!empty($this->$typeField)
                            && !empty($idFieldVal)
                            && !empty($parentLinks[$this->$typeField]['name'])
                            && $this->deleted != 1
                        ) {
                            //Now add the new parent
                            $parentLink = $parentLinks[$this->$typeField]['name'];
                            if ($this->load_relationship($parentLink)) {
                                $this->$parentLink->add($idFieldVal);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Finally, we update a field listed in the _REQUEST['%/relate_id']/_REQUEST['relate_to'] mechanism
     * (if it has not already been updated)
     *
     * @api
     * @see save_relationship_changes
     * @param string|bool $new_rel_id
     * @param string $new_rel_link
     * @return bool
     */
    protected function handle_request_relate($new_rel_id, $new_rel_link)
    {
        if (!empty($new_rel_id)) {
            if ($this->load_relationship($new_rel_link)) {
                return $this->$new_rel_link->add($new_rel_id);
            }
            $lower_link = strtolower($new_rel_link);
            if ($this->load_relationship($lower_link)) {
                return $this->$lower_link->add($new_rel_id);
            }
            require_once('data/Link2.php');
            $rel = Relationship::retrieve_by_modules(
                $new_rel_link,
                $this->module_dir,
                $this->db,
                'many-to-many'
            );

            if (!empty($rel)) {
                foreach ($this->field_defs as $field => $def) {
                    if ($def['type'] == 'link' && !empty($def['relationship'])
                        && $def['relationship'] == $rel) {
                        $this->load_relationship($field);
                        return $this->$field->add($new_rel_id);
                    }
                }
                //ok so we didn't find it in the field defs let's save it anyway if we have the relationship

                $this->$rel = new Link2($rel, $this, array());
                return $this->$rel->add($new_rel_id);
            }
        }

        // nothing was saved
        return false;
    }

    /**
     * Trigger custom logic for this module that is defined for the provided hook
     * The custom logic file is located under custom/modules/[CURRENT_MODULE]/logic_hooks.php.
     * That file should define the $hook_version that should be used.
     * It should also define the $hook_array.  The $hook_array will be a two dimensional array
     * the first dimension is the name of the event, the second dimension is the information needed
     * to fire the hook.  Each entry in the top level array should be defined on a single line to make it
     * easier to automatically replace this file.  There should be no contents of this file that are not replaceable.
     *
     * $hook_array['before_save'][] = Array(1, 'test type', 'custom/modules/Leads/test12.php', 'TestClass',
     * 'lead_before_save_1');
     * This sample line creates a before_save hook.  The hooks are processed in the order in which they
     * are added to the array.  The second dimension is an array of:
     *        processing index (for sorting before exporting the array)
     *        A logic type hook
     *        label/type
     *        php file to include
     *        php class the method is in
     *        php method to call
     *
     * The method signature for version 1 hooks is:
     * function NAME(&$bean, $event, $arguments)
     *        $bean - $this bean passed in by reference.
     *        $event - The string for the current event (i.e. before_save)
     *        $arguments - An object or array of arguments that are specific to the event.
     *
     * @param string $event
     * @param object|array $arguments
     */
    public function call_custom_logic($event, $arguments = null)
    {
        if (!isset($this->processed) || !$this->processed) {
            //add some logic to ensure we do not get into an infinite loop
            if (!empty($this->logicHookDepth[$event])) {
                if ($this->logicHookDepth[$event] > $this->max_logic_depth) {
                    return;
                }
            } else {
                $this->logicHookDepth[$event] = 0;
            }

            //we have to put the increment operator here
            //otherwise we may never increase the depth for that event in the case
            //where one event will trigger another as in the case of before_save and after_save
            //Also keeping the depth per event allow any number of hooks to be called on the bean
            //and we only will return if one event gets caught in a loop. We do not increment globally
            //for each event called.
            $this->logicHookDepth[$event]++;

            //method defined in 'include/utils/LogicHook.php'

            $logicHook = new LogicHook();
            $logicHook->setBean($this);
            $logicHook->call_custom_logic($this->module_dir, $event, $arguments);
            $this->logicHookDepth[$event]--;
        }
    }

    /**
     * Checks if Bean has email defs
     *
     * @return bool
     */
    public function hasEmails()
    {
        return (!empty($this->field_defs['email_addresses'])
            && $this->field_defs['email_addresses']['type'] == 'link'
            && !empty($this->field_defs['email_addresses_non_primary'])
            && $this->field_defs['email_addresses_non_primary']['type'] == 'email'
        );
    }

    /**
     * This function processes the fields before save.
     * Internal function, do not override.
     */
    public function preprocess_fields_on_save()
    {
        $GLOBALS['log']->deprecated('SugarBean.php: preprocess_fields_on_save() is deprecated');
    }

    /**
     * Send assignment notifications and invites for meetings and calls
     *
     * @param bool $check_notify
     */
    private function _sendNotifications($check_notify)
    {
        if ($check_notify || (isset($this->notify_inworkflow) && $this->notify_inworkflow)
            && !$this->isOwner($this->created_by)
        ) {
            // cn: bug 42727 no need to send email to owner (within workflow)

            $admin = BeanFactory::newBean('Administration');
            $admin->retrieveSettings();
            $sendNotifications = false;

            if ($admin->settings['notify_on']) {
                $GLOBALS['log']->info("Notifications: user assignment has changed, " .
                    "checking if user receives notifications");
                $sendNotifications = true;
            } elseif (isset($_REQUEST['send_invites']) && $_REQUEST['send_invites'] == 1) {
                // cn: bug 5795 Send Invites failing for Contacts
                $sendNotifications = true;
            } else {
                $GLOBALS['log']->info("Notifications: not sending e-mail, notify_on is set to OFF");
            }


            if ($sendNotifications) {
                $notify_list = $this->get_notification_recipients();
                foreach ($notify_list as $notify_user) {
                    $this->send_assignment_notifications($notify_user, $admin);
                }
            }
        }
    }

    /**
     * Determines which users receive a notification
     *
     * @return User[]
     */
    public function get_notification_recipients()
    {
        $notify_user = BeanFactory::newBean('Users');
        $notify_user->retrieve($this->assigned_user_id);
        $this->new_assigned_user_name = $notify_user->full_name;

        $GLOBALS['log']->info("Notifications: recipient is $this->new_assigned_user_name");

        return array($notify_user);
    }

    /**
     * Handles sending out email notifications when items are first assigned to users
     *
     * @param User $notify_user user to notify
     * @param Administration $admin the admin user that sends out the notification
     */
    public function send_assignment_notifications($notify_user, $admin)
    {
        global $current_user;

        if ((($this->object_name == 'Meeting' || $this->object_name == 'Call') || $notify_user->receive_notifications) && !in_array($notify_user->id, $this->sentAssignmentNotifications, true)) {
            $sendToEmail = $notify_user->emailAddress->getPrimaryAddress($notify_user);
            $sendEmail = true;
            if (empty($sendToEmail)) {
                $GLOBALS['log']->warn("Notifications: no e-mail address set for user " .
                    "{$notify_user->user_name}, cancelling send");
                $sendEmail = false;
            }

            $notify_mail = $this->create_notification_email($notify_user);
            $notify_mail->setMailerForSystem();

            if (empty($admin->settings['notify_send_from_assigning_user'])) {
                if (!isset($admin->settings['notify_fromaddress'])) {
                    LoggerManager::getLogger()->warn('admin settings / notify from address is not set');
                    $adminSettingsNotifyFromAddress = null;
                } else {
                    $adminSettingsNotifyFromAddress = $admin->settings['notify_fromaddress'];
                }

                $notify_mail->From = $adminSettingsNotifyFromAddress;
                isValidEmailAddress($notify_mail->From);
                $notify_mail->FromName = (empty($admin->settings['notify_fromname']))
                    ? "" : $admin->settings['notify_fromname'];
            } else {
                // Send notifications from the current user's e-mail (if set)
                $fromAddress = $current_user->emailAddress->getReplyToAddress($current_user);
                $fromAddress = !empty($fromAddress) ? $fromAddress : $admin->settings['notify_fromaddress'];
                $notify_mail->From = $fromAddress;
                isValidEmailAddress($notify_mail->From);
                //Use the users full name is available otherwise default to system name
                $from_name = !empty($admin->settings['notify_fromname']) ? $admin->settings['notify_fromname'] : "";
                $from_name = !empty($current_user->full_name) ? $current_user->full_name : $from_name;
                $notify_mail->FromName = $from_name;
            }

            $oe = new OutboundEmail();
            $oe = $oe->getUserMailerSettings($current_user);
            //only send if smtp server is defined
            if ($sendEmail) {
                $smtpVerified = false;

                //first check the user settings
                if (!empty($oe->mail_smtpserver)) {
                    $smtpVerified = true;
                }

                //if still not verified, check against the system settings
                if (!$smtpVerified) {
                    $oe = $oe->getSystemMailerSettings();
                    if (!empty($oe->mail_smtpserver)) {
                        $smtpVerified = true;
                    }
                }
                //if smtp was not verified against user or system, then do not send out email
                if (!$smtpVerified) {
                    $GLOBALS['log']->fatal("Notifications: error sending e-mail, smtp server was not found ");
                    //break out
                    return;
                }

                if (!$notify_mail->send()) {
                    $GLOBALS['log']->fatal("Notifications: error sending e-mail (method: {$notify_mail->Mailer}), " .
                        "(error: {$notify_mail->ErrorInfo})");
                } else {
                    $this->sentAssignmentNotifications[] = $notify_user->id;
                    $GLOBALS['log']->info("Notifications: e-mail successfully sent");
                }
            }
        }
    }

    /**
     * This function handles create the email notifications email.
     * @param string $notify_user the user to send the notification email to
     * @return SugarPHPMailer
     */
    public function create_notification_email($notify_user)
    {
        global $sugar_version;
        global $sugar_config;
        global $current_user;
        global $locale;
        global $beanList;
        $OBCharset = $locale->getPrecedentPreference('default_email_charset');


        require_once("include/SugarPHPMailer.php");

        $notify_address = $notify_user->emailAddress->getPrimaryAddress($notify_user);
        $notify_name = $notify_user->full_name;
        $GLOBALS['log']->debug("Notifications: user has e-mail defined");

        $notify_mail = new SugarPHPMailer();
        $notify_mail->addAddress(
            $notify_address,
            $locale->translateCharsetMIME(trim($notify_name), 'UTF-8', $OBCharset)
        );


        $current_language = get_current_language();
        $xtpl = new XTemplate(get_notify_template_file($current_language));
        if ($this->module_dir == "Cases") {
            //we should use Case, you can refer to the en_us.notify_template.html.
            $template_name = "Case";
        } else {
            $template_name = $beanList[$this->module_dir];
        }

        $this->current_notify_user = $notify_user;

        if (in_array('set_notification_body', get_class_methods($this))) {
            $xtpl = $this->set_notification_body($xtpl, $this);
        } else {
            $xtpl->assign("OBJECT", translate('LBL_MODULE_NAME', $this->module_name));
            $template_name = "Default";
        }
        if (!empty($_SESSION["special_notification"]) && $_SESSION["special_notification"]) {
            $template_name = $beanList[$this->module_dir] . 'Special';
        }
        if ($this->special_notification) {
            $template_name = $beanList[$this->module_dir] . 'Special';
        }
        $xtpl->assign("ASSIGNED_USER", $this->new_assigned_user_name);
        $xtpl->assign("ASSIGNER", $current_user->name);

        $parsedSiteUrl = parse_url((string) $sugar_config['site_url']);
        $host = $parsedSiteUrl['host'];
        if (!isset($parsedSiteUrl['port'])) {
            $parsedSiteUrl['port'] = 80;
        }

        $port = ($parsedSiteUrl['port'] != 80) ? ":" . $parsedSiteUrl['port'] : '';
        $path = !empty($parsedSiteUrl['path']) ? $parsedSiteUrl['path'] : "";
        $cleanUrl = "{$parsedSiteUrl['scheme']}://{$host}{$port}{$path}";

        $xtpl->assign("URL", $cleanUrl . "/index.php?module={$this->module_dir}&action=DetailView&record={$this->id}");
        $xtpl->assign("SUGAR", "Sugar v{$sugar_version}");
        $xtpl->parse($template_name);
        $xtpl->parse($template_name . "_Subject");

        // NOTE: Crowdin translation system requires some HTML tags in the template, namely <p> and <br>.
        // These will go into the HTML version of the email, but not into the text version, nor the subject line.

        $tempBody = from_html(trim($xtpl->text($template_name)));
        $notify_mail->msgHTML($tempBody);

        // Improve the text version of the email with some "reverse linkification",
        // making "<a href=link>text</a>" links readable as "text [link]"
        $tempBody = preg_replace('/<a href=([\"\']?)(.*?)\1>(.*?)<\/a>/', "\\3 [\\2]", (string) $tempBody);

        // all the other HTML tags get removed from the text version:
        $notify_mail->AltBody = strip_tags($tempBody);

        // strip_tags is used because subject lines NEVER include HTML tags, according to official specification:
        $notify_mail->Subject = strip_tags(from_html($xtpl->text($template_name . "_Subject")));

        // cn: bug 8568 encode notify email in User's outbound email encoding
        $notify_mail->prepForOutbound();

        return $notify_mail;
    }

    /**
     * Tracks the viewing of a detail record.
     * This leverages get_summary_text() which is object specific.
     *
     * Internal function, do not override.
     * @param string $user_id - String value of the user that is viewing the record.
     * @param string $current_module - String value of the module being processed.
     * @param string $current_view - String value of the current view
     */
    public function track_view($user_id, $current_module, $current_view = '')
    {
        $trackerManager = TrackerManager::getInstance();
        if ($monitor = $trackerManager->getMonitor('tracker')) {
            $monitor->setValue('date_modified', $GLOBALS['timedate']->nowDb());
            $monitor->setValue('user_id', $user_id);
            $monitor->setValue('module_name', $current_module);
            $monitor->setValue('action', $current_view);
            $monitor->setValue('item_id', $this->id);
            $monitor->setValue('item_summary', $this->get_summary_text());
            $monitor->setValue('visible', $this->tracker_visibility);
            $trackerManager->saveMonitor($monitor);
        }
    }

    /**
     * Returns the summary text that should show up in the recent history list for this object.
     *
     * @return string
     */
    public function get_summary_text()
    {
        return "Base Implementation.  Should be overridden.";
    }

    /**
     * Add any required joins to the list count query.  The joins are required if there
     * is a field in the $where clause that needs to be joined.
     *
     * @param string $query
     * @param string $where
     *
     * Internal Function, do Not override.
     */
    public function add_list_count_joins(&$query, $where)
    {
        $custom_join = $this->getCustomJoin();
        $query .= $custom_join['join'];
    }

    /**
     * This function returns a paged list of the current object type.  It is intended to allow for
     * hopping back and forth through pages of data.  It only retrieves what is on the current page.
     *
     * @internal This method must be called on a new instance.  It trashes the values of all the fields
     * in the current one.
     * @param string $order_by
     * @param string $where Additional where clause
     * @param int $row_offset Optional,default 0, starting row number
     * @param int $limit Optional, default -1
     * @param int $max Optional, default -1
     * @param int $show_deleted Optional, default 0, if set to 1 system will show deleted records.
     * @param bool $singleSelect
     * @param array $select_fields
     * @return array Fetched data.
     *
     * Internal function, do not override.
     */
    public function get_list(
        $order_by = "",
        $where = "",
        $row_offset = 0,
        $limit = -1,
        $max = -1,
        $show_deleted = 0,
        $singleSelect = false,
        $select_fields = array()
    ) {
        $GLOBALS['log']->debug("get_list:  order_by = '$order_by' and where = '$where' and limit = '$limit'");
        if (isset($_SESSION['show_deleted'])) {
            $show_deleted = 1;
        }

        $query = $this->create_new_list_query(
            $order_by,
            $where,
            $select_fields,
            array(),
            $show_deleted,
            '',
            false,
            null,
            $singleSelect
        );
        return $this->process_list_query($query, $row_offset, $limit, $max, $where);
    }

    /**
     * Gets there where statement for checking if a user is an owner
     *
     * @param string $user_id GUID
     * @return string
     */
    public function getOwnerWhere($user_id)
    {
        if (isset($this->field_defs['assigned_user_id'])) {
            return " $this->table_name.assigned_user_id ='$user_id' ";
        }
        if (isset($this->field_defs['created_by'])) {
            return " $this->table_name.created_by ='$user_id' ";
        }
        return '';
    }


    /**
     * @param string $view
     * @param User $user
     * @return string
     */
    public function buildAccessWhere($view, $user = null)
    {
        global $current_user, $sugar_config;

        $conditions = [];
        $user = $user === null ? $current_user : $user;

        if ($this->bean_implements('ACL') && ACLController::requireOwner($this->module_dir, $view)) {
            $ownerWhere = $this->getOwnerWhere($user->id);
            if (!empty($ownerWhere)) {
                $conditions['owner'] = $ownerWhere;
            }
        }

        /* BEGIN - SECURITY GROUPS */
        $SecurityGroupFile = BeanFactory::getBeanFile('SecurityGroups');
        require_once $SecurityGroupFile;
        if ($view === 'list' && $this->module_dir === 'Users' && !is_admin($user)
            && isset($sugar_config['securitysuite_filter_user_list'])
            && $sugar_config['securitysuite_filter_user_list']
        ) {
            $groupWhere = SecurityGroup::getGroupUsersWhere($user->id);
            $conditions['group'] = $groupWhere;
        } elseif ($this->bean_implements('ACL') && ACLController::requireSecurityGroup($this->module_dir, $view)) {
            $ownerWhere = $this->getOwnerWhere($user->id);
            $groupWhere = SecurityGroup::getGroupWhere($this->table_name, $this->module_dir, $user->id);
            if (!empty($ownerWhere)) {
                $conditions['group'] = " (" . $ownerWhere . " or " . $groupWhere . ") ";
            } else {
                $conditions['group'] = $groupWhere;
            }
        }
        /* END - SECURITY GROUPS */

        $args = new stdClass();
        $args->view = $view;
        $args->user = $user;
        $args->conditions = $conditions;

        $this->call_custom_logic('before_acl_query', $args);

        return implode(' AND ', $args->conditions);
    }

    /**
     * Return the list query used by the list views and export button.
     * Next generation of create_new_list_query function.
     *
     * Override this function to return a custom query.
     *
     * @param string $order_by custom order by clause
     * @param string $where custom where clause
     * @param array $filter Optional
     * @param array $params Optional     *
     * @param int $show_deleted Optional, default 0, show deleted records is set to 1.
     * @param string $join_type
     * @param bool $return_array Optional, default false, response as array
     * @param object $parentbean creating a subquery for this bean.
     * @param bool $singleSelect Optional, default false.
     * @param bool $ifListForExport
     * @return String select query string, optionally an array value will be returned if $return_array= true.
     */
    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ) {
        $selectedFields = array();
        $secondarySelectedFields = array();
        $ret_array = array();
        $distinct = '';

        $accessWhere = $this->buildAccessWhere('list');
        if (!empty($accessWhere)) {
            $where .= empty($where) ? $accessWhere : ' AND ' . $accessWhere;
        }

        if (!empty($params['distinct'])) {
            $distinct = ' DISTINCT ';
        }
        if (empty($filter)) {
            $ret_array['select'] = " SELECT $distinct $this->table_name.* ";
        } else {
            $ret_array['select'] = " SELECT $distinct $this->table_name.id ";
        }
        $ret_array['from'] = " FROM $this->table_name ";
        $ret_array['from_min'] = $ret_array['from'];
        $ret_array['secondary_from'] = $ret_array['from'];
        $ret_array['where'] = '';
        $ret_array['order_by'] = '';
        //secondary selects are selects that need to be run after the primary query to retrieve additional info on main
        if ($singleSelect) {
            $ret_array['secondary_select'] =& $ret_array['select'];
            $ret_array['secondary_from'] = &$ret_array['from'];
        } else {
            $ret_array['secondary_select'] = '';
        }
        $custom_join = $this->getCustomJoin(empty($filter) ? true : $filter);
        if ((!isset($params['include_custom_fields']) || $params['include_custom_fields'])) {
            $ret_array['select'] .= $custom_join['select'];
        }
        $ret_array['from'] .= $custom_join['join'];
        // Bug 52490 - Captivea (Sve) - To be able to add custom fields inside where clause in a subpanel
        $ret_array['from_min'] .= $custom_join['join'];
        $jtcount = 0;
        //LOOP AROUND FOR FIXING VARDEF ISSUES
        require('include/VarDefHandler/listvardefoverride.php');
        if (file_exists('custom/include/VarDefHandler/listvardefoverride.php')) {
            require('custom/include/VarDefHandler/listvardefoverride.php');
        }

        $joined_tables = array();
        if (!empty($params['joined_tables'])) {
            foreach ($params['joined_tables'] as $table) {
                $joined_tables[$table] = 1;
            }
        }

        if (!empty($filter)) {
            $filterKeys = array_keys($filter);
            if (is_numeric($filterKeys[0])) {
                $fields = array();
                foreach ($filter as $field) {
                    $field = strtolower($field);
                    //remove out id field so we don't duplicate it
                    if ($field == 'id' && !empty($filter)) {
                        continue;
                    }
                    if (isset($this->field_defs[$field])) {
                        $fields[$field] = $this->field_defs[$field];
                    } else {
                        $fields[$field] = array('force_exists' => true);
                    }
                }
            } else {
                $fields = $filter;
            }
        } else {
            $fields = $this->field_defs;
        }

        $used_join_key = array();

        //walk through the fields and for every relationship field add their relationship_info field
        //relationshipfield-aliases are resolved in SugarBean::create_new_list_query
        // through their relationship_info field
        $addrelate = array();
        foreach ($fields as $field => $value) {
            if (isset($this->field_defs[$field]) && isset($this->field_defs[$field]['source']) &&
                $this->field_defs[$field]['source'] == 'non-db'
            ) {
                $addrelatefield = $this->get_relationship_field($field);
                if ($addrelatefield) {
                    $addrelate[$addrelatefield] = true;
                }
            }
            if (!empty($this->field_defs[$field]['id_name'])) {
                $addrelate[$this->field_defs[$field]['id_name']] = true;
            }
        }

        $fields = array_merge($addrelate, $fields);

        foreach ($fields as $field => $value) {
            //alias is used to alias field names
            $alias = '';
            if (isset($value['alias'])) {
                $alias = ' as ' . $value['alias'] . ' ';
            }

            if (empty($this->field_defs[$field]) || !empty($value['force_blank'])) {
                if (!empty($filter) && isset($filter[$field]['force_exists']) && $filter[$field]['force_exists']) {
                    if (isset($filter[$field]['force_default'])) {
                        $ret_array['select'] .= ", {$filter[$field]['force_default']} $field ";
                    } else {
                        //  spaces are a fix for length issue problem with unions.
                        //  The union only returns the maximum number of characters from the first select statement.
                        $ret_array['select'] .= ", '                                                                 " .
                            "                                                                                        " .
                            "                                                                                        " .
                            "             ' $field ";
                    }
                }
                continue;
            }
            $data = $this->field_defs[$field];


            //ignore fields that are a part of the collection and a field has been removed as a result of
            //layout customization.. this happens in subpanel customizations, use case, from the contacts subpanel
            //in opportunities module remove the contact_role/opportunity_role field.
            if (isset($data['relationship_fields']) && !empty($data['relationship_fields'])) {
                $process_field = false;
                foreach ($data['relationship_fields'] as $field_name) {
                    if (isset($fields[$field_name])) {
                        $process_field = true;
                        break;
                    }
                }

                if (!$process_field) {
                    continue;
                }
            }

            if ((!isset($data['source']) || $data['source'] == 'db') && (!empty($alias) || !empty($filter))) {
                $ret_array['select'] .= ", $this->table_name.$field $alias";
                $selectedFields["$this->table_name.$field"] = true;
            } elseif ((!isset($data['source']) || $data['source'] == 'custom_fields')
                && (!empty($alias) || !empty($filter))) {
                //add this column only if it has NOT already been added to select statement string
                $colPos = strpos($ret_array['select'], $this->table_name . "_cstm" . ".$field");
                if (!$colPos || $colPos < 0) {
                    $ret_array['select'] .= ", $this->table_name" . "_cstm" . ".$field $alias";
                }

                $selectedFields["$this->table_name.$field"] = true;
            }

            if ($data['type'] != 'relate' && isset($data['db_concat_fields'])) {
                $ret_array['select'] .= ", " . $this->db->concat($this->table_name, $data['db_concat_fields'])
                    . " as $field";
                $selectedFields[$this->db->concat($this->table_name, $data['db_concat_fields'])] = true;
            }
            //Custom relate field or relate fields built in module builder which have no link field associated.
            if ($data['type'] == 'relate' && (isset($data['custom_module']) || isset($data['ext2']))) {
                $joinTableAlias = 'jt' . $jtcount;
                $relateJoinInfo = $this->custom_fields->getRelateJoin($data, $joinTableAlias, false);
                $ret_array['select'] .= $relateJoinInfo['select'];
                $ret_array['from'] .= $relateJoinInfo['from'];
                //Replace any references to the relationship in the where clause with the new alias
                $field_name = $relateJoinInfo['rel_table'] . '.' . !empty($data['name']) ? $data['name'] : 'name';
                $where = preg_replace('/(^|[\s(])' . $field_name . '/', '${1}' . $relateJoinInfo['name_field'], $where);
                $jtcount++;
            }
            //Parent Field
            if ($data['type'] == 'parent') {
                //See if we need to join anything by inspecting the where clause
                $match = preg_match(
                    '/(^|[\s(])parent_([a-zA-Z]+_?[a-zA-Z]+)_([a-zA-Z]+_?[a-zA-Z]+)\.name/',
                    $where,
                    $matches
                );
                if ($match) {
                    $joinTableAlias = 'jt' . $jtcount;
                    $joinModule = $matches[2];
                    $joinTable = $matches[3];
                    $localTable = $this->table_name;
                    if (!empty($data['custom_module'])) {
                        $localTable .= '_cstm';
                    }
                    global $beanFiles, $beanList;
                    require_once($beanFiles[$beanList[$joinModule]]);
                    $rel_mod = new $beanList[$joinModule]();
                    $nameField = "$joinTableAlias.name";
                    if (isset($rel_mod->field_defs['name'])) {
                        $name_field_def = $rel_mod->field_defs['name'];
                        if (isset($name_field_def['db_concat_fields'])) {
                            $nameField = $this->db->concat($joinTableAlias, $name_field_def['db_concat_fields']);
                        }
                    }
                    $ret_array['select'] .= ", $nameField {$data['name']} ";
                    $ret_array['from'] .= " LEFT JOIN $joinTable $joinTableAlias
                        ON $localTable.{$data['id_name']} = $joinTableAlias.id";
                    //Replace any references to the relationship in the where clause with the new alias
                    $where = preg_replace(
                        '/(^|[\s(])parent_' . $joinModule . '_' . $joinTable . '\.name/',
                        '${1}' . $nameField,
                        $where
                    );
                    $jtcount++;
                }
            }

            if ($this->is_relate_field($field)) {
                $linkField = $data['link'];
                $this->load_relationship($linkField);
                if (!empty($this->$linkField)) {
                    $params = array();
                    if (empty($join_type)) {
                        $params['join_type'] = ' LEFT JOIN ';
                    } else {
                        $params['join_type'] = $join_type;
                    }
                    if (isset($data['join_name'])) {
                        $params['join_table_alias'] = $data['join_name'];
                    } else {
                        $params['join_table_alias'] = 'jt' . $jtcount;
                    }
                    if (isset($data['join_link_name'])) {
                        $params['join_table_link_alias'] = $data['join_link_name'];
                    } else {
                        $params['join_table_link_alias'] = 'jtl' . $jtcount;
                    }
                    $join_primary = !isset($data['join_primary']) || $data['join_primary'];

                    $join = $this->$linkField->getJoin($params, true);
                    $used_join_key[] = $join['rel_key'];
                    $rel_module = $this->$linkField->getRelatedModuleName();
                    $table_joined = !empty($joined_tables[$params['join_table_alias']])
                        || (!empty($joined_tables[$params['join_table_link_alias']])
                            && isset($data['link_type']) && $data['link_type'] == 'relationship_info');

                    //if rname is set to 'name', and bean files exist, then check if field should be a concatenated name
                    global $beanFiles, $beanList;
                    // °3/21/2014 FIX NS-TEAM - Relationship fields could not be displayed in subpanels
                    if (isset($data['rname']) && $data['rname'] == 'name'
                        && !empty($beanFiles[$beanList[$rel_module]])) {

                        //create an instance of the related bean
                        require_once($beanFiles[$beanList[$rel_module]]);
                        $rel_mod = new $beanList[$rel_module]();
                        //if bean has first and last name fields, then name should be concatenated
                        if (isset($rel_mod->field_name_map['first_name'])
                            && isset($rel_mod->field_name_map['last_name'])) {
                            $data['db_concat_fields'] = array(0 => 'first_name', 1 => 'last_name');
                        }
                    }


                    if ($join['type'] == 'many-to-many') {
                        if (empty($ret_array['secondary_select'])) {
                            $ret_array['secondary_select'] = " SELECT $this->table_name.id ref_id  ";

                            if (!empty($beanFiles[$beanList[$rel_module]]) && $join_primary) {
                                require_once($beanFiles[$beanList[$rel_module]]);
                                $rel_mod = new $beanList[$rel_module]();
                                if (isset($rel_mod->field_defs['assigned_user_id'])) {
                                    $ret_array['secondary_select'] .= " , " . $params['join_table_alias']
                                        . ".assigned_user_id {$field}_owner, '$rel_module' {$field}_mod";
                                } else {
                                    if (isset($rel_mod->field_defs['created_by'])) {
                                        $ret_array['secondary_select'] .= " , " . $params['join_table_alias']
                                            . ".created_by {$field}_owner , '$rel_module' {$field}_mod";
                                    }
                                }
                            }
                        }

                        if (isset($data['db_concat_fields'])) {
                            $ret_array['secondary_select'] .= ' , '
                                . $this->db->concat($params['join_table_alias'], $data['db_concat_fields'])
                                . ' ' . $field;
                        } else {
                            if (!isset($data['relationship_fields'])) {
                                $ret_array['secondary_select'] .= ' , ' . $params['join_table_alias']
                                    . '.' . $data['rname'] . ' ' . $field;
                            }
                        }
                        if (!$singleSelect) {
                            $ret_array['select'] .= ", '                                                             " .
                                "                                                                                    " .
                                "                                                                                    " .
                                "                         ' $field ";
                        }
                        $count_used = 0;
                        foreach ($used_join_key as $used_key) {
                            if ($used_key == $join['rel_key']) {
                                $count_used++;
                            }
                        }
                        if ($count_used <= 1) {
                            //27416, the $ret_array['secondary_select'] should always generate, regardless the dbtype
                            // add rel_key only if it was not already added
                            if (!$singleSelect) {
                                $ret_array['select'] .= ", '                                    '  "
                                    . $join['rel_key'] . ' ';
                            }
                            $ret_array['secondary_select'] .= ', ' . $params['join_table_link_alias']
                                . '.' . $join['rel_key'] . ' ' . $join['rel_key'];
                        }
                        if (isset($data['relationship_fields'])) {
                            foreach ($data['relationship_fields'] as $r_name => $alias_name) {
                                if (!empty($secondarySelectedFields[$alias_name])) {
                                    continue;
                                }
                                $ret_array['secondary_select'] .= ', ' . $params['join_table_link_alias']
                                    . '.' . $r_name . ' ' . $alias_name;
                                $secondarySelectedFields[$alias_name] = true;
                            }
                        }
                        if (!$table_joined) {
                            $ret_array['secondary_from'] .= ' ' . $join['join'] . ' AND '
                                . $params['join_table_alias'] . '.deleted=0';
                            if (isset($data['link_type']) && $data['link_type'] == 'relationship_info'
                                && ($parentbean instanceof SugarBean)) {
                                $ret_array['secondary_where'] = $params['join_table_link_alias'] . '.'
                                    . $join['rel_key'] . "='" . $parentbean->id . "'";
                            }
                        }
                    } else {
                        if (isset($data['db_concat_fields'])) {
                            $ret_array['select'] .= ' , '
                                . $this->db->concat($params['join_table_alias'], $data['db_concat_fields'])
                                . ' ' . $field;
                        } else {
                            $ret_array['select'] .= ' , ' . $params['join_table_alias']
                                . '.' . $data['rname'] . ' ' . $field;
                        }
                        if (isset($data['additionalFields'])) {
                            foreach ($data['additionalFields'] as $k => $v) {
                                if (!empty($data['id_name']) && $data['id_name'] == $v
                                    && !empty($fields[$data['id_name']])) {
                                    continue;
                                }
                                $ret_array['select'] .= ' , ' . $params['join_table_alias'] . '.' . $k . ' ' . $v;
                            }
                        }
                        if (!$table_joined) {
                            $ret_array['from'] .= ' ' . $join['join'] . ' AND ' . $params['join_table_alias']
                                . '.deleted=0';
                            if (!empty($beanList[$rel_module]) && !empty($beanFiles[$beanList[$rel_module]])) {
                                require_once($beanFiles[$beanList[$rel_module]]);
                                $rel_mod = new $beanList[$rel_module]();
                                if (isset($value['target_record_key']) && !empty($filter)) {
                                    $selectedFields[$this->table_name . '.' . $value['target_record_key']] = true;
                                    $ret_array['select'] .= " , $this->table_name.{$value['target_record_key']} ";
                                }
                                if (isset($rel_mod->field_defs['assigned_user_id'])) {
                                    $ret_array['select'] .= ' , ' . $params['join_table_alias'] . '.assigned_user_id '
                                        . $field . '_owner';
                                } else {
                                    $ret_array['select'] .= ' , ' . $params['join_table_alias'] . '.created_by '
                                        . $field . '_owner';
                                }
                                $ret_array['select'] .= "  , '" . $rel_module . "' " . $field . '_mod';
                            }
                        }
                    }
                    // To fix SOAP stuff where we are trying to retrieve all the accounts data where accounts.id = ..
                    // and this code changes accounts to jt4 as there is a self join with the accounts table.
                    //Martin fix #27494
                    if (isset($data['db_concat_fields'])) {
                        $buildWhere = false;
                        if (in_array('first_name', $data['db_concat_fields'])
                            && in_array('last_name', $data['db_concat_fields'])) {
                            $exp = '/\(\s*?' . $data['name'] . '.*?\%\'\s*?\)/';
                            if (preg_match($exp, $where, $matches)) {
                                $search_expression = $matches[0];
                                //Create three search conditions - first + last, first, last
                                $first_name_search = str_replace(
                                    $data['name'],
                                    $params['join_table_alias'] . '.first_name',
                                    $search_expression
                                );
                                $last_name_search = str_replace(
                                    $data['name'],
                                    $params['join_table_alias'] . '.last_name',
                                    $search_expression
                                );
                                $full_name_search = str_replace(
                                    $data['name'],
                                    $this->db->concat($params['join_table_alias'], $data['db_concat_fields']),
                                    $search_expression
                                );
                                $buildWhere = true;
                                $where = str_replace(
                                    $search_expression,
                                    '(' . $full_name_search . ' OR ' . $first_name_search
                                    . ' OR ' . $last_name_search . ')',
                                    $where
                                );
                            }
                        }

                        if (!$buildWhere) {
                            $db_field = $this->db->concat($params['join_table_alias'], $data['db_concat_fields']);
                            $where = preg_replace('/' . $data['name'] . '/', $db_field, $where);

                            // For relationship fields replace their alias by the corresponding link table and r_name
                            if (isset($data['relationship_fields'])) {
                                foreach ($data['relationship_fields'] as $r_name => $alias_name) {
                                    $db_field = $this->db->concat($params['join_table_link_alias'], $r_name);
                                    $where = preg_replace('/' . $alias_name . '/', $db_field, $where);
                                }
                            }
                        }
                    } else {
                        $where = preg_replace(
                            '/(^|[\s(])' . $data['name'] . '/',
                            '${1}' . $params['join_table_alias'] . '.' . $data['rname'],
                            $where
                        );

                        // For relationship fields replace their alias by the corresponding link table and r_name
                        if (isset($data['relationship_fields'])) {
                            foreach ($data['relationship_fields'] as $r_name => $alias_name) {
                                $where = preg_replace(
                                    '/(^|[\s(])' . $alias_name . '/',
                                    '${1}' . $params['join_table_link_alias'] . '.' . $r_name,
                                    $where
                                );
                            }
                        }
                    }
                    if (!$table_joined) {
                        $joined_tables[$params['join_table_alias']] = 1;
                        $joined_tables[$params['join_table_link_alias']] = 1;
                    }

                    $jtcount++;
                }
            }
        }
        if (!empty($filter)) {
            if (isset($this->field_defs['assigned_user_id'])
                && empty($selectedFields[$this->table_name . '.assigned_user_id'])) {
                $ret_array['select'] .= ", $this->table_name.assigned_user_id ";
            } elseif (isset($this->field_defs['created_by'])
                && empty($selectedFields[$this->table_name . '.created_by'])) {
                $ret_array['select'] .= ", $this->table_name.created_by ";
            }
            if (isset($this->field_defs['system_id']) && empty($selectedFields[$this->table_name . '.system_id'])) {
                $ret_array['select'] .= ", $this->table_name.system_id ";
            }
        }

        if ($ifListForExport && isset($this->field_defs['email1'])) {
            $ret_array['select'] .= " ,email_addresses.email_address email1";
            $ret_array['from'] .= " LEFT JOIN email_addr_bean_rel on {$this->table_name}.id = " .
                "email_addr_bean_rel.bean_id and email_addr_bean_rel.bean_module='{$this->module_dir}' and " .
                "email_addr_bean_rel.deleted=0 and email_addr_bean_rel.primary_address=1 LEFT JOIN " .
                "email_addresses on email_addresses.id = email_addr_bean_rel.email_address_id ";
        }

        $where_auto = '1=1';
        if ($show_deleted == 0) {
            $where_auto = "$this->table_name.deleted=0";
        } elseif ($show_deleted == 1) {
            $where_auto = "$this->table_name.deleted=1";
        }
        if ($where != "") {
            $ret_array['where'] = " where ($where) AND $where_auto";
        } else {
            $ret_array['where'] = " where $where_auto";
        }

        //make call to process the order by clause
        $order_by = $this->process_order_by($order_by);
        if (!empty($order_by)) {
            $ret_array['order_by'] = " ORDER BY " . $order_by;
        }
        if ($singleSelect) {
            unset($ret_array['secondary_where']);
            unset($ret_array['secondary_from']);
            unset($ret_array['secondary_select']);
        }

        if ($return_array) {
            return $ret_array;
        }

        return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
    }

    /**
     * @param $field
     *
     * @return bool|string
     */
    public function get_relationship_field($field)
    {
        foreach ($this->field_defs as $field_def => $value) {
            if (isset($value['relationship_fields']) &&
                in_array($field, $value['relationship_fields']) &&
                (!isset($value['link_type']) || $value['link_type'] != 'relationship_info')
            ) {
                return $field_def;
            }
        }

        return false;
    }

    /**
     * Determine whether the given field is a relate field
     *
     * @param string $field Field name
     * @return bool
     */
    protected function is_relate_field($field)
    {
        if (!isset($this->field_defs[$field])) {
            return false;
        }

        $field_def = $this->field_defs[$field];

        return isset($field_def['type'])
            && $field_def['type'] == 'relate'
            && isset($field_def['link']);
    }

    /**
     * Prefixes column names with this bean's table name.
     *
     * @param string $order_by Order by clause to be processed
     * @param SugarBean $submodule name of the module this order by clause is for
     * @param bool $suppress_table_name Whether table name should be suppressed
     * @return string Processed order by clause
     *
     * Internal function, do not override.
     */
    public function process_order_by($order_by, $submodule = null, $suppress_table_name = false)
    {
        if (empty($order_by)) {
            return $order_by;
        }
        //submodule is empty,this is for list object in focus
        if (empty($submodule)) {
            $bean_queried = $this;
        } else {
            //submodule is set, so this is for subpanel, use submodule
            $bean_queried = $submodule;
        }

        $raw_elements = explode(',', $order_by);
        $valid_elements = array();
        foreach ($raw_elements as $key => $value) {
            $is_valid = false;

            //value might have ascending and descending decorations
            $list_column = preg_split('/\s/', trim($value), 2);
            $list_column = array_map('trim', $list_column);

            $list_column_name = $list_column[0];
            if (isset($bean_queried->field_defs[$list_column_name])) {
                $field_defs = $bean_queried->field_defs[$list_column_name];
                $source = isset($field_defs['source']) ? $field_defs['source'] : 'db';

                if (empty($field_defs['table']) && !$suppress_table_name) {
                    if ($source == 'db') {
                        $list_column[0] = $bean_queried->table_name . '.' . $list_column[0];
                    } elseif ($source == 'custom_fields') {
                        $list_column[0] = $bean_queried->table_name . '_cstm.' . $list_column[0];
                    }
                }

                // Bug 38803 - Use CONVERT() function when doing an order by on ntext, text, and image fields
                if ($source != 'non-db'
                    && $this->db->isTextType($this->db->getFieldType($bean_queried->field_defs[$list_column_name]))
                ) {
                    // array(10000) is for db2 only. It tells db2manager to cast 'clob' to varchar(10000)
                    // for this 'sort by' column
                    $list_column[0] = $this->db->convert($list_column[0], "text2char", array(10000));
                }

                $is_valid = true;

                if (isset($list_column[1])) {
                    switch (strtolower($list_column[1])) {
                        case 'asc':
                        case 'desc':
                            break;
                        default:
                            $GLOBALS['log']->debug("process_order_by: ($list_column[1]) is not a valid order.");
                            unset($list_column[1]);
                            break;
                    }
                }
            } else {
                $GLOBALS['log']->debug("process_order_by: ($list_column[0]) does not have a vardef entry.");
            }

            if ($is_valid) {
                $valid_elements[$key] = implode(' ', $list_column);
            }
        }

        return implode(', ', $valid_elements);
    }

    /**
     * Processes the list query and return fetched row.
     *
     * Internal function, do not override.
     * @param string $query select query to be processed.
     * @param int $row_offset starting position
     * @param int $limit Optional, default -1
     * @param int $max_per_page Optional, default -1
     * @param string $where Optional, additional filter criteria.
     * @return array Fetched data
     */
    public function process_list_query($query, $row_offset, $limit = -1, $max_per_page = -1, $where = '')
    {
        $rows_found = 0;
        global $sugar_config;
        $db = DBManagerFactory::getInstance('listviews');
        /**
         * if the row_offset is set to 'end' go to the end of the list
         */
        $toEnd = (string)$row_offset == 'end';
        $GLOBALS['log']->debug("process_list_query: " . $query);
        if ($max_per_page == -1) {
            $max_per_page = $sugar_config['list_max_entries_per_page'];
        }
        // Check to see if we have a count query available.
        if (empty($sugar_config['disable_count_query']) || $toEnd) {
            $count_query = $this->create_list_count_query($query);
            if (!empty($count_query) && (empty($limit) || $limit == -1)) {
                // We have a count query.  Run it and get the results.
                $result = $db->query($count_query, true, "Error running count query for $this->object_name List: ");
                $assoc = $db->fetchByAssoc($result);
                if (!empty($assoc['c'])) {
                    $rows_found = $assoc['c'];
                    $limit = $sugar_config['list_max_entries_per_page'];
                }
                if ($toEnd) {
                    $row_offset = (floor(($rows_found - 1) / $limit)) * $limit;
                }
            }
        } else {
            if ((empty($limit) || $limit == -1)) {
                $limit = $max_per_page + 1;
                $max_per_page = $limit;
            }
        }

        if (empty($row_offset)) {
            $row_offset = 0;
        }
        if (!empty($limit) && $limit != -1 && $limit != -99) {
            $result = $db->limitQuery($query, $row_offset, $limit, true, "Error retrieving $this->object_name list: ");
        } else {
            $result = $db->query($query, true, "Error retrieving $this->object_name list: ");
        }

        $list = array();

        $previous_offset = $row_offset - $max_per_page;
        $next_offset = $row_offset + $max_per_page;

        $class = get_class($this);
        //FIXME: Bug? we should remove the magic number -99
        //use -99 to return all
        $index = $row_offset;
        while ($max_per_page == -99 || ($index < $row_offset + $max_per_page)) {
            $row = $db->fetchByAssoc($result);
            if (empty($row)) {
                break;
            }

            //instantiate a new class each time. This is because php5 passes
            //by reference by default so if we continually update $this, we will
            //at the end have a list of all the same objects
            /** @var SugarBean $temp */
            $temp = new $class();

            foreach ($this->field_defs as $field => $value) {
                if (isset($row[$field])) {
                    $temp->$field = $row[$field];
                    $owner_field = $field . '_owner';
                    if (isset($row[$owner_field])) {
                        $temp->$owner_field = $row[$owner_field];
                    }

                    $GLOBALS['log']->debug("$temp->object_name({$row['id']}): " . $field . " = " . $temp->$field);
                } elseif (isset($row[$this->table_name . '.' . $field])) {
                    $temp->$field = $row[$this->table_name . '.' . $field];
                } else {
                    $temp->$field = "";
                }
            }

            $temp->check_date_relationships_load();
            $temp->fill_in_additional_list_fields();
            if ($temp->hasCustomFields()) {
                $temp->custom_fields->fill_relationships();
            }
            $temp->call_custom_logic("process_record");

            // fix defect #44206. implement the same logic as sugar_currency_format
            // Smarty modifier does.
            $temp->populateCurrencyFields();
            $list[] = $temp;

            $index++;
        }
        if (!empty($sugar_config['disable_count_query']) && !empty($limit)) {
            $rows_found = $row_offset + count($list);

            if (!$toEnd) {
                $next_offset--;
                $previous_offset++;
            }
        } elseif ($rows_found == 0) {
            $rows_found = $row_offset + count($list);
        }

        $response = array();
        $response['list'] = $list;
        $response['row_count'] = $rows_found;
        $response['next_offset'] = $next_offset;
        $response['previous_offset'] = $previous_offset;
        $response['current_offset'] = $row_offset;
        return $response;
    }

    /**
     * Changes the select expression of the given query to be 'count(*)' so you
     * can get the number of items the query will return.  This is used to
     * populate the upper limit on ListViews.
     *
     * @param string $query Select query string
     * @return string count query
     *
     * Internal function, do not override.
     */
    public function create_list_count_query($query)
    {
        // remove the 'order by' clause which is expected to be at the end of the query
        $pattern = '/\sORDER BY.*/is';
        $replacement = '';
        $query = preg_replace($pattern, $replacement, $query);
        //handle distinct clause
        $star = '*';
        if (substr_count(strtolower($query), 'distinct')) {
            if (!empty($this->seed) && !empty($this->seed->table_name)) {
                $star = 'DISTINCT ' . $this->seed->table_name . '.id';
            } else {
                $star = 'DISTINCT ' . $this->table_name . '.id';
            }
        }

        // change the select expression to 'count(*)'
        $pattern = '/SELECT(.*?)(\s){1}FROM(\s){1}/is';
        $replacement = 'SELECT count(' . $star . ') c FROM ';

        //if the passed query has union clause then replace all instances of the pattern.
        //this is very rare. I have seen this happening only from projects module.
        //in addition to this added a condition that has  union clause and uses
        //sub-selects.
        if (strstr($query, " UNION ALL ") !== false) {

            //separate out all the queries.
            $union_qs = explode(" UNION ALL ", $query);
            foreach ($union_qs as $key => $union_query) {
                $star = '*';
                preg_match($pattern, $union_query, $matches);
                if (!empty($matches) && stristr($matches[0], "distinct")) {
                    if (!empty($this->seed) && !empty($this->seed->table_name)) {
                        $star = 'DISTINCT ' . $this->seed->table_name . '.id';
                    } else {
                        $star = 'DISTINCT ' . $this->table_name . '.id';
                    }
                } // if
                $replacement = 'SELECT count(' . $star . ') c FROM ';
                $union_qs[$key] = preg_replace($pattern, $replacement, $union_query, 1);
            }
            $modified_select_query = implode(" UNION ALL ", $union_qs);
        } else {
            $modified_select_query = preg_replace($pattern, $replacement, $query, 1);
        }


        return $modified_select_query;
    }

    /*
     * Fill in a link field
     */

    /**
     * This is designed to be overridden and add specific fields to each record.
     * This allows the generic query to fill in the major fields, and then targeted
     * queries to get related fields and add them to the record.  The contact's
     * account for instance.  This method is only used for populating extra fields
     * in lists.
     */
    public function fill_in_additional_list_fields()
    {
        if (!empty($this->field_defs['parent_name']) && empty($this->parent_name)) {
            $this->fill_in_additional_parent_fields();
        }
    }

    /**
     * @return bool
     */
    public function hasCustomFields()
    {
        return !empty($GLOBALS['dictionary'][$this->object_name]['custom_fields']);
    }

    /**
     * Returns a detail object like retrieving of the current object type.
     *
     * It is intended for use in navigation buttons on the DetailView.  It will pass an offset
     * and limit argument to the sql query.
     * @internal This method must be called on a new instance.  It overrides the values of all the fields
     * in the current one.
     *
     * @param string $order_by
     * @param string $where Additional where clause
     * @param int $offset
     * @param int $row_offset Optional,default 0, starting row number
     * @param int $limit Optional, default -1
     * @param int $max Optional, default -1
     * @param int $show_deleted Optional, default 0, if set to 1 system will show deleted records.
     * @return array Fetched data.
     *
     * Internal function, do not override.
     */
    public function get_detail(
        $order_by = "",
        $where = "",
        $offset = 0,
        $row_offset = 0,
        $limit = -1,
        $max = -1,
        $show_deleted = 0
    ) {
        $GLOBALS['log']->debug("get_detail:  order_by = '$order_by' and where = '$where' and limit = '$limit' " .
            "and offset = '$offset'");
        if (isset($_SESSION['show_deleted'])) {
            $show_deleted = 1;
        }

        $query = $this->create_new_list_query($order_by, $where, array(), array(), $show_deleted, $offset);

        return $this->process_detail_query($query, $row_offset, $limit, $max, $where, $offset);
    }

    /**
     * Applies pagination window to select queries used by detail view,
     * executes the query and returns fetched data.
     *
     * Internal function, do not override.
     * @param string $query query to be processed.
     * @param int $row_offset
     * @param int $limit optional, default -1
     * @param int $max_per_page Optional, default -1
     * @param string $where Custom where clause.
     * @param int $offset Optional, default 0
     * @return array Fetched data.
     *
     */
    public function process_detail_query($query, $row_offset, $limit = -1, $max_per_page = -1, $where = '', $offset = 0)
    {
        global $sugar_config;
        $GLOBALS['log']->debug("process_detail_query: " . $query);
        if ($max_per_page == -1) {
            $max_per_page = $sugar_config['list_max_entries_per_page'];
        }

        // Check to see if we have a count query available.
        $count_query = $this->create_list_count_query($query);

        if (!empty($count_query) && (empty($limit) || $limit == -1)) {
            // We have a count query.  Run it and get the results.
            $result = $this->db->query($count_query, true, "Error running count query for $this->object_name List: ");
            $assoc = $this->db->fetchByAssoc($result);
            if (!empty($assoc['c'])) {
                $total_rows = $assoc['c'];
            }
        }

        if (empty($row_offset)) {
            $row_offset = 0;
        }

        $result = $this->db->limitQuery($query, $offset, 1, true, "Error retrieving $this->object_name list: ");

        $previous_offset = $row_offset - $max_per_page;
        $next_offset = $row_offset + $max_per_page;

        $row = $this->db->fetchByAssoc($result);
        $this->retrieve($row['id']);

        $response = array();
        $response['bean'] = $this;
        if (empty($total_rows)) {
            $total_rows = 0;
        }
        $response['row_count'] = $total_rows;
        $response['next_offset'] = $next_offset;
        $response['previous_offset'] = $previous_offset;

        return $response;
    }/** @noinspection PhpDocSignatureInspection */
    /** @noinspection PhpDocSignatureInspection */
    /** @noinspection PhpDocSignatureInspection */

    /**
     * Function fetches a single row of data given the primary key value.
     *
     * The fetched data is then set into the bean. The function also processes the fetched data by formatting
     * date/time and numeric values.
     *
     * @param string|int $id Optional, default -1, is set to -1 id value from the bean is used, else,
     * passed value is used
     * @param bool $encode Optional, default true, encodes the values fetched from the database.
     * @param bool $deleted Optional, default true, if set to false deleted filter will not be added.
     * @return SugarBean|null
     *
     * Internal function, do not override.
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        $custom_logic_arguments = [];
        $custom_logic_arguments['id'] = $id;
        $this->call_custom_logic('before_retrieve', $custom_logic_arguments);

        if ($id == -1) {
            $id = $this->id;
        }
        $custom_join = $this->getCustomJoin();

        $query = "SELECT $this->table_name.*" . $custom_join['select'] . " FROM $this->table_name ";

        $query .= $custom_join['join'];
        $query .= " WHERE $this->table_name.id = " . $this->db->quoted($id);
        if ($deleted) {
            $query .= " AND $this->table_name.deleted=0";
        }
        $GLOBALS['log']->debug("Retrieve $this->object_name : " . $query);
        $result = $this->db->limitQuery($query, 0, 1, false, "Retrieving record by id $this->table_name:$id found ");
        if (empty($result)) {
            return null;
        }

        $row = $this->db->fetchByAssoc($result, $encode);
        if (empty($row)) {
            return null;
        }

        //make copy of the fetched row for construction of audit record and for business logic/workflow
        $row = $this->convertRow($row);
        $this->fetched_row = $row;
        $this->populateFromRow($row);

        // fix defect #52438. implement the same logic as sugar_currency_format
        // Smarty modifier does.
        $this->populateCurrencyFields();

        global $module, $action;
        //Just to get optimistic locking working for this release
        if ($this->optimistic_lock && $module == $this->module_dir && $action == 'EditView' && isset($_REQUEST["record"]) && $id == $_REQUEST['record']) {
            $_SESSION['o_lock_id'] = $id;
            $_SESSION['o_lock_dm'] = $this->date_modified;
            $_SESSION['o_lock_on'] = $this->object_name;
        }
        $this->processed_dates_times = array();
        $this->check_date_relationships_load();

        if (isset($this->custom_fields)) {
            $this->custom_fields->fill_relationships();
        }

        $this->is_updated_dependent_fields = false;
        $this->fill_in_additional_detail_fields();
        $this->fill_in_relationship_fields();
        // save related fields values for audit
        foreach ($this->get_related_fields() as $rel_field_name) {
            $field_name = $rel_field_name['name'];
            if (!empty($this->$field_name)) {
                $this->fetched_rel_row[$rel_field_name['name']] = $this->$field_name;
            } else {
                $this->fetched_rel_row[$rel_field_name['name']] = '';
            }
        }
        //make a copy of fields in the relationship_fields array. These field values will be used to
        //clear relationship.
        foreach ($this->field_defs as $key => $def) {
            if ($def['type'] == 'relate' && isset($def['id_name']) && isset($def['link']) && isset($def['save'])) {
                if (isset($this->$key)) {
                    $this->rel_fields_before_value[$key] = $this->$key;
                    $defIdName = $def['id_name'];
                    if (isset($this->$defIdName)) {
                        $this->rel_fields_before_value[$defIdName] = $this->$defIdName;
                    }
                } else {
                    $this->rel_fields_before_value[$key] = null;
                }
            }
        }
        if (isset($this->relationship_fields) && is_array($this->relationship_fields)) {
            foreach ($this->relationship_fields as $rel_id => $rel_name) {
                if (isset($this->$rel_id)) {
                    $this->rel_fields_before_value[$rel_id] = $this->$rel_id;
                } else {
                    $this->rel_fields_before_value[$rel_id] = null;
                }
            }
        }

        // call the custom business logic
        $custom_logic_arguments['id'] = $id;
        $custom_logic_arguments['encode'] = $encode;
        $this->call_custom_logic("after_retrieve", $custom_logic_arguments);
        unset($custom_logic_arguments);
        return $this;
    }

    /**
     * Proxy method for DynamicField::getJOIN
     * @param bool $expandedList
     * @param bool $includeRelates
     * @param string|bool $where
     * @return array
     */
    public function getCustomJoin($expandedList = false, $includeRelates = false, &$where = false)
    {
        $result = array(
            'select' => '',
            'join' => ''
        );
        if (isset($this->custom_fields)) {
            $result = $this->custom_fields->getJOIN($expandedList, $includeRelates, $where);
        }
        return $result;
    }

    /**
     * Convert row data from DB format to internal format
     * Mostly useful for dates/times
     * @param array $row
     * @return array $row
     */
    public function convertRow($row)
    {
        foreach ($this->field_defs as $name => $fieldDef) {
            // skip empty fields and non-db fields
            if (isset($name) && !empty($row[$name])) {
                $row[$name] = $this->convertField($row[$name], $fieldDef);
            }
        }
        return $row;
    }

    /**
     * Converts the field value based on the provided fieldDef
     * @param $fieldValue
     * @param $fieldDef
     * @return string
     */
    public function convertField($fieldValue, $fieldDef)
    {
        if (!empty($fieldValue)
            && !(isset($fieldDef['source'])
                && !in_array($fieldDef['source'], array('db', 'custom_fields', 'relate'))
                && !isset($fieldDef['dbType']))
        ) {
            // fromConvert other fields
            $fieldValue = $this->db->fromConvert($fieldValue, $this->db->getFieldType($fieldDef));
        }
        return $fieldValue;
    }

    /**
     * Sets value from fetched row into the bean.
     *
     * @param array $row Fetched row
     * @todo loop through vardefs instead
     * @internal runs into an issue when populating from field_defs for users - corrupts user prefs
     *
     * Internal function, do not override.
     */
    public function populateFromRow($row)
    {
        $null_value = '';
        foreach ($this->field_defs as $field => $field_value) {
            if (($field == 'user_preferences' && $this->module_dir == 'Users')
                || ($field == 'internal' && $this->module_dir == 'Cases')) {
                continue;
            }
            if (isset($row[$field])) {
                $this->$field = $row[$field];
                $owner = $field . '_owner';
                if (!empty($row[$owner])) {
                    $this->$owner = $row[$owner];
                }
            } else {
                $this->$field = $null_value;
            }
        }
    }

    /**
     * Populates currency fields in case of currency is default and it's
     * attributes are not retrieved from database (bugs ##44206, 52438)
     */
    protected function populateCurrencyFields()
    {
        if (property_exists($this, 'currency_id') && $this->currency_id == -99) {
            // manually retrieve default currency object as long as it's
            // not stored in database and thus cannot be joined in query
            $currency = BeanFactory::getBean('Currencies', $this->currency_id);

            if ($currency) {
                // walk through all currency-related fields
                foreach ($this->field_defs as $this_field) {
                    if (isset($this_field['type']) && $this_field['type'] == 'relate'
                        && isset($this_field['module']) && $this_field['module'] == 'Currencies'
                        && isset($this_field['id_name']) && $this_field['id_name'] == 'currency_id'
                    ) {
                        // populate related properties manually
                        $this_property = $this_field['name'];
                        $currency_property = $this_field['rname'];
                        $this->$this_property = $currency->$currency_property;
                    }
                }
            }
        }
    }

    /**
     * This function retrieves a record of the appropriate type from the DB.
     * It fills in all of the fields from the DB into the object it was called on.
     *
     * @return mixed this - The object that it was called upon or null if exactly 1 record was not found.
     *
     */

    public function check_date_relationships_load()
    {
        global $disable_date_format;
        global $timedate;
        if (empty($timedate)) {
            $timedate = TimeDate::getInstance();
        }

        if (empty($this->field_defs)) {
            return;
        }
        foreach ($this->field_defs as $fieldDef) {
            if (!isset($fieldDef['name'])) {
                LoggerManager::getLogger()->warn("Field definition has not 'name'");
                return;
            }
            $field = $fieldDef['name'];
            if (!isset($this->processed_dates_times[$field])) {
                $this->processed_dates_times[$field] = '1';
                if (empty($this->$field)) {
                    continue;
                }
                if ($field == 'date_modified' || $field == 'date_entered') {
                    $this->$field = $this->db->fromConvert($this->$field, 'datetime');
                    if (empty($disable_date_format)) {
                        $this->$field = $timedate->to_display_date_time($this->$field);
                    }
                } elseif (isset($this->field_name_map[$field]['type'])) {
                    $type = $this->field_name_map[$field]['type'];

                    if ($type == 'relate' && isset($this->field_name_map[$field]['custom_module'])) {
                        $type = $this->field_name_map[$field]['type'];
                    }

                    if ($type == 'date') {
                        if ($this->$field == '0000-00-00' || empty($this->$field)) {
                            $this->$field = '';
                            continue;
                        }
                        if (empty($disable_date_format)) {
                            if (!empty($this->field_name_map[$field]['rel_field'])) {
                                $rel_field = $this->field_name_map[$field]['rel_field'];
                                if (!empty($this->$rel_field)) {
                                    $merge_time = $timedate->merge_date_time($this->$field, $this->$rel_field);
                                    $this->$field = $timedate->to_display_date($merge_time);
                                    $this->$rel_field = $timedate->to_display_time($merge_time);
                                    continue;
                                }
                            }
                            $this->$field = $timedate->to_display_date($this->$field, false);
                        }
                    } elseif ($type == 'datetime' || $type == 'datetimecombo') {
                        if ($this->$field == '0000-00-00 00:00:00' || empty($this->$field)) {
                            $this->$field = '';
                        } else {
                            if (empty($disable_date_format)) {
                                $this->$field = $timedate->to_display_date_time($this->$field, true, true);
                            }
                        }
                    } elseif ($type == 'time') {
                        if ($this->$field == '00:00:00' || empty($this->$field)) {
                            $this->$field = '';
                        } else {
                            if (empty($this->field_name_map[$field]['rel_field']) && empty($disable_date_format)) {
                                $this->$field = $timedate->to_display_time($this->$field, true, false);
                            }
                        }
                    } elseif (($type == 'encrypt' && empty($disable_date_format)) || isTrue($fieldDef['db_encrypted'] ?? false)) {
                        $this->$field = $this->decrypt_after_retrieve($this->$field);
                    }
                }
            }
        }
    }

    /**
     * Decode and decrypt a base 64 encoded string with field type 'encrypt' in this bean using Blowfish.
     * @param string $value - an encrypted and base 64 encoded string.
     * @return string
     */
    public function decrypt_after_retrieve($value)
    {
        if (empty($value)) {
            return $value;
        }
        require_once("include/utils/encryption_utils.php");
        return blowfishDecode($this->getEncryptKey(), $value);
    }

    /**
     * This is designed to be overridden and add specific fields to each record.
     * This allows the generic query to fill in the major fields, and then targeted
     * queries to get related fields and add them to the record.  The contact's
     * account for instance.  This method is only used for populating extra fields
     * in the detail form
     */
    public function fill_in_additional_detail_fields()
    {
        if (!empty($this->field_defs['assigned_user_name']) && !empty($this->assigned_user_id)) {
            $this->assigned_user_name = get_assigned_user_name($this->assigned_user_id);
        }
        if (!empty($this->field_defs['created_by']) && !empty($this->created_by)) {
            $this->created_by_name = get_assigned_user_name($this->created_by);
        }
        if (!empty($this->field_defs['modified_user_id']) && !empty($this->modified_user_id)) {
            $this->modified_by_name = get_assigned_user_name($this->modified_user_id);
        }

        if (!empty($this->field_defs['parent_name'])) {
            $this->fill_in_additional_parent_fields();
        }
    }

    /**
     * This is designed to be overridden or called from extending bean. This method
     * will fill in any parent_name fields.
     *
     * @return bool
     */
    public function fill_in_additional_parent_fields()
    {
        if (!empty($this->parent_id) && !empty($this->last_parent_id) && $this->last_parent_id == $this->parent_id) {
            return false;
        }
        $this->parent_name = '';

        if (!empty($this->parent_type)) {
            $this->last_parent_id = $this->parent_id;
            $this->getRelatedFields(
                $this->parent_type,
                $this->parent_id,
                array(
                    'name' => 'parent_name',
                    'document_name' => 'parent_document_name',
                    'first_name' => 'parent_first_name',
                    'last_name' => 'parent_last_name')
            );
            if (!empty($this->parent_first_name) || !empty($this->parent_last_name)) {
                $this->parent_name = $GLOBALS['locale']->getLocaleFormattedName(
                    $this->parent_first_name,
                    $this->parent_last_name
                );
            } elseif (!empty($this->parent_document_name)) {
                $this->parent_name = $this->parent_document_name;
            }
        }
        return true;
    }

    /**
     * @param $module
     * @param $id
     * @param $fields
     * @param bool $return_array
     *
     * @return string
     */
    public function getRelatedFields($module, $id, $fields, $return_array = false)
    {
        if (empty($GLOBALS['beanList'][$module])) {
            return '';
        }
        $object = BeanFactory::getObjectName($module);

        VardefManager::loadVardef($module, $object);
        if (empty($GLOBALS['dictionary'][$object]['table'])) {
            return '';
        }
        $table = $GLOBALS['dictionary'][$object]['table'];
        $query = 'SELECT id';
        foreach ($fields as $field => $alias) {
            if (!empty($GLOBALS['dictionary'][$object]['fields'][$field]['db_concat_fields'])) {
                $concat = $this->db->concat(
                    $table,
                    $GLOBALS['dictionary'][$object]['fields'][$field]['db_concat_fields']
                );
                $query .= ' ,' . $concat . ' as ' . $alias;
            } elseif (!empty($GLOBALS['dictionary'][$object]['fields'][$field]) &&
                (empty($GLOBALS['dictionary'][$object]['fields'][$field]['source']) ||
                    $GLOBALS['dictionary'][$object]['fields'][$field]['source'] != "non-db")
            ) {
                $query .= ' ,' . $table . '.' . $field . ' as ' . $alias;
            }
            if (!$return_array) {
                $this->$alias = '';
            }
        }
        if ($query == 'SELECT id' || empty($id)) {
            return '';
        }


        if (isset($GLOBALS['dictionary'][$object]['fields']['assigned_user_id'])) {
            $query .= " , " . $table . ".assigned_user_id owner";
        } elseif (isset($GLOBALS['dictionary'][$object]['fields']['created_by'])) {
            $query .= " , " . $table . ".created_by owner";
        }
        $query .= ' FROM ' . $table . ' WHERE deleted=0 AND id=';
        $result = DBManagerFactory::getInstance()->query($query . "'$id'");
        $row = DBManagerFactory::getInstance()->fetchByAssoc($result);
        if ($return_array) {
            return $row;
        }
        $owner = (empty($row['owner'])) ? '' : $row['owner'];
        foreach ($fields as $alias) {
            $this->$alias = (!empty($row[$alias])) ? $row[$alias] : '';
            $alias .= '_owner';
            $this->$alias = $owner;
            $a_mod = $alias . '_mod';
            $this->$a_mod = $module;
        }
    }

    /**
     * Fill in fields where type = relate
     */
    public function fill_in_relationship_fields()
    {
        global $fill_in_rel_depth;
        if (empty($fill_in_rel_depth) || $fill_in_rel_depth < 0) {
            $fill_in_rel_depth = 0;
        }

        if ($fill_in_rel_depth > 1) {
            return;
        }

        $fill_in_rel_depth++;

        foreach ($this->field_defs as $field) {
            if (0 == strcmp($field['type'], 'relate') && !empty($field['module'])) {
                $name = $field['name'];
                if (empty($this->$name)) {
                    // set the value of this relate field in this bean ($this->$field['name']) to the value of the
                    // 'name' field in the related module for the record identified
                    // by the value of $this->$field['id_name']
                    $related_module = $field['module'];
                    $id_name = $field['id_name'];

                    if (empty($this->$id_name)) {
                        $this->fill_in_link_field($id_name, $field);
                    }
                    if (!empty($this->$id_name) &&
                        ($this->object_name != $related_module ||
                            ($this->object_name == $related_module && $this->$id_name != $this->id))
                    ) {
                        if (!empty($this->$id_name) && isset($this->$name)) {
                            $mod = BeanFactory::getShallowBean($related_module, $this->$id_name);
                            if ($mod) {
                                if (!empty($field['rname'])) {
                                    $rname = $field['rname'];
                                    $this->$name = $mod->$rname;
                                } else {
                                    if (isset($mod->name)) {
                                        $this->$name = $mod->name;
                                    }
                                }
                            }
                        }
                    }
                    if (!empty($this->$id_name) && isset($this->$name)) {
                        if (!isset($field['additionalFields'])) {
                            $field['additionalFields'] = array();
                        }
                        if (!empty($field['rname'])) {
                            $field['additionalFields'][$field['rname']] = $name;
                        } else {
                            $field['additionalFields']['name'] = $name;
                        }
                        $this->getRelatedFields($related_module, $this->$id_name, $field['additionalFields']);
                    }
                }
            }
        }
        $fill_in_rel_depth--;
    }

    /**
     * @param $linkFieldName
     * @param $def
     */
    public function fill_in_link_field($linkFieldName, $def)
    {
        $idField = $linkFieldName;
        //If the id_name provided really was an ID, don't try to load it as a link. Use the normal link
        if (!empty($this->field_defs[$linkFieldName]['type']) && $this->field_defs[$linkFieldName]['type'] == "id"
            && !empty($def['link'])) {
            $linkFieldName = $def['link'];
        }
        if ($this->load_relationship($linkFieldName)) {
            $list = $this->$linkFieldName->get();
            // match up with null value in $this->populateFromRow()
            $this->$idField = '';
            if (!empty($list)) {
                $this->$idField = $list[0];
            }
        }
    }

    /**
     * Returns an array of fields that are of type relate.
     *
     * @return array List of fields.
     *
     * Internal function, do not override.
     */
    public function get_related_fields()
    {
        $related_fields = array();

        $fieldDefs = $this->getFieldDefinitions();

        //find all definitions of type link.
        if (!empty($fieldDefs)) {
            foreach ($fieldDefs as $name => $properties) {
                if (array_search('relate', $properties, true) === 'type') {
                    $related_fields[$name] = $properties;
                }
            }
        }

        return $related_fields;
    }

    /**
     * Fetches data from all related tables.
     *
     * @param object $child_seed
     * @param string $related_field_name relation to fetch data for
     * @param string $order_by Optional, default empty
     * @param string $where Optional, additional where clause
     * @param int $row_offset
     * @param int $limit
     * @param int $max
     *
     * @return array Fetched data.
     *
     * Internal function, do not override.
     */
    public function get_related_list(
        $child_seed,
        $related_field_name,
        $order_by = "",
        $where = "",
        $row_offset = 0,
        $limit = -1,
        $max = -1
    ) {
        global $layout_edit_mode;

        if (isset($layout_edit_mode) && $layout_edit_mode) {
            $response = array();
            $child_seed->assign_display_fields($child_seed->module_dir);
            $response['list'] = array($child_seed);
            $response['row_count'] = 1;
            $response['next_offset'] = 0;
            $response['previous_offset'] = 0;

            return $response;
        }
        $GLOBALS['log']->debug("get_related_list:  order_by = '$order_by' and where = '$where' and limit = '$limit'");

        $this->load_relationship($related_field_name);

        if ($this->$related_field_name instanceof Link) {
            $query_array = $this->$related_field_name->getQuery(true);
        } else {
            $query_array = $this->$related_field_name->getQuery(array(
                "return_as_array" => true,
                'where' => '1=1'
            ));
        }

        $entire_where = $query_array['where'];
        if (!empty($where)) {
            if (empty($entire_where)) {
                $entire_where = ' WHERE ' . $where;
            } else {
                $entire_where .= ' AND ' . $where;
            }
        }

        $query = 'SELECT ' . $child_seed->table_name . '.* ' . $query_array['from'] . ' ' . $entire_where;
        if (!empty($order_by)) {
            $query .= " ORDER BY " . $order_by;
        }

        return $child_seed->process_list_query($query, $row_offset, $limit, $max, $where);
    }

    /**
     * Returns a full (ie non-paged) list of the current object type.
     *
     * @param string $order_by the order by SQL parameter. defaults to ""
     * @param string $where where clause. defaults to ""
     * @param bool $check_dates . defaults to false
     * @param int $show_deleted show deleted records. defaults to 0
     * @return null|SugarBean[]
     */
    public function get_full_list($order_by = "", $where = "", $check_dates = false, $show_deleted = 0)
    {
        $GLOBALS['log']->debug("get_full_list:  order_by = '$order_by' and where = '$where'");
        if (isset($_SESSION['show_deleted'])) {
            $show_deleted = 1;
        }
        $query = $this->create_new_list_query($order_by, $where, array(), array(), $show_deleted);
        return $this->process_full_list_query($query, $check_dates);
    }

    /**
     * Processes fetched list view data
     *
     * Internal function, do not override.
     * @param string $query query to be processed.
     * @param bool $check_date Optional, default false. if set to true date time values are processed.
     * @return null|array Fetched data.
     *
     */
    public function process_full_list_query($query, $check_date = false)
    {
        $GLOBALS['log']->debug("process_full_list_query: query is " . $query);
        $result = $this->db->query($query, false);
        $GLOBALS['log']->debug("process_full_list_query: result is " . print_r($result, true));
        $class = get_class($this);
        $isFirstTime = true;
        $bean = new $class();

        $list = [];
        // We have some data.
        while (($row = $bean->db->fetchByAssoc($result)) != null) {
            $row = $this->convertRow($row);
            if (!$isFirstTime) {
                $bean = new $class();
            }
            $isFirstTime = false;

            foreach ($bean->field_defs as $field => $value) {
                if (isset($row[$field])) {
                    $bean->$field = $row[$field];
                    $GLOBALS['log']->debug("process_full_list: $bean->object_name({$row['id']}): "
                        . $field . " = " . $bean->$field);
                } else {
                    $bean->$field = '';
                }
            }
            if ($check_date) {
                $bean->processed_dates_times = array();
                $bean->check_date_relationships_load();
            }
            $bean->fill_in_additional_list_fields();
            $bean->call_custom_logic("process_record");
            $bean->fetched_row = $row;

            $list[] = $bean;
        }
        if (isset($list)) {
            return $list;
        }
        return null;
    }

    /**
     * This is a helper function that is used to quickly created indexes when creating tables.
     * @param string $query
     */
    public function create_index($query)
    {
        $GLOBALS['log']->info("create_index: $query");

        $this->db->query($query, true, "Error creating index:");
    }

    /**
     * This function should be overridden in each module.  It marks an item as deleted.
     *
     * If it is not overridden, then marking this type of item is not allowed
     * @param string $id
     */
    public function mark_deleted($id)
    {
        global $current_user;
        $date_modified = $GLOBALS['timedate']->nowDb();
        $id = $this->db->quote($id);
        if (isset($_SESSION['show_deleted'])) {
            $this->mark_undeleted($id);
        } else {
            // call the custom business logic
            $custom_logic_arguments = [];
            $custom_logic_arguments['id'] = $id;
            $this->call_custom_logic("before_delete", $custom_logic_arguments);
            $this->deleted = 1;
            $this->mark_relationships_deleted($id);
            if (isset($this->field_defs['modified_user_id'])) {
                if (!empty($current_user)) {
                    $this->modified_user_id = $current_user->id;
                } else {
                    $this->modified_user_id = 1;
                }
                $query = "UPDATE $this->table_name set deleted=1 , date_modified = '$date_modified', " .
                    "modified_user_id = '$this->modified_user_id' where id='$id'";
            } else {
                $query = "UPDATE $this->table_name set deleted=1 , date_modified = '$date_modified' where id='$id'";
            }
            $this->db->query($query, true, "Error marking record deleted: ");

            SugarRelationship::resaveRelatedBeans();

            // Take the item off the recently viewed lists
            $tracker = BeanFactory::newBean('Trackers');
            $tracker->makeInvisibleForAll($id);


            $this->deleteFiles();

            // call the custom business logic
            $this->call_custom_logic("after_delete", $custom_logic_arguments);
        }
    }

    /**
     * Restores data deleted by call to mark_deleted() function.
     *
     * Internal function, do not override.
     * @param string $id
     */
    public function mark_undeleted($id)
    {
        $custom_logic_arguments = [];
        // call the custom business logic
        $custom_logic_arguments['id'] = $id;
        $this->call_custom_logic("before_restore", $custom_logic_arguments);

        $date_modified = $GLOBALS['timedate']->nowDb();
        $query = "UPDATE $this->table_name set deleted=0 , date_modified = '$date_modified' where id='"
            . $this->db->quote($id) . "'";
        $this->db->query($query, true, "Error marking record undeleted: ");

        $this->restoreFiles();

        // call the custom business logic
        $this->call_custom_logic("after_restore", $custom_logic_arguments);
    }

    /**
     * Restores files from deleted folder
     *
     * @return bool success of operation
     */
    protected function restoreFiles()
    {
        if (!$this->id || !$this->haveFiles()) {
            return true;
        }
        $files = $this->getFiles();
        if (empty($files)) {
            return true;
        }

        $directory = $this->deleteFileDirectory();

        foreach ($files as $file) {
            if (is_file('upload://deleted/' . $directory . '/' . $file)
                && !sugar_rename('upload://deleted/' . $directory . '/' . $file, 'upload://' . $file)
            ) {
                $GLOBALS['log']->error('Could not move file ' . $directory . '/' . $file . ' from deleted directory');
            }
        }

        /**
         * @var DBManager $db
         */
        $db = DBManagerFactory::getInstance();
        $db->query('DELETE FROM cron_remove_documents WHERE bean_id=' . $db->quoted($this->id));

        return true;
    }

    /**
     * Method returns true if bean has files
     *
     * @return bool
     */
    public function haveFiles()
    {
        $return = false;
        if ($this->bean_implements('FILE')
            || $this instanceof File
            || !empty(static::$fileFields[$this->module_name])
        ) {
            $return = true;
        } elseif (!empty($this->field_defs)) {
            foreach ($this->field_defs as $fieldDef) {
                if (!isset($fieldDef['type']) || $fieldDef['type'] != 'image') {
                    continue;
                }
                $return = true;
                break;
            }
        }
        return $return;
    }

    /**
     * Method returns array of names of files for current bean
     *
     * @return array
     */
    public function getFiles()
    {
        $files = array();
        foreach ($this->getFilesFields() as $field) {
            if (!empty($this->$field)) {
                $files[] = $this->$field;
            }
        }
        return $files;
    }

    /**
     * Method returns array of name of fields which contain names of files
     *
     * @param bool $resetCache do not use cache
     * @return array
     */
    public function getFilesFields($resetCache = false)
    {
        if (isset(static::$fileFields[$this->module_name]) && !$resetCache) {
            return static::$fileFields[$this->module_name];
        }

        static::$fileFields = array();
        if ($this->bean_implements('FILE') || $this instanceof File) {
            static::$fileFields[$this->module_name][] = 'id';
        }
        foreach ($this->field_defs as $fieldName => $fieldDef) {
            if ($fieldDef['type'] != 'image') {
                continue;
            }
            static::$fileFields[$this->module_name][] = $fieldName;
        }

        return static::$fileFields[$this->module_name];
    }

    /**
     * Returns path for files of bean or false on error
     *
     * @return bool|string
     */
    public function deleteFileDirectory()
    {
        if (empty($this->id)) {
            return false;
        }
        return preg_replace('/^(..)(..)(..)/', '$1/$2/$3/', $this->id);
    }

    /**
     * This function deletes relationships to this object.  It should be overridden
     * to handle the relationships of the specific object.
     * This function is called when the item itself is being deleted.
     *
     * @param int $id id of the relationship to delete
     */
    public function mark_relationships_deleted($id)
    {
        $this->delete_linked($id);
    }


    /* When creating a custom field of type Dropdown, it creates an enum row in the DB.
     A typical get_list_view_array() result will have the *KEY* value from that drop-down.
     Since custom _dom objects are flat-files included in the $app_list_strings variable,
     We need to generate a key-key pair to get the true value like so:
     ([module]_cstm->fields_meta_data->$app_list_strings->*VALUE*)*/

    /**
     * Iterates through all the relationships and deletes all records for reach relationship.
     *
     * @param string $id Primary key value of the parent record
     */
    public function delete_linked($id)
    {
        $linked_fields = $this->get_linked_fields();
        foreach ($linked_fields as $name => $value) {
            if ($this->load_relationship($name)) {
                $this->$name->delete($id);
            } else {
                $GLOBALS['log']->fatal("error loading relationship $name");
            }
        }
    }

    /**
     * Moves file to deleted folder
     *
     * @return bool success of movement
     */
    public function deleteFiles()
    {
        if (!$this->id || !$this->haveFiles()) {
            return true;
        }
        $files = $this->getFiles();
        if (empty($files)) {
            return true;
        }

        $directory = $this->deleteFileDirectory();

        $isCreated = is_dir('upload://deleted/' . $directory);
        if (!$isCreated) {
            sugar_mkdir('upload://deleted/' . $directory, 0777, true);
            $isCreated = is_dir('upload://deleted/' . $directory);
        }
        if (!$isCreated) {
            return false;
        }

        foreach ($files as $file) {
            if (file_exists('upload://' . $file)
                && !sugar_rename('upload://' . $file, 'upload://deleted/' . $directory . '/' . $file)
            ) {
                $GLOBALS['log']->error('Could not move file ' . $file . ' to deleted directory');
            }
        }

        /**
         * @var DBManager $db
         */
        $db = DBManagerFactory::getInstance();
        $record = array(
            'bean_id' => $db->quoted($this->id),
            'module' => $db->quoted($this->module_name),
            'date_modified' => $db->convert($db->quoted(date('Y-m-d H:i:s')), 'datetime')
        );
        $recordDB = $db->fetchOne(
            "SELECT id FROM cron_remove_documents WHERE module={$record['module']} AND bean_id={$record['bean_id']}"
        );
        if (!empty($recordDB)) {
            $record['id'] = $db->quoted($recordDB['id']);
        }
        if (empty($record['id'])) {
            $record['id'] = $db->quoted(create_guid());
            $qry = 'INSERT INTO cron_remove_documents (' . implode(', ', array_keys($record)) . ') ' .
                'VALUES(' . implode(', ', $record) . ')';
        } else {
            $qry = "UPDATE cron_remove_documents SET date_modified={$record['date_modified']} WHERE id={$record['id']}";
        }
        $db->query($qry);

        return true;
    }

    /**
     * This function is used to execute the query and create an array template objects
     * from the resulting ids from the query.
     * It is currently used for building sub-panel arrays.
     *
     * @param string $query - the query that should be executed to build the list
     * @param object $template - The object that should be used to copy the records.
     * @param int $row_offset Optional, default 0
     * @param int $limit Optional, default -1
     * @return array
     */
    public function build_related_list($query, &$template, $row_offset = 0, $limit = -1)
    {
        $GLOBALS['log']->debug("Finding linked records $this->object_name: " . $query);
        $db = DBManagerFactory::getInstance('listviews');

        if (!empty($row_offset) && $row_offset != 0 && !empty($limit) && $limit != -1) {
            $result = $db->limitQuery(
                $query,
                $row_offset,
                $limit,
                true,
                "Error retrieving $template->object_name list: "
            );
        } else {
            $result = $db->query($query, true);
        }

        $list = array();
        $isFirstTime = true;
        $class = get_class($template);
        while ($row = $this->db->fetchByAssoc($result)) {
            if (!$isFirstTime) {
                $template = new $class();
            }
            $isFirstTime = false;
            $record = $template->retrieve($row['id']);

            if ($record != null) {
                // this copies the object into the array
                $list[] = $template;
            }
        }
        return $list;
    }
    /* END - SECURITY GROUPS */

    /**
     * This function is used to execute the query and create an array template objects
     * from the resulting ids from the query.
     * It is currently used for building sub-panel arrays. It supports an additional
     * where clause that is executed as a filter on the results
     *
     * @param string $query - the query that should be executed to build the list
     * @param object $template - The object that should be used to copy the records.
     * @param string $where
     * @param string $in
     * @param $order_by
     * @param string $limit
     * @param int $row_offset
     * @return array
     */
    public function build_related_list_where(
        $query,
        &$template,
        $where = '',
        $in = '',
        $order_by = '',
        $limit = '',
        $row_offset = 0
    ) {
        $db = DBManagerFactory::getInstance('listviews');
        // No need to do an additional query
        $GLOBALS['log']->debug("Finding linked records $this->object_name: " . $query);
        if (empty($in) && !empty($query)) {
            $idList = $this->build_related_in($query);
            $in = $idList['in'];
        }
        // MFH - Added Support For Custom Fields in Searches
        $custom_join = $this->getCustomJoin();

        $query = "SELECT id ";

        $query .= $custom_join['select'];
        $query .= " FROM $this->table_name ";

        $query .= $custom_join['join'];

        $query .= " WHERE deleted=0 AND id IN $in";
        if (!empty($where)) {
            $query .= " AND $where";
        }


        if (!empty($order_by)) {
            $query .= "ORDER BY $order_by";
        }
        if (!empty($limit)) {
            $result = $db->limitQuery($query, $row_offset, $limit, true, "Error retrieving $this->object_name list: ");
        } else {
            $result = $db->query($query, true);
        }

        $list = array();
        $isFirstTime = true;
        $class = get_class($template);

        $disable_security_flag = $template->disable_row_level_security;
        while ($row = $db->fetchByAssoc($result)) {
            if (!$isFirstTime) {
                $template = new $class();
                $template->disable_row_level_security = $disable_security_flag;
            }
            $isFirstTime = false;
            $record = $template->retrieve($row['id']);
            if ($record != null) {
                // this copies the object into the array
                $list[] = $template;
            }
        }

        return $list;
    }

    /**
     * Constructs an comma separated list of ids from passed query results.
     *
     * @param string @query query to be executed.
     * @return array
     *
     */
    public function build_related_in($query)
    {
        $idList = array();
        $result = $this->db->query($query, true);
        $ids = '';
        while ($row = $this->db->fetchByAssoc($result)) {
            $idList[] = $row['id'];
            if (empty($ids)) {
                $ids = "('" . $row['id'] . "'";
            } else {
                $ids .= ",'" . $row['id'] . "'";
            }
        }
        if (empty($ids)) {
            $ids = "('')";
        } else {
            $ids .= ')';
        }

        return array('list' => $idList, 'in' => $ids);
    }

    /**
     * Optionally copies values from fetched row into the bean.
     *
     * Internal function, do not override.
     *
     * @param string $query - the query that should be executed to build the list
     * @param object $template - The object that should be used to copy the records
     * @param array $field_list List of  fields.
     * @return array
     */
    public function build_related_list2($query, &$template, &$field_list)
    {
        $GLOBALS['log']->debug("Finding linked values $this->object_name: " . $query);

        $result = $this->db->query($query, true);

        $list = array();
        $isFirstTime = true;
        $class = get_class($template);
        while ($row = $this->db->fetchByAssoc($result)) {
            // Create a blank copy
            $copy = $template;
            if (!$isFirstTime) {
                $copy = new $class();
            }
            $isFirstTime = false;
            foreach ($field_list as $field) {
                // Copy the relevant fields
                $copy->$field = $row[$field];
            }

            // this copies the object into the array
            $list[] = $copy;
        }

        return $list;
    }

    /**
     * Let implementing classes to fill in row specific columns of a list view form
     * @param $list_form
     */
    public function list_view_parse_additional_sections(&$list_form)
    {
    }

    /**
     * Override this function to set values in the array used to render list view data.
     *
     */
    public function get_list_view_data()
    {
        return $this->get_list_view_array();
    }

    /**
     * Assigns all of the values into the template for the list view
     *
     * @return array
     */
    public function get_list_view_array()
    {
        static $cache = array();
        // cn: bug 12270 - sensitive fields being passed arbitrarily in listViews
        $sensitiveFields = array('user_hash' => '');

        $return_array = array();
        global $app_list_strings, $mod_strings;
        foreach ($this->field_defs as $field => $value) {
            if (isset($this->$field)) {

                // cn: bug 12270 - sensitive fields being passed arbitrarily in listViews
                if (isset($sensitiveFields[$field])) {
                    continue;
                }
                if (!isset($cache[$field])) {
                    $cache[$field] = strtoupper($field);
                }

                //Fields hidden by Dependent Fields
                if (isset($value['hidden']) && $value['hidden'] === true) {
                    $return_array[$cache[$field]] = "";
                } elseif (((!empty($value['type']) && ($value['type'] == 'enum' || $value['type'] == 'radioenum')))
                    //cn: if $field is a _dom, detect and return VALUE not KEY
                    //cl: empty function check for meta-data enum types that have values loaded from a function
                    && empty($value['function'])) {
                    if (!empty($value['options']) && !empty($app_list_strings[$value['options']][$this->$field])) {
                        $return_array[$cache[$field]] = $app_list_strings[$value['options']][$this->$field];
                    } elseif (!empty($value['options']) && !empty($mod_strings[$value['options']][$this->$field])) {
                        $return_array[$cache[$field]] = $mod_strings[$value['options']][$this->$field];
                    } else {
                        $return_array[$cache[$field]] = $this->$field;
                    }
                } else {
                    $return_array[$cache[$field]] = $this->$field;
                }
                // handle "Assigned User Name"
                if ($field == 'assigned_user_name') {
                    $return_array['ASSIGNED_USER_NAME'] = get_assigned_user_name($this->assigned_user_id);
                }
            }
        }
        return $return_array;
    }

    /**
     * Constructs a select query and fetch 1 row using this query, and then process the row
     *
     * Internal function, do not override.
     * @param array @fields_array  array of name value pairs used to construct query.
     * @param bool $encode Optional, default true, encode fetched data.
     * @param bool $deleted Optional, default true, if set to false deleted filter will not be added.
     * @return object Instance of this bean with fetched data.
     */
    public function retrieve_by_string_fields($fields_array, $encode = true, $deleted = true)
    {
        $where_clause = $this->get_where($fields_array, $deleted);
        $custom_join = $this->getCustomJoin();
        $query = "SELECT $this->table_name.*" . $custom_join['select'] .
            " FROM $this->table_name " . $custom_join['join'];
        $query .= " $where_clause";
        $GLOBALS['log']->debug("Retrieve $this->object_name: " . $query);
        $result = $this->db->limitQuery($query, 0, 1, true, "Retrieving record $where_clause:");

        if (empty($result)) {
            return null;
        }
        $row = $this->db->fetchByAssoc($result, $encode);
        if (empty($row)) {
            return null;
        }
        // Removed getRowCount-if-clause earlier and insert duplicates_found here
        // as it seems that we have found something
        // if we didn't return null in the previous clause.
        $this->duplicates_found = true;
        $row = $this->convertRow($row);
        $this->fetched_row = $row;
        $this->fromArray($row);
        $this->is_updated_dependent_fields = false;
        $this->fill_in_additional_detail_fields();
        return $this;
    }

    /**
     * Construct where clause from a list of name-value pairs.
     * @param array $fields_array Name/value pairs for column checks
     * @param bool $deleted Optional, default true, if set to false deleted filter will not be added.
     * @return string The WHERE clause
     */
    public function get_where($fields_array, $deleted = true)
    {
        $where_clause = "";
        foreach ($fields_array as $name => $value) {
            if (!empty($where_clause)) {
                $where_clause .= " AND ";
            }
            $name = $this->db->getValidDBName($name);

            $where_clause .= "$name = " . $this->db->quoted($value);
        }
        if (!empty($where_clause)) {
            if ($deleted) {
                return "WHERE $where_clause AND deleted=0";
            }
            return "WHERE $where_clause";
        }
        return "";
    }

    /**
     * Converts an array into an acl mapping name value pairs into files
     *
     * @param array $arr
     */
    public function fromArray($arr)
    {
        foreach ($arr as $name => $value) {
            $this->$name = $value;
        }
    }

    /**
     * This method is called during an import before inserting a bean
     * Define an associative array called $special_fields
     * the keys are user defined, and don't directly map to the bean's fields
     * the value is the method name within that bean that will do extra
     * processing for that field. example: 'full_name'=>'get_names_from_full_name'
     *
     */
    public function process_special_fields()
    {
        foreach ($this->special_functions as $func_name) {
            if (method_exists($this, $func_name)) {
                $this->$func_name();
            }
        }
    }

    /**
     * Override this function to build a where clause based on the search criteria set into bean .
     * @abstract
     * @param $value
     */
    public function build_generic_where_clause($value)
    {
    }

    /**
     * @param $list_form
     * @param $xTemplateSection
     *
     * @return mixed
     */
    public function &parse_additional_headers(&$list_form, $xTemplateSection)
    {
        return $list_form;
    }

    /**
     * @param $currentModule
     */
    public function assign_display_fields($currentModule)
    {
        global $timedate;
        foreach ($this->column_fields as $field) {
            if (isset($this->field_name_map[$field]) && empty($this->$field)) {
                if ($this->field_name_map[$field]['type'] != 'date'
                    && $this->field_name_map[$field]['type'] != 'enum') {
                    $this->$field = $field;
                }
                if ($this->field_name_map[$field]['type'] == 'date') {
                    $this->$field = $timedate->to_display_date('1980-07-09');
                }
                if ($this->field_name_map[$field]['type'] == 'enum') {
                    $dom = $this->field_name_map[$field]['options'];
                    global $current_language, $app_list_strings;
                    $mod_strings = return_module_language($current_language, $currentModule);

                    if (isset($mod_strings[$dom])) {
                        $options = $mod_strings[$dom];
                        foreach ($options as $key => $value) {
                            if (!empty($key) && empty($this->$field)) {
                                $this->$field = $key;
                            }
                        }
                    }
                    if (isset($app_list_strings[$dom])) {
                        $options = $app_list_strings[$dom];
                        foreach ($options as $key => $value) {
                            if (!empty($key) && empty($this->$field)) {
                                $this->$field = $key;
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * @param $table
     * @param $relate_values
     * @param bool $check_duplicates
     * @param bool $do_update
     * @param null $data_values
     */
    public function set_relationship(
        $table,
        $relate_values,
        $check_duplicates = true,
        $do_update = false,
        $data_values = null
    ) {
        $where = '';

        // make sure there is a date modified
        $date_modified = $this->db->convert("'" . $GLOBALS['timedate']->nowDb() . "'", 'datetime');

        $row = null;
        if ($check_duplicates) {
            $query = "SELECT * FROM $table ";
            $where = "WHERE deleted = '0'  ";
            foreach ($relate_values as $name => $value) {
                $where .= " AND $name = '$value' ";
            }
            $query .= $where;
            $result = $this->db->query($query, false, "Looking For Duplicate Relationship:" . $query);
            $row = $this->db->fetchByAssoc($result);
        }

        if (!$check_duplicates || empty($row)) {
            unset($relate_values['id']);
            if (isset($data_values)) {
                $relate_values = array_merge($relate_values, $data_values);
            }
            $query = "INSERT INTO $table (id, " . implode(',', array_keys($relate_values)) . ", date_modified) " .
                "VALUES ('" . create_guid() . "', " . "'" .
                implode("', '", $relate_values) . "', " . $date_modified . ")";

            $this->db->query($query, false, "Creating Relationship:" . $query);
        } elseif ($do_update) {
            $conds = array();
            foreach ($data_values as $key => $value) {
                array_push($conds, $key . "='" . $this->db->quote($value) . "'");
            }
            $query = "UPDATE $table SET " . implode(',', $conds) . ",date_modified=" . $date_modified . " " . $where;
            $this->db->query($query, false, "Updating Relationship:" . $query);
        }
    }

    /**
     * @param $table
     * @param $values
     * @param $select_id
     *
     * @return array
     */
    public function retrieve_relationships($table, $values, $select_id)
    {
        $query = "SELECT $select_id FROM $table WHERE deleted = 0  ";
        foreach ($values as $name => $value) {
            $query .= " AND $name = '$value' ";
        }
        $query .= " ORDER BY $select_id ";
        $result = $this->db->query($query, false, "Retrieving Relationship:" . $query);
        $ids = array();
        while ($row = $this->db->fetchByAssoc($result)) {
            $ids[] = $row;
        }
        return $ids;
    }

    public function loadLayoutDefs()
    {
        global $layout_defs;
        if (empty($this->layout_def) && file_exists('modules/' . $this->module_dir . '/layout_defs.php')) {
            include_once('modules/' . $this->module_dir . '/layout_defs.php');
            if (file_exists('custom/modules/' . $this->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php')) {
                include_once('custom/modules/' . $this->module_dir . '/Ext/Layoutdefs/layoutdefs.ext.php');
            }
            if (empty($layout_defs[get_class($this)])) {
                echo "\$layout_defs[" . get_class($this) . "]; does not exist";
            }

            $this->layout_def = $layout_defs[get_class($this)];
        }
    }

    /**
     * @param $name
     *
     * @return bool
     */
    public function getRealKeyFromCustomFieldAssignedKey($name)
    {
        if ($this->custom_fields->avail_fields[$name]['ext1']) {
            $realKey = 'ext1';
        } elseif ($this->custom_fields->avail_fields[$name]['ext2']) {
            $realKey = 'ext2';
        } elseif ($this->custom_fields->avail_fields[$name]['ext3']) {
            $realKey = 'ext3';
        } else {
            $GLOBALS['log']->fatal("SUGARBEAN: cannot find Real Key for custom field of type dropdown - " .
                "cannot return Value.");
            return false;
        }
        if (isset($realKey)) {
            return $this->custom_fields->avail_fields[$name][$realKey];
        }
    }

    /**
     * Get owner field
     *
     * @param bool $returnFieldName
     * @return string
     */
    public function getOwnerField($returnFieldName = false)
    {
        if (isset($this->field_defs['assigned_user_id'])) {
            return $returnFieldName ? 'assigned_user_id' : $this->assigned_user_id;
        }

        if (isset($this->field_defs['created_by'])) {
            return $returnFieldName ? 'created_by' : $this->created_by;
        }

        return '';
    }

    /**
     *
     * Used in order to manage ListView links and if they should
     * links or not based on the ACL permissions of the user
     *
     * @return string[]
     */
    public function listviewACLHelper()
    {
        $array_assign = array();
        if ($this->ACLAccess('DetailView')) {
            $array_assign['MAIN'] = 'a';
        } else {
            $array_assign['MAIN'] = 'span';
        }
        return $array_assign;
    }

    /**
     * Check whether the user has access to a particular view for the current bean/module
     * @param string $view required, the view to determine access for i.e. DetailView, ListView...
     * @param bool|string $is_owner bool optional, this is part of the ACL check if the current user
     * is an owner they will receive different access
     * @param bool|string $in_group
     * @return bool
     */
    public function ACLAccess($view, $is_owner = 'not_set', $in_group = 'not_set')
    {
        global $current_user;
        if ($current_user->isAdmin() || !$this->bean_implements('ACL')) {
            return true;
        }
        $view = strtolower($view);
        switch ($view) {
            case 'list':
            case 'index':
            case 'listview':
                $view = "list";
                break;
            case 'edit':
            case 'save':
            case 'popupeditview':
            case 'editview':
                $view = "edit";
                break;
            case 'view':
            case 'detail':
            case 'detailview':
                $view = "view";
                break;
            case 'delete':
                $view = "delete";
                break;
            case 'export':
                $view = "export";
                break;
            case 'import':
                $view = "import";
                $is_owner = true;
                break;
            default:
                return true;
        }
        if ($is_owner === 'not_set') {
            $is_owner = $this->isOwner($current_user->id);
            if ($view == 'edit' && !$is_owner && !empty($this->id)) {
                $class = get_class($this);
                $temp = new $class();
                if (!empty($this->fetched_row) && !empty($this->fetched_row['id'])
                    && !empty($this->fetched_row['assigned_user_id']) && !empty($this->fetched_row['created_by'])) {
                    $temp->populateFromRow($this->fetched_row);
                } else {
                    $temp->retrieve($this->id);
                }
                $is_owner = $temp->isOwner($current_user->id);
            }
        }
        if ($in_group === 'not_set') {
            require_once("modules/SecurityGroups/SecurityGroup.php");
            $in_group = SecurityGroup::groupHasAccess($this->module_dir, $this->id, $view);
        }

        $args = new stdClass();
        $args->view = $view;
        $args->is_owner = $is_owner;
        $args->in_group = $in_group;
        $args->access = true;
        $args->override_acl_check = false;

        $this->call_custom_logic('before_acl_check', $args);

        if ($args->override_acl_check) {
            return $args->access;
        }

        return $args->access
            && ACLController::checkAccess(
                $this->module_dir,
                $args->view,
                $args->is_owner,
                $this->acltype,
                $args->in_group
            );
    }

    /**
     * Loads a row of data into instance of a bean. The data is passed as an array to this function
     *
     * @param array $arr row of data fetched from the database.
     *
     * Internal function do not override.
     */
    public function loadFromRow($arr)
    {
        $this->populateFromRow($arr);
        $this->processed_dates_times = array();
        $this->check_date_relationships_load();

        $this->fill_in_additional_list_fields();

        if ($this->hasCustomFields()) {
            $this->custom_fields->fill_relationships();
        }
        $this->call_custom_logic("process_record");
    }

    /**
     * Ensure that fields within order by clauses are properly qualified with
     * their tablename.  This qualification is a requirement for sql server support.
     *
     * @param string $order_by original order by from the query
     * @param string $qualify prefix for columns in the order by list.
     * @return string prefixed
     *
     * Internal function do not override.
     */
    public function create_qualified_order_by($order_by, $qualify)
    {
        // if the column is empty, but the sort order is defined,
        // the value will throw an error, so do not proceed if no order by is given
        if (empty($order_by)) {
            return $order_by;
        }
        $order_by_clause = " ORDER BY ";
        $tmp = explode(",", $order_by);
        $comma = ' ';
        foreach ($tmp as $stmp) {
            $stmp = (substr_count($stmp, ".") > 0 ? trim($stmp) : "$qualify." . trim($stmp));
            $order_by_clause .= $comma . $stmp;
            $comma = ", ";
        }
        return $order_by_clause;
    }

    /**
     * Combined the contents of street field 2 through 4 into the main field
     *
     * @param string $street_field
     */

    public function add_address_streets(
        $street_field
    ) {
        if (isset($this->$street_field)) {
            $street_field_2 = $street_field . '_2';
            $street_field_3 = $street_field . '_3';
            $street_field_4 = $street_field . '_4';
            if (isset($this->$street_field_2)) {
                $this->$street_field .= "\n" . $this->$street_field_2;
                unset($this->$street_field_2);
            }
            if (isset($this->$street_field_3)) {
                $this->$street_field .= "\n" . $this->$street_field_3;
                unset($this->$street_field_3);
            }
            if (isset($this->$street_field_4)) {
                $this->$street_field .= "\n" . $this->$street_field_4;
                unset($this->$street_field_4);
            }
            $this->$street_field = trim($this->$street_field, "\n");
        }
    }

    /**
     * Called from ImportFieldSanitize::relate(), when creating a new bean in a related module. Will
     * copies fields over from the current bean into the related. Designed to be overridden in child classes.
     *
     * @param SugarBean $new_bean newly created related bean
     */
    public function populateRelatedBean(
        SugarBean $new_bean
    ) {
    }

    /**
     * Called during the import process before a bean save, to handle any needed pre-save logic when
     * importing a record
     */
    public function beforeImportSave()
    {
    }

    /**
     * Called during the import process after a bean save, to handle any needed post-save logic when
     * importing a record
     */
    public function afterImportSave()
    {
    }

    /**
     * Returns the query used for the export functionality for a module. Override this method if you wish
     * to have a custom query to pull this data together instead
     *
     * @param string $order_by
     * @param string $where
     * @return string SQL query
     */
    public function create_export_query($order_by, $where)
    {
        return $this->create_new_list_query($order_by, $where, array(), array(), 0, '', false, $this, true, true);
    }

    /**
     * Checks auditing is enables and then carrys out an audit on the current bean.
     *
     * @param $isUpdate
     */
    public function auditBean($isUpdate)
    {
        if ($this->is_AuditEnabled() && $isUpdate && !$this->createdAuditRecords) {
            $auditDataChanges = $this->db->getAuditDataChanges($this);

            if (!empty($auditDataChanges)) {
                $this->createAuditRecord($auditDataChanges);
            } else {
                $GLOBALS['log']->debug('Auditing: createAuditRecord was not called, audit record will not be created.');
            }
        }
    }

    /**
     * Takes the audit changes array and creates entries in audit table.
     * Reset's the bean fetched row so changes are not duplicated.
     *
     * @param array $auditDataChanges
     */
    protected function createAuditRecord(array $auditDataChanges)
    {
        foreach ($auditDataChanges as $change) {
            $this->db->save_audit_records($this, $change);
            $this->fetched_row[$change['field_name']] = $change['after'];
        }
        $this->createdAuditRecords = true;
    }
}
