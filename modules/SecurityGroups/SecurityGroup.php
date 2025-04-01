<?php

require_once 'modules/SecurityGroups/SecurityGroup_sugar.php';

#[\AllowDynamicProperties]
class SecurityGroup extends SecurityGroup_sugar
{
    /**
     * SecurityGroup constructor.
     */
    public function __construct()
    {
        parent::__construct();
    }

    public $last_run = array('module' => '', 'record' => '', 'action' => '', 'response' => '');

    /**
     * Gets the join statement used for returning all rows in a list view that a user has group rights to.
     * Make sure any use of this also return records that the user has owner access to.
     * (e.g. caller uses getOwnerWhere as well).
     *
     * @param string $table_name
     * @param string $module
     * @param string $user_id
     * @return string
     */
    public static function getGroupWhere($table_name, $module, $user_id)
    {
        $db = DBManagerFactory::getInstance();
        $quotedUserId = $db->quote($user_id);

        //need a different query if doing a securitygroups check
        if ($module == 'SecurityGroups') {
            return " $table_name.id in (
                select secg.id from securitygroups secg
                inner join securitygroups_users secu on secg.id = secu.securitygroup_id and secu.deleted = 0
                    and secu.user_id = '$quotedUserId'
                where secg.deleted = 0
            )";
        } else {
            return " EXISTS (SELECT  1
                  FROM    securitygroups secg
                          INNER JOIN securitygroups_users secu
                            ON secg.id = secu.securitygroup_id
                               AND secu.deleted = 0
                               AND secu.user_id = '$quotedUserId'
                          INNER JOIN securitygroups_records secr
                            ON secg.id = secr.securitygroup_id
                               AND secr.deleted = 0
                               AND secr.module = '$module'
                       WHERE   secr.record_id = " . $table_name . '.id
                               AND secg.deleted = 0) ';
        }
    }

    /**
     * Gets the join statement used for returning all users that a given user is in the same group with.
     *
     * @param string $user_id
     *
     * @return string
     */
    public static function getGroupUsersWhere($user_id)
    {
        $db = DBManagerFactory::getInstance();
        $quotedUserId = $db->quote($user_id);

        return " users.id in (
            select sec.user_id from securitygroups_users sec
            inner join securitygroups_users secu on sec.securitygroup_id = secu.securitygroup_id and secu.deleted = 0
                and secu.user_id = '$quotedUserId'
            where sec.deleted = 0
        )";
    }

    /**
     * Gets the join statement used for returning all rows in a list view that a user has group rights to.
     * Make sure any use of this also return records that the user has owner access to.
     * (e.g. caller uses getOwnerWhere as well).
     *
     * NOTE: Make sure to add the check in the where clause for ($table_name.assigned_user_id or securitygroup_join.record_id is not null)
     *
     * @param string $table_name
     * @param string $module
     * @param string $user_id
     *
     * @return string
     */
    public static function getGroupJoin($table_name, $module, $user_id)
    {
        $db = DBManagerFactory::getInstance();
        $quotedUserId = $db->quote($user_id);

        //need a different query if doing a securitygroups check
        if ($module == 'SecurityGroups') {
            return " LEFT JOIN (select distinct secg.id from securitygroups secg
    inner join securitygroups_users secu on secg.id = secu.securitygroup_id and secu.deleted = 0
            and secu.user_id = '" . $quotedUserId . "'
    where secg.deleted = 0
) securitygroup_join on securitygroup_join.id = " . $table_name . '.id ';
        } else {
            return " LEFT JOIN (select distinct secr.record_id as id from securitygroups secg
    inner join securitygroups_users secu on secg.id = secu.securitygroup_id and secu.deleted = 0
            and secu.user_id = '" . $quotedUserId . "'
    inner join securitygroups_records secr on secg.id = secr.securitygroup_id and secr.deleted = 0
             and secr.module = '" . $module . "'
    where secg.deleted = 0
) securitygroup_join on securitygroup_join.id = " . $table_name . '.id ';
        }
    }

    /**
     * Gets the join statement used for returning all users that a given user is in the same group with.
     *
     * @param string $user_id
     *
     * @return string
     */
    public static function getGroupUsersJoin($user_id)
    {
        $db = DBManagerFactory::getInstance();
        $quotedUserId = $db->quote($user_id);

        return " LEFT JOIN (
            select distinct sec.user_id as id from securitygroups_users sec
            inner join securitygroups_users secu on sec.securitygroup_id = secu.securitygroup_id and secu.deleted = 0
                and secu.user_id = '$quotedUserId'
            where sec.deleted = 0
        ) securitygroup_join on securitygroup_join.id = users.id ";
    }

    /**
     * @param string $module
     * @param string $id
     * @param string $action
     * @return bool true if group is assigned to the record
     */
    public static function groupHasAccess($module, $id, $action = '')
    {
        if (!isset($id) || $id == '[SELECT_ID_LIST]') {
            return true; //means that this is a listview and everybody is an owner of the listview
        }

        global $current_user;
        global $sugar_config;
        $db = DBManagerFactory::getInstance();
        $quotedId = $db->quote($id);
        $query = 'select count(securitygroups.id) as results from securitygroups '
            . 'inner join securitygroups_users on securitygroups.id = securitygroups_users.securitygroup_id'
            . ' and securitygroups_users.deleted = 0 '
            . "  and securitygroups_users.user_id = '$current_user->id' "
            . 'inner join securitygroups_records on securitygroups.id = securitygroups_records.securitygroup_id'
            . ' and securitygroups_records.deleted = 0 '
            . "  and securitygroups_records.record_id = '$quotedId' "
            . "  and securitygroups_records.module = '$module' ";
        if (!empty($action)
            && isset($sugar_config['securitysuite_strict_rights'])
            && $sugar_config['securitysuite_strict_rights'] == true
        ) {
            $query .= ' inner join securitygroups_acl_roles on securitygroups.id = securitygroups_acl_roles.securitygroup_id and securitygroups_acl_roles.deleted = 0'
                . ' inner join acl_roles_actions on securitygroups_acl_roles.role_id = acl_roles_actions.role_id and acl_roles_actions.deleted = 0 '
                . ' inner join acl_actions on acl_actions.id = acl_roles_actions.action_id and acl_actions.deleted = 0 '
                . " and acl_actions.category = '$module' "
                . " and acl_actions.name = '$action' ";
        }
        $query .= 'where securitygroups.deleted = 0 ';

        if (!empty($action)
            && isset($sugar_config['securitysuite_strict_rights'])
            && $sugar_config['securitysuite_strict_rights'] == true
        ) {
            $query .= ' and acl_roles_actions.access_override = 80  ';
        }
        $GLOBALS['log']->debug("SecuritySuite: groupHasAccess $query");
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        if (isset($row) && $row['results'] > 0) {
            return true;
        }

        return false;
    }

    /**
     * @param SugarBean $focus
     * @param boolean $isUpdate
     */
    public static function inherit($focus, $isUpdate)
    {
        global $sugar_config;
        self::assign_default_groups($focus, $isUpdate); //this must be first because it does not check for dups

        self::inherit_assigned($focus);
        self::inherit_parent($focus, $isUpdate);

        //don't do creator inheritance if popup selector method is chosen and a user is making the request...
        //don't if saving from a popup (subpanel_field_name check. Save2 is the action but to be safe use the subpanel check)
        if (
            (isset($sugar_config['securitysuite_popup_select']) && $sugar_config['securitysuite_popup_select'] == true
                && isset($_REQUEST['action']) && $_REQUEST['action'] == 'Save')
            || (!empty($_REQUEST['subpanel_field_name']))
        ) {
            //check to see if a member of more than 1 group...if not then just inherit the one.
            //Otherwise, this is taken on the edit view on create now
            $security_modules = self::getSecurityModules();
            if (array_key_exists($focus->module_dir, $security_modules)) {
                //check if user is in more than 1 group. If so then set the session var otherwise inherit it's only group
                global $current_user;

                $memberships = self::getMembershipCount($current_user->id);
                if ($memberships > 1) {
                    return;
                }
            }
        }
        self::inherit_creator($focus, $isUpdate);
    }

    /**
     * @param SugarBean $focus
     * @param boolean $isUpdate
     */
    public static function assign_default_groups($focus, $isUpdate)
    {
        if (!$isUpdate) {
            //inherit only for those that support Security Groups
            $security_modules = self::getSecurityModules();
            if (!array_key_exists($focus->module_dir, $security_modules)) {
                return;
            }

            $defaultGroups = self::retrieveDefaultGroups();
            foreach ($defaultGroups as $default_id => $defaultGroup) {
                if ($defaultGroup['module'] == 'All' || $defaultGroup['module'] == $focus->module_dir) {
                    if ($focus->module_dir == 'Users') {
                        $query = 'insert into securitygroups_users(id,date_modified,deleted,securitygroup_id,user_id,noninheritable) '
                            . "select distinct '" . create_guid() . "'," . $focus->db->convert(
                                '',
                                'today'
                            ) . ",0,g.id,'$focus->id',1 "
                            . 'from securitygroups g '
                            . "left join securitygroups_users d on d.securitygroup_id = g.id and d.user_id = '$focus->id' and d.deleted = 0 "
                            . "where d.id is null and g.id = '" . $defaultGroup['securitygroup_id'] . "' and g.deleted = 0 ";
                    } else {
                        $query = 'insert into securitygroups_records(id,securitygroup_id,record_id,module,date_modified,deleted) '
                            . "select distinct '" . create_guid() . "',g.id,'$focus->id','$focus->module_dir',"
                            . $focus->db->convert('', 'today') . ',0 '
                            . 'from securitygroups g '
                            . "left join securitygroups_records d on d.securitygroup_id = g.id and d.record_id = '$focus->id' and d.module = '$focus->module_dir' and d.deleted = 0 "
                            . "where d.id is null and g.id = '" . $defaultGroup['securitygroup_id'] . "' and g.deleted = 0 ";
                    }
                    $GLOBALS['log']->debug("SecuritySuite: Assign Default Groups: $query");
                    $focus->db->query($query, true);
                }
            } //end foreach default group
        }
    }

    /**
     * @param SugarBean $focus
     * @param boolean $isUpdate
     */
    public static function inherit_creator($focus, $isUpdate)
    {
        global $sugar_config;
        global $current_user;
        if (!$isUpdate && isset($sugar_config['securitysuite_inherit_creator']) && $sugar_config['securitysuite_inherit_creator'] == true) {
            if (isset($_SESSION['portal_id']) && isset($_SESSION['user_id'])) {
                return; //don't inherit if from portal
            }

            //inherit only for those that support Security Groups
            $security_modules = self::getSecurityModules();

            if (array_key_exists($focus->module_dir, $security_modules)) {
                $query = 'INSERT INTO securitygroups_records(id,securitygroup_id,record_id,module,date_modified,deleted) '
                    . 'SELECT DISTINCT ';
                if ($focus->db->dbType == 'mysql') {
                    $query .= ' uuid() ';
                } elseif ($focus->db->dbType == 'mssql') {
                    $query .= ' lower(newid()) ';
                }
                $currentUserId = isset($current_user->id) ? $focus->db->quote($current_user->id) : null;
                $recordId =  $focus->db->quote($focus->id);
                $query .= ",u.securitygroup_id,'$recordId','$focus->module_dir',"
                    . $focus->db->convert('', 'today') . ',0 '
                    . 'from securitygroups_users u '
                    . 'inner join securitygroups g on u.securitygroup_id = g.id and g.deleted = 0 and (g.noninheritable is null or g.noninheritable <> 1) '
                    . "left join securitygroups_records d on d.securitygroup_id = u.securitygroup_id and d.record_id = '$recordId' and d.module = '$focus->module_dir' and d.deleted = 0 "
                    . "where d.id is null and u.user_id = '$currentUserId' and u.deleted = 0 and (u.noninheritable is null or u.noninheritable <> 1)";
                $GLOBALS['log']->debug("SecuritySuite: Inherit from Creator: $query");
                $focus->db->query($query, true);
            }
        }
    }

    /**
     * @param SugarBean $focus
     */
    public static function inherit_assigned($focus)
    {
        global $sugar_config;
        if (isset($sugar_config['securitysuite_inherit_assigned']) && $sugar_config['securitysuite_inherit_assigned'] == true) {
            if (!empty($focus->assigned_user_id)) {
                $assigned_user_id = $focus->db->quote($focus->assigned_user_id);
                //inherit only for those that support Security Groups
                $security_modules = self::getSecurityModules();
                //if(in_array($focus->module_dir,$security_modules)) {
                if (array_key_exists($focus->module_dir, $security_modules)) {
                    $query = 'INSERT INTO securitygroups_records(id,securitygroup_id,record_id,module,date_modified,deleted) '
                        . 'SELECT DISTINCT ';
                    if ($focus->db->dbType == 'mysql') {
                        $query .= ' uuid() ';
                    } elseif ($focus->db->dbType == 'mssql') {
                        $query .= ' lower(newid()) ';
                    }
                    $recordId =  $focus->db->quote($focus->id);
                    $query .= ",u.securitygroup_id,'$recordId','$focus->module_dir',"
                        . $focus->db->convert('', 'today') . ',0 '
                        . 'from securitygroups_users u '
                        . 'inner join securitygroups g on u.securitygroup_id = g.id and g.deleted = 0 and (g.noninheritable is null or g.noninheritable <> 1) '
                        . "left join securitygroups_records d on d.securitygroup_id = u.securitygroup_id and d.record_id = '$recordId' and d.module = '$focus->module_dir' and d.deleted = 0 "
                        . "where d.id is null and u.user_id = '$assigned_user_id' and u.deleted = 0  and (u.noninheritable is null or u.noninheritable <> 1)";
                    $GLOBALS['log']->debug("SecuritySuite: Inherit from Assigned: $query");
                    $focus->db->query($query, true);
                }
            } //if !empty assigned_user_id
        }
    }

    /**
     * @param SugarBean $focus
     * @param boolean $isUpdate
     */
    public static function inherit_parent($focus, $isUpdate)
    {
        global $sugar_config;
        //new record or if update from soap api for cases or bugs
        if (!$isUpdate
            && isset($sugar_config['securitysuite_inherit_parent']) && $sugar_config['securitysuite_inherit_parent'] == true
        ) {
            $focus_module_dir = $focus->module_dir;
            $focus_id = $focus->id;

            //inherit only for those that support Security Groups
            $security_modules = self::getSecurityModules();
            //if(!in_array($focus_module_dir,$security_modules)) {
            if (!array_key_exists($focus_module_dir, $security_modules)) {
                //rost fix2
                return; //don't inherit for this module
            }

            //from subpanel
            //PHP Notice error fix
            $parent_type = '';
            $parent_id = '';

            if (isset($_REQUEST['relate_to']) && isset($_REQUEST['relate_id'])) {
                //relate_to is not guaranteed to be a module name anymore.
                //if it isn't load the relationship and find the module name that way
                if (!array_key_exists($_REQUEST['relate_to'], $security_modules)) {
                    //check to see if relate_to is the relationship name
                    require_once 'modules/Relationships/Relationship.php';
                    $relationship = BeanFactory::newBean('Relationships');
                    $rel_module = $relationship->get_other_module(
                        $_REQUEST['relate_to'],
                        $focus_module_dir,
                        $focus->db
                    );
                    if (isset($rel)) {
                        $parent_type = $rel_module;
                        $parent_id = $_REQUEST['relate_id'];
                    }
                } else {
                    $parent_type = $_REQUEST['relate_to'];
                    $parent_id = $_REQUEST['relate_id'];
                }
            }

            if (isset($_SESSION['portal_id'])) {
                $parent_id = $_SESSION['user_id']; //soap stores contact id in user_id field
                $parent_type = 'Contacts';
            }

            //from activity type creation
            if ((empty($parent_type) || empty($parent_id)) && isset($_REQUEST['parent_type']) && isset($_REQUEST['parent_id'])) {
                $parent_type = $_REQUEST['parent_type'];
                $parent_id = $_REQUEST['parent_id'];
            }

            //full form from subpanel
            if ((empty($parent_type) || empty($parent_id)) && isset($_REQUEST['return_module']) && isset($_REQUEST['return_id'])) {
                $parent_type = $_REQUEST['return_module'];
                $parent_id = $_REQUEST['return_id'];
            }

            /* need to find relate fields...for example for Cases look to see if account_id is set */
            //allow inheritance for all relate field types....iterate through and inherit each related field
            foreach ($focus->field_name_map as $name => $def) {
                if ((!isset($def['type']) || ($def['type'] == 'relate' && isset($def['id_name'])))
                    && isset($def['module']) && strtolower($def['module']) != 'users'
                ) {
                    if (isset($_REQUEST[$def['id_name']])) {
                        $relate_parent_id = $_REQUEST[$def['id_name']];
                        $relate_parent_type = $def['module'];

                        self::inherit_parentQuery(
                            $focus,
                            $relate_parent_type,
                            $relate_parent_id,
                            $focus_id,
                            $focus_module_dir
                        );
                    } elseif (isset($_SESSION['portal_id']) && isset($_SESSION[$def['id_name']])) { //soap account
                        $relate_parent_id = $_SESSION[$def['id_name']];
                        $relate_parent_type = $def['module'];

                        self::inherit_parentQuery(
                            $focus,
                            $relate_parent_type,
                            $relate_parent_id,
                            $focus_id,
                            $focus_module_dir
                        );
                    }
                }
            }

            if (!empty($parent_type) && !empty($parent_id)) {
                self::inherit_parentQuery($focus, $parent_type, $parent_id, $focus_id, $focus_module_dir);
            } //end if parent type/id
        } //end if new record
    }

    /**
     * @param SugarBean $focus
     * @param string $parent_type
     * @param string $parent_id
     * @param string $focus_id
     * @param string $focus_module_dir
     */
    public static function inherit_parentQuery($focus, $parent_type, $parent_id, $focus_id, $focus_module_dir)
    {
        if (empty($parent_type) || empty($parent_id)) {
            return;
        } //no info passed

        /* can speed this up by doing one query */
        //should be just one query but need a unique guid for each insert
        //WE NEED A UNIQUE GUID SO USE THE BUILT IN SQL GUID METHOD
        $query = 'INSERT INTO securitygroups_records(id,securitygroup_id,record_id,module,date_modified,deleted) '
            . 'SELECT DISTINCT ';
        if ($focus->db->dbType == 'mysql') {
            $query .= ' uuid() ';
        } elseif ($focus->db->dbType == 'mssql') {
            $query .= ' lower(newid()) ';
        }
        $query .= ",r.securitygroup_id,'$focus_id','$focus_module_dir'," . $focus->db->convert('', 'today') . ',0 '
            . 'from securitygroups_records r '
            . 'inner join securitygroups g on r.securitygroup_id = g.id and g.deleted = 0 and (g.noninheritable is null or g.noninheritable <> 1) '
            . "left join securitygroups_records d on d.securitygroup_id = r.securitygroup_id and d.record_id = '"
            . $focus->db->quote($focus_id) . "' and d.module = '"
            . $focus->db->quote($focus_module_dir) . "' and d.deleted = 0 "
            . "where d.id is null and r.module = '" . $focus->db->quote($parent_type) . "' "
            . "and r.record_id = '" . $focus->db->quote($parent_id) . "' "
            . 'and r.deleted = 0 ';
        $GLOBALS['log']->debug("SecuritySuite: Inherit from Parent: $query");
        $focus->db->query($query, true);
    }

    /**
     * If user is a member of just one group inherit group for new record
     * returns true if inherit just one else false.
     * @param string $user_id
     * @param string $record_id
     * @param string $module
     * @return boolean
     */
    public static function inheritOne($user_id, $record_id, $module)
    {
        //check to see if in just one group...if so, inherit that group and return true
        $db = DBManagerFactory::getInstance();

        $query = 'select count(securitygroups.id) as results from securitygroups '
            . 'inner join securitygroups_users on securitygroups.id = securitygroups_users.securitygroup_id '
            . ' and securitygroups_users.deleted = 0 '
            . " where securitygroups.deleted = 0 and securitygroups_users.user_id = '$user_id' "
            . '  and (securitygroups.noninheritable is null or securitygroups.noninheritable <> 1) '
            . '  and (securitygroups_users.noninheritable is null or securitygroups_users.noninheritable <> 1) ';
        $GLOBALS['log']->debug("SecuritySuite: Inherit One Pre-Check Qualifier: $query");
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        $recordId = $db->quote($record_id);
        $userId = $db->quote($user_id);
        $moduleName = $db->quote($module);
        if (isset($row) && $row['results'] == 1) {
            $query = 'insert into securitygroups_records(id,securitygroup_id,record_id,module,date_modified,deleted) '
                . "select distinct '" . create_guid() . "',u.securitygroup_id,'$recordId','$moduleName',"
                . $db->convert('', 'today') . ',0 '
                . 'from securitygroups_users u '
                . 'inner join securitygroups g on u.securitygroup_id = g.id and g.deleted = 0 and (g.noninheritable is null or g.noninheritable <> 1) '
                . "left join securitygroups_records d on d.securitygroup_id = u.securitygroup_id and d.record_id = '$recordId' and d.module = '$moduleName' and d.deleted = 0 "
                . "where d.id is null and u.user_id = '$userId' and u.deleted = 0 and (u.noninheritable is null or u.noninheritable <> 1)";
            $GLOBALS['log']->debug("SecuritySuite: Inherit One: $query");
            $db->query($query, true);

            return true;
        }

        return false;
    }

    /**
     * returns # of groups a user is a member of that are inheritable.
     * @param string $user_id
     * @return
     */
    public static function getMembershipCount($user_id)
    {
        $db = DBManagerFactory::getInstance();

        if (!isset($_SESSION['securitygroup_count'])) {
            $query = 'select count(securitygroups.id) as results from securitygroups '
                . 'inner join securitygroups_users on securitygroups.id = securitygroups_users.securitygroup_id '
                . ' and securitygroups_users.deleted = 0 '
                . " where securitygroups.deleted = 0 and securitygroups_users.user_id = '$user_id' "
                . '  and (securitygroups.noninheritable is null or securitygroups.noninheritable <> 1) '
                . '  and (securitygroups_users.noninheritable is null or securitygroups_users.noninheritable <> 1) ';
            $GLOBALS['log']->debug("SecuritySuite: Inherit One Pre-Check Qualifier: $query");
            $result = $db->query($query);
            $row = $db->fetchByAssoc($result);
            if (isset($row)) {
                $_SESSION['securitygroup_count'] = $row['results'];
            }
        }

        return $_SESSION['securitygroup_count'];
    }

    /**
     * @return array
     */
    public static function retrieveDefaultGroups()
    {
        $db = DBManagerFactory::getInstance();

        $default_groups = array();
        $query = 'select securitygroups_default.id, securitygroups.name, securitygroups_default.module, securitygroups_default.securitygroup_id '
            . 'from securitygroups_default '
            . 'inner join securitygroups on securitygroups_default.securitygroup_id = securitygroups.id '
            . 'where securitygroups_default.deleted = 0 and securitygroups.deleted = 0';
        $GLOBALS['log']->debug("SecuritySuite: Retrieve Default Groups: $query");
        $result = $db->query($query);
        while (($row = $db->fetchByAssoc($result)) != null) {
            $default_groups[$row['id']] = array(
                'group' => $row['name'],
                'module' => $row['module'],
                'securitygroup_id' => $row['securitygroup_id']
            );
        }

        return $default_groups;
    }

    /**
     * @param string $group_id
     * @param string $module
     */
    public static function saveDefaultGroup($group_id, $module)
    {
        $db = DBManagerFactory::getInstance();

        $query = 'INSERT INTO securitygroups_default (id, securitygroup_id, module, date_modified, deleted) '
            . 'VALUES ( ';
        if ($db->dbType === 'mysql') {
            $query .= ' uuid() ';
        } elseif ($db->dbType === 'mssql') {
            $query .= ' lower(newid()) ';
        }
        $query .= ",'" . htmlspecialchars($group_id, ENT_QUOTES) . "', '" . htmlspecialchars(
            $module,
                ENT_QUOTES
        ) . "'," . $db->convert('', 'today') . ',0 )';

        $GLOBALS['log']->debug("SecuritySuite: Save Default Group: $query");
        $db->query($query);
    }

    /**
     * @param string $default_id
     */
    public static function removeDefaultGroup($default_id)
    {
        $db = DBManagerFactory::getInstance();

        $query = "DELETE FROM securitygroups_default WHERE id = '" . htmlspecialchars($default_id,
                ENT_QUOTES | ENT_HTML5) . "' ";
        $db->query($query);
    }

    /**
     * Used to get the modules that are tied to security groups.
     * There should be a relationship of some sort in order to tie the two together.
     *
     * This will be used for things such as default groups for modules, etc.
     */
    public static function getSecurityModules()
    {
        global $app_list_strings;

        $security_modules = array();

        //https://www.sugaroutfitters.com/support/securitysuite/496
        //There are some modules that shouldn't ever inherit groups...
        $module_blacklist = array('SchedulersJobs', 'Schedulers', 'Trackers');

        require_once 'modules/Relationships/Relationship.php';
        $rs = BeanFactory::newBean('Relationships');
        $query = "SELECT lhs_module, rhs_module FROM $rs->table_name WHERE deleted=0 AND (lhs_module = 'SecurityGroups' OR rhs_module='SecurityGroups')";
        $GLOBALS['log']->debug("SecuritySuite: Get SecuritySuite Enabled Modules: $query");
        $result = $rs->db->query($query);

        while (($row = $rs->db->fetchByAssoc($result)) != null) {
            if ($row['lhs_module'] === 'SecurityGroups') {
                if (in_array($row['rhs_module'], $module_blacklist)) {
                    continue;
                }

                if (isset($app_list_strings['moduleList'][$row['rhs_module']])) {
                    $security_modules[$row['rhs_module']] = $app_list_strings['moduleList'][$row['rhs_module']];//rost fix
                }
            } else {
                if (in_array($row['lhs_module'], $module_blacklist)) {
                    continue;
                }

                if (isset($app_list_strings['moduleList'][$row['lhs_module']])) {
                    $security_modules[$row['lhs_module']] = $app_list_strings['moduleList'][$row['lhs_module']];
                }
            }
        }

        return $security_modules;
    }

    /** To get the link name used to call load_relationship
     * @param string $this_module
     * @param string $rel_module
     * @return
     */
    public static function getLinkName($this_module, $rel_module)
    {
        $GLOBALS['log']->debug("SecurityGroup->getLinkName this_module: $this_module rel_module: $rel_module");
        include_once 'modules/Relationships/RelationshipHandler.php';
        $db = DBManagerFactory::getInstance();
        $rh = new RelationshipHandler($db, $this_module);
        $rh->process_by_rel_bean($rel_module);
        $rh->build_info();
        $rh->get_rel1_vardef_field_base($rh->base_bean->field_defs);

        return $rh->rel1_vardef_field_base;
    }

    /**
     * Add a Security Group to a record.
     * @param string $module
     * @param string $record_id
     * @param string $securitygroup_id
     */
    public function addGroupToRecord($module, $record_id, $securitygroup_id)
    {
        if (empty($module) || empty($record_id) || empty($securitygroup_id)) {
            return; //missing data
        }
        $db = DBManagerFactory::getInstance();
        $query = 'insert into securitygroups_records(id,securitygroup_id,record_id,module,date_modified,deleted) '
            . "values( '" . create_guid() . "','" . $securitygroup_id . "','$record_id','$module'," . $db->convert(
                '',
                'today'
            ) . ',0) ';
        $GLOBALS['log']->debug("SecuritySuite: addGroupToRecord: $query");
        $db->query($query, true);
    }

    /**
     * Remove a Security Group from a record.
     * @param string $module
     * @param string $record_id
     * @param string $securitygroup_id
     */
    public static function removeGroupFromRecord($module, $record_id, $securitygroup_id)
    {
        if (empty($module) || empty($record_id) || empty($securitygroup_id)) {
            return; //missing data
        }
        $db = DBManagerFactory::getInstance();
        $query = 'update securitygroups_records set deleted = 1, date_modified = ' . $db->convert('', 'today') . ' '
            . "where securitygroup_id = '" . $securitygroup_id . "' and record_id = '$record_id' and module = '$module'";
        $GLOBALS['log']->debug("SecuritySuite: addGroupToRecord: $query");
        $db->query($query, true);
    }

    /**
     * Return a list of groups that this user belongs to.
     * @param string $user_id
     * @return array
     */
    public static function getUserSecurityGroups($user_id)
    {
        $db = DBManagerFactory::getInstance();
        $userId = $db->quote($user_id);
        $query = 'select securitygroups.id, securitygroups.name from securitygroups_users '
            . 'inner join securitygroups on securitygroups_users.securitygroup_id = securitygroups.id '
            . '      and securitygroups.deleted = 0 '
            . "where securitygroups_users.user_id='$userId' and securitygroups_users.deleted = 0 "
            . 'order by securitygroups.name asc ';
        $result = $db->query($query, true, 'Error finding the full membership list for a user: ');

        $group_array = array();
        while (($row = $db->fetchByAssoc($result)) != null) {
            $group_array[$row['id']] = $row;
        }

        return $group_array;
    }

    /**
     * Return a list of all groups.
     */
    public static function getAllSecurityGroups()
    {
        $db = DBManagerFactory::getInstance();
        $query = 'SELECT id, name FROM securitygroups '
            . 'WHERE securitygroups.deleted = 0 '
            . 'ORDER BY name';
        $result = $db->query($query, true, 'Error finding the full membership list for a user: ');

        $group_array = array();
        while (($row = $db->fetchByAssoc($result)) != null) {
            $group_array[$row['id']] = $row;
        }

        return $group_array;
    }

    /**
     * Return a list of all members of a group.
     */
    public function getMembers()
    {
        $db = DBManagerFactory::getInstance();

        $query = 'select users.id, users.user_name, users.first_name, users.last_name '
            . 'from securitygroups '
            . 'inner join securitygroups_users on securitygroups.id = securitygroups_users.securitygroup_id '
            . ' and securitygroups_users.deleted = 0 '
            . 'inner join users on securitygroups_users.user_id = users.id and users.deleted = 0 '
            . " where securitygroups.deleted = 0 and users.employee_status = 'Active' "
            . "  and securitygroups.id = '$this->id' "
            . ' order by users.user_name asc ';
        $GLOBALS['log']->debug("SecuritySuite: getMembers: $query");
        $user_array = array();
        $result = $db->query($query);
        while (($row = $db->fetchByAssoc($result)) != null) {
            $user_array[$row['id']] = $row;
        }

        return $user_array;
    }

    /**
     * For the current user, grab the user's primary group (if none, then first related group).
     *
     * Used in the various MVC views to determine which group layout to load.
     */
    public static function getPrimaryGroupID()
    {
        $primary_group_id = null;
        global $current_user;
        $db = DBManagerFactory::getInstance();
        $query = 'select ';
        if ($db->dbType == 'mssql') {
            $query .= ' top 1 ';
        }
        $userId = $db->quote($current_user->id);
        $query .= "securitygroups.id from securitygroups_users
inner join securitygroups on securitygroups_users.securitygroup_id = securitygroups.id
      and securitygroups.deleted = 0
where securitygroups_users.user_id='" . $userId . "' and securitygroups_users.deleted = 0
order by securitygroups_users.primary_group desc ";
        if ($db->dbType == 'mysql') {
            $query .= ' limit 0,1 ';
        }

        $result = $db->query($query, true, 'Error finding the current users primary group: ');
        if (($row = $db->fetchByAssoc($result)) != null) {
            $primary_group_id = $row['id'];
        }

        return $primary_group_id;
    }

    //used in EditView2 to figure out what the parent security groups are set to
    public static function getParentGroups($focus)
    {
        $parent_groups = [];

        if (empty($_REQUEST['return_module']) || empty($_REQUEST['return_id'])) {
            //not a subpanel create so bounce
            return $parent_groups;
        }

        $parent_type = $_REQUEST['return_module'];
        $parent_id = $_REQUEST['return_id'];

        $parent_bean = self::getParentBean($parent_id, $parent_type);

        if (!empty($parent_bean)) {
            $rel_name = 'SecurityGroups';
            if ($parent_type !== 'Users') {
                $rel_name = self::getLinkName($parent_type, 'SecurityGroups');
            }

            $parent_bean->load_relationship($rel_name);
            if (!empty($parent_bean->$rel_name)) {
                $groups = $parent_bean->$rel_name->getBeans();
                //reorganize to index by id
                if (!empty($groups)) {
                    foreach ($groups as $group) {
                        $parent_groups[$group->id] = $group;
                    }
                }
            }
        }


        return $parent_groups;
    }

    //for displaying on the list, detail, edit views
    public static function getRecordGroups($focus)
    {
        $parent_groups = [];

        if (empty($focus)) {
            return $parent_groups;
        }

        $rel_name = 'SecurityGroups';
        if ($focus->module_dir !== 'Users') {
            $rel_name = self::getLinkName($focus->module_dir, 'SecurityGroups');
        }

        $focus->load_relationship($rel_name);

        if (!empty($focus->$rel_name)) {
            $groups = $focus->$rel_name->getBeans();
            //reorganize to index by id
            if (!empty($groups)) {
                foreach ($groups as $group) {
                    $parent_groups[$group->id] = $group;
                }
            }
        }

        return $parent_groups;
    }

    public static function getParentBean($parent_id, $parent_type)
    {

        if (empty($parent_id) || empty($parent_type)) {
            return false;
        }

        return BeanFactory::getBean($parent_type, $parent_id);
    }
}
