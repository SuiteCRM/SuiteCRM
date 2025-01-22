<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/*
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

//Load all relationship metadata
include_once 'modules/TableDictionary.php';
require_once 'data/BeanFactory.php';

define('REL_LHS', 'LHS');
define('REL_RHS', 'RHS');
define('REL_BOTH', 'BOTH_SIDES');
define('REL_MANY_MANY', 'many-to-many');
define('REL_ONE_MANY', 'one-to-many');
define('REL_ONE_ONE', 'one-to-one');

/**
 * A relationship is between two modules.
 * It contains at least two links.
 * Each link represents a connection from one record to the records linked in this relationship.
 * Links have a context(focus) bean while relationships do not.
 *
 * @api
 */
#[\AllowDynamicProperties]
abstract class SugarRelationship
{
    protected $def;
    protected $lhsLink;
    protected $rhsLink;
    protected $ignore_role_filter = false;
    protected $self_referencing = false; //A relationship is self referencing when LHS module = RHS Module
    public $type;
    public $name;

    /**
     * @var SugarBean[]
     */
    protected static $beansToResave = array();

    /**
     * @param $lhs
     * @param $rhs
     * @param array $additionalFields
     * @return mixed
     */
    abstract public function add($lhs, $rhs, $additionalFields = array());

    /**
     * @abstract
     *
     * @param SugarBean $lhs
     * @param SugarBean $rhs
     *
     * @return bool
     */
    abstract public function remove($lhs, $rhs);

    /**
     * @abstract
     *
     * @param Link2 $link loads the rows for this relationship that match the given link
     * @param array $params
     */
    abstract public function load($link, $params = array());

    /**
     * Gets the query to load a link.
     * This is currently public, but should prob be made protected later.
     * See Link2->getQuery.
     *
     * @abstract
     *
     * @param Link2 $link Object to get query for.
     * @param array $params
     *
     * @return array|string query used to load this relationship
     */
    abstract public function getQuery($link, $params = array());

    /**
     * @abstract
     *
     * @param Link2 $link
     * @param array $params
     * @param bool  $return_array
     *
     * @return array|string the query to join against the related modules table for the given link.
     */
    abstract public function getJoin($link, $params = array(), $return_array = false);

    /**
     * @abstract
     *
     * @param SugarBean $lhs
     * @param SugarBean $rhs
     *
     * @return bool
     */
    abstract public function relationship_exists($lhs, $rhs);

    /**
     * @abstract
     *
     * @return string name of the table for this relationship
     */
    abstract public function getRelationshipTable();

    /**
     * @param Link2 $link removes all the beans associated with this link from the relationship
     *
     * @return bool true if all beans were successfully removed or there
     *              were not related beans, false otherwise
     */
    public function removeAll($link)
    {
        $focus = $link->getFocus();
        $related = $link->getBeans();
        $result = true;
        foreach ($related as $relBean) {
            if (empty($relBean->id)) {
                continue;
            }

            if ($link->getSide() == REL_LHS) {
                $sub_result = $this->remove($focus, $relBean);
            } else {
                $sub_result = $this->remove($relBean, $focus);
            }

            $result = $result && $sub_result;
        }

        return $result;
    }

    /**
     * @param string $rowID id of SugarBean to remove from the relationship
     */
    public function removeById($rowID)
    {
        $this->removeRow(array('id' => $rowID));
    }

    /**
     * @return string name of right hand side module.
     */
    public function getRHSModule()
    {
        return $this->def['rhs_module'];
    }

    /**
     * @return string name of left hand side module.
     */
    public function getLHSModule()
    {
        return $this->def['lhs_module'];
    }

    /**
     * @return string left link in relationship.
     */
    public function getLHSLink()
    {
        return $this->lhsLink;
    }

    /**
     * @return string right link in relationship.
     */
    public function getRHSLink()
    {
        return $this->rhsLink;
    }

    /**
     * @return array names of fields stored on the relationship
     */
    public function getFields()
    {
        return isset($this->def['fields']) ? $this->def['fields'] : array();
    }

    /**
     * @param array $row values to be inserted into the relationship
     *
     * @return bool|resource null if new row was inserted and true if an existing row was updated
     */
    protected function addRow($row)
    {
        $existing = $this->checkExisting($row);
        if (!empty($existing)) { //Update the existing row, overriding the values with those passed in
            return $this->updateRow($existing['id'], array_merge($existing, $row));
        }

        $values = array();
        foreach ($this->getFields() as $def) {
            $field = $def['name'];
            if (isset($row[$field])) {
                $values[$field] = "'{$row[$field]}'";
            }
        }
        $columns = implode(',', array_keys($values));
        $values = implode(',', $values);
        if (!empty($values)) {
            $query = "INSERT INTO {$this->getRelationshipTable()} ($columns) VALUES ($values)";
            DBManagerFactory::getInstance()->query($query);
        }

        return null;
    }

    /**
     * @param string $id id of row to update
     * @param array $values values to insert into row
     *
     * @return resource result of update statement
     */
    protected function updateRow($id, $values)
    {
        $newVals = array();
        //Unset the ID since we are using it to update the row
        if (isset($values['id'])) {
            unset($values['id']);
        }
        foreach ($values as $field => $val) {
            $newVals[] = "$field='$val'";
        }

        $newVals = implode(',', $newVals);

        $query = "UPDATE {$this->getRelationshipTable()} set $newVals WHERE id='$id'";

        return DBManagerFactory::getInstance()->query($query);
    }

    /**
     * Removes one or more rows from the relationship table.
     *
     * @param array $where array of field=>value pairs to match
     *
     * @return bool|resource
     */
    protected function removeRow($where)
    {
        if (empty($where)) {
            return false;
        }

        $date_modified = TimeDate::getInstance()->getNow()->asDb();
        $stringSets = array();
        foreach ($where as $field => $val) {
            $stringSets[] = "$field = '$val'";
        }
        $whereString = 'WHERE '.implode(' AND ', $stringSets);

        $query = "UPDATE {$this->getRelationshipTable()} set deleted=1 , date_modified = '$date_modified' $whereString";

        return DBManagerFactory::getInstance()->query($query);
    }

    /**
     * Checks for an existing row who's keys match the one passed in.
     *
     * @param $row
     *
     * @return array|bool returns false if now row is found, otherwise the row is returned
     */
    protected function checkExisting($row)
    {
        $leftIDName = $this->def['join_key_lhs'];
        $rightIDName = $this->def['join_key_rhs'];
        if (empty($row[$leftIDName]) || empty($row[$rightIDName])) {
            return false;
        }

        $leftID = $row[$leftIDName];
        $rightID = $row[$rightIDName];
        //Check the relationship role as well
        $roleCheck = $this->getRoleWhere();

        $query = "SELECT * FROM {$this->getRelationshipTable()} WHERE $leftIDName='$leftID' AND $rightIDName='$rightID' $roleCheck AND deleted=0";

        $db = DBManagerFactory::getInstance();
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        if (!empty($row)) {
            return $row;
        } else {
            return false;
        }
    }

    /**
     * Gets the relationship role column check for the where clause.
     *
     * @param string $table
     * @param bool   $ignore_role_filter
     *
     * @return string
     */
    protected function getRoleWhere($table = '', $ignore_role_filter = false)
    {
        $ignore_role_filter = $ignore_role_filter || $this->ignore_role_filter;
        $roleCheck = '';
        if (empty($table)) {
            $table = $this->getRelationshipTable();
        }
        if (!empty($this->def['relationship_role_column']) && !empty($this->def['relationship_role_column_value']) && !$ignore_role_filter) {
            if (empty($table)) {
                $roleCheck = " AND $this->relationship_role_column";
            } else {
                $roleCheck = " AND $table.{$this->relationship_role_column}";
            }
            //role column value.
            if (empty($this->def['relationship_role_column_value'])) {
                $roleCheck .= ' IS NULL';
            } else {
                $roleCheck .= " = '$this->relationship_role_column_value'";
            }
        }

        return $roleCheck;
    }

    /**
     * @param SugarBean $focus     base bean the hooks is triggered from
     * @param SugarBean $related   bean being added/removed/updated from relationship
     * @param string    $link_name name of link being triggered
     *
     * @return array base arguments to pass to relationship logic hooks
     */
    protected function getCustomLogicArguments($focus, $related, $link_name)
    {
        $custom_logic_arguments = array();
        $custom_logic_arguments['id'] = $focus->id;
        $custom_logic_arguments['related_id'] = $related->id;
        $custom_logic_arguments['module'] = $focus->module_dir;
        $custom_logic_arguments['related_module'] = $related->module_dir;
        $custom_logic_arguments['related_bean'] = $related;
        $custom_logic_arguments['link'] = $link_name;
        $custom_logic_arguments['relationship'] = $this->name;

        return $custom_logic_arguments;
    }

    /**
     * Call the before add logic hook for a given link.
     *
     * @param SugarBean $focus     base bean the hooks is triggered from
     * @param SugarBean $related   bean being added/removed/updated from relationship
     * @param string    $link_name name of link being triggered
     */
    protected function callBeforeAdd($focus, $related, $link_name = '')
    {
        $custom_logic_arguments = $this->getCustomLogicArguments($focus, $related, $link_name);
        $focus->call_custom_logic('before_relationship_add', $custom_logic_arguments);
    }

    /**
     * Call the after add logic hook for a given link.
     *
     * @param SugarBean $focus     base bean the hooks is triggered from
     * @param SugarBean $related   bean being added/removed/updated from relationship
     * @param string    $link_name name of link being triggered
     */
    protected function callAfterAdd($focus, $related, $link_name = '')
    {
        $custom_logic_arguments = $this->getCustomLogicArguments($focus, $related, $link_name);
        $focus->call_custom_logic('after_relationship_add', $custom_logic_arguments);
    }

    /**
     * @param SugarBean $focus
     * @param SugarBean $related
     * @param string    $link_name
     */
    protected function callBeforeDelete($focus, $related, $link_name = '')
    {
        $custom_logic_arguments = $this->getCustomLogicArguments($focus, $related, $link_name);
        $focus->call_custom_logic('before_relationship_delete', $custom_logic_arguments);
    }

    /**
     * @param SugarBean $focus
     * @param SugarBean $related
     * @param string    $link_name
     */
    protected function callAfterDelete($focus, $related, $link_name = '')
    {
        $custom_logic_arguments = $this->getCustomLogicArguments($focus, $related, $link_name);
        $focus->call_custom_logic('after_relationship_delete', $custom_logic_arguments);
    }

    /**
     * @param $optional_array array clause to add to the where query when populating this relationship. It should be in the
     *
     * @return string
     */
    protected function getOptionalWhereClause($optional_array)
    {
        //lhs_field, operator, and rhs_value must be set in optional_array
        foreach (array('lhs_field', 'operator', 'rhs_value') as $required) {
            if (empty($optional_array[$required])) {
                return '';
            }
        }

        return $optional_array['lhs_field'].''.$optional_array['operator']."'".$optional_array['rhs_value']."'";
    }

    /**
     * Adds a related Bean to the list to be resaved along with the current bean.
     *
     * @static
     *
     * @param SugarBean $bean
     */
    public static function addToResaveList($bean)
    {
        if (!isset(self::$beansToResave[$bean->module_dir])) {
            self::$beansToResave[$bean->module_dir] = array();
        }
        self::$beansToResave[$bean->module_dir][$bean->id] = $bean;
    }

    /**
     * @static
     */
    public static function resaveRelatedBeans()
    {
        $GLOBALS['resavingRelatedBeans'] = true;

        //Resave any bean not currently in the middle of a save operation
        foreach (self::$beansToResave as $module => $beans) {
            foreach ($beans as $bean) {
                if (empty($bean->deleted) && empty($bean->in_save)) {
                    $bean->save();
                } else {
                    // Bug 55942 save the in-save id which will be used to send workflow alert later
                    if (isset($bean->id) && !empty($_SESSION['WORKFLOW_ALERTS'])) {
                        $_SESSION['WORKFLOW_ALERTS']['id'] = $bean->id;
                    }
                }
            }
        }

        $GLOBALS['resavingRelatedBeans'] = false;

        //Reset the list of beans that will need to be resaved
        self::$beansToResave = array();
    }

    /**
     * @return bool true if the relationship is a flex / parent relationship
     */
    public function isParentRelationship()
    {
        //check role fields to see if this is a parent (flex relate) relationship
        if (!empty($this->def['relationship_role_column']) && !empty($this->def['relationship_role_column_value'])
            && $this->def['relationship_role_column'] == 'parent_type' && $this->def['rhs_key'] == 'parent_id'
        ) {
            return true;
        }

        return false;
    }

    /**
     * @param $name
     * @return array|string
     */
    public function __get($name)
    {
        if (isset($this->def[$name])) {
            return $this->def[$name];
        }

        switch ($name) {
            case 'relationship_type':
                return $this->type;
            case 'relationship_name':
                return $this->name;
            case 'lhs_module':
                return $this->getLHSModule();
            case 'rhs_module':
                return $this->getRHSModule();
            case 'lhs_table':
                return isset($this->def['lhs_table']) ? $this->def['lhs_table'] : '';
            case 'rhs_table':
                return isset($this->def['rhs_table']) ? $this->def['rhs_table'] : '';
            case 'list_fields':
                return array('lhs_table', 'lhs_key', 'rhs_module', 'rhs_table', 'rhs_key', 'relationship_type');
        }

        if (isset($this->$name)) {
            return $this->$name;
        }

        return '';
    }
}
