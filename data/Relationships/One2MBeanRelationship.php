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


require_once("data/Relationships/One2MRelationship.php");

/**
 * Represents a one to many relationship that is table based.
 * @api
 */
#[\AllowDynamicProperties]
class One2MBeanRelationship extends One2MRelationship
{
    //Type is read in sugarbean to determine query construction
    public $type = "one-to-many";

    public function __construct($def)
    {
        parent::__construct($def);
    }

    /**
     * @param SugarBean $lhs left side bean to add to the relationship.
     * @param SugarBean $rhs right side bean to add to the relationship.
     * @param mixed $additionalFields key=>value pairs of fields to save on the relationship
     * @return boolean true if successful
     */
    public function add($lhs, $rhs, $additionalFields = array())
    {
        // test to see if the relationship exist if the relationship between the two beans
        // exist then we just fail out with false as we don't want to re-trigger this
        // the save and such as it causes problems with the related() in sugarlogic
        if ($this->relationship_exists($lhs, $rhs) && !empty($GLOBALS['resavingRelatedBeans'])) {
            return false;
        }

        $lhsLinkName = $this->lhsLink;
        $rhsLinkName = $this->rhsLink;

        //Since this is bean based, we know updating the RHS's field will overwrite any old value,
        //But we need to use delete to make sure custom logic is called correctly
        if ($rhs->load_relationship($rhsLinkName)) {
            $oldLink = $rhs->$rhsLinkName;
            $prevRelated = $oldLink->getBeans(null);
            foreach ($prevRelated as $oldLHS) {
                if ($oldLHS->id != $lhs->id) {
                    $this->remove($oldLHS, $rhs, false);
                }
            }
        }

        //Make sure we load the current relationship state to the LHS link
        if ((isset($lhs->$lhsLinkName) && is_a($lhs->$lhsLinkName, "Link2")) || $lhs->load_relationship($lhsLinkName)) {
            $lhs->$lhsLinkName->load();
        }

        if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
            $this->callBeforeAdd($lhs, $rhs);
            $this->callBeforeAdd($rhs, $lhs);
        }

        $this->updateFields($lhs, $rhs, $additionalFields);

        if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
            //Need to call save to update the bean as the relationship is saved on the main table
            //We don't want to create a save loop though, so make sure we aren't already in the middle of saving this bean
            SugarRelationship::addToResaveList($rhs);

            $this->updateLinks($lhs, $lhsLinkName, $rhs, $rhsLinkName);

            $this->callAfterAdd($lhs, $rhs);
            $this->callAfterAdd($rhs, $lhs);
        }

        //One2MBean relationships require that the RHS bean be saved or else the relationship will not be saved.
        //If we aren't already in a relationship save, intitiate a save now.
        if (empty($GLOBALS['resavingRelatedBeans'])) {
            SugarRelationship::resaveRelatedBeans();
        }

        return true;
    }

    protected function updateLinks($lhs, $lhsLinkName, $rhs, $rhsLinkName)
    {
        if (isset($lhs->$lhsLinkName)) {
            $lhs->$lhsLinkName->addBean($rhs);
        }
        //RHS only has one bean ever, so we don't need to preload the relationship
        if (isset($rhs->$rhsLinkName)) {
            $rhs->$rhsLinkName->beans = array($lhs->id => $lhs);
        }
    }

    protected function updateFields($lhs, $rhs, $additionalFields)
    {
        //Now update the RHS bean's ID field
        $rhsID = $this->def['rhs_key'];
        $rhs->$rhsID = $lhs->id;
        foreach ($additionalFields as $field => $val) {
            $rhs->$field = $val;
        }
        //Update role fields
        if (!empty($this->def["relationship_role_column"]) && !empty($this->def["relationship_role_column_value"])) {
            $roleField = $this->def["relationship_role_column"];
            $rhs->$roleField = $this->def["relationship_role_column_value"];
        }
    }

    public function remove($lhs, $rhs, $save = true)
    {
        $rhsID = $this->def['rhs_key'];

        //If this relationship has already been removed, we can just return
        if ($rhs->$rhsID != $lhs->id) {
            return false;
        }

        $rhs->$rhsID = '';

        if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
            $this->callBeforeDelete($lhs, $rhs);
            $this->callBeforeDelete($rhs, $lhs);
        }

        if ($save && !$rhs->deleted) {
            $rhs->in_relationship_update = true;
            $rhs->save();
        }

        if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
            $this->callAfterDelete($lhs, $rhs);
            $this->callAfterDelete($rhs, $lhs);
        }

        return true;
    }

    /**
     * @param Link2 $link loads the relationship for this link.
     * @return void
     */
    public function load($link, $params = array())
    {
        $relatedModule = $link->getSide() == REL_LHS ? $this->def['rhs_module'] : $this->def['lhs_module'];
        $rows = array();
        //The related bean ID is stored on the RHS table.
        //If the link is RHS, just grab it from the focus.
        if ($link->getSide() == REL_RHS) {
            $rhsID = $this->def['rhs_key'];

            $id = null;
            if (isset($link->getFocus()->$rhsID)) {
                $id = $link->getFocus()->$rhsID;
            } else {
                LoggerManager::getLogger()->warn('Incorrect linked relationship rhs ID: ' . get_class($link->getFocus()) . '::$' . $rhsID . ' is undefined');
            }

            if (!empty($id)) {
                $rows[$id] = array('id' => $id);
            }
        } else { //If the link is LHS, we need to query to get the full list and load all the beans.
            $db = DBManagerFactory::getInstance();
            $query = $this->getQuery($link, $params);
            if (empty($query)) {
                $GLOBALS['log']->fatal("query for {$this->name} was empty when loading from   {$this->lhsLink}\n");
                return array("rows" => array());
            }
            $result = $db->query($query);
            while ($row = $db->fetchByAssoc($result, false)) {
                $id = $row['id'];
                $rows[$id] = $row;
            }
        }

        return array("rows" => $rows);
    }

    public function getQuery($link, $params = array())
    {
        //There was an old signature with $return_as_array as the second parameter. We should respect this if $params is true
        if ($params === true) {
            $params = array("return_as_array" => true);
        }

        if ($link->getSide() == REL_RHS) {
            return false;
        }
        $lhsKey = $this->def['lhs_key'];
        $rhsTable = $this->def['rhs_table'];
        $rhsTableKey = "{$rhsTable}.{$this->def['rhs_key']}";
        $relatedSeed = BeanFactory::getBean($this->getRHSModule());
        $deleted = !empty($params['deleted']) ? 1 : 0;

        if (!isset($link->getFocus()->$lhsKey)) {
            LoggerManager::getLogger()->warn('One2MBeanRelationship getQuery: Trying to get property of non-object');
            $linkFocusLhsKey = null;
        } else {
            $linkFocusLhsKey = $link->getFocus()->$lhsKey;
        }

        $where = "WHERE $rhsTableKey = '{$linkFocusLhsKey}' AND {$rhsTable}.deleted=$deleted";
        $order_by = '';

        //Check for role column
        if (!empty($this->def["relationship_role_column"]) && !empty($this->def["relationship_role_column_value"])) {
            $roleField = $this->def["relationship_role_column"];
            $roleValue = $this->def["relationship_role_column_value"];
            $where .= " AND $rhsTable.$roleField = '$roleValue'";
        }

        //Add any optional where clause
        if (!empty($params['where'])) {
            $add_where = is_string($params['where']) ? $params['where'] : "$rhsTable." . $this->getOptionalWhereClause($params['where']);
            if (!empty($add_where)) {
                $where .= " AND $add_where";
            }
        }

        //Add any optional order clauses
        if (!empty($params['order_by'])) {
            $order_by = $relatedSeed->process_order_by($params['order_by']);
        }

        $from = $rhsTable;
        if (!empty($params['where']) || !empty($params['order_by'])) {
            if (isset($relatedSeed->custom_fields)) {
                $customJoin = $relatedSeed->getCustomJoin();
                $from .= $customJoin['join'];
            }
        }

        if (empty($params['return_as_array'])) {
            //Limit is not compatible with return_as_array
            $query = "SELECT id FROM $from $where";
            if (!empty($order_by)) {
                $query .= ' ORDER BY '.$order_by;
            }
            if (!empty($params['limit']) && $params['limit'] > 0) {
                $offset = isset($params['offset']) ? $params['offset'] : 0;
                $query = DBManagerFactory::getInstance()->limitQuery($query, $offset, $params['limit'], false, "", false);
            }
            return $query;
        }
        return array(
                    'select' => "SELECT {$rhsTable}.id",
                    'from' => "FROM $from",
                    'where' => $where,
                    'order_by' => $order_by
                );
    }

    public function getJoin($link, $params = array(), $return_array = false)
    {
        $linkIsLHS = $link->getSide() == REL_LHS;
        $startingTable = (empty($params['left_join_table_alias']) ? $this->def['lhs_table'] : $params['left_join_table_alias']);
        if (!$linkIsLHS) {
            $startingTable = (empty($params['right_join_table_alias']) ? $this->def['rhs_table'] : $params['right_join_table_alias']);
        }
        $startingKey = $linkIsLHS ? $this->def['lhs_key'] : $this->def['rhs_key'];
        $targetTable = $linkIsLHS ? $this->def['rhs_table'] : $this->def['lhs_table'];
        $targetTableWithAlias = $targetTable;
        $targetKey = $linkIsLHS ? $this->def['rhs_key'] : $this->def['lhs_key'];
        $join_type= isset($params['join_type']) ? $params['join_type'] : ' INNER JOIN ';
        $join = '';

        //Set up any table aliases required
        if (! empty($params['join_table_alias'])) {
            $targetTableWithAlias = $targetTable. " ".$params['join_table_alias'];
            $targetTable = $params['join_table_alias'];
        }

        //First join the relationship table
        $join .= "$join_type $targetTableWithAlias ON $startingTable.$startingKey=$targetTable.$targetKey AND $targetTable.deleted=0\n"
        //Next add any role filters
               . $this->getRoleWhere(($linkIsLHS) ? $targetTable : $startingTable) . "\n";

        if ($return_array) {
            return array(
                'join' => $join,
                'type' => $this->type,
                'rel_key' => $targetKey,
                'join_tables' => array($targetTable),
                'where' => "",
                'select' => "$targetTable.id",
            );
        }
        return $join;
    }

    public function getSubpanelQuery($link, $params = array(), $return_array = false)
    {
        $linkIsLHS = $link->getSide() == REL_RHS;
        $startingTable = (empty($params['left_join_table_alias']) ? $this->def['lhs_table'] : $params['left_join_table_alias']);
        if (!$linkIsLHS) {
            $startingTable = (empty($params['right_join_table_alias']) ? $this->def['rhs_table'] : $params['right_join_table_alias']);
        }
        $startingKey = $linkIsLHS ? $this->def['lhs_key'] : $this->def['rhs_key'];
        $targetTable = $linkIsLHS ? $this->def['rhs_table'] : $this->def['lhs_table'];
        $targetKey = $linkIsLHS ? $this->def['rhs_key'] : $this->def['lhs_key'];
        $join_type= isset($params['join_type']) ? $params['join_type'] : ' INNER JOIN ';
        $query = '';

        $alias = empty($params['join_table_alias']) ? "{$link->name}_rel": $params['join_table_alias'];
        $alias = DBManagerFactory::getInstance()->getValidDBName($alias, false, 'alias');

        $tableInRoleFilter = $startingTable;
        if (
            (
                $startingTable == "meetings"
                || $startingTable == "notes"
                || $startingTable == "tasks"
                || $startingTable == "calls"
                || $startingTable == "emails"
            )
            &&
            (
                $targetTable == "meetings"
                || $targetTable == "notes"
                || $targetTable == "tasks"
                || $targetTable == "calls"
            )
            && substr((string) $alias, 0, 12 + strlen((string) $targetTable)) == $targetTable . "_activities_"
        ) {
            $tableInRoleFilter = $linkIsLHS ? $alias : $startingTable;
        }
        
        //Set up any table aliases required
        $targetTableWithAlias = "$targetTable $alias";
        $targetTable = $alias;

        $query .= "$join_type $targetTableWithAlias ON $startingTable.$startingKey=$targetTable.$targetKey AND $targetTable.deleted=0\n"
        //Next add any role filters
               . $this->getRoleWhere($tableInRoleFilter) . "\n";

        if (!empty($params['return_as_array'])) {
            $return_array = true;
        }

        if ($return_array) {
            return array(
                'join' => $query,
                'type' => $this->type,
                'rel_key' => $targetKey,
                'join_tables' => array($targetTable),
                'where' => "WHERE $startingTable.$startingKey='{$link->focus->id}'",
                'select' => " ",
            );
        }
        return $query;
    }

    /**
     * Check to see if the relationship already exist.
     *
     * If it does return true otherwise return false
     *
     * @param SugarBean $lhs        Left hand side of the relationship
     * @param SugarBean $rhs        Right hand side of the relationship
     * @return boolean
     */
    public function relationship_exists($lhs, $rhs)
    {
        // we need the key that is stored on the rhs to compare tok
        $lhsIDName = $this->def['rhs_key'];

        return (isset($rhs->fetched_row[$lhsIDName]) && $rhs->$lhsIDName == $rhs->fetched_row[$lhsIDName] && $rhs->$lhsIDName == $lhs->id);
    }

    public function getRelationshipTable()
    {
        if (isset($this->def['table'])) {
            return $this->def['table'];
        }
        return $this->def['rhs_table'];
    }
}
