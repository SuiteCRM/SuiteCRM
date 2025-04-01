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

require_once("data/Relationships/SugarRelationship.php");

/**
 * Represents a many to many relationship that is table based.
 * @api
 */
#[\AllowDynamicProperties]
class M2MRelationship extends SugarRelationship
{
    public $type = "many-to-many";

    public function __construct($def)
    {
        $this->def = $def;
        $this->name = $def['name'];

        $lhsModule = $def['lhs_module'];
        $this->lhsLinkDef = $this->getLinkedDefForModuleByRelationship($lhsModule);
        if (is_bool($this->lhsLinkDef)) {
            $this->lhsLink = null;
        } else {
            $this->lhsLink = $this->lhsLinkDef['name'];
        }

        $rhsModule = $def['rhs_module'];
        $this->rhsLinkDef = $this->getLinkedDefForModuleByRelationship($rhsModule);
        if (is_bool($this->rhsLinkDef)) {
            $this->rhsLink = null;
        } else {
            $this->rhsLink = $this->rhsLinkDef['name'];
        }

        if (isset($def['self_referencing'])) {
            $this->self_referencing = $def['self_referencing'];
        } else {
            $this->self_referencing = $lhsModule == $rhsModule;
        }
    }

    /**
     * Find the link entry for a particular relationship and module.
     *
     * @param $module
     * @return array|bool
     */
    public function getLinkedDefForModuleByRelationship($module)
    {
        $results = VardefManager::getLinkFieldForRelationship($module, BeanFactory::getObjectName($module), $this->name);
        //Only a single link was found
        if (isset($results['name'])) {
            return $results;
        }
        //Multiple links with same relationship name
        else {
            if (is_array($results)) {
                $GLOBALS['log']->warn("Multiple links found for relationship {$this->name} within module {$module}");
                return $this->getMostAppropriateLinkedDefinition($results);
            } else {
                return false;
            }
        }
    }

    /**
     * Find the most 'appropriate' link entry for a relationship/module in which there are multiple link entries with the
     * same relationship name.
     *
     * @param $links
     * @return bool
     */
    protected function getMostAppropriateLinkedDefinition($links)
    {
        //First priority is to find a link name that matches the relationship name
        foreach ($links as $link) {
            if (isset($link['name']) && $link['name'] == $this->name) {
                return $link;
            }
        }
        //Next would be a relationship that has a side defined
        foreach ($links as $link) {
            if (isset($link['id_name'])) {
                return $link;
            }
        }
        //Unable to find an appropriate link, guess and use the first one
        $GLOBALS['log']->error("Unable to determine best appropriate link for relationship {$this->name}");
        return $links[0];
    }
    /**
     * @param SugarBean $lhs left side bean to add to the relationship.
     * @param SugarBean $rhs right side bean to add to the relationship.
     * @param mixed $additionalFields key=>value pairs of fields to save on the relationship
     * @return boolean true if successful
     */
    public function add($lhs, $rhs, $additionalFields = array())
    {
        $lhsLinkName = $this->lhsLink;
        $rhsLinkName = $this->rhsLink;

        /* BEGIN - SECURITY GROUPS */
        //Need to hijack this as security groups will not contain a link on the module side
        //due to the way the module works. Plus it would remove the relative ease of adding custom module support

        if (get_class($rhs) != 'User' && get_class($rhs) != 'ACLRole' && get_class($lhs) == 'SecurityGroup') {
            $rhs->$rhsLinkName->addBean($lhs);
            $this->callBeforeAdd($rhs, $lhs, $rhsLinkName);

            $dataToInsert = $this->getRowToInsert($lhs, $rhs, $additionalFields);
            $this->addRow($dataToInsert);
            $rhs->$rhsLinkName->addBean($lhs);
            $this->callAfterAdd($lhs, $rhs, $lhsLinkName);
        } elseif (get_class($lhs) != 'User' && get_class($lhs) != 'ACLRole' && get_class($rhs) == 'SecurityGroup') {
            $lhs->$lhsLinkName->addBean($rhs);
            $this->callBeforeAdd($lhs, $rhs, $lhsLinkName);

            $dataToInsert = $this->getRowToInsert($lhs, $rhs, $additionalFields);
            $this->addRow($dataToInsert);
            $lhs->$lhsLinkName->addBean($rhs);
            $this->callAfterAdd($rhs, $lhs, $rhsLinkName);
        } else {
            /* END - SECURITY GROUPS */

            if (empty($lhs->$lhsLinkName) && !$lhs->load_relationship($lhsLinkName)) {
                $lhsClass = get_class($lhs);
                $GLOBALS['log']->fatal("could not load LHS $lhsLinkName in $lhsClass");
                return false;
            }
            if (empty($rhs->$rhsLinkName) && !$rhs->load_relationship($rhsLinkName)) {
                $rhsClass = get_class($rhs);
                $GLOBALS['log']->fatal("could not load RHS $rhsLinkName in $rhsClass");
                return false;
            }

            $lhs->$lhsLinkName->addBean($rhs);
            $rhs->$rhsLinkName->addBean($lhs);

            $this->callBeforeAdd($lhs, $rhs, $lhsLinkName);
            $this->callBeforeAdd($rhs, $lhs, $rhsLinkName);

            //Many to many has no additional logic, so just add a new row to the table and notify the beans.
            $dataToInsert = $this->getRowToInsert($lhs, $rhs, $additionalFields);

            $this->addRow($dataToInsert);

            if ($this->self_referencing) {
                $this->addSelfReferencing($lhs, $rhs, $additionalFields);
            }

            $lhs->$lhsLinkName->addBean($rhs);
            $rhs->$rhsLinkName->addBean($lhs);

            $this->callAfterAdd($lhs, $rhs, $lhsLinkName);
            $this->callAfterAdd($rhs, $lhs, $rhsLinkName);

            /* BEGIN - SECURITY GROUPS */
        } //end normal
        /* END - SECURITY GROUPS */

        return true;
    }

    protected function getRowToInsert($lhs, $rhs, $additionalFields = array())
    {
        $row = array(
            "id" => create_guid(),
            $this->def['join_key_lhs'] => $lhs->id,
            $this->def['join_key_rhs'] => $rhs->id,
            'date_modified' => TimeDate::getInstance()->nowDb(),
            'deleted' => 0,
        );


        if (!empty($this->def['relationship_role_column']) && !empty($this->def['relationship_role_column_value']) && !$this->ignore_role_filter) {
            $row[$this->relationship_role_column] = $this->relationship_role_column_value;
        }

        if (!empty($this->def['fields'])) {
            foreach ($this->def['fields'] as $fieldDef) {
                if (!empty($fieldDef['name']) && !isset($row[$fieldDef['name']]) && !empty($fieldDef['default'])) {
                    $row[$fieldDef['name']] = $fieldDef['default'];
                }
            }
        }
        if (!empty($additionalFields)) {
            $row = array_merge($row, $additionalFields);
        }

        return $row;
    }

    /**
     * Adds the reversed version of this relationship to the table so that it can be accessed from either side equally
     * @param $lhs
     * @param $rhs
     * @param array $additionalFields
     * @return void
     */
    protected function addSelfReferencing($lhs, $rhs, $additionalFields = array())
    {
        if ($rhs->id != $lhs->id) {
            $dataToInsert = $this->getRowToInsert($rhs, $lhs, $additionalFields);
            $this->addRow($dataToInsert);
        }
    }

    public function remove($lhs, $rhs)
    {
        if (!($lhs instanceof SugarBean) || !($rhs instanceof SugarBean)) {
            $GLOBALS['log']->fatal("LHS and RHS must be beans");
            return false;
        }
        $lhsLinkName = $this->lhsLink;
        $rhsLinkName = $this->rhsLink;

        if (!($lhs instanceof SugarBean)) {
            $GLOBALS['log']->fatal("LHS is not a SugarBean object");
            return false;
        }
        if (!($rhs instanceof SugarBean)) {
            $GLOBALS['log']->fatal("RHS is not a SugarBean object");
            return false;
        }

        /* BEGIN - SECURITY GROUPS */
        //Need to hijack this as security groups will not contain a link on the module side
        //due to the way the module works. Plus it would remove the relative ease of adding custom module support

        if (get_class($lhs) == 'SecurityGroup' || get_class($rhs) == 'SecurityGroup') {
            $dataToRemove = array(
                $this->def['join_key_lhs'] => $lhs->id,
                $this->def['join_key_rhs'] => $rhs->id
            );

            if (!empty($this->def['relationship_role_column']) && !empty($this->def['relationship_role_column_value'])) {
                $dataToRemove[$this->def['relationship_role_column']] = $this->def['relationship_role_column_value'];
            }
            $dataToRemove['deleted'] = 0;

            if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
                if (get_class($lhs) != 'SecurityGroup' && $lhs->$lhsLinkName instanceof Link2) {
                    $lhs->$lhsLinkName->load();
                    $this->callBeforeDelete($lhs, $rhs, $lhsLinkName);
                }

                if (get_class($rhs) != 'SecurityGroup' && $rhs->$rhsLinkName instanceof Link2) {
                    $rhs->$rhsLinkName->load();
                    $this->callBeforeDelete($rhs, $lhs, $rhsLinkName);
                }
            }

            $this->removeRow($dataToRemove);

            if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
                if (get_class($lhs) != 'SecurityGroup' && $lhs->$lhsLinkName instanceof Link2) {
                    $lhs->$lhsLinkName->load();
                    $this->callAfterDelete($lhs, $rhs, $lhsLinkName);
                }

                if (get_class($rhs) != 'SecurityGroup' && $rhs->$rhsLinkName instanceof Link2) {
                    $rhs->$rhsLinkName->load();
                    $this->callAfterDelete($rhs, $lhs, $rhsLinkName);
                }
            }
        } else {
            /* END - SECURITY GROUPS */
            if (empty($lhs->$lhsLinkName) && !$lhs->load_relationship($lhsLinkName)) {
                $GLOBALS['log']->fatal("could not load LHS $lhsLinkName");
                return false;
            }
            if (empty($rhs->$rhsLinkName) && !$rhs->load_relationship($rhsLinkName)) {
                $GLOBALS['log']->fatal("could not load RHS $rhsLinkName");
                return false;
            }

            if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
                if ($lhs->$lhsLinkName instanceof Link2) {
                    $lhs->$lhsLinkName->load();
                    $this->callBeforeDelete($lhs, $rhs, $lhsLinkName);
                }

                if ($rhs->$rhsLinkName instanceof Link2) {
                    $rhs->$rhsLinkName->load();
                    $this->callBeforeDelete($rhs, $lhs, $rhsLinkName);
                }
            }

            $dataToRemove = array(
            $this->def['join_key_lhs'] => $lhs->id,
            $this->def['join_key_rhs'] => $rhs->id
        );

            $this->removeRow($dataToRemove);

            if ($this->self_referencing) {
                $this->removeSelfReferencing($lhs, $rhs);
            }

            if (empty($_SESSION['disable_workflow']) || $_SESSION['disable_workflow'] != "Yes") {
                if ($lhs->$lhsLinkName instanceof Link2) {
                    $lhs->$lhsLinkName->load();
                    $this->callAfterDelete($lhs, $rhs, $lhsLinkName);
                }

                if ($rhs->$rhsLinkName instanceof Link2) {
                    $rhs->$rhsLinkName->load();
                    $this->callAfterDelete($rhs, $lhs, $rhsLinkName);
                }
            }
            /* BEGIN - SECURITY GROUPS */
        } //end normal
        /* END - SECURITY GROUPS */

        return true;
    }

    /**
     * Removes the reversed version of this relationship
     * @param $lhs
     * @param $rhs
     * @param array $additionalFields
     * @return void
     */
    protected function removeSelfReferencing($lhs, $rhs, $additionalFields = array())
    {
        if ($rhs->id != $lhs->id) {
            $dataToRemove = array(
                $this->def['join_key_lhs'] => $rhs->id,
                $this->def['join_key_rhs'] => $lhs->id
            );
            $this->removeRow($dataToRemove);
        }
    }

    /**
     * @param Link2 $link loads the relationship for this link.
     * @return void
     */
    public function load($link, $params = array())
    {
        $db = DBManagerFactory::getInstance();
        $query = $this->getQuery($link, $params);
        $result = $db->query($query);
        $rows = array();
        $idField = $link->getSide() == REL_LHS ? $this->def['join_key_rhs'] : $this->def['join_key_lhs'];
        while ($row = $db->fetchByAssoc($result, false)) {
            if (empty($row['id']) && empty($row[$idField])) {
                continue;
            }
            $id = empty($row['id']) ? $row[$idField] : $row['id'];
            $rows[$id] = $row;
        }
        return array("rows" => $rows);
    }

    protected function linkIsLHS($link)
    {
        return $link->getSide() == REL_LHS;
    }

    public function getQuery($link, $params = array())
    {
        $whereTable = '';
        if ($this->linkIsLHS($link)) {
            $knownKey = $this->def['join_key_lhs'];
            $targetKey = $this->def['join_key_rhs'];
            $relatedSeed = BeanFactory::getBean($this->getRHSModule());
            $relatedSeedKey = $this->def['rhs_key'];
            if (!empty($params['where']) || !empty($params['order_by'])) {
                $whereTable = (empty($params['right_join_table_alias']) ? $relatedSeed->table_name : $params['right_join_table_alias']);
            }
        } else {
            $knownKey = $this->def['join_key_rhs'];
            $targetKey = $this->def['join_key_lhs'];
            $relatedSeed = BeanFactory::getBean($this->getLHSModule());
            $relatedSeedKey = $this->def['lhs_key'];
            if (!empty($params['where']) || !empty($params['order_by'])) {
                $whereTable = (empty($params['left_join_table_alias']) ? $relatedSeed->table_name : $params['left_join_table_alias']);
            }
        }
        $rel_table = $this->getRelationshipTable();

        $tmpFocus = $link->getFocus();
        if (!isset($tmpFocus->id)) {
            if (is_object($tmpFocus)) {
                $focusInfo = get_class($tmpFocus);
            } elseif (is_bool($tmpFocus)) {
                $focusInfo = ' (bool)' . ($tmpFocus ? 'TRUE' : 'FALSE');
            } else {
                $focusInfo = ' (' . gettype($tmpFocus) . ')' . $tmpFocus;
            }
            LoggerManager::getLogger()->warn('No focus from link when M2MRelationship get query. Focus was: ' . $focusInfo);
            $tmpId = null;
        } else {
            $tmpId = $tmpFocus->id;
        }

        $where = "$rel_table.$knownKey = '{$tmpId}'" . $this->getRoleWhere();
        $order_by = '';

        //Add any optional where clause
        if (!empty($params['where'])) {
            $add_where = is_string($params['where']) ? $params['where'] : "$whereTable." . $this->getOptionalWhereClause($params['where']);
            if (!empty($add_where)) {
                $where .= " AND $add_where";
            }
        }

        //Add any optional order clauses
        if (!empty($params['order_by'])) {
            $order_by = $relatedSeed->process_order_by($params['order_by']);
        }

        $deleted = !empty($params['deleted']) ? 1 : 0;
        $from = $rel_table . " ";
        if (!empty($params['where']) || !empty($params['order_by'])) {
            $from .= ", $whereTable";
            if (isset($relatedSeed->custom_fields)) {
                $customJoin = $relatedSeed->custom_fields->getJOIN();
                $from .= $customJoin ? $customJoin['join'] : '';
            }
            $where .= " AND $rel_table.$targetKey=$whereTable.id";
        }

        $SelectIncludedMiddleTableFields = '';
        if (isset($params['include_middle_table_fields']) && $params['include_middle_table_fields'] === true) {
            $middle_table_field_defs = $this->def['fields'];
            $middle_table = array();
            foreach ($middle_table_field_defs as $field_def) {
                if ($field_def['name'] === 'id') {
                    continue;
                }
                $middle_table[] = $field_def['name'];
            }
            $SelectIncludedMiddleTableFields = ', ' . implode(',', $middle_table);
        }

        if (empty($params['return_as_array'])) {
            $query = "SELECT $targetKey id $SelectIncludedMiddleTableFields FROM $from WHERE $where AND $rel_table.deleted=$deleted";
            if (!empty($order_by)) {
                $query .= ' ORDER BY '.$order_by;
            }
            //Limit is not compatible with return_as_array
            if (!empty($params['limit']) && $params['limit'] > 0) {
                $offset = isset($params['offset']) ? $params['offset'] : 0;
                $query = DBManagerFactory::getInstance()->limitQuery($query, $offset, $params['limit'], false, "", false);
            }
            return $query;
        } else {
            return array(
                'select' => "SELECT $targetKey id",
                'from' => "FROM $from",
                'where' => "WHERE $where AND $rel_table.deleted=$deleted",
                'order_by' => $order_by
            );
        }
    }

    public function getJoin($link, $params = array(), $return_array = false)
    {
        $linkIsLHS = $link->getSide() == REL_LHS;
        if ($linkIsLHS) {
            $startingTable = (empty($params['left_join_table_alias']) ? $link->getFocus()->table_name : $params['left_join_table_alias']);
        } else {
            $startingTable = (empty($params['right_join_table_alias']) ? $link->getFocus()->table_name : $params['right_join_table_alias']);
        }

        $startingKey = $linkIsLHS ? $this->def['lhs_key'] : $this->def['rhs_key'];
        $startingJoinKey = $linkIsLHS ? $this->def['join_key_lhs'] : $this->def['join_key_rhs'];
        $joinTable = $this->getRelationshipTable();
        $joinTableWithAlias = $joinTable;
        $joinKey = $linkIsLHS ? $this->def['join_key_rhs'] : $this->def['join_key_lhs'];
        $targetTable = $linkIsLHS ? $this->def['rhs_table'] : $this->def['lhs_table'];
        $targetTableWithAlias = $targetTable;
        $targetKey = $linkIsLHS ? $this->def['rhs_key'] : $this->def['lhs_key'];
        $join_type= isset($params['join_type']) ? $params['join_type'] : ' INNER JOIN ';

        $join = '';

        //Set up any table aliases required
        if (!empty($params['join_table_link_alias'])) {
            $joinTableWithAlias = $joinTable . " ". $params['join_table_link_alias'];
            $joinTable = $params['join_table_link_alias'];
        }
        if (! empty($params['join_table_alias'])) {
            $targetTableWithAlias = $targetTable . " ". $params['join_table_alias'];
            $targetTable = $params['join_table_alias'];
        }

        $join1 = "$startingTable.$startingKey=$joinTable.$startingJoinKey";
        $join2 = "$targetTable.$targetKey=$joinTable.$joinKey";
        $where = "";


        //First join the relationship table
        $join .= "$join_type $joinTableWithAlias ON $join1 AND $joinTable.deleted=0\n"
        //Next add any role filters
               . $this->getRoleWhere($joinTable) . "\n"
        //Then finally join the related module's table
               . "$join_type $targetTableWithAlias ON $join2 AND $targetTable.deleted=0\n";

        if ($return_array) {
            return array(
                'join' => $join,
                'type' => $this->type,
                'rel_key' => $joinKey,
                'join_tables' => array($joinTable, $targetTable),
                'where' => $where,
                'select' => "$targetTable.id",
            );
        }
        return $join . $where;
    }

    /**
     * Similar to getQuery or Get join, except this time we are starting from the related table and
     * searching for items with id's matching the $link->focus->id
     * @param mixed $link
     * @param array $params
     * @param bool $return_array
     * @return String|Array
     */
    public function getSubpanelQuery($link, $params = array(), $return_array = false)
    {
        $targetIsLHS = $link->getSide() == REL_RHS;
        $startingTable = $targetIsLHS ? $this->def['lhs_table'] : $this->def['rhs_table'];
        ;
        $startingKey = $targetIsLHS ? $this->def['lhs_key'] : $this->def['rhs_key'];
        $startingJoinKey = $targetIsLHS ? $this->def['join_key_lhs'] : $this->def['join_key_rhs'];
        $joinTable = $this->getRelationshipTable();
        $joinTableWithAlias = $joinTable;
        $joinKey = $targetIsLHS ? $this->def['join_key_rhs'] : $this->def['join_key_lhs'];
        $targetKey = $targetIsLHS ? $this->def['rhs_key'] : $this->def['lhs_key'];
        $join_type= isset($params['join_type']) ? $params['join_type'] : ' INNER JOIN ';

        $query = '';

        //Set up any table aliases required
        if (!empty($params['join_table_link_alias'])) {
            $joinTableWithAlias = $joinTable . " ". $params['join_table_link_alias'];
            $joinTable = $params['join_table_link_alias'];
        }

        $where = "$startingTable.$startingKey=$joinTable.$startingJoinKey AND $joinTable.$joinKey='{$link->getFocus()->$targetKey}'";

        //Check if we should ignore the role filter.
        $ignoreRole = !empty($params['ignore_role']);

        //First join the relationship table
        $query .= "$join_type $joinTableWithAlias ON $where AND $joinTable.deleted=0\n"
        //Next add any role filters
               . $this->getRoleWhere($joinTable, $ignoreRole) . "\n";

        if (!empty($params['return_as_array'])) {
            $return_array = true;
        }
        if ($return_array) {
            return array(
                'join' => $query,
                'type' => $this->type,
                'rel_key' => $joinKey,
                'join_tables' => array($joinTable),
                'where' => "",
                'select' => " ",
            );
        }
        return $query;
    }

    protected function getRoleFilterForJoin()
    {
        $ret = "";
        if (!empty($this->relationship_role_column) && !$this->ignore_role_filter) {
            $ret .= " AND ".$this->getRelationshipTable().'.'.$this->relationship_role_column;
            //role column value.
            if (empty($this->relationship_role_column_value)) {
                $ret.=' IS NULL';
            } else {
                $ret.= "='".$this->relationship_role_column_value."'";
            }
            $ret.= "\n";
        }
        return $ret;
    }

    /**
     * @param $lhs
     * @param $rhs
     * @return bool
     */
    public function relationship_exists($lhs, $rhs)
    {
        $query = "SELECT id FROM {$this->getRelationshipTable()} WHERE {$this->join_key_lhs} = '{$lhs->id}' AND {$this->join_key_rhs} = '{$rhs->id}'";

        //Roles can allow for multiple links between two records with different roles
        $query .= $this->getRoleWhere() . " and deleted = 0";

        return DBManagerFactory::getInstance()->getOne($query);
    }

    /**
     * @return Array - set of fields that uniquely identify an entry in this relationship
     */
    protected function getAlternateKeyFields()
    {
        $fields = array($this->join_key_lhs, $this->join_key_rhs);

        //Roles can allow for multiple links between two records with different roles
        if (!empty($this->def['relationship_role_column']) && !$this->ignore_role_filter) {
            $fields[] = $this->relationship_role_column;
        }

        return $fields;
    }

    public function getRelationshipTable()
    {
        if (!empty($this->def['table'])) {
            return $this->def['table'];
        } else {
            if (!empty($this->def['join_table'])) {
                return $this->def['join_table'];
            }
        }

        return false;
    }

    public function getFields()
    {
        if (!empty($this->def['fields'])) {
            return $this->def['fields'];
        }
        $fields = array(
            "id" => array('name' => 'id'),
            'date_modified' => array('name' => 'date_modified'),
            'modified_user_id' => array('name' => 'modified_user_id'),
            'created_by' => array('name' => 'created_by'),
            $this->def['join_key_lhs'] => array('name' => $this->def['join_key_lhs']),
            $this->def['join_key_rhs'] => array('name' => $this->def['join_key_rhs'])
        );
        if (!empty($this->def['relationship_role_column'])) {
            $fields[$this->def['relationship_role_column']] = array("name" => $this->def['relationship_role_column']);
        }
        $fields['deleted'] = array('name' => 'deleted');

        return $fields;
    }
}
