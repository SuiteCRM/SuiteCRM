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


require_once("data/Relationships/M2MRelationship.php");

/**
 * Represents a one to many relationship that is table based.
 * @api
 */
#[\AllowDynamicProperties]
class One2MRelationship extends M2MRelationship
{
    public function __construct($def)
    {
        global $dictionary;

        $this->def = $def;
        $this->name = $def['name'];

        $this->selfReferencing = $def['lhs_module'] == $def['rhs_module'];
        $lhsModule = $def['lhs_module'];
        $rhsModule = $def['rhs_module'];

        if ($this->selfReferencing) {
            $links = VardefManager::getLinkFieldForRelationship(
                $lhsModule,
                BeanFactory::getObjectName($lhsModule),
                $this->name
            );
            if (empty($links)) {
                $GLOBALS['log']->fatal("No Links found for relationship {$this->name}");
            } else {
                if (!is_array($links)) { //Only one link for a self referencing relationship, this is very bad.
                    $this->lhsLinkDef = $this->rhsLinkDef = $links;
                } else {
                    if (!empty($links[0]) && !empty($links[1])) {
                        if ((!empty($links[0]['side']) && $links[0]['side'] == "right")
                        || (!empty($links[0]['link_type']) && $links[0]['link_type'] == "one")) {
                            //$links[0] is the RHS
                            $this->lhsLinkDef = $links[1];
                            $this->rhsLinkDef = $links[0];
                        } else {
                            //$links[0] is the LHS
                            $this->lhsLinkDef = $links[0];
                            $this->rhsLinkDef = $links[1];
                        }
                    }
                }
            }
        } else {
            $this->lhsLinkDef = VardefManager::getLinkFieldForRelationship(
                $lhsModule,
                BeanFactory::getObjectName($lhsModule),
                $this->name
            );
            $this->rhsLinkDef = VardefManager::getLinkFieldForRelationship(
                $rhsModule,
                BeanFactory::getObjectName($rhsModule),
                $this->name
            );
            if (!isset($this->lhsLinkDef['name']) && isset($this->lhsLinkDef[0])) {
                $this->lhsLinkDef = $this->lhsLinkDef[0];
            }
            if (!isset($this->rhsLinkDef['name']) && isset($this->rhsLinkDef[0])) {
                $this->rhsLinkDef = $this->rhsLinkDef[0];
            }
        }
        if (!is_bool($this->lhsLinkDef)) {
            $this->lhsLink = $this->lhsLinkDef['name'];
        }
        if (!is_bool($this->rhsLinkDef)) {
            $this->rhsLink = $this->rhsLinkDef['name'];
        }
    }

    protected function linkIsLHS($link)
    {
        return ($link->getSide() == REL_LHS && !$this->selfReferencing) ||
               ($link->getSide() == REL_RHS && $this->selfReferencing);
    }

    /**
     * @param SugarBean $lhs left side bean to add to the relationship.
     * @param SugarBean $rhs right side bean to add to the relationship.
     * @param mixed $additionalFields key=>value pairs of fields to save on the relationship
     * @return boolean true if successful
     */
    public function add($lhs, $rhs, $additionalFields = array())
    {
        $dataToInsert = $this->getRowToInsert($lhs, $rhs, $additionalFields);
        
        //If the current data matches the existing data, don't do anything
        if (!$this->checkExisting($dataToInsert)) {
            // Pre-load the RHS relationship, which is used later in the add() function and expects a Bean
            // and we also use it for clearing relationships in case of non self-referencing O2M relations
            // (should be preloaded because when using the relate_to field for updating/saving relationships,
            // only the bean id is loaded into $rhs->$rhsLinkName)
            $rhsLinkName = $this->rhsLink;
            $rhs->load_relationship($rhsLinkName);
            
            // If it's a One2Many self-referencing relationship
            // the positions of the default One (LHS) and Many (RHS) are swaped
            // so we should clear the links from the many (left) side
            if ($this->selfReferencing) {
                // Load right hand side relationship name
                $linkName = $this->rhsLink;
                // Load the relationship into the left hand side bean
                $lhs->load_relationship($linkName);
                
                // Pick the loaded link
                $link = $lhs->$linkName;
                // Get many (LHS) side bean
                $focus = $link->getFocus();
                // Get relations
                $related = $link->getBeans();
                
                // Clear the relations from many side bean
                foreach ($related as $relBean) {
                    $this->remove($focus, $relBean);
                }
            } else { // For non self-referencing, remove all the relationships from the many (RHS) side
                $this->removeAll($rhs->$rhsLinkName);
            }

            // Add relationship
            return parent::add($lhs, $rhs, $additionalFields);
        }
    }

    /**
     * Just overriding the function from M2M to prevent it from occuring
     *
     * The logic for dealing with adding self-referencing one-to-many relations is in the add() method
     */
    protected function addSelfReferencing($lhs, $rhs, $additionalFields = array())
    {
        //No-op on One2M.
    }

    /**
     * Just overriding the function from M2M to prevent it from occuring
     */
    protected function removeSelfReferencing($lhs, $rhs, $additionalFields = array())
    {
        //No-op on One2M.
    }
}
