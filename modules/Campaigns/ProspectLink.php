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

/*********************************************************************************
 * Description: Bug 40166. Need for return right join for campaign's target list relations.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once 'data/Link2.php';

/**
 * @brief Bug #40166. Campaign Log Report will not display Contact/Account Names
 */
#[\AllowDynamicProperties]
class ProspectLink extends Link2
{
    /**
     * This method changes join of any item to campaign through target list
     * if you want to use this join method you should add code below to your vardef.php
     * 'link_class' => 'ProspectLink',
     * 'link_file' => 'modules/Campaigns/ProspectLink.php'.
     *
     * @see Link::getJoin method
     * @param array $params
     * @param bool $return_array
     * @return array|string
     */
    public function getJoin($params, $return_array = false)
    {
        $join_type = ' INNER JOIN ';
        if (isset($params['join_type'])) {
            $join_type = $params['join_type'];
        }
        $join = '';
        $linkIsLHS = $this->getSide() == REL_LHS;

        if (
            $this->_relationship->relationship_type == 'one-to-many'
            && $linkIsLHS
        ) {
            $table_with_alias = $table = $this->_relationship->rhs_table;
            $key = $this->_relationship->rhs_key;
            $module = $this->_relationship->rhs_module;
            $other_table = (empty($params['left_join_table_alias']) ? $this->_relationship->lhs_table : $params['left_join_table_alias']);
            $other_key = $this->_relationship->lhs_key;
            $alias_prefix = $table;
            if (!empty($params['join_table_alias'])) {
                $table_with_alias = $table.' '.$params['join_table_alias'];
                $table = $params['join_table_alias'];
                $alias_prefix = $params['join_table_alias'];
            }

            $join .= ' '.$join_type.' prospect_list_campaigns '.$alias_prefix.'_plc ON';
            $join .= ' '.$alias_prefix.'_plc.'.$key.' = '.$other_table.'.'.$other_key."\n";

            // join list targets
            $join .= ' '.$join_type.' prospect_lists_prospects '.$alias_prefix.'_plp ON';
            $join .= ' '.$alias_prefix.'_plp.prospect_list_id = '.$alias_prefix.'_plc.prospect_list_id AND';
            $join .= ' '.$alias_prefix.'_plp.related_type = '.DBManagerFactory::getInstance()->quoted($module)."\n";

            // join target
            $join .= ' '.$join_type.' '.$table_with_alias.' ON';
            $join .= ' '.$table.'.id = '.$alias_prefix.'_plp.related_id AND';
            $join .= ' '.$table.'.deleted=0'."\n";

            if ($return_array) {
                $ret_arr = array();
                $ret_arr['join'] = $join;
                $ret_arr['type'] = $this->_relationship->relationship_type;
                if ($linkIsLHS) {
                    $ret_arr['rel_key'] = $this->_relationship->join_key_rhs;
                } else {
                    $ret_arr['rel_key'] = $this->_relationship->join_key_lhs;
                }

                return $ret_arr;
            }

            return $join;
        } else {
            return parent::getJoin($params, $return_array);
        }
    }
}
