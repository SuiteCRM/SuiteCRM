<?php
if (!defined('sugarEntry')) {
    define('sugarEntry', true);
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


/**
 * This class is an implementation class for all the rest services
 */
require_once('service/core/SugarWebServiceImpl.php');

#[\AllowDynamicProperties]
class SugarWebServiceImplv2_1 extends SugarWebServiceImpl
{
    /**
     * Retrieve a list of beans.  This is the primary method for getting list of SugarBeans from Sugar using the SOAP API.
     *
     * @param string $session -- Session ID returned by a previous call to login.
     * @param string $module_name -- The name of the module to return records from.  This name should be the name the module was developed under (changing a tab name is studio does not affect the name that should be passed into this method)..
     * @param string $query -- SQL where clause without the word 'where'
     * @param string $order_by -- SQL order by clause without the phrase 'order by'
     * @param integer $offset -- The record offset to start from.
     * @param array $select_fields -- A list of the fields to be included in the results. This optional parameter allows for only needed fields to be retrieved.
     * @param array $link_name_to_fields_array -- A list of link_names and for each link_name, what fields value to be returned. For ex.'link_name_to_fields_array' => array(array('name' =>  'email_addresses', 'value' => array('id', 'email_address', 'opt_out', 'primary_address')))
     * @param integer $max_results -- The maximum number of records to return.  The default is the sugar configuration value for 'list_max_entries_per_page'
     * @param bool $deleted -- false if deleted records should not be include, true if deleted records should be included.
     * @return array 'result_count' -- integer - The number of records returned
     *               'next_offset' -- integer - The start of the next page (This will always be the previous offset plus the number of rows returned.  It does not indicate if there is additional data unless you calculate that the next_offset happens to be closer than it should be.
     *               'entry_list' -- Array - The records that were retrieved
     *                 'relationship_list' -- Array - The records link field data. The example is if asked about accounts email address then return data would look like Array ( [0] => Array ( [name] => email_addresses [records] => Array ( [0] => Array ( [0] => Array ( [name] => id [value] => 3fb16797-8d90-0a94-ac12-490b63a6be67 ) [1] => Array ( [name] => email_address [value] => hr.kid.qa@example.com ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 1 ) ) [1] => Array ( [0] => Array ( [name] => id [value] => 403f8da1-214b-6a88-9cef-490b63d43566 ) [1] => Array ( [name] => email_address [value] => kid.hr@example.name ) [2] => Array ( [name] => opt_out [value] => 0 ) [3] => Array ( [name] => primary_address [value] => 0 ) ) ) ) )
     * @exception 'SoapFault' -- The SOAP error, if any
     */
    public function get_entry_list(
        $session = null,
        $module_name = null,
        $query = null,
        $order_by = null,
        $offset = null,
        $select_fields = null,
        $link_name_to_fields_array = null,
        $max_results = null,
        $deleted = false
    ) {
        $result = parent::get_entry_list($session, $module_name, $query, $order_by, $offset, $select_fields, $link_name_to_fields_array, $max_results, $deleted);
        if (empty($result)) {
            return null;
        }
        $relationshipList = $result['relationship_list'];
        $returnRelationshipList = array();
        foreach ($relationshipList as $rel) {
            $link_output = array();
            foreach ($rel as $row) {
                $rowArray = array();
                foreach ($row['records'] as $record) {
                    $rowArray[]['link_value'] = $record;
                }
                $link_output[] = array('name' => $row['name'], 'records' => $rowArray);
            }
            $returnRelationshipList[]['link_list'] = $link_output;
        }
        $result['relationship_list'] = $returnRelationshipList;
        return $result;
    } // fn
}
