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

include_once 'modules/Emails/include/ListView/ListViewDataEmailsSearchAbstract.php';

class ListViewDataEmailsSearchOnCrm extends ListViewDataEmailsSearchAbstract
{

    /**
     * @param array $filterFields
     * @param array $request $_REQUEST
     * @param string $where
     * @param InboundEmail $inboundEmail
     * @param array $params
     * @param Email $seed
     * @param bool $singleSelect
     * @param string $id
     * @param int $limit
     * @param User $currentUser
     * @param string $idField
     * @param int $offset
     * @return array
     */
    public function search($filterFields, $request, $where, InboundEmail $inboundEmail, $params, Email $seed, $singleSelect, $id, $limit, User $currentUser, $idField, $offset)
    {
        $ret_array = [];
        $filter_fields = [];
        $pageData = [];
        // Fix fields in filter fields

        if (!is_string($id)) {
            $GLOBALS['log']->warn("ID should be a string: {$id}");
        }

        $filterFields = $this->lvde->fixFieldsInFilter($filterFields, $request, $where);


        // Filter imported emails based on the UID of the results from the IMap server

        if (!empty($where)) {
            $where .= ' AND ';
        }
        if ($inboundEmail->id) {
            $inboundEmailIdQuoted = DBManagerFactory::getInstance()->quote($inboundEmail->id);
        } else {
            $inboundEmailIdQuoted = '';
            LoggerManager::getLogger()->warn('Unable to quote Inbound Email ID, Inbound Email is not set.');
        }
        $crmWhere = $where . "mailbox_id LIKE " ."'" . $inboundEmailIdQuoted . "'";


        // Populates CRM fields

        $crmQueryArray = $this->lvde->getCrmQueryArray($crmWhere, $filterFields, $params, $seed, $singleSelect);


        // get crm emails query

        $crmEmailsQuery = $this->lvde->getCrmEmailsQuery($crmQueryArray, $params);




        $this->lvde->setVariableName($seed->object_name, $crmWhere, $this->lvde->listviewName, $id);

        SugarVCR::erase($seed->module_dir);
        $this->lvde->seed =& $seed;
        $totalCounted = empty($GLOBALS['sugar_config']['disable_count_query']);
        $_SESSION['MAILMERGE_MODULE_FROM_LISTVIEW'] = $seed->module_dir;
        if (empty($request['action']) || $request['action'] != 'Popup') {
            $_SESSION['MAILMERGE_MODULE'] = $seed->module_dir;
        }

        $this->lvde->setVariableName($seed->object_name, $where, $this->lvde->listviewName, $id);

        $this->lvde->seed->id = '[SELECT_ID_LIST]';

        // if $params tell us to override all ordering
        if (!empty($params['overrideOrder']) && !empty($params['orderBy'])) {
            $order = $this->lvde->getOrderBy(strtolower($params['orderBy']), (empty($params['sortOrder']) ? '' : $params['sortOrder'])); // retreive from $_REQUEST
        } else {
            $order = $this->lvde->getOrderBy(); // retreive from $_REQUEST
        }

        // still empty? try to use settings passed in $param
        if (empty($order['orderBy']) && !empty($params['orderBy'])) {
            $order['orderBy'] = $params['orderBy'];
            $order['sortOrder'] =  (empty($params['sortOrder']) ? '' : $params['sortOrder']);
        }

        if (empty($params['skipOrderSave'])) { // don't save preferences if told so
            $currentUser->setPreference('listviewOrder', $order, 0, $this->lvde->var_name); // save preference
        }

        $data = array();

        $rows = array();
        $count = 0;
        $idIndex = array();
        $id_list = '';

        $crmEmails = $this->lvde->db->query($crmEmailsQuery);

        while (($row = $this->lvde->db->fetchByAssoc($crmEmails)) != null) {
            if ($count < $limit) {
                $id_list .= ',\''.$row[$idField].'\'';
                $idIndex[$row[$idField]][] = count($rows);
                $rows[] = $seed->convertRow($row);
            }
            $count++;
        }

        if (!empty($id_list)) {
            $id_list = '('.substr($id_list, 1).')';
        }

        SugarVCR::store($this->lvde->seed->module_dir, $crmEmailsQuery);
        if ($count != 0) {
            //NOW HANDLE SECONDARY QUERIES
            if (!empty($ret_array['secondary_select'])) {
                $secondary_query = $ret_array['secondary_select'] . $ret_array['secondary_from'] . ' WHERE '.$this->lvde->seed->table_name.'.id IN ' .$id_list;
                if (isset($ret_array['order_by'])) {
                    $secondary_query .= ' ' . $ret_array['order_by'];
                }

                $secondary_result = $this->lvde->db->query($secondary_query);

                $ref_id_count = array();
                while ($row = $this->lvde->db->fetchByAssoc($secondary_result)) {
                    $ref_id_count[$row['ref_id']][] = true;
                    foreach ($row as $name=>$value) {
                        //add it to every row with the given id
                        foreach ($idIndex[$row['ref_id']] as $index) {
                            $rows[$index][$name]=$value;
                        }
                    }
                }

                $rows_keys = array_keys($rows);
                foreach ($rows_keys as $key) {
                    $rows[$key]['secondary_select_count'] = count($ref_id_count[$rows[$key]['ref_id']]);
                }
            }

            // retrieve parent names
            if (!empty($filter_fields['parent_name']) && !empty($filter_fields['parent_id']) && !empty($filter_fields['parent_type'])) {
                foreach ($idIndex as $id => $rowIndex) {
                    if (!isset($post_retrieve[$rows[$rowIndex[0]]['parent_type']])) {
                        $post_retrieve[$rows[$rowIndex[0]]['parent_type']] = array();
                    }
                    if (!empty($rows[$rowIndex[0]]['parent_id'])) {
                        $post_retrieve[$rows[$rowIndex[0]]['parent_type']][] = array('child_id' => $id , 'parent_id'=> $rows[$rowIndex[0]]['parent_id'], 'parent_type' => $rows[$rowIndex[0]]['parent_type'], 'type' => 'parent');
                    }
                }
                if (isset($post_retrieve)) {
                    $parent_fields = $seed->retrieve_parent_fields($post_retrieve);
                    foreach ($parent_fields as $child_id => $parent_data) {
                        //add it to every row with the given id
                        foreach ($idIndex[$child_id] as $index) {
                            $rows[$index]['parent_name']= $parent_data['parent_name'];
                        }
                    }
                }
            }

            reset($rows);
            while ($row = current($rows)) {
                $temp = clone $seed;
                $dataIndex = count($data);

                $temp->setupCustomFields($temp->module_dir);
                $temp->loadFromRow($row);
                if (empty($this->lvde->seed->assigned_user_id) && !empty($temp->assigned_user_id)) {
                    $this->lvde->seed->assigned_user_id = $temp->assigned_user_id;
                }
                if ($idIndex[$row[$idField]][0] === $dataIndex) {
                    $pageData['tag'][$dataIndex] = $temp->listviewACLHelper();
                } else {
                    $pageData['tag'][$dataIndex] = $pageData['tag'][$idIndex[$row[$idField]][0]];
                }
                $data[$dataIndex] = $temp->get_list_view_data();
                $detailViewAccess = $temp->ACLAccess('DetailView');
                $editViewAccess = $temp->ACLAccess('EditView');
                $pageData['rowAccess'][$dataIndex] = array('view' => $detailViewAccess, 'edit' => $editViewAccess);
                $additionalDetailsAllow = $this->lvde->additionalDetails && $detailViewAccess && (file_exists(
                    'modules/' . $temp->module_dir . '/metadata/additionalDetails.php'
                        ) || file_exists('custom/modules/' . $temp->module_dir . '/metadata/additionalDetails.php'));
                $additionalDetailsEdit = $editViewAccess;
                if ($additionalDetailsAllow) {
                    if ($this->lvde->additionalDetailsAjax) {
                        $ar = $this->lvde->getAdditionalDetailsAjax($data[$dataIndex]['ID']);
                    } else {
                        $additionalDetailsFile = 'modules/' . $this->lvde->seed->module_dir . '/metadata/additionalDetails.php';
                        if (file_exists('custom/modules/' . $this->lvde->seed->module_dir . '/metadata/additionalDetails.php')) {
                            $additionalDetailsFile = 'custom/modules/' . $this->lvde->seed->module_dir . '/metadata/additionalDetails.php';
                        }
                        require_once($additionalDetailsFile);
                        $ar = $this->lvde->getAdditionalDetails(
                            $data[$dataIndex],
                            (empty($this->lvde->additionalDetailsFunction) ? 'additionalDetails' : $this->lvde->additionalDetailsFunction) . $this->lvde->seed->object_name,
                            $additionalDetailsEdit
                        );
                    }
                    $pageData['additionalDetails'][$dataIndex] = $ar['string'];
                    $pageData['additionalDetails']['fieldToAddTo'] = $ar['fieldToAddTo'];
                }
                next($rows);
            }
        }
        $nextOffset = -1;
        $prevOffset = -1;
        $endOffset = -1;
        if ($count > $limit) {
            $nextOffset = $offset + $limit;
        }

        if ($offset > 0) {
            $prevOffset = $offset - $limit;
            if ($prevOffset < 0) {
                $prevOffset = 0;
            }
        }
        $totalCount = $count + $offset;

        if ($count >= $limit && $totalCounted) {
            $totalCount  = $this->lvde->getTotalCount($crmEmailsQuery);
        }
        SugarVCR::recordIDs($this->lvde->seed->module_dir, array_keys($idIndex), $offset, $totalCount);
        $module_names = array(
            'Prospects' => 'Targets'
        );
        $endOffset = (floor(($totalCount - 1) / $limit)) * $limit;
        $pageData['ordering'] = $order;
        $pageData['ordering']['sortOrder'] = $this->lvde->getReverseSortOrder($pageData['ordering']['sortOrder']);
        //get url parameters as an array
        $pageData['queries'] = $this->lvde->callGenerateQueries($pageData['ordering']['sortOrder'], $offset, $prevOffset, $nextOffset, $endOffset, $totalCounted);
        //join url parameters from array to a string
        $pageData['urls'] = $this->lvde->callGenerateURLS($pageData['queries']);
        $pageData['offsets'] = array( 'current'=>$offset, 'next'=>$nextOffset, 'prev'=>$prevOffset, 'end'=>$endOffset, 'total'=>$totalCount, 'totalCounted'=>$totalCounted);
        $pageData['bean'] = array('objectName' => $seed->object_name, 'moduleDir' => $seed->module_dir, 'moduleName' => strtr($seed->module_dir, $module_names));
        $pageData['stamp'] = $this->lvde->stamp;
        $pageData['access'] = array('view' => $this->lvde->seed->ACLAccess('DetailView'), 'edit' => $this->lvde->seed->ACLAccess('EditView'));
        $pageData['idIndex'] = $idIndex;
        if (!$this->lvde->seed->ACLAccess('ListView')) {
            $pageData['error'] = 'ACL restricted access';
        }

        $queryString = '';

        if ((isset($request["searchFormTab"]) && $request["searchFormTab"] == "advanced_search") ||
            (isset($request["type_basic"]) && ((is_countable($request["type_basic"]) ? count($request["type_basic"]) : 0) > 1 || $request["type_basic"][0] != "")) ||
            (isset($request["module"]) && $request["module"] == "MergeRecords")) {
            $queryString = "-advanced_search";
        } else {
            if (isset($request["searchFormTab"]) && $request["searchFormTab"] == "basic_search") {
                // TODO: figure out what was the SearchFormReports???
                if ($seed->module_dir == "Reports") {
                    $searchMetaData = SearchFormReports::retrieveReportsSearchDefs();
                } else {
                    $searchMetaData = SearchForm::retrieveSearchDefs($seed->module_dir);
                } // TODO: figure out which SearchForm is it?

                $basicSearchFields = array();

                if (isset($searchMetaData['searchdefs']) && isset($searchMetaData['searchdefs'][$seed->module_dir]['layout']['basic_search'])) {
                    $basicSearchFields = $searchMetaData['searchdefs'][$seed->module_dir]['layout']['basic_search'];
                }

                foreach ($basicSearchFields as $basicSearchField) {
                    $field_name = (is_array($basicSearchField) && isset($basicSearchField['name'])) ? $basicSearchField['name'] : $basicSearchField;
                    $field_name .= "_basic";
                    if (isset($request[$field_name])  && (!is_array($basicSearchField) || !isset($basicSearchField['type']) || $basicSearchField['type'] == 'text' || $basicSearchField['type'] == 'name')) {
                        // Ensure the encoding is UTF-8
                        $queryString = htmlentities((string) $request[$field_name], null, 'UTF-8');
                        break;
                    }
                }
            }
        }



        $ret =  array('data'=>$data , 'pageData'=>$pageData, 'query' => $queryString);

        return $ret;
    }
}
