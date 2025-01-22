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

require_once('soap/SoapHelperFunctions.php');
#[\AllowDynamicProperties]
class MailMergeController extends SugarController
{
    public function __construct()
    {
        parent::__construct();
    }




    public function action_search()
    {
        global $beanList;
        
        //set ajax view
        $this->view = 'ajax';
        //get the module
        $module = !empty($_REQUEST['qModule']) ? $_REQUEST['qModule'] : '';
        //lowercase module name
        $lmodule = strtolower($module);
        //get the search term
        $term = !empty($_REQUEST['term']) ? DBManagerFactory::getInstance()->quote($_REQUEST['term']) : '';

        $max = !empty($_REQUEST['max']) ? $_REQUEST['max'] : 10;
        $order_by = !empty($_REQUEST['order_by']) ? $_REQUEST['order_by'] : $lmodule.".name";
        $offset = !empty($_REQUEST['offset']) ? $_REQUEST['offset'] : 0;
        $response = array();

        if (!empty($module)) {
            $where = '';
            $deleted = '0';
            $using_cp = false;

            if (!empty($term)) {
                if ($module == 'Contacts' || $module == 'Leads') {
                    $where = $lmodule.".first_name like '%".$term."%' OR ".$lmodule.".last_name like '%".$term."%'";
                    $order_by = $lmodule.".last_name";
                } else {
                    $where = $lmodule.".name like '".$term."%'";
                }
            }

            if ($module === 'CampaignProspects') {
                $using_cp = true;
                $module = 'Prospects';
                //in the case of Campaigns we need to use the related module
                $relModule = !empty($_REQUEST['rel_module']) ? $_REQUEST['rel_module'] : null;
                $lmodule = strtolower($relModule);

                if (isset($beanList[$relModule])) {
                    $campaignWhere = $_SESSION['MAILMERGE_WHERE'];
                    $where = $lmodule . ".first_name like '%" . $term . "%' OR " . $lmodule . ".last_name like '%" . $term . "%'";
                    if ($campaignWhere) {
                        $where .= ' AND ' . $campaignWhere;
                    }
                    $where .= ' AND related_type = #' . $lmodule . '#';
                }
            }

            $seed = SugarModule::get($module)->loadBean();

            if ($using_cp) {
                $fields = array('id', 'first_name', 'last_name');
                $dataList = $seed->retrieveTargetList($where, $fields, $offset, -1, $max, $deleted);
            } else {
                $dataList = $seed->get_list($order_by, $where, $offset, -1, $max, $deleted);
            }

            $list = $dataList['list'];
            $row_count = $dataList['row_count'];

            $output_list = array();
            foreach ($list as $value) {
                $output_list[] = get_return_value($value, $module);
            }

            $response['result'] = array('result_count'=>$row_count,'entry_list'=>$output_list);
        }

        $json = getJSONobj();
        $json_response = $json->encode($response, true);
        print $json_response;
    }
}
