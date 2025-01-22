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



/*
ARGS:
 $_REQUEST['method']; : options: 'SaveRelationship','Save','DeleteRelationship','Delete'
 $_REQUEST['module']; : the module associated with this Bean instance (will be used to get the class name)
 $_REQUEST['record']; : the id of the Bean instance
// $_REQUEST['related_field']; : the field name on the Bean instance that contains the relationship
// $_REQUEST['related_record']; : the id of the related record
// $_REQUEST['related_']; : the
// $_REQUEST['return_url']; : the URL to redirect to
//$_REQUEST['return_type']; : when set the results of a report will be linked with the parent.
*/


require_once('include/formbase.php');

$refreshsubpanel=true;
if (isset($_REQUEST['return_type'])  && $_REQUEST['return_type'] == 'report') {
    save_from_report(
        $_REQUEST['subpanel_id'] //report_id
                     ,
        $_REQUEST['record'] //parent_id
                     ,
        $_REQUEST['module'] //module_name
                     ,
        $_REQUEST['subpanel_field_name'] //link attribute name
    );
} else {
    if (isset($_REQUEST['return_type'])  && $_REQUEST['return_type'] == 'addtoprospectlist') {
        $GLOBALS['log']->debug(print_r($_REQUEST, true));
        if (!empty($_REQUEST['prospect_list_id']) && !empty($_REQUEST['prospect_ids'])) {
            add_prospects_to_prospect_list(
            $_REQUEST['prospect_list_id'],
            $_REQUEST['prospect_ids']
        );
        } else {
            $parent = BeanFactory::getBean($_REQUEST['module'], $_REQUEST['record']);
            add_to_prospect_list(
                urldecode($_REQUEST['subpanel_module_name']),
                $_REQUEST['parent_module'],
                $_REQUEST['parent_type'],
                $_REQUEST['subpanel_id'],
            $_REQUEST['child_id'],
                $_REQUEST['link_attribute'],
                $_REQUEST['link_type'],
                $parent
            );
        }

        $refreshsubpanel=false;
    } else {
        if (isset($_REQUEST['return_type'])  && $_REQUEST['return_type'] == 'addcampaignlog') {
            //if param is set to "addcampaignlog", then we need to create a campaign log entry
            //for each campaign id passed in.

            // Get a list of campaigns selected.
            if (isset($_REQUEST['subpanel_id'])  && !empty($_REQUEST['subpanel_id'])) {
                $campaign_ids = $_REQUEST['subpanel_id'];
                global $beanFiles;
                global $beanList;
                //retrieve current bean
                $bean_name = $beanList[$_REQUEST['module']];
                require_once($beanFiles[$bean_name]);
                $focus = new $bean_name();
                $focus->retrieve($_REQUEST['record']);

                require_once('modules/Campaigns/utils.php');
                //call util function to create the campaign log entry
                foreach ($campaign_ids as $id) {
                    create_campaign_log_entry($id, $focus, $focus->module_dir, $focus, $focus->id);
                }
                $refreshsubpanel=true;
            }
        } else {
            global $beanFiles,$beanList;
            $bean_name = $beanList[$_REQUEST['module']];
            require_once($beanFiles[$bean_name]);
            $focus = new $bean_name();

            $focus->retrieve($_REQUEST['record']);

            // If the user selected "All records" from the selection menu, we pull up the list
            // based on the query they used on that popup to relate them to the parent record
            if (!empty($_REQUEST['select_entire_list']) &&  $_REQUEST['select_entire_list'] != 'undefined' && isset($_REQUEST['current_query_by_page'])) {
                $order_by = '';
                $current_query_by_page = $_REQUEST['current_query_by_page'];
                $current_query_by_page_array = json_decode(html_entity_decode((string) $current_query_by_page), true, 512, JSON_THROW_ON_ERROR);

                $module = $current_query_by_page_array['module'];
                $seed = BeanFactory::getBean($module);
                if (empty($seed)) {
                    sugar_die($GLOBALS['app_strings']['ERROR_NO_BEAN']);
                }
                $where_clauses = '';
                require_once('include/SearchForm/SearchForm2.php');

                if (file_exists('custom/modules/'.$module.'/metadata/metafiles.php')) {
                    require('custom/modules/'.$module.'/metadata/metafiles.php');
                } elseif (file_exists('modules/'.$module.'/metadata/metafiles.php')) {
                    require('modules/'.$module.'/metadata/metafiles.php');
                }

                if (file_exists('custom/modules/'.$module.'/metadata/searchdefs.php')) {
                    require_once('custom/modules/'.$module.'/metadata/searchdefs.php');
                } elseif (!empty($metafiles[$module]['searchdefs'])) {
                    require_once($metafiles[$module]['searchdefs']);
                } elseif (file_exists('modules/'.$module.'/metadata/searchdefs.php')) {
                    require_once('modules/'.$module.'/metadata/searchdefs.php');
                }

                if (!empty($metafiles[$module]['searchfields'])) {
                    require_once($metafiles[$module]['searchfields']);
                } elseif (file_exists('modules/'.$module.'/metadata/SearchFields.php')) {
                    require_once('modules/'.$module.'/metadata/SearchFields.php');
                }
                if (!empty($searchdefs) && !empty($searchFields)) {
                    $searchForm = new SearchForm($seed, $module);
                    $searchForm->setup($searchdefs, $searchFields, 'SearchFormGeneric.tpl');
                    $searchForm->populateFromArray($current_query_by_page_array, 'advanced');
                    $where_clauses_arr = $searchForm->generateSearchWhere(true, $module);
                    if ((is_countable($where_clauses_arr) ? count($where_clauses_arr) : 0) > 0) {
                        $where_clauses = '('. implode(' ) AND ( ', $where_clauses_arr) . ')';
                    }
                }
        
                $query = $seed->create_new_list_query($order_by, $where_clauses);
                $result = DBManagerFactory::getInstance()->query($query, true);
                $uids = array();
                while ($val = DBManagerFactory::getInstance()->fetchByAssoc($result, false)) {
                    array_push($uids, $val['id']);
                }
                $_REQUEST['subpanel_id'] = $uids;
            }

            if ($bean_name == 'Team') {
                $subpanel_id = $_REQUEST['subpanel_id'];
                if (is_array($subpanel_id)) {
                    foreach ($subpanel_id as $id) {
                        $focus->add_user_to_team($id);
                    }
                } else {
                    $focus->add_user_to_team($subpanel_id);
                }
            } else {
                //find request paramters with with prefix of REL_ATTRIBUTE_
                //convert them into an array of name value pairs add pass them as
                //parameters to the add metod.
                $add_values =array();
                foreach ($_REQUEST as $key=>$value) {
                    if (strpos($key, "REL_ATTRIBUTE_") !== false) {
                        $add_values[substr($key, 14)]=$value;
                    }
                }
                $relName = $_REQUEST['subpanel_field_name'];
                $focus->load_relationship($relName);
                $focus->$relName->add($_REQUEST['subpanel_id'], $add_values);
                $focus->save();
            }
        }
    }
}

if ($refreshsubpanel) {
    //refresh contents of the sub-panel.
    $GLOBALS['log']->debug("Location: index.php?sugar_body_only=1&module=".$_REQUEST['module']."&subpanel=".$_REQUEST['subpanel_module_name']."&action=SubPanelViewer&inline=1&record=".$_REQUEST['record']);
    if (empty($_REQUEST['refresh_page']) || $_REQUEST['refresh_page'] != 1) {
        $inline = isset($_REQUEST['inline'])?$_REQUEST['inline']: $inline;
        header("Location: index.php?sugar_body_only=1&module=".$_REQUEST['module']."&subpanel=".$_REQUEST['subpanel_module_name']."&action=SubPanelViewer&inline=$inline&record=".$_REQUEST['record']);
    }
    exit;
}
