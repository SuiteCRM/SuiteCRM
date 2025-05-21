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

require_once('include/ytree/Node.php');



//function returns an array of objects of Node type.
function get_node_data($params, $get_array=false)
{
    $ret=array();
    $click_level=$params['TREE']['depth'];
    $subcat_id=$params['NODES'][$click_level]['id'];
    $cat_id=$params['NODES'][$click_level-1]['id'];
    $href=true;
    if (isset($params['TREE']['caller']) && $params['TREE']['caller']=='Documents') {
        $href=false;
    }
    $nodes=get_documents($cat_id, $subcat_id, $href);
    foreach ($nodes as $node) {
        $ret['nodes'][]=$node->get_definition();
    }
    $json = new JSON();
    $str=$json->encode($ret);
    return $str;
}

/*
 *
 *
 */
 function get_category_nodes($href_string)
 {
     $nodes=array();
     global $mod_strings;
     global $app_list_strings;
     $query="select distinct category_id, subcategory_id from documents where deleted=0 order by category_id, subcategory_id";
     $result=DBManagerFactory::getInstance()->query($query);
     $current_cat_id=null;
     $cat_node=null;
     while (($row=DBManagerFactory::getInstance()->fetchByAssoc($result))!= null) {
         if (empty($row['category_id'])) {
             $cat_id='null';
             $cat_name=$mod_strings['LBL_CAT_OR_SUBCAT_UNSPEC'];
         } else {
             $cat_id=$row['category_id'];
             $cat_name=$app_list_strings['document_category_dom'][$row['category_id']];
         }
         if (empty($current_cat_id) || $current_cat_id != $cat_id) {
             $current_cat_id = $cat_id;
             if (!empty($cat_node)) {
                 $nodes[]=$cat_node;
             }
            
             $cat_node = new Node($cat_id, $cat_name);
             $cat_node->set_property("href", $href_string);
             $cat_node->expanded = true;
             $cat_node->dynamic_load = false;
         }

         if (empty($row['subcategory_id'])) {
             $subcat_id='null';
             $subcat_name=$mod_strings['LBL_CAT_OR_SUBCAT_UNSPEC'];
         } else {
             $subcat_id=$row['subcategory_id'];
             $subcat_name=$app_list_strings['document_subcategory_dom'][$row['subcategory_id']];
         }
         $subcat_node = new Node($subcat_id, $subcat_name);
         $subcat_node->set_property("href", $href_string);
         $subcat_node->expanded = false;
         $subcat_node->dynamic_load = true;
        
         $cat_node->add_node($subcat_node);
     }
     if (!empty($cat_node)) {
         $nodes[]=$cat_node;
     }

     return $nodes;
 }
 
function get_documents($cat_id, $subcat_id, $href=true)
{
    $nodes=array();
    $href_string = "javascript:select_document('doctree')";
    $query="select * from documents where deleted=0";
    if ($cat_id != 'null') {
        $query.=" and category_id='" . DBManagerFactory::getInstance()->quote($cat_id) . "'";
    } else {
        $query.=" and category_id is null";
    }
        
    if ($subcat_id != 'null') {
        $query.=" and subcategory_id='" . DBManagerFactory::getInstance()->quote($subcat_id) . "'";
    } else {
        $query.=" and subcategory_id is null";
    }
    $result=DBManagerFactory::getInstance()->query($query);
    $current_cat_id=null;
    while (($row=DBManagerFactory::getInstance()->fetchByAssoc($result))!= null) {
        $node = new Node($row['id'], $row['document_name']);
        if ($href) {
            $node->set_property("href", $href_string);
        }
        $node->expanded = true;
        $node->dynamic_load = false;
        
        $nodes[]=$node;
    }
    return $nodes;
}
