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

/*********************************************************************************

 * Description:  is a form helper
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

/**
 * Check for null or zero for list of values
 * @param $prefix the prefix of value to be checked
 * @param $required array of value to be checked
 * @return boolean true if all values are set in the array
 */
function checkRequired($prefix, $required)
{
    foreach ($required as $key) {
        if (!isset($_POST[$prefix.$key]) || number_empty($_POST[$prefix.$key])) {
            return false;
        }
    }
    return true;
}

/**
 * Populating bean from $_POST
 *
 * @param string $prefix of name of fields
 * @param SugarBean $focus bean
 * @param bool $skipRetrieve do not retrieve data of bean
 * @param bool $checkACL do not update fields if they are forbidden for current user
 * @return SugarBean
 */
function populateFromPost($prefix, &$focus, $skipRetrieve = false, $checkACL = false)
{
    global $current_user;

    /* BEGIN - SECURITY GROUPS */
    if(!empty($_REQUEST['dup_checked']) && !empty($_REQUEST[$prefix.'id']))
    {
        $focus->new_with_id = true;
    }
    /* END - SECURITY GROUPS */

    if (!empty($_REQUEST[$prefix.'record']) && !$skipRetrieve) {
        $focus->retrieve($_REQUEST[$prefix.'record']);
    }

    if (!empty($_POST['assigned_user_id']) &&
        ($focus->assigned_user_id != $_POST['assigned_user_id']) &&
        ($_POST['assigned_user_id'] != $current_user->id)) {
        $GLOBALS['check_notify'] = true;
    }
    if (isset($_POST['dup_checked']) && $_POST['dup_checked'] && isset($_POST['id']) && $_POST['id'] != '') {
        $focus->new_with_id = true;
    }
    require_once('include/SugarFields/SugarFieldHandler.php');
    $sfh = new SugarFieldHandler();

    $isOwner = $focus->isOwner($current_user->id);
    $relatedFields = array();
    foreach ($focus->field_defs as $field => $def) {
        if (empty($def['type']) || $def['type'] != 'relate') {
            continue;
        }
        if (empty($def['source']) || $def['source'] != 'non-db') {
            continue;
        }
        if (empty($def['id_name']) || $def['id_name'] == $field) {
            continue;
        }
        $relatedFields[$def['id_name']] = $field;
    }

    foreach ($focus->field_defs as $field=>$def) {
        if ($field == 'id' && !empty($focus->id)) {
            // Don't try and overwrite the ID
            continue;
        }


        $type = !empty($def['custom_type']) ? $def['custom_type'] : $def['type'];
        $sf = $sfh::getSugarField($type);
        if ($sf != null) {
            $sf->save($focus, $_POST, $field, $def, $prefix);
        } else {
            $GLOBALS['log']->fatal("Field '$field' does not have a SugarField handler");
        }

        /*
                if(isset($_POST[$prefix.$field])) {
        			if(is_array($_POST[$prefix.$field]) && !empty($focus->field_defs[$field]['isMultiSelect'])) {
        				if($_POST[$prefix.$field][0] === '' && !empty($_POST[$prefix.$field][1]) ) {
        					unset($_POST[$prefix.$field][0]);
        				}
        				$_POST[$prefix.$field] = encodeMultienumValue($_POST[$prefix.$field]);
        			}

        			$focus->$field = $_POST[$prefix.$field];
        			/*
        			 * overrides the passed value for booleans.
        			 * this will be fully deprecated when the change to binary booleans is complete.
        			 /
        			if(isset($focus->field_defs[$prefix.$field]) && $focus->field_defs[$prefix.$field]['type'] == 'bool' && isset($focus->field_defs[$prefix.$field]['options'])) {
        				$opts = explode("|", $focus->field_defs[$prefix.$field]['options']);
        				$bool = $_POST[$prefix.$field];

        				if(is_int($bool) || ($bool === "0" || $bool === "1" || $bool === "2")) {
        					// 1=on, 2=off
        					$selection = ($_POST[$prefix.$field] == "0") ? 1 : 0;
        				} elseif(is_bool($_POST[$prefix.$field])) {
        					// true=on, false=off
        					$selection = ($_POST[$prefix.$field]) ? 0 : 1;
        				}
        				$focus->$field = $opts[$selection];
        			}
        		} else if(!empty($focus->field_defs[$field]['isMultiSelect']) && !isset($_POST[$prefix.$field]) && isset($_POST[$prefix.$field . '_multiselect'])) {
        			$focus->$field = '';
        		}
        */
    }

    foreach ($focus->additional_column_fields as $field) {
        if (isset($_POST[$prefix.$field])) {
            $value = $_POST[$prefix.$field];
            $focus->$field = $value;
        }
    }
    return $focus;
}

function add_hidden_elements($key, $value)
{
    $elements = '';

    // if it's an array, we need to loop into the array and use square brackets []
    if (is_array($value)) {
        foreach ($value as $k=>$v) {
            $elements .= "<input type='hidden' name='$key"."[$k]' value='$v'>\n";
        }
    } else {
        $elements = "<input type='hidden' name='$key' value='$value'>\n";
    }

    return $elements;
}


function getPostToForm($ignore='', $isRegularExpression=false)
{
    $fields = '';
    if (!empty($ignore) && $isRegularExpression) {
        foreach ($_POST as $key=>$value) {
            if (!preg_match($ignore, $key)) {
                $fields .= add_hidden_elements($key, $value);
            }
        }
    } else {
        foreach ($_POST as $key=>$value) {
            if ($key != $ignore) {
                $fields .= add_hidden_elements($key, $value);
            }
        }
    }
    return $fields;
}

function getGetToForm($ignore='', $usePostAsAuthority = false)
{
    global $log;
    $fields = '';
    foreach ($_GET as $key => $value) {
        if (is_array($key)) {
            if (!empty($key)) {
                $log->warn('$key must be a string');
            }
            continue;
        }

        if (is_array($value)) {
            if (!empty($value)) {
                $log->warn('$value must be a string');
            }
            continue;
        }
        if ($key != $ignore) {
            if (!$usePostAsAuthority || !isset($_POST[$key])) {
                $fields.= "<input type='hidden' name='$key' value='$value'>";
            }
        }
    }
    return $fields;
}
function getAnyToForm($ignore='', $usePostAsAuthority = false)
{
    $fields = getPostToForm($ignore);
    $fields .= getGetToForm($ignore, $usePostAsAuthority);
    return $fields;
}

function handleRedirect($return_id='', $return_module='', $additionalFlags = false)
{
    if (isset($_REQUEST['return_url']) && $_REQUEST['return_url'] != "") {
        header("Location: ". $_REQUEST['return_url']);
        exit;
    }

    $url = buildRedirectURL($return_id, $return_module);
    header($url);
    exit;
}

//eggsurplus: abstract to simplify unit testing
function buildRedirectURL($return_id='', $return_module='')
{
    $status = '';
    if (isset($_REQUEST['return_module']) && $_REQUEST['return_module'] != "") {
        $return_module = $_REQUEST['return_module'];
    } else {
        $return_module = $return_module;
    }
    if (isset($_REQUEST['return_action']) && $_REQUEST['return_action'] != "") {

       //if we are doing a "Close and Create New"
        if (isCloseAndCreateNewPressed()) {
            $return_action = "EditView";
            $isDuplicate = "true";
            $status = "";

            // Meeting Integration
            if (isset($_REQUEST['meetingIntegrationFlag']) && $_REQUEST['meetingIntegrationFlag'] == 1) {
                $additionalFlags = array('meetingIntegrationShowForm' => '1');
            }
            // END Meeting Integration
        }
        // if we create a new record "Save", we want to redirect to the DetailView
        else {
            if (isset($_REQUEST['action']) && $_REQUEST['action'] == "Save"
            && $_REQUEST['return_module'] != 'Activities'
            && $_REQUEST['return_module'] != 'Home'
            && $_REQUEST['return_module'] != 'Forecasts'
            && $_REQUEST['return_module'] != 'Calendar'
            && $_REQUEST['return_module'] != 'MailMerge'
            ) {
                $return_action = 'DetailView';
            } elseif ($_REQUEST['return_module'] == 'Activities' || $_REQUEST['return_module'] == 'Calendar') {
                $return_module = $_REQUEST['module'];
                $return_action = $_REQUEST['return_action'];
            // wp: return action needs to be set for one-click close in task list
            } else {
                // if we "Cancel", we go back to the list view.
                $return_action = $_REQUEST['return_action'];
            }
        }
    } else {
        $return_action = "DetailView";
    }

    if (isset($_REQUEST['return_id']) && $_REQUEST['return_id'] != "") {
        $return_id = $_REQUEST['return_id'];
    }

    $add = "";
    if (isset($additionalFlags) && !empty($additionalFlags)) {
        foreach ($additionalFlags as $k => $v) {
            $add .= "&{$k}={$v}";
        }
    }

    if (!isset($isDuplicate) || !$isDuplicate) {
        $url="index.php?action=$return_action&module=$return_module&record=$return_id&return_module=$return_module&return_action=$return_action{$add}";
        if (isset($_REQUEST['offset']) && empty($_REQUEST['duplicateSave'])) {
            $url .= "&offset=".$_REQUEST['offset'];
        }
        if (!empty($_REQUEST['ajax_load'])) {
            $ajax_ret = array(
                'content' => "<script>SUGAR.ajaxUI.loadContent('$url');</script>\n",
                'menu' => array(
                    'module' => $return_module,
                    'label' => translate($return_module),
                ),
            );
            $json = getJSONobj();
            echo $json->encode($ajax_ret);
        } else {
            return "Location: $url";
        }
    } else {
        $standard = "action=$return_action&module=$return_module&record=$return_id&isDuplicate=true&return_module=$return_module&return_action=$return_action&status=$status";
        $url="index.php?{$standard}{$add}";
        if (!empty($_REQUEST['ajax_load'])) {
            $ajax_ret = array(
                 'content' => "<script>SUGAR.ajaxUI.loadContent('$url');</script>\n",
                 'menu' => array(
                     'module' => $return_module,
                     'label' => translate($return_module),
                 ),
            );
            $json = getJSONobj();
            echo $json->encode($ajax_ret);
        } else {
            return "Location: $url";
        }
    }
}

function getLikeForEachWord($fieldname, $value, $minsize=4)
{
    $value = trim($value);
    $values = explode(' ', $value);
    $ret = '';
    foreach ($values as $val) {
        if (strlen($val) >= $minsize) {
            if (!empty($ret)) {
                $ret .= ' or';
            }
            $ret .= ' '. $fieldname . ' LIKE %'.$val.'%';
        }
    }
}

function isCloseAndCreateNewPressed()
{
    return isset($_REQUEST['action']) &&
           $_REQUEST['action'] == "Save" &&
           isset($_REQUEST['isSaveAndNew']) &&
           $_REQUEST['isSaveAndNew'] == 'true';
}


/**
 * Functions from Save2.php
 * @see include/generic/Save2.php
 */

function add_prospects_to_prospect_list($parent_id, $child_id)
{
    $focus=BeanFactory::getBean('Prospects');
    if (is_array($child_id)) {
        $uids = $child_id;
    } else {
        $uids = array($child_id);
    }

    $relationship = '';
    foreach ($focus->get_linked_fields() as $field => $def) {
        if ($focus->load_relationship($field)) {
            if ($focus->$field->getRelatedModuleName() == 'ProspectLists') {
                $relationship = $field;
                break;
            }
        }
    }

    if ($relationship != '') {
        foreach ($uids as $id) {
            $focus->retrieve($id);
            $focus->load_relationship($relationship);
            $focus->prospect_lists->add($parent_id);
        }
    }
}

function add_to_prospect_list($query_panel, $parent_module, $parent_type, $parent_id, $child_id, $link_attribute, $link_type, $parent)
{
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$query_panel);
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$parent_module);
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$parent_type);
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$parent_id);
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$child_id);
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$link_attribute);
    $GLOBALS['log']->debug('add_prospects_to_prospect_list:parameters:'.$link_type);
    require_once __DIR__ . '/../include/SubPanel/SubPanelTiles.php';

    $allowed_module = true;

	if (!is_admin($GLOBALS['current_user'])) {
        $allowed_module = ACLController::checkModuleAllowed($parent_module, ACLAction::getUserActions($GLOBALS['current_user']->id, false));
	}

    $parent_types = explode(' ', $parent_type);
    $disabled_types = ACLController::disabledModuleList($parent_types, false, 'list');
    foreach ($disabled_types as $disabled_type) {
        unset($parent_types[$disabled_type]);
    }

    if ($allowed_module === false) {
        return false;
    }

    if (!class_exists($parent_type)) {
        require_once __DIR__ . '/../modules/'
            . cleanDirName($parent_module)
            . '/' . cleanDirName((string)$parent_types)
            . '.php';
    }

    $focus = new $parent_type();
    $focus->retrieve($parent_id);
    if (empty($focus->id)) {
        return false;
    }
    if (empty($parent)) {
        return false;
    }

    //if link_type is default then load relationship once and add all the child ids.
    $relationship_attribute=$link_attribute;

    //find all prospects based on the query

    $subpanel = new SubPanelTiles($parent, $parent->module_dir);
    $thisPanel=$subpanel->subpanel_definitions->load_subpanel($query_panel);
    if (empty($thisPanel)) {
        return false;
    }

    // bugfix #57850  filter prospect list based on marketing_id (if it's present)
    if (isset($_REQUEST['marketing_id']) && $_REQUEST['marketing_id'] != 'all') {
        $thisPanel->_instance_properties['function_parameters']['EMAIL_MARKETING_ID_VALUE'] = $_REQUEST['marketing_id'];
    }

    $result = SugarBean::get_union_related_list($parent, '', '', '', 0, -99, -99, '', $thisPanel);

    if (!empty($result['list'])) {
        foreach ($result['list'] as $object) {
            if ($link_type != 'default') {
                $relationship_attribute=strtolower($object->$link_attribute);
            }
            $GLOBALS['log']->debug('add_prospects_to_prospect_list:relationship_attribute:'.$relationship_attribute);
            // load relationship for the first time or on change of relationship atribute.
            if (empty($focus->$relationship_attribute)) {
                $focus->load_relationship($relationship_attribute);
            }
            //add
            $focus->$relationship_attribute->add($object->$child_id);
        }
    }
}

//Link rows returned by a report to parent record.
function save_from_report($report_id, $parent_id, $module_name, $relationship_attr_name)
{
    global $beanFiles;
    global $beanList;

    $GLOBALS['log']->debug("Save2: Linking with report output");
    $GLOBALS['log']->debug("Save2:Report ID=".$report_id);
    $GLOBALS['log']->debug("Save2:Parent ID=".$parent_id);
    $GLOBALS['log']->debug("Save2:Module Name=".$module_name);
    $GLOBALS['log']->debug("Save2:Relationship Attribute Name=".$relationship_attr_name);

    $GLOBALS['log']->debug("Save2:Bean Name=" . $module_name);
    $focus = BeanFactory::newBean($module_name);

    $focus->retrieve($parent_id);
    $focus->load_relationship($relationship_attr_name);

    //fetch report definition.
    global $current_language, $report_modules, $modules_report;

    $mod_strings = return_module_language($current_language, "Reports");


    $saved = new SavedReport();
    $saved->disable_row_level_security = true;
    $saved->retrieve($report_id, false);

    //initiailize reports engine with the report definition.
    require_once('modules/Reports/SubpanelFromReports.php');
    $report = new SubpanelFromReports($saved);
    $report->run_query();

    $sql = $report->query_list[0];
    $GLOBALS['log']->debug("Save2:Report Query=".$sql);
    $result = $report->db->query($sql);

    $reportBean = BeanFactory::newBean($saved->module);
    while ($row = $report->db->fetchByAssoc($result)) {
        $reportBean->id = $row['primaryid'];
        $focus->$relationship_attr_name->add($reportBean);
    }
}
