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






#[\AllowDynamicProperties]
class SugarWidgetSubPanelRemoveButtonMeetings extends SugarWidgetField
{
    public function displayHeaderCell($layout_def)
    {
        return '&nbsp;';
    }

    public function displayList(&$layout_def)
    {
        global $app_strings;
        
                
        $parent_record_id = $_REQUEST['record'];
        $parent_module = $_REQUEST['module'];

        $action = 'DeleteRelationship';
        $record = $layout_def['fields']['ID'];

        $return_module = $_REQUEST['module'];
        $return_action = 'SubPanelViewer';
        $subpanel = $layout_def['subpanel_id'];
        $return_id = $_REQUEST['record'];
        
        
        if (isset($GLOBALS['FOCUS'])) {
            $focus = $GLOBALS['FOCUS'];
        }
        
        /* Handle case where we generate subpanels from MySettings/LoadTabSubpanels.php */
        else {
            if ($return_module == 'MySettings') {
                global $beanList, $beanFiles;
                $return_module = $_REQUEST['loadModule'];
            
                $class = $beanList[$return_module];
                require_once($beanFiles[$class]);
                $focus = new $class();
                $focus->retrieve($return_id);
            }
        }
        
        //CCL - Comment out restriction to not remove assigned user
        //if($focus->assigned_user_id == $record) return '';
        
        if (isset($layout_def['linked_field_set']) && !empty($layout_def['linked_field_set'])) {
            $linked_field= $layout_def['linked_field_set'] ;
        } else {
            $linked_field = $layout_def['linked_field'];
        }
        $refresh_page = 0;
        if (!empty($layout_def['refresh_page'])) {
            $refresh_page = 1;
        }
        $return_url = "index.php?module=$return_module&action=$return_action&subpanel=$subpanel&record=$return_id&sugar_body_only=1";

        $icon_remove_text = $app_strings['LBL_ID_FF_REMOVE'];
        $remove_url = $layout_def['start_link_wrapper']
            . "index.php?module=$parent_module"
            . "&action=$action"
            . "&record=$parent_record_id"
            . "&linked_field=$linked_field"
            . "&linked_id=$record"
            . "&return_url=" . urlencode(urlencode($return_url))
            . "&refresh_page=$refresh_page"
            . $layout_def['end_link_wrapper'];
        $remove_confirmation_text = $app_strings['NTC_REMOVE_CONFIRMATION'];
        //based on listview since that lets you select records
        if ($layout_def['ListView']) {
            return "<a href=\"javascript:sub_p_rem('$subpanel', '$linked_field'" .", '$record', $refresh_page);\""
                    . ' class="listViewTdToolsS1"' . " onclick=\"return sp_rem_conf();\"" . ">$icon_remove_text</a>";
        } else {
            return '';
        }
    }
}
