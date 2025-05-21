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
class SugarWidgetSubPanelRemoveButtonProjects extends SugarWidgetField
{
    public function displayHeaderCell($layout_def)
    {
        return '&nbsp;';
    }

    public function displayList($layout_def)
    {
        global $app_strings;

        global $current_user;

        $parent_record_id = $_REQUEST['record'];
        $parent_module = $_REQUEST['module'];

        if ($layout_def['module'] == 'Holidays') {
            $action = 'DeleteHolidayRelationship';
        } else {
            if ($layout_def['module'] == 'Users' || $layout_def['module'] == 'Contacts') {
                $action = 'DeleteResourceRelationship';
            } else {
                $action = 'DeleteRelationship';
            }
        }

        $record = $layout_def['fields']['ID'];
        $current_module=$layout_def['module'];
        $hideremove=false;

        $return_module = $_REQUEST['module'];
        $return_action = 'SubPanelViewer';
        $subpanel = $layout_def['subpanel_id'];
        $return_id = $_REQUEST['record'];


        $focus = BeanFactory::newBean('Project');

        $focus->retrieve($return_id);

        if ($current_user->id == $focus->assigned_user_id || is_admin($current_user)) {
            $is_owner = true;
        } else {
            $is_owner = false;
        }

        if (isset($layout_def['linked_field_set']) && !empty($layout_def['linked_field_set'])) {
            $linked_field= $layout_def['linked_field_set'] ;
        } else {
            $linked_field = $layout_def['linked_field'];
        }
        $refresh_page = 0;
        if (!empty($layout_def['refresh_page'])) {
            $refresh_page = 1;
        }
        $return_url = "index.php?module=$return_module&action=$return_action&subpanel=$subpanel&record=$return_id&sugar_body_only=1&inline=1";

        $icon_remove_text = strtolower($app_strings['LBL_ID_FF_REMOVE']);
        $icon_remove_html = SugarThemeRegistry::current()->getImage('delete_inline', 'align="absmiddle" border="0"', null, null, '.gif', '');//setting alt to blank on purpose on subpanels for 508
        $remove_url = $layout_def['start_link_wrapper']
            . "index.php?module=$parent_module"
            . "&action=$action"
            . "&record=$parent_record_id"
            . "&linked_field=$linked_field"
            . "&linked_id=$record"
            . "&return_url=" . urlencode(urlencode($return_url))
            . "&refresh_page=1"
            . $layout_def['end_link_wrapper'];
        $remove_confirmation_text = $app_strings['NTC_REMOVE_CONFIRMATION'];
        //based on listview since that lets you select records
        if ($layout_def['ListView'] && !$hideremove && $is_owner) {
            return '<a href="' . $remove_url . '"'
            . ' class="listViewTdToolsS1"'
            . " onclick=\"return confirm('$remove_confirmation_text');\""
            . ">$icon_remove_html&nbsp;$icon_remove_text</a>";
        } else {
            return '';
        }
    }
}
