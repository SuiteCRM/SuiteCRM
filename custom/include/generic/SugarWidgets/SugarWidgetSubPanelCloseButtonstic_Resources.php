<?php
/**
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 */
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 * Class SugarWidgetSubPanelCloseButtonstic_Resources
 *
 * Extends SugarWidgetField to add a close button in the subpanel of the stic_Resources module.
 * 
 * This button allows closing a resource in a booking by creating a new one
 * where the end date is set to the selected close button's date.
 *
 * @category  SugarCRM
 * @package   CustomWidgets
 * @extends   SugarWidgetField
 */
class SugarWidgetSubPanelCloseButtonstic_Resources extends SugarWidgetField
{
    public function displayList(&$layout_def)
    {
        global $app_strings;
        global $subpanel_item_count;
        $return_module = $_REQUEST['module'];
        $return_id = $_REQUEST['record'];
        $module_name = $layout_def['module'];
        $record_id = $layout_def['fields']['ID'];
        $unique_id = $layout_def['subpanel_id'] . "_close_" . $subpanel_item_count;

        if ($module_name === 'stic_Resources') {

            $html = "<a id=\"$unique_id\" href=\"javascript:void(0);\" onclick='closeResource(\"$record_id\", \"$return_id\");'>" . $app_strings['LNK_CLOSE'] . "</a>";
            return $html;
        } else {
            return parent::displayList($layout_def);
        }
    }
}