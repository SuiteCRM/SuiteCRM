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



//this widget is used only by the contracts module..


#[\AllowDynamicProperties]
class SugarWidgetSubPanelGetLatestButton extends SugarWidgetField
{
    public function displayHeaderCell($layout_def)
    {
        return '&nbsp;';
    }

    public function displayList(&$layout_def)
    {
        //if the contract has been executed or selected_revision is same as latest revision
        //then hide the latest button.
        //if the contract state is executed or document is not a template hide this action.
        if (
            (
                !empty($layout_def['fields']['CONTRACT_STATUS']) &&
                $layout_def['fields']['CONTRACT_STATUS'] === 'executed'
            ) ||
            $layout_def['fields']['SELECTED_REVISION_ID'] === $layout_def['fields']['LATEST_REVISION_ID']
        ) {
            return "";
        }

        global $app_strings;


        $href = 'index.php?module=' . $layout_def['module']
            . '&action=' . 'GetLatestRevision'
            . '&record=' . $layout_def['fields']['ID']
            . '&return_module=' . $_REQUEST['module']
            . '&return_action=' . 'DetailView'
            . '&return_id=' . $_REQUEST['record']
            . '&get_latest_for_id=' . $layout_def['fields']['LINKED_ID'];

        $edit_icon_html = SugarThemeRegistry::current()->getImage('getLatestDocument', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LNK_GET_LATEST']);
        if ($layout_def['EditView']) {
            return '<a href="' . $href . '"' . "title ='". $app_strings['LNK_GET_LATEST_TOOLTIP']  ."'"
            . 'class="listViewTdToolsS1">' . $edit_icon_html . '&nbsp;' . $app_strings['LNK_GET_LATEST'] .'</a>&nbsp;';
        } else {
            return '';
        }
    }
}
