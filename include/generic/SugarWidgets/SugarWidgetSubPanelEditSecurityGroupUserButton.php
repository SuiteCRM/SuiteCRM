<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 * SugarWidgetSubPanelEditRoleButton
 *
 * SugarCRM is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004 - 2007 SugarCRM Inc.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, see http://www.gnu.org/licenses or write to the Free
 * Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 *
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo. If the display of the logo is not reasonably feasible for
 * technical reasons, the Appropriate Legal Notices must display the words
 * "Powered by SugarCRM".
 */



require_once('include/generic/SugarWidgets/SugarWidgetField.php');

#[\AllowDynamicProperties]
class SugarWidgetSubPanelEditSecurityGroupUserButton extends SugarWidgetField
{
    public function displayHeaderCell($layout_def)
    {
        return '&nbsp;';
    }

    public function & displayDetail($layout_def)
    {
        return $this->displayList($layout_def);
    }

    public function displayList(&$layout_def)
    {
        global $app_strings;
        global $image_path;

        $href = 'index.php?module=SecurityGroups'
            . '&action=' . 'SecurityGroupUserRelationshipEdit'
            . '&record=' . $layout_def['fields']['SECURITYGROUP_NONINHERIT_ID']
            . '&return_module=' . $_REQUEST['module']
            . '&return_action=' . 'DetailView'
            . '&return_id=' . $_REQUEST['record'];

        $edit_icon_html = SugarThemeRegistry::current()->getImage('edit_inline', 'align="absmiddle" border="0"', null, null, '.gif', $app_strings['LNK_EDIT']);
        //based on listview since that lets you select records
        if ($layout_def['ListView']) {
            return '<a href="' . $href . '"'
                . 'class="listViewTdToolsS1">' . $edit_icon_html . '&nbsp;' . $app_strings['LNK_EDIT'] .'</a>&nbsp;';
        }
        return '';
    }
}
