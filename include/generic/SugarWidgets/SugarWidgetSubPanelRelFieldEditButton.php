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





//TODO Rename this to edit link
#[\AllowDynamicProperties]
class SugarWidgetSubPanelRelFieldEditButton extends SugarWidgetField
{
    public function displayHeaderCell($layout_def)
    {
        return '&nbsp;';
    }

    public function displayList($layout_def)
    {
        die("<pre>" . print_r($layout_def, true) . "</pre>");

        $rel = $layout_def['linked_field'];
        $module = $layout_def['module'];


        global $app_strings;

        $edit_icon_html = SugarThemeRegistry::current()->getImage(
            'edit_inline',
            'align="absmiddle" alt="' . $app_strings['LNK_EDIT'] . '" border="0"'
        );

        $script = "
        function editRel(name, id, module) {
            editRelPanel = new YAHOO.SUGAR.AsyncPanel('rel_edit', {
                width: 500,
                draggable: true,
                close: true,
                constraintoviewport: true,
                fixedcenter: false
            });
            var a = editRelPanel;
			a.setHeader( 'Edit Properties' );
			a.render(document.body);
			a.params = {
                module: 'Relationships',
                action: 'editfields',
                rel_module: module,
                id: id,
                rel: name,
                to_pdf: 1
            };
            a.load('index.php?' + SUGAR.util.paramsToUrl(a.params));
            a.show();
            a.center();
		}";

        return "<script>$script</script>"
             . '<div onclick="editRel(\'p1_b1_accounts\', \'cac203f3-0380-495f-3231-4cf58f089f00\', \'Accounts\')">'
             . $edit_icon_html . "</div>";
    }
}
