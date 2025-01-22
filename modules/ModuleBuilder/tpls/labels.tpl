{*
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

*}
<form name = 'editlabels' id = 'editlabels' onsubmit='return false;'>
<input type='hidden' name='module' value='ModuleBuilder'>
<input type='hidden' name='action' value='saveLabels'>
<input type='hidden' name='view_module' value='{$view_module}'>
{if $view_package}
    <input type='hidden' name='view_package' value='{$view_package}'>
{/if}
{if $inPropertiesTab}
<input type='hidden' name='editLayout' value='{$editLayout}'>
{elseif $mb}
<input class='button' name = 'saveBtn' id = "saveBtn" type='button' value='{$mod_strings.LBL_BTN_SAVE}' onclick='ModuleBuilder.handleSave("editlabels" );'>
{else}
<input class='button' name = 'publishBtn' id = "publishBtn" type='button' value='{$mod_strings.LBL_BTN_SAVEPUBLISH}' onclick='ModuleBuilder.handleSave("editlabels" );'>
<input class='button' name = 'renameModBtn' id = "renameModBtn" type='button' value='{$mod_strings.LBL_BTN_RENAME_MODULE}'
       onclick='document.location.href = "index.php?action=wizard&module=Studio&wizard=StudioWizard&option=RenameTabs"'>
{/if}
{if !empty($labels_choice)}
<div style="float: right">
            {html_options name='labels' options=$labels_choice selected=$labels_current onchange='this.form.action.value="EditLabels";ModuleBuilder.handleSave("editlabels")'}
            </div>
{/if}
<hr >
<input type='hidden' name='to_pdf' value='1'>

<table class='mbLBL'>
    <tr>
        <td align="right" style="padding: 0 1em 0 0">
            {$mod_strings.LBL_LANGUAGE}
        </td>
        <td align='left'>
	{html_options name='selected_lang' options=$available_languages selected=$selected_lang onchange='this.form.action.value="EditLabels";ModuleBuilder.handleSave("editlabels")'}
        </td>
	</tr>
    {foreach from=$MOD_LABELS item='label' key='key'}
    <tr>
        <td align="right" style="padding: 0 1em 0 0">{$key}:</td>
        <td><input type='hidden' name='label_{$key}' id='label_{$key}' value='no_change'><input id='input_{$key}' onchange='document.getElementById("label_{$key}").value = this.value; ModuleBuilder.state.isDirty=true;' value='{$label}' size='60'></td>
    </tr>
    {/foreach}

</table>
{if $inPropertiesTab}
    <input class='button' type='button' value='{$APP.LBL_CANCEL_BUTTON_LABEL}' onclick="ModuleBuilder.hidePropertiesTab();">
{/if}
</form>
<script>
    //ModuleBuilder.helpRegisterByID('editlabels', 'a');
    ModuleBuilder.helpRegister('editlabels');
    ModuleBuilder.helpSetup('labelsHelp','default');
</script>
