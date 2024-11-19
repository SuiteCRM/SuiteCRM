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
<div id="overDiv" style="position:absolute; visibility:hidden; z-index:1000"></div>
{literal}
<script>
addForm('popup_form');
// STIC-Custom 20240916 MHP - Reference to the closing cross of the tab
// https://github.com/SinergiaTIC/SinergiaCRM/pull/464
var closeButtonSelector = "a.sugar-tab-close[href='javascript:void(0);']";
// END STIC-Custom
</script>
{/literal}

<form name='popup_form' id='popup_form_id' onsubmit='return false;'>
<input type='hidden' name='module' value='ModuleBuilder'>
<input type='hidden' name='action' value='{$action}'>
<input type='hidden' name='new_dropdown' value=''>
<input type='hidden' name='to_pdf' value='true'>
<input type='hidden' name='view_module' value='{$module->name}'>
{if isset($package->name)}
    <input type='hidden' name='view_package' value='{$package->name}'>
{/if}
{if $is_update}
	<input type='hidden' name='is_update' value='true'>
{else}
	<input type='hidden' name='is_update' value='false'>
{/if}
	{if $hideLevel < 5}
	    &nbsp;
	    <input type='button' class='button' name='fsavebtn' value='{$mod_strings.LBL_BTN_SAVE}' 
			onclick='{literal}if(validate_type_selection() && check_form("popup_form")){ {/literal}{$preSave} {literal}ModuleBuilder.submitForm("popup_form_id"); }{/literal}'>
		{* STIC-Custom 20240916 MHP - Close tab when clicking cancel
		   https://github.com/SinergiaTIC/SinergiaCRM/pull/464
		<input type='button' name='cancelbtn' value='{$mod_strings.LBL_BTN_CANCEL}' 
			onclick='ModuleBuilder.tabPanel.removeTab(ModuleBuilder.findTabById("east"));' class='button'> *}
		<input type='button' name='cancelbtn' value='{$mod_strings.LBL_BTN_CANCEL}' 
			onclick='document.querySelector(closeButtonSelector).click();' class='button'>
		{* END STIC-Custom *}
	    {if !empty($vardef.name)}
	        {if $hideLevel < 3}
	        {literal}
	            &nbsp;<input type='button' class='button' name='fdeletebtn' value='{/literal}{$mod_strings.LBL_BTN_DELETE}{literal}' onclick='if(confirm("{/literal}{$mod_strings.LBL_CONFIRM_FIELD_DELETE}{literal}")){document.popup_form.action.value="DeleteField";ModuleBuilder.submitForm("popup_form_id");}'>
	        {/literal}
	        {/if}
	        {if !$no_duplicate}
			{* STIC-Custom 20240916 MHP - Close the editor before sending the clone request
			   https://github.com/SinergiaTIC/SinergiaCRM/pull/464 
			&nbsp;<input type='button' class='button' name='fclonebtn' value='{/literal}{$mod_strings.LBL_BTN_CLONE}{literal}' onclick='document.popup_form.action.value="CloneField";ModuleBuilder.submitForm("popup_form_id");'> *}	        
			{literal}
			&nbsp;<input type='button' class='button' name='fclonebtn' value='{/literal}{$mod_strings.LBL_BTN_CLONE}{literal}' onclick='if (tinyMCE.get("htmlarea")) { tinymce.execCommand("mceRemoveControl", false, "htmlarea"); } document.popup_form.action.value="CloneField";ModuleBuilder.submitForm("popup_form_id");'>
	        {/literal}
			{* END STIC-Custom *}			
	    {/if}
	    {/if}
	
	{else}
	    {literal}
	     <input type='button' class='button' name='lsavebtn' value='{/literal}{$mod_strings.LBL_BTN_SAVE}{literal}' onclick='if(check_form("popup_form")){this.form.action.value = "{/literal}{$action}{literal}";ModuleBuilder.submitForm("popup_form_id")};'>
	    {/literal}
		{* STIC-Custom 20240916 MHP - Close the editor before sending the clone request
		   https://github.com/SinergiaTIC/SinergiaCRM/pull/464		 
		&nbsp;<input type='button' class='button' name='fclonebtn' value='{/literal}{$mod_strings.LBL_BTN_CLONE}{literal}' onclick='document.popup_form.action.value="CloneField";ModuleBuilder.submitForm("popup_form_id");'> *}	        
	    {literal}
	        &nbsp;<input type='button' class='button' name='fclonebtn' value='{/literal}{$mod_strings.LBL_BTN_CLONE}{literal}' onclick='if (tinyMCE.get("htmlarea")) { tinymce.execCommand("mceRemoveControl", false, "htmlarea"); } document.popup_form.action.value="CloneField";ModuleBuilder.submitForm("popup_form_id");'>
	     {/literal}
		{* END STIC-Custom *}			 
		 {literal}
	        &nbsp;<input type='button' class='button' name='cancel' value='{/literal}{$mod_strings.LBL_BTN_CANCEL}{literal}' onclick='ModuleBuilder.tabPanel.get("activeTab").close()'>
	        {/literal}
	        
{/if}
<hr>

<table width="400px" >
<tr>
    <td class="mbLBL" style="width:92px;">{sugar_translate module="DynamicFields" label="COLUMN_TITLE_DATA_TYPE"}:</td>
    <td >{if empty($vardef.name) && $isClone == 0}
                {html_options name="type" id="type"  options=$field_types selected=$vardef.type onchange='ModuleBuilder.moduleLoadField("", this.options[this.selectedIndex].value);'}
                {sugar_help text=$mod_strings.LBL_POPHELP_FIELD_DATA_TYPE FIXX=250 FIXY=80}
            {else}
                {$field_types[$vardef.type]}
            {/if}
            {if empty($field_types[$vardef.type]) && !empty($vardef.type)}({$vardef.type}){/if}
            <input type='hidden' name='type' value={$vardef.type} />
    </td>
</tr>
</table>
{$fieldLayout}
</form>

<script>
{literal}
function validate_type_selection(){
    var typeSel = document.getElementById('type');
    if(typeSel && typeSel.options){
        if(typeSel.options[typeSel.selectedIndex].value == ''){
            alert('{/literal}{sugar_translate module="DynamicFields" label="ERR_SELECT_FIELD_TYPE"}{literal}');
            return false;
        }
    }
    if (document.getElementById("customTypeValidate")){
        return document.getElementById("customTypeValidate").onchange(); 
    }
    return true;
}
{/literal}
ModuleBuilder.helpSetup('fieldsEditor','{$help_group}');
</script>
{* <script>//Need this to work in FF4. Bug where last script isn't executed.</script> *}
