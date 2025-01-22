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


*}<table width="100%" cellpadding="0" cellspacing="0" border="0" class="actionsContainer">
    <tr>
        <td>
        {if $bean->aclAccess("save")}<input title="{$APP.LBL_SAVE_BUTTON_TITLE}" accessKey="{$APP.LBL_SAVE_BUTTON_KEY}" class="button primary" onclick="this.form.action.value='Save';this.form.return_action.value='ProjectTemplatesListView';if(check_form('{$view}'))return DCMenu.save(this.form.id, 'Project_subpanel_save_button');return false;" type="submit" name="Project_dcmenu_save_button" id="Project_dcmenu_save_button" value="{$APP.LBL_SAVE_BUTTON_LABEL}">{/if}
        {{foreach from=$form.buttons key=val item=button}}
           {{sugar_button module="$module" id=$button view="$view"}}
        {{/foreach}}
        <input title="{$APP.LBL_FULL_FORM_BUTTON_TITLE}" accessKey="{$APP.LBL_FULL_FORM_BUTTON_KEY}" class="button" accept=""  onclick="disableOnUnloadEditView(this.form); this.form.return_action.value='ProjectTemplatesDetailView'; this.form.action.value='ProjectTemplatesEditView'; this.form.return_module.value='Project';this.form.return_id.value=this.form.record.value;if(typeof(this.form.to_pdf)!='undefined') this.form.to_pdf.value='0';SUGAR.ajaxUI.submitForm(this.form,null,true);DCMenu.closeOverlay();"  type="button" name="Project_subpanel_full_form_button"  id="Project_subpanel_full_form_button"  value="{$APP.LBL_FULL_FORM_BUTTON_LABEL}">
        <input type="hidden" name="full_form" value="full_form">
        </td>
        <td align="right" nowrap>
            <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span> {$APP.NTC_REQUIRED}
        </td>
    </tr>
</table>
<script>
    {literal}
    //lets overwrite dcmenu value that is prepoulated and passed into ajaxui to navigate.  This makes sure we go to
    //projectstemplate list view after the save has been processed.
    if(DCMenu){
        DCMenu.qe_refresh = 'SUGAR.ajaxUI.loadContent("index.php?module=Project&action=ProjectTemplatesListView&ignore='+new Date().getTime()+'");';
    }
    {/literal}
</script>
