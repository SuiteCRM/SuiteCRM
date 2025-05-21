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
{literal}
    <script type="text/javascript">

        /**
         *  Start Bug#50590
         *  mod_array global array that contains required modules
         *  addDropdownElements fills lead_conv_ac_op_sel with all required modules except Contacts
         *  as this module is in the list by default
         */

        var mod_array = new Array;
        function addDropdownElements() {
            var i;
            for (i = 0; i <= mod_array.length - 1; i++) {
                if (mod_array[i] != 'Contacts') {
                    var dropdown = document.getElementById('lead_conv_ac_op_sel');
                    //if dropdown does not exist, then return and exit function
                    if (!dropdown) {
                        return;
                    }
                    var opt = document.createElement("option");
                    opt.text = SUGAR.language.get('app_list_strings', "moduleListSingular")[mod_array[i]];
                    opt.value = mod_array[i];
                    opt.label = opt.text;
                    dropdown.options.add(opt);
                }
            }
        }
        /**
         *   End Bug#50590
         */


        function addRemoveDropdownElement(module) {
            var accountText = document.getElementById('account_name');
            var checkbox = document.getElementById('new' + module);
            var dropdown = document.getElementById('lead_conv_ac_op_sel');
            if (!checkbox || !dropdown) {
                return;
            }
            var found = false;
            var i;
            for (i = dropdown.options.length - 1; i >= 0; i--) {
                if (dropdown.options[i].value == module) {
                    found = true;
                    if (!checkbox.checked) {
                        // if this is Accounts and the text of account name is not empty, do not remove
                        if (module != 'Accounts' || !accountText || accountText.value == '') {
                            dropdown.remove(i);
                        }
                    }
                    break;
                }
            }
            if (!found && checkbox.checked) {
                var opt = document.createElement("option");
                opt.text = SUGAR.language.get('app_list_strings', "moduleListSingular")[module];
                opt.value = module;
                opt.label = opt.text;
                dropdown.options.add(opt);
            }
        }
    </script>
{/literal}

<form action="index.php" method="POST" name="{$form_name}" id="{$form_id}" enctype="multipart/form-data" class="">
    <table width="100%" cellpadding="0" cellspacing="0" border="0">
        <tr>
            <td class="buttons">
                <input type="hidden" name="module" value="{$module}">
                <input type="hidden" name="record" value="{$record_id}">
                <input type="hidden" name="isDuplicate" value="false">
                <input type="hidden" name="action" value="ConvertLead">
                <input type="hidden" name="handle" value="save">
                <input type="hidden" name="return_module" value="{$smarty.request.return_module}">
                <input type="hidden" name="return_action" value="{$smarty.request.return_action}">
                <input type="hidden" name="return_id" value="{$smarty.request.return_id}">
                <input type="hidden" name="module_tab">
                <input type="hidden" name="contact_role">
                {if !empty($smarty.request.return_module) || !empty($smarty.request.relate_to)}
                    <input type="hidden" name="relate_to"
                           value="{if $smarty.request.return_relationship}{$smarty.request.return_relationship}{elseif $smarty.request.relate_to && empty($smarty.request.from_dcmenu)}{$smarty.request.relate_to}{elseif empty($isDCForm) && empty($smarty.request.from_dcmenu)}{$smarty.request.return_module}{/if}">
                    <input type="hidden" name="relate_id" value="{$smarty.request.return_id}">
                {/if}
                <input type="hidden" name="offset" value="{$offset}">
                {if $bean->aclAccess("save")}
                <input title='{sugar_translate label="LBL_SAVE_BUTTON_LABEL"}' id='SAVE_HEADER'
                       accessKey="{sugar_translate label='LBL_SAVE_BUTTON_KEY'}" class="button primary"
        onclick="return check_form('{$form_name}');"
        type="submit" name="button" value="{sugar_translate label='LBL_SAVE_BUTTON_LABEL'}">
{/if}

{if !empty($smarty.request.return_action) && ($smarty.request.return_action == "DetailView" && !empty($record_id))}
    <input title="{sugar_translate label='LBL_CANCEL_BUTTON'}" id="CANCEL_HEADER" accessKey="{sugar_translate label='LBL_CANCEL_BUTTON_KEY'}" class="button"
        onclick="this.form.action.value='DetailView'; this.form.module.value='{$smarty.request.return_module}'; this.form.record.value='{$smarty.request.return_id}';" 
        type="submit" name="button" value="{sugar_translate label='LBL_CANCEL_BUTTON_LABEL'}">
{elseif !empty($smarty.request.return_action) && ($smarty.request.return_action == "DetailView" && !empty($smarty.request.return_id))}';
<input title="{sugar_translate label='LBL_CANCEL_BUTTON_TITLE'}" id="CANCEL_HEADER" accessKey="{sugar_translate label='LBL_CANCEL_BUTTON_KEY'}" class="button"
onclick="this.form.action.value='DetailView'; this.form.module.value='{$smarty.request.return_module}'; this.form.record.value='{$smarty.request.return_id}';"
type="submit" name="button" value="{sugar_translate label='LBL_CANCEL_BUTTON_LABEL'}">
{else}
<input title="{sugar_translate label='LBL_CANCEL_BUTTON_TITLE'}" id="CANCEL_HEADER" accessKey="{sugar_translate label='LBL_CANCEL_BUTTON_KEY'}" class="button"
onclick="this.form.action.value='DetailView'; this.form.module.value='Leads'; this.form.record.value='{$smarty.request.record}';"
type="submit" name="button" value="{sugar_translate label='LBL_CANCEL_BUTTON_LABEL'}">
{/if}
</td>
</tr>
</table>
<div class="panel panel-default panel-convert-lead">
<div class="panel-heading">&nbsp</div>
<div class="panel-body">
