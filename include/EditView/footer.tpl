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
<script language="javascript">
    var _form_id = '{$form_id}';
    {literal}
    SUGAR.util.doWhen(function(){
        _form_id = (_form_id == '') ? 'EditView' : _form_id;
        return document.getElementById(_form_id) != null;
    }, SUGAR.themes.actionMenu);
    {/literal}
</script>

<table width="100%" cellpadding="0" cellspacing="0" border="0" class="dcQuickEdit">
    <tr>
        <td class="buttons">
            {assign var='place' value="_FOOTER"}
            <!-- to be used for id for buttons with custom code in def files-->
            {{if isset($form.hidden)}}
            {{foreach from=$form.hidden item=field}}
            {{$field}}
            {{/foreach}}
            {{/if}}
            {{if empty($form.button_location) || $form.button_location == 'bottom'}}
            {{if !empty($form) && !empty($form.buttons)}}
            {{foreach from=$form.buttons key=val item=button}}
            {{sugar_button module="$module" id=$button form_id="$form_id" view="$view" appendTo="footer_buttons" location="FOOTER"}}
            {{/foreach}}
            {{else}}
            {{sugar_button module="$module" id="SAVE" view="$view" form_id="$form_id" location="FOOTER" appendTo="footer_buttons"}}
            {{sugar_button module="$module" id="CANCEL" view="$view" form_id="$form_id" location="FOOTER" appendTo="footer_buttons"}}
            {{/if}}
            {{if empty($form.hideAudit) || !$form.hideAudit}}
            {{sugar_button module="$module" id="Audit" view="$view" form_id="$form_id" appendTo="footer_buttons"}}
            {{/if}}
            {{/if}}
            {{sugar_action_menu buttons=$footer_buttons class="fancymenu" flat=true}}
        </td>
        <td align='right' class="edit-view-pagination">{{$ADMIN_EDIT}}
            {{if $panelCount == 0}}
            {{* Render tag for VCR control if SHOW_VCR_CONTROL is true *}}
            {{if $SHOW_VCR_CONTROL}}
            {$PAGINATION}
            {{/if}}
            {{/if}}
        </td>
    </tr>
</table>
</form>
{{if $externalJSFile}}
{sugar_include include=$externalJSFile}
{{/if}}

{$set_focus_block}

{{if isset($scriptBlocks)}}
<!-- Begin Meta-Data Javascript -->
{{$scriptBlocks}}
<!-- End Meta-Data Javascript -->
{{/if}}
<script>SUGAR.util.doWhen("document.getElementById('EditView') != null",
        function(){ldelim}SUGAR.util.buildAccessKeyLabels();{rdelim});
</script>
