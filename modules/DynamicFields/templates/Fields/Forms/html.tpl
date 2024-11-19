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


{include file="modules/DynamicFields/templates/Fields/Forms/coreTop.tpl"}

<tr>
    <td class="mbLBL">{sugar_translate module="DynamicFields" label="COLUMN_TITLE_HTML_CONTENT"}:</td>
    <td>
    {if $hideLevel < 5}
        <textarea name='htmlarea' id='htmlarea' cols=50 rows=10>{$HTML_EDITOR}</textarea>
        <input type='hidden' name='ext4' id='ext4' value='{$cf.ext4}'/>
    {else}
        <textarea name='htmlarea' id='htmlarea' cols=50 rows=10 disabled>{$HTML_EDITOR}</textarea>
        <input type='hidden' name='htmlarea' value='{$HTML_EDITOR}'/>
    {/if}
        <br>
    </td>
</tr>
{include file="modules/DynamicFields/templates/Fields/Forms/coreBottom.tpl"}

<script type="text/javascript" language="Javascript">
SUGAR.ajaxLoad = true;
{if $hideLevel < 5}
    setTimeout("tinyMCE.execCommand('mceAddControl', false, 'htmlarea');", 500);  
	ModuleBuilder.tabPanel.get("activeTab").closeEvent.subscribe(function(){ldelim}tinyMCE.execCommand('mceRemoveControl', false, 'htmlarea');{rdelim});
	setTimeout("document.forms.popup_form.required.value = false;YAHOO.util.Dom.getAncestorByTagName(document.forms.popup_form.required, 'tr').style.display='none';", 500);
{/if}
{literal}
document.popup_form.presave = function(){
    var tiny = tinyMCE.getInstanceById('htmlarea');
    if ( (null != tiny) || ("undefined" != typeof(tiny)) ) {
         document.getElementById('ext4').value = tiny.getContent();
    } else {
         document.getElementById('ext4').value = document.getElementById('htmlarea').value;
    }
    document.getElementById('ext4').style.display = '';
};
</script>
{/literal}