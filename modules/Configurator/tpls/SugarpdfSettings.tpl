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

<script type='text/javascript'>var fileFields = new Array();</script>
<BR>
<form name="ConfigureSugarpdfSettings" enctype='multipart/form-data' method="POST" action="index.php?action=SugarpdfSettings&module=Configurator" onSubmit="if(checkFileType(null,1))return (check_form('ConfigureSugarpdfSettings'));else return false;">
<span class='error'>{$error}</span>
<table width="100%" cellpadding="0" cellspacing="0" border="0">
    <tr>
        <td style="padding-bottom: 2px;">
            <input title="{$APP.LBL_SAVE_BUTTON_TITLE}" accessKey="{$APP.LBL_SAVE_BUTTON_KEY}" class="button"  type="submit"  name="save" value="  {$APP.LBL_SAVE_BUTTON_LABEL}  " >
            &nbsp;<input title="{$MOD.LBL_RESTORE_BUTTON_LABEL}" class="button"  type="submit"  name="restore" value="  {$MOD.LBL_RESTORE_BUTTON_LABEL}  " >
            &nbsp;<input title="{$MOD.LBL_CANCEL_BUTTON_TITLE}"  onclick="document.location.href='index.php?module=Administration&action=index'" class="button"  type="button" name="cancel" value="  {$APP.LBL_CANCEL_BUTTON_LABEL}  " >
        </td>
    </tr>
</table>
<table width="100%" border="0" cellspacing="0" cellpadding="0">
    <tr>
    <td>

    <table width="100%" border="0" cellspacing="0" cellpadding="0"  class="edit view" {if $pdf_enable_ezpdf=="0"}style="display:none"{/if}>
        <tr>
           <td scope="row" style="text-align: center;">{html_radios name="sugarpdf_pdf_class" options=$pdf_class selected=$selected_pdf_class separator='    ' onchange='processPDFClass()'}</td>
        </tr>
    </table>

    <table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view" id="settingsForTCPDF">
        <tr>
            <th align="left" scope="row" colspan="4"><h4 >{$MOD.SUITEPDF_BASIC_SETTINGS}</h4></th>
        </tr>
        <tr>
            <td scope="row">
            <table width="100%" border="0" cellspacing="0" cellpadding="0">
                {counter start=0 assign='count'}
                {foreach from=$SugarpdfSettings item=property key=name}
                    {if $property.class == "basic"}
                        {counter}
                        {include file="modules/Configurator/tpls/SugarpdfSettingsFields.tpl"}
                    {/if}
                {/foreach}
                {if $count is odd}
                        <td  ></td>
                        <td  ></td>
                    </tr>
                {/if}
            </table>
            </td>
        </tr>
    </table>


    <table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
        <tr>
            <th align="left" scope="row" colspan="4"><h4 >{$MOD.SUITEPDF_LOGO_SETTINGS}</h4></th>
        </tr>
        <tr>
            <td scope="row">
            <table width="100%" border="0" cellspacing="0" cellpadding="0">
                {counter start=0 assign='count'}
                {foreach from=$SugarpdfSettings item=property key=name}
                    {if $property.class == "logo"}
                        {counter}
                        {include file="modules/Configurator/tpls/SugarpdfSettingsFields.tpl"}
                    {/if}
                {/foreach}
                {if $count is odd}
                        <td  ></td>
                        <td  ></td>
                    </tr>
                {/if}
            </table>
            </td>
        </tr>
    </table>
<!--
    <table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
        <tr>
            <th align="left" scope="row" colspan="4"><h4 >{$MOD.SUITEPDF_ADVANCED_SETTINGS}</h4></th>
        </tr>
        <tr>
            <td scope="row" scope="row">
            <table width="100%" border="0" cellspacing="0" cellpadding="0">
                {counter start=0 assign='count'}
                {foreach from=$SugarpdfSettings item=property key=name}
                    {if $property.class == "advanced"}
                        {counter}
                        {include file="modules/Configurator/tpls/SugarpdfSettingsFields.tpl"}
                    {/if}
                {/foreach}
                {if $count is odd}
                        <td  ></td>
                        <td  ></td>
                    </tr>
                {/if}
            </table>
            </td>
        </tr>
    </table>
-->
    </td>
    </tr>
</table>

<div style="padding-top: 2px;">
<input title="{$APP.LBL_SAVE_BUTTON_TITLE}" class="button"  type="submit" name="save" value="  {$APP.LBL_SAVE_BUTTON_LABEL}  " />
&nbsp;<input title="{$MOD.LBL_RESTORE_BUTTON_LABEL}" class="button"  type="submit"  name="restore" value="  {$MOD.LBL_RESTORE_BUTTON_LABEL}  " >
&nbsp;<input title="{$MOD.LBL_CANCEL_BUTTON_TITLE}"  onclick="document.location.href='index.php?module=Administration&action=index'" class="button"  type="button" name="cancel" value="  {$APP.LBL_CANCEL_BUTTON_LABEL}  " />
</div>
{$JAVASCRIPT}
</form>
{literal}
<script type='text/javascript'>
function checkFileType(id, submit) {
	if (submit == 0) {
		var fileName = document.getElementById(id).value;
	  	if ({/literal}{$GD_WARNING}{literal}==1 && (fileName.lastIndexOf(".png") != -1||
	  		fileName.lastIndexOf(".PNG") != -1)) {
	  		fileFields[id]=id;
	  		//alert({/literal}SUGAR.language.get('Configurator', 'PDF_GD_WARNING'{literal}));
	  	}
  	}
  	else if (submit == 1) {
  		for (fileField in fileFields) {
			var fileName = document.getElementById(fileField).value;
		  	if ({/literal}{$GD_WARNING}{literal}==1 && (fileName.lastIndexOf(".png") != -1||
		  		fileName.lastIndexOf(".PNG") != -1)) {
		  		//add_error_style('ConfigureSugarpdfSettings', fileField, SUGAR.language.get('Configurator', 'PDF_GD_WARNING'));
		  		alert({/literal}SUGAR.language.get('Configurator', 'PDF_GD_WARNING'{literal}));
		  		return false;
		  	}
  		}
  	}
  	return true;
 }
 function verifyPercent(id){
     var s = document.getElementById(id).value;
     if(isInteger(s)){
         if(inRange(s, 0, 100)){
             return true;
         }else{
             document.getElementById(id).value = "";
             return false;
         }
     }else{
         document.getElementById(id).value = "";
         return false;
     }
 }
 function verifyNumber(id){
     var s = document.getElementById(id).value;
     if(isNumeric(s)){
         return true;
     }else{
         document.getElementById(id).value = "";
         return false;
     }
 }
 function processPDFClass(){
     document.getElementById('settingsForTCPDF').style.display="";
     if(!check_form('ConfigureSugarpdfSettings')){
         for (var i = 0; i <document.ConfigureSugarpdfSettings.sugarpdf_pdf_class.length; i++) {
             if(document.ConfigureSugarpdfSettings.sugarpdf_pdf_class[i].value == "TCPDF"){
                 document.ConfigureSugarpdfSettings.sugarpdf_pdf_class[i].checked=true;
             }
         }
     }else{
         var chosen = "";
         for (var i = 0; i <document.ConfigureSugarpdfSettings.sugarpdf_pdf_class.length; i++) {
             if (document.ConfigureSugarpdfSettings.sugarpdf_pdf_class[i].checked) {
                 chosen = document.ConfigureSugarpdfSettings.sugarpdf_pdf_class[i].value;
             }
         }
         if(chosen == "EZPDF"){
             document.getElementById('settingsForTCPDF').style.display="none";
         }
     }
 }
 processPDFClass();
</script>
{/literal}
