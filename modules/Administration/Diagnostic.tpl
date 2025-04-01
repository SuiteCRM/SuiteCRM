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

/**

 */
*}

<form name="Diagnostic" method="POST" action="index.php">
<input type="hidden" name="module" value="Administration">
<input type="hidden" name="action" value="DiagnosticRun">
 
<table cellpadding="0" cellspacing="0" border="0" width="100%" class="actionsContainer">
	<tr>
	<td>
	<input title="{$MOD.LBL_DIAG_EXECUTE_BUTTON}" class="button" onclick="this.form.action.value='DiagnosticRun';" type="submit" name="button" value="  {$MOD.LBL_DIAG_EXECUTE_BUTTON}  " >
	<input title="{$MOD.LBL_DIAG_CANCEL_BUTTON}" class="button" onclick="this.form.action.value='index'; this.form.module.value='Administration'; " type="submit" name="button" value="  {$MOD.LBL_DIAG_CANCEL_BUTTON}  "></td>

	</tr>
</table>
<div id="table" style="visibility:visible">
<table id="maintable" width="430" border="0" cellspacing="0" cellpadding="0" class="edit view">
<tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_CONFIGPHP}</span></td>
	<td ><span><input name='configphp' class="checkbox" type="checkbox" tabindex='1'></span></td>
	</tr><tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_CUSTOMDIR}</span></td>
	<td ><span><input name='custom_dir' class="checkbox" type="checkbox" tabindex='2'></span></td>
	</tr><tr>

	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_PHPINFO}</span></td>
	<td ><span><input name='phpinfo' class="checkbox" type="checkbox" tabindex='3'></span></td>
	</tr><tr>
	<td scope="row"><span>{$DB_NAME} - {$MOD.LBL_DIAGNOSTIC_MYSQLDUMPS}</span></td>
	<td ><span><input name='mysql_dumps' class="checkbox" type="checkbox" tabindex='4'></span></td>
	</tr><tr>
	<td scope="row"><span>{$DB_NAME} - {$MOD.LBL_DIAGNOSTIC_MYSQLSCHEMA}</span></td>

	<td ><span><input name='mysql_schema' class="checkbox" type="checkbox" tabindex='5'></span></td>
	</tr><tr>
	<td scope="row"><span>{$DB_NAME} - {$MOD.LBL_DIAGNOSTIC_MYSQLINFO}</span></td>
	<td ><span><input name='mysql_info' class="checkbox" type="checkbox" tabindex='6'></span></td>
	</tr><tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_MD5}</span></td>
	<td ><span><input name='md5' class="checkbox" type="checkbox" tabindex='7' onclick="md5checkboxes()"></span></td>
	</tr><tr>

	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_FILESMD5}</span></td>
	<td ><span><input name='md5filesmd5' class="checkbox" type="checkbox" tabindex='8'></span></td>
	</tr><tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_CALCMD5}</span></td>
	<td ><span><input name='md5calculated' class="checkbox" type="checkbox" tabindex='9'></span></td>
	</tr><tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_BLBF}</span></td>

	<td ><span><input name='beanlistbeanfiles' class="checkbox" type="checkbox" tabindex='10'></span></td>
	</tr><tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_SUITELOG}</span></td>
	<td ><span><input name='sugarlog' class="checkbox" type="checkbox" tabindex='11'></span></td>
	</tr><tr>
	<td scope="row"><span>{$MOD.LBL_DIAGNOSTIC_VARDEFS}</span></td>
	<td ><span><input name='vardefs' class="checkbox" type="checkbox" tabindex='11'></span></td>
	</tr>
</table>
</div>
</form>

{literal}
<script type="text/javascript" language="Javascript">
  var md5filesmd5_checked;
  var md5calculated_checked;
  function show(id) {
      document.getElementById(id).style.display="block";
  }
  function md5checkboxes(){
    if (document.Diagnostic.md5.checked == false){
      md5filesmd5_checked = document.Diagnostic.md5filesmd5.checked;
      md5calculated_checked = document.Diagnostic.md5calculated.checked;
      document.Diagnostic.md5filesmd5.checked=false;
      document.Diagnostic.md5calculated.checked=false;
      document.Diagnostic.md5filesmd5.disabled=true;
      document.Diagnostic.md5calculated.disabled=true;
    }
    else{
      document.Diagnostic.md5filesmd5.disabled=false;
      document.Diagnostic.md5calculated.disabled=false;
      document.Diagnostic.md5filesmd5.checked=md5filesmd5_checked;
      document.Diagnostic.md5calculated.checked=md5calculated_checked;
    }
  }
</script>
{/literal}