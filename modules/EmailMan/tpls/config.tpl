<!--
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
-->

<!-- BEGIN: main -->
<script type="text/javascript" src="{sugar_getjspath file='modules/Users/User.js'}"></script>
<script type="text/javascript" src="{sugar_getjspath file='cache/include/javascript/sugar_grp_yui_widgets.js'}"></script>
{literal}
<script type="text/javascript" >
<!--
function change_state(radiobutton) {

	if (radiobutton.value == '1') {
		radiobutton.form['massemailer_tracking_entities_location'].disabled=true;
		radiobutton.form['massemailer_tracking_entities_location'].value='{/literal}{$MOD.TRACKING_ENTRIES_LOCATION_DEFAULT_VALUE}{literal}';
	} else {
		radiobutton.form['massemailer_tracking_entities_location'].disabled=false;
		radiobutton.form['massemailer_tracking_entities_location'].value=null;
	}
}
-->
</script>
{/literal}
{$ROLLOVER}
<form name="ConfigureSettings" id="EditView" method="POST" >
	<input type="hidden" name="module" value="EmailMan">
	<input type="hidden" name="action">
	<input type="hidden" name="return_module" value="{$RETURN_MODULE}">
	<input type="hidden" name="return_action" value="{$RETURN_ACTION}">
	<input type="hidden" name="source_form" value="config" />

<table width="100%" cellpadding="0" cellspacing="0" border="0">
	<tr>

		<td>
			<input title="{$APP.LBL_SAVE_BUTTON_TITLE}" accessKey="{$APP.LBL_SAVE_BUTTON_KEY}" class="button primary" onclick="this.form.action.value='Save';return verify_data(this);" type="submit" name="button" id="btn_save" value=" {$APP.LBL_SAVE_BUTTON_LABEL} ">
			<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" accessKey="{$APP.LBL_CANCEL_BUTTON_KEY}" class="button" onclick="this.form.action.value='{$RETURN_ACTION}'; this.form.module.value='{$RETURN_MODULE}';" type="submit" name="button" value=" {$APP.LBL_CANCEL_BUTTON_LABEL} ">
		</td>
		<td align="right" nowrap>
			<span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span> {$APP.NTC_REQUIRED}
		</td>
	</tr>
</table>

<div id="EditView_tabs">
	<div class="panel-content">
		<div class="panel panel-default">
			<div class="panel-heading ">
				<a class="" role="button" data-toggle="collapse-edit" aria-expanded="false">
					<div class="col-xs-10 col-sm-11 col-md-11">
						{$MOD.LBL_EMAIL_OUTBOUND_CONFIGURATION}
					</div>
				</a>
			</div>
			<div class="panel-body">
				<div class="tab-content">
					<table width="100%" border="1" cellspacing="0" cellpadding="0" class="edit view">
						<tr>
							<td align="left" scope="row" colspan="4">
								{$MOD.LBL_OUTGOING_SECTION_HELP}
								<br />&nbsp;
							</td>
						</tr>
						<tr class="{$OUTBOUND_TYPE_CLASS}">
							<td width="20%" scope="row">{$MOD.LBL_MAIL_SENDTYPE}</td>
							<td width="30%">
								<select id="mail_sendtype" name="mail_sendtype" onChange="notify_setrequired(document.ConfigureSettings); SUGAR.user.showHideGmailDefaultLink(this);" tabindex="1">{$mail_sendtype_options}</select>
							</td>
							<td scope="row">&nbsp;</td>
							<td >&nbsp;</td>
						</tr>
						<tr>
							<td width="20%" scope="row">{$MOD.LBL_NOTIFY_FROMNAME} <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></td>
							<td width="30%" > <input id='notify_fromname' name='notify_fromname' tabindex='1' size='25' maxlength='128' type="text" value="{$notify_fromname}"></td>
						</tr>
						<tr>
							<td width="20%" scope="row">{$MOD.LBL_NOTIFY_FROMADDRESS} <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></td>
							<td width="30%"><input id='notify_fromaddress' name='notify_fromaddress' tabindex='1' size='25' maxlength='128' type="text" value="{$notify_fromaddress}"></td>
						</tr>
						<tr>
							<td align="left" scope="row" colspan="4">{$MOD.LBL_CHOOSE_EMAIL_PROVIDER}</td>
						</tr>
						<tr>
							<td colspan="4">
								<div id="smtpButtonGroup" class="yui-buttongroup">
				<span id="gmail" class="yui-button yui-radio-button{if $mail_smtptype == 'gmail'} yui-button-checked{/if}">
					<span class="first-child">
						<button type="button" name="mail_smtptype" value="gmail" class="btn btn-danger">
							&nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_GMAIL}&nbsp;&nbsp;&nbsp;&nbsp;
						</button>
					</span>
				</span>
									<span id="yahoomail" class="yui-button yui-radio-button{if $mail_smtptype == 'yahoomail'} yui-button-checked{/if}">
					<span class="first-child">
						<button type="button" name="mail_smtptype" value="yahoomail" class="btn btn-danger">
							&nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_YAHOO}&nbsp;&nbsp;&nbsp;&nbsp;
						</button>
					</span>
				</span>
									<span id="exchange" class="yui-button yui-radio-button{if $mail_smtptype == 'exchange'} yui-button-checked{/if}">
					<span class="first-child">
						<button type="button" name="mail_smtptype" value="exchange" class="btn btn-danger">
							&nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_EXCHANGE}&nbsp;&nbsp;&nbsp;&nbsp;
						</button>
					</span>
				</span>
									<span id="other" class="yui-button yui-radio-button{if $mail_smtptype == 'other' || empty($mail_smtptype)} yui-button-checked{/if}">
					<span class="first-child">
						<button type="button" name="mail_smtptype" value="other" class="btn btn-danger">
							&nbsp;&nbsp;&nbsp;&nbsp;{$APP.LBL_SMTPTYPE_OTHER}&nbsp;&nbsp;&nbsp;&nbsp;
						</button>
					</span>
				</span>
								</div>
							</td>
						</tr>
						<tr>
							<td colspan="4">
								<div id="smtp_settings">
									<table width="100%" cellpadding="0" cellspacing="0">
										<tr id="mailsettings1">
											<td width="20%" scope="row"><span id="mail_smtpserver_label">{$MOD.LBL_MAIL_SMTPSERVER}</span> <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></td>
											<td width="30%" ><input type="text" id="mail_smtpserver" name="mail_smtpserver" tabindex="1" size="25" maxlength="255" value="{$mail_smtpserver}"></td>
											<td width="20%" scope="row"><span id="mail_smtpport_label">{$MOD.LBL_MAIL_SMTPPORT}</span> <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></td>
											<td width="30%" ><input type="text" id="mail_smtpport" name="mail_smtpport" tabindex="1" size="5" maxlength="5" value="{$mail_smtpport}"></td>
										</tr>
										<tr id="mailsettings2">
											<td scope="row"><span id='mail_smtpauth_req_label'>{$MOD.LBL_MAIL_SMTPAUTH_REQ}</span></td>
											<td >
												<input id='mail_smtpauth_req' name='mail_smtpauth_req' type="checkbox" class="checkbox" value="1" tabindex='1'
													   onclick="notify_setrequired(document.ConfigureSettings);" {$mail_smtpauth_req}>
											</td>
											<td width="15%" scope="row"><span id="mail_smtpssl_label">{$APP.LBL_EMAIL_SMTP_SSL_OR_TLS}</span></td>
											<td width="35%" >
												<select id="mail_smtpssl" name="mail_smtpssl" tabindex="501" onchange="setDefaultSMTPPort();" >{$MAIL_SSL_OPTIONS}</select>
											</td>
										</tr>
										<tr id="smtp_auth1">
											<td width="20%" scope="row"><span id="mail_smtpuser_label">{$MOD.LBL_MAIL_SMTPUSER}</span> <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></td>
											<td width="30%" ><input type="text" id="mail_smtpuser" name="mail_smtpuser" size="25" maxlength="255" value="{$mail_smtpuser}" tabindex='1' ></td>
											<td width="20%">&nbsp;</td>
											<td width="30%">&nbsp;</td>
										</tr>
										<tr id="smtp_auth2">
											<td width="20%" scope="row"><span id="mail_smtppass_label">{$MOD.LBL_MAIL_SMTPPASS}</span> <span class="required">{$APP.LBL_REQUIRED_SYMBOL}</span></td>
											<td width="30%" >
												<input type="password" id="mail_smtppass" name="mail_smtppass" size="25" maxlength="255" tabindex='1'>
												<a href="javascript:void(0)" id='mail_smtppass_link' onClick="SUGAR.util.setEmailPasswordEdit('mail_smtppass')" style="display: none">{$APP.LBL_CHANGE_PASSWORD}</a>
											</td>
											<td width="20%">&nbsp;</td>
											<td width="30%">&nbsp;</td>
										</tr>
									</table>
									<table width="100%" cellpadding="0" cellspacing="0">
										<tr id="mail_allow_user">
											<td width="20%" scope="row">
												{$MOD.LBL_ALLOW_DEFAULT_SELECTION}&nbsp;
												<img border="0" class="inlineHelpTip" onclick="return SUGAR.util.showHelpTips(this,'{$MOD.LBL_ALLOW_DEFAULT_SELECTION_HELP}','','','dialogHelpPopup')" src="index.php?entryPoint=getImage&themeName={$THEME}&imageName=helpInline.gif">
											</td>
											<td width="30%">
												<input type='hidden' id="notify_allow_default_outbound_hidden_input" name='notify_allow_default_outbound' value='0'>
												<input id="notify_allow_default_outbound" name='notify_allow_default_outbound' value="2" tabindex='1' class="checkbox" type="checkbox" {$notify_allow_default_outbound_on}>
											</td>
										</tr>
										<tr class="legacy-compose-option" {if isset($legacyEmailConfigEnabled)}style="display:none"{/if}>
											<td width="20%" scope="row">
												{$MOD.LBL_ALLOW_SEND_AS_USER}&nbsp;
												<img border="0" class="inlineHelpTip" onclick="return SUGAR.util.showHelpTips(this,'{$MOD.LBL_ALLOW_SEND_AS_USER_DESC}','','','dialogHelpPopup')" src="index.php?entryPoint=getImage&themeName={$THEME}&imageName=helpInline.gif">
											</td>
											<td width="30%">
												<input type='hidden' id="mail_allowusersend_hidden_input" name='mail_allowusersend' value='0'>
												<input id='mail_allowusersend' name='mail_allowusersend' type="checkbox" class="checkbox" value="1" tabindex='1' {$mail_allow_user_send}>
											</td>
											<td></td>
											<td></td>
										</tr>
									</table>
								</div>
							</td>
						</tr>
						<tr><td colspan="4">&nbsp;</tr>
						<tr>
							<td width="15%"><input type="button" class="btn btn-info" value="{$APP.LBL_EMAIL_TEST_OUTBOUND_SETTINGS}" onclick="testOutboundSettings();">&nbsp;</td>
							<td width="15%">&nbsp;</td>
							<td width="40%">&nbsp;</td>
							<td width="40%">&nbsp;</td>
						</tr>
					</table>
				</div>
			</div>
		</div>
		<div class="panel panel-default">
			<div class="panel-heading ">
				<a class="" role="button" data-toggle="collapse-edit" aria-expanded="false">
					<div class="col-xs-10 col-sm-11 col-md-11">
						{$MOD.LBL_NOTIFY_TITLE}
					</div>
				</a>
			</div>
			<div class="panel-body">
				<div class="tab-content">
					<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
						<tr>
							<td width="20%" scope="row" valign='top'>
								{$MOD.LBL_NOTIFY_ON}:&nbsp;
								<img border="0" class="inlineHelpTip" onclick="return SUGAR.util.showHelpTips(this,'{$MOD.LBL_NOTIFICATION_ON_DESC}','','','dialogHelpPopup')" src="index.php?entryPoint=getImage&themeName={$THEME}&imageName=helpInline.gif">
							</td>
							<td width="30%"  valign='top'>
								<input type='hidden' name='notify_on' value='0'><input name="notify_on" tabindex='1' value="1" class="checkbox" type="checkbox" {$notify_on}>
							</td>
							<td scope="row" width="17%"></td>
							<td></td>
						</tr>
						<tr>
							<td width="20%" scope="row" valign='top'>
								{$MOD.LBL_EMAIL_WARNING_NOTIFICATIONS}:&nbsp;
							</td>
							<td width="30%"  valign='top'>
								<input type='checkbox' class='checkbox' name='email_warning_notifications' value="1" {$LBL_EMAIL_WARNING_NOTIFICATIONS}>
							</td>
						</tr>
						<tr>
							<td width="20%" scope="row" valign='top'>
								{$MOD.LBL_EMAIL_DEFAULT_DELETE_ATTACHMENTS}:&nbsp;
							</td>
							<td width="30%"  valign='top'>
								<input type='checkbox' class='checkbox' name='email_default_delete_attachments' value="1" {$DEFAULT_EMAIL_DELETE_ATTACHMENTS}>
							</td>

							<td scope="row" width="20%">
								{$MOD.LBL_NOTIFY_SEND_FROM_ASSIGNING_USER}:
								<img border="0" class="inlineHelpTip" onclick="return SUGAR.util.showHelpTips(this,'{$MOD.LBL_FROM_ADDRESS_HELP}','','','dialogHelpPopup')" src="index.php?entryPoint=getImage&themeName={$THEME}&imageName=helpInline.gif">
							</td>
							<td width="30%"  valign='top'><input type='hidden' name='notify_send_from_assigning_user' value='0'><input name='notify_send_from_assigning_user' value="2" tabindex='1' class="checkbox" type="checkbox" {$notify_send_from_assigning_user}></td>

						</tr>
						<tr>
							<td width="20%" scope="row" valign='top'>
								{$MOD.LBL_EMAIL_ENABLE_CONFIRM_OPT_IN}:&nbsp;
							</td>
							<td width="30%"  valign='top'>
								<select name="email_enable_confirm_opt_in">{$EMAIL_ENABLE_CONFIRM_OPT_IN}</select>
							</td>

						</tr>
                        <tr>
                            <td width="20%" scope="row" valign='top'>
                                {$MOD.LBL_EMAIL_ENABLE_SEND_OPT_IN}:&nbsp;
                            </td>
                            <td width="30%"  valign='top'>
                                <input type='checkbox' name='email_enable_auto_send_opt_in' value="1" {$EMAIL_ENABLE_AUTO_SEND_OPT_IN}>
                            </td>

                        </tr>
                        <tr>
							<td width="20%" scope="row" valign='top'>
								{$MOD.LBL_EMAIL_CONFIRM_OPT_IN_TEMPLATE_ID}:&nbsp;
							</td>
							<td width="30%"  valign='top'>
								<select name="email_template_id_opt_in">{$EMAIL_OPT_IN_TEMPLATES}</select>
							</td>

						</tr>
                        <tr>
							<td width="20%" scope="row" valign='top'>
								{$MOD.LBL_LEGACY_EMAIL_COMPOSE_BEHAVIOR}:&nbsp;
							</td>
							<td width="30%"  valign='top'>
								<input id="legacy_email_behaviour" name='legacy_email_behaviour' value="true" tabindex='1' class="checkbox" type="checkbox" {if !empty($legacyEmailConfigEnabled)}checked="checked{/if}">
							</td>

						</tr>
					</table>
				</div>
			</div>
		</div>
		<div class="panel panel-default">
			<div class="panel-heading ">
				<a class="" role="button" data-toggle="collapse-edit" aria-expanded="false">
					<div class="col-xs-10 col-sm-11 col-md-11">
						{$MOD.LBL_SECURITY_TITLE}
					</div>
				</a>
			</div>
			<div class="panel-body">
				<div class="tab-content">
					<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
						<tr>
							<td align="left" scope="row" colspan="4">
								{$MOD.LBL_SECURITY_DESC}
							</td>
						</tr>
						<tr>
							<td valign="middle" valign="top" scope="row" colspan="3">
								{$MOD.LBL_SECURITY_OUTLOOK_DEFAULTS}
							</td>
							<td width="10%" NOWRAP valign="top" >
								<input type="checkbox" value="1" name="set_outlook_defaults" id="set_outlook_defaults" onclick="setOutlookDefaults();">&nbsp;
								{literal}
									<script type="text/javascript" language="Javascript">
									  <!--
									  function toggleAllSecurityOptions() {
										document.getElementById('set_outlook_defaults').checked = false;

										var check = false;

										if(document.getElementById('toggle_all').checked == true) {
										  check = true;
										}
										document.getElementById('applet').checked = check;
										document.getElementById('base').checked = check;
										document.getElementById('embed').checked = check;
										document.getElementById('form').checked = check;
										document.getElementById('frame').checked = check;
										document.getElementById('frameset').checked = check;
										document.getElementById('iframe').checked = check;
										document.getElementById('import').checked = check;
										document.getElementById('layer').checked = check;
										document.getElementById('link').checked = check;
										document.getElementById('object').checked = check;
										document.getElementById('style').checked = check;
										document.getElementById('xmp').checked = check;
									  }

									  function setOutlookDefaults() {
										document.getElementById('toggle_all').checked = false;

										document.getElementById('applet').checked = true;
										document.getElementById('base').checked = true;
										document.getElementById('embed').checked = true;
										document.getElementById('form').checked = true;
										document.getElementById('frame').checked = true;
										document.getElementById('frameset').checked = true;
										document.getElementById('iframe').checked = true;
										document.getElementById('import').checked = true;
										document.getElementById('layer').checked = true;
										document.getElementById('link').checked = true;
										document.getElementById('object').checked = true;
										document.getElementById('style').checked = false;
										document.getElementById('xmp').checked = true;
									  }
									  -->
									</script>
								{/literal}
							</td>
						</tr>
						<tr>
							<td valign="middle" valign="top" scope="row" colspan="3">
								{$MOD.LBL_SECURITY_TOGGLE_ALL}
							</td>
							<td width="10%" NOWRAP valign="top" >
								<input type="checkbox" value="1" name="toggle_all" id="toggle_all" onclick="toggleAllSecurityOptions();">&nbsp;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_APPLET}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="applet" id="applet" {$appletChecked}>&nbsp; &lt;applet&gt;
							</td>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_BASE}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="base" id="base" {$baseChecked}>&nbsp; &lt;base&gt;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_EMBED}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="embed" id="embed" {$embedChecked}>&nbsp; &lt;embed&gt;
							</td>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_FORM}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="form" id="form" {$formChecked}>&nbsp; &lt;form&gt;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_FRAME}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="frame" id="frame" {$frameChecked}>&nbsp; &lt;frame&gt;
							</td>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_FRAMESET}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="frameset" id="frameset" {$framesetChecked}>&nbsp; &lt;frameset&gt;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_IFRAME}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="iframe" id="iframe" {$iframeChecked}>&nbsp; &lt;iframe&gt;
							</td>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_IMPORT}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="import" id="import" {$importChecked}>&nbsp; &lt;import&gt;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_LAYER}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="layer" id="layer" {$layerChecked}>&nbsp; &lt;layer&gt;
							</td>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_LINK}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="link" id="link" {$linkChecked}>&nbsp; &lt;link&gt;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_OBJECT}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="object" id="object" {$objectChecked}>&nbsp; &lt;object&gt;
							</td>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_STYLE}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="style" id="style" {if isset($styleChecked)}{$styleChecked}{else}{log msg="styleChecked is not set"}{/if}>&nbsp; &lt;style&gt;
							</td>
						</tr>
						<tr>
							<td width="10%" valign="middle" scope="row">
								{$MOD.LBL_SECURITY_XMP}
							</td>
							<td width="40%" NOWRAP valign="middle" >
								<input type="checkbox" value="1" name="xmp" id="xmp" {$xmpChecked}>&nbsp; &lt;xmp&gt;
							</td>
							<td scope="row">&nbsp;</td>
							<td>&nbsp;</td>
						</tr>
					</table>
				</div>
			</div>
		</div>
	</div>
</div>


<div id="testOutboundDialog" class="yui-hidden">
    <div id="testOutbound">
		<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
			<tr>
				<td scope="row">
					{$APP.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR}
					<span class="required">
						{$APP.LBL_REQUIRED_SYMBOL}
					</span>
				</td>
				<td >
					<input type="text" id="outboundtest_from_address" name="outboundtest_from_address" size="35" maxlength="64" value="{$CURRENT_USER_EMAIL}">
				</td>
			</tr>
			<tr>
				<td scope="row" colspan="2">
					<input type="button" class="button" value="   {$APP.LBL_EMAIL_SEND}   " onclick="javascript:sendTestEmail();">&nbsp;
					<input type="button" class="button" value="   {$APP.LBL_CANCEL_BUTTON_LABEL}   " onclick="javascript:EmailMan.testOutboundDialog.hide();">&nbsp;
				</td>
			</tr>

		</table>
	</div>
</div>

<div style="padding-top:2px;">
			<input title="{$APP.LBL_SAVE_BUTTON_TITLE}" class="button primary" onclick="this.form.action.value='Save';return verify_data(this);" type="submit" name="button" value=" {$APP.LBL_SAVE_BUTTON_LABEL} ">
			<input title="{$APP.LBL_CANCEL_BUTTON_TITLE}" class="button" onclick="this.form.action.value='{$RETURN_ACTION}'; this.form.module.value='{$RETURN_MODULE}';" type="submit" name="button" value=" {$APP.LBL_CANCEL_BUTTON_LABEL} ">
</div>

	</form>
{$JAVASCRIPT}
{literal}
<script type="text/javascript">
<!--
var loader = new YAHOO.util.YUILoader({
    require : ["element","sugarwidgets"],
    loadOptional: true,
    skin: { base: 'blank', defaultSkin: '' },
    allowRollup: true,
    base: "include/javascript/yui/build/"
});
loader.addModule({
    name :"sugarwidgets",
    type : "js",
    fullpath: "include/javascript/sugarwidgets/SugarYUIWidgets.js",
    varName: "YAHOO.SUGAR",
    requires: ["datatable", "dragdrop", "treeview", "tabview"]
});
loader.insert();

EmailMan = {};

var first_load = true;
function testOutboundSettings() {
	if (document.getElementById('mail_sendtype').value == 'sendmail') {
		testOutboundSettingsDialog();
		return;
	}
	var errorMessage = '';
	var isError = false;
	var fromAddress = document.getElementById("outboundtest_from_address").value;
    var errorMessage = '';
    var isError = false;
    var smtpServer = document.getElementById('mail_smtpserver').value;
    var smtpPort = document.getElementById('mail_smtpport').value;
    var smtpssl  = document.getElementById('mail_smtpssl').value;
    var mailsmtpauthreq = document.getElementById('mail_smtpauth_req');
    if(trim(smtpServer) == '') {
        isError = true;
        errorMessage += "{/literal}{$APP.LBL_EMAIL_ACCOUNTS_SMTPSERVER}{literal}" + "<br/>";
    }
    if(trim(smtpPort) == '') {
        isError = true;
        errorMessage += "{/literal}{$APP.LBL_EMAIL_ACCOUNTS_SMTPPORT}{literal}" + "<br/>";
    }
    if(mailsmtpauthreq.checked) {
        if(trim(document.getElementById('mail_smtpuser').value) == '') {
            isError = true;
            errorMessage += "{/literal}{$APP.LBL_EMAIL_ACCOUNTS_SMTPUSER}{literal}" + "<br/>";
        }
    }
    if(isError) {
        overlay("{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS}{literal}", errorMessage, 'alert');
        return false;
    }

    testOutboundSettingsDialog();

}

function sendTestEmail()
{
    var toAddress = document.getElementById("outboundtest_from_address").value;
    var fromAddress = document.getElementById("notify_fromaddress").value;
    if (trim(toAddress) == "")
    {
        overlay("{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS}{literal}", "{/literal}{$APP.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR}{literal}", 'alert');
        return;
    }
    else if (!isValidEmail(toAddress)) {
        overlay("{/literal}{$APP.ERR_INVALID_REQUIRED_FIELDS}{literal}", "{/literal}{$APP.LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR}{literal}", 'alert');
        return;
    }
    if (trim(fromAddress) == "")
    {
        overlay("{/literal}{$APP.ERR_MISSING_REQUIRED_FIELDS}{literal}", "{/literal}{$APP.LBL_EMAIL_SETTINGS_FROM_ADDR}{literal}", 'alert');
        return;
    }
    else if (!isValidEmail(fromAddress)) {
        overlay("{/literal}{$APP.ERR_INVALID_REQUIRED_FIELDS}{literal}", "{/literal}{$APP.LBL_EMAIL_SETTINGS_FROM_ADDR}{literal}", 'alert');
        return;
    }
    //Hide the email address window and show a message notifying the user that the test email is being sent.
    EmailMan.testOutboundDialog.hide();
    overlay("{/literal}{$APP.LBL_EMAIL_PERFORMING_TASK}{literal}", "{/literal}{$APP.LBL_EMAIL_ONE_MOMENT}{literal}", 'alert');

    var callbackOutboundTest = {
    	success	: function(o) {
    		hideOverlay();
			var responseObject = YAHOO.lang.JSON.parse(o.responseText);
			if (responseObject.status)
				overlay("{/literal}{$APP.LBL_EMAIL_TEST_OUTBOUND_SETTINGS}{literal}", "{/literal}{$APP.LBL_EMAIL_TEST_NOTIFICATION_SENT}{literal}", 'alert');
			else
				overlay("{/literal}{$APP.LBL_EMAIL_TEST_OUTBOUND_SETTINGS}{literal}", responseObject.errorMessage, 'alert');
		}
    };
    var smtpServer = document.getElementById('mail_smtpserver').value;
    var smtpPort = document.getElementById('mail_smtpport').value;
    var smtpssl  = document.getElementById('mail_smtpssl').value;
    var mailsmtpauthreq = document.getElementById('mail_smtpauth_req');
    var mail_sendtype = document.getElementById('mail_sendtype').value;

    var from_name = document.getElementById('notify_fromname').value;
	var postDataString = 'mail_type=system&mail_sendtype=' + mail_sendtype + '&mail_smtpserver=' + smtpServer + "&mail_smtpport=" + smtpPort + "&mail_smtpssl=" + smtpssl +
	                      "&mail_smtpauth_req=" + mailsmtpauthreq.checked + "&mail_smtpuser=" + trim(document.getElementById('mail_smtpuser').value) +
	                      "&mail_smtppass=" + trim(document.getElementById('mail_smtppass').value) + "&outboundtest_to_address=" + encodeURIComponent(toAddress) +
                          "&outboundtest_from_address=" + fromAddress + "&mail_from_name=" + from_name;

	YAHOO.util.Connect.asyncRequest("POST", "index.php?action=testOutboundEmail&module=EmailMan&to_pdf=true&sugar_body_only=true", callbackOutboundTest, postDataString);
}
function testOutboundSettingsDialog() {
        // lazy load dialogue
        if(!EmailMan.testOutboundDialog) {
        	EmailMan.testOutboundDialog = new YAHOO.widget.Dialog("testOutboundDialog", {
                modal:true,
				visible:true,
            	fixedcenter:true,
            	constraintoviewport: true,
                width	: 600,
                shadow	: false
            });
            EmailMan.testOutboundDialog.setHeader("{/literal}{$APP.LBL_EMAIL_TEST_OUTBOUND_SETTINGS}{literal}");
            YAHOO.util.Dom.removeClass("testOutboundDialog", "yui-hidden");
        } // end lazy load

        EmailMan.testOutboundDialog.render();
        EmailMan.testOutboundDialog.show();
} // fn

function overlay(reqtitle, body, type) {
    var config = { };
    config.type = type;
    config.title = reqtitle;
    config.msg = body;
    YAHOO.SUGAR.MessageBox.show(config);
}

function hideOverlay() {
	YAHOO.SUGAR.MessageBox.hide();
}

function notify_setrequired(f) {

	document.getElementById("smtp_settings").style.display = (f.mail_sendtype.value == "SMTP") ? "inline" : "none";
	document.getElementById("smtp_settings").style.visibility = (f.mail_sendtype.value == "SMTP") ? "visible" : "hidden";
	document.getElementById("smtp_auth1").style.display = (document.getElementById('mail_smtpauth_req').checked) ? "" : "none";
	document.getElementById("smtp_auth1").style.visibility = (document.getElementById('mail_smtpauth_req').checked) ? "visible" : "hidden";
	document.getElementById("smtp_auth2").style.display = (document.getElementById('mail_smtpauth_req').checked) ? "" : "none";
	document.getElementById("smtp_auth2").style.visibility = (document.getElementById('mail_smtpauth_req').checked) ? "visible" : "hidden";

	return true;
}

function setDefaultSMTPPort()
{
    if (!first_load)
    {
        useSSLPort = !document.getElementById("mail_smtpssl").options[0].selected;

        if ( useSSLPort && document.getElementById("mail_smtpport").value == '25' ) {
            document.getElementById("mail_smtpport").value = '465';
        }
        if ( !useSSLPort && document.getElementById("mail_smtpport").value == '465' ) {
            document.getElementById("mail_smtpport").value = '25';
        }
    }
    else
    {
        first_load = false;
    }
}

/**
*  If the outlook options are all set on page load then enable the outlook field so that the user has an indication
*  that that filter has been applied.
*/
function setOutlookDefault()
{
    var shouldToggle = true;
    var aCheckFields = ['applet','base', 'embed','form','frame','frameset', 'iframe','import','layer','link', 'object', 'xmp'];

    for(var i=0;i<aCheckFields.length;i++)
    {
        var tmpName = aCheckFields[i];

        if( ! document.getElementById(tmpName).checked )
        {
            shouldToggle = false;
            break;
        }
    }

    if(shouldToggle && !document.getElementById('style').checked)
        document.getElementById('set_outlook_defaults').checked = true;

}
YAHOO.util.Event.onDOMReady(setOutlookDefault);
notify_setrequired(document.ConfigureSettings);

function changeEmailScreenDisplay(smtptype, clear)
{
    if(clear) {
	    document.getElementById("mail_smtpserver").value = '';
	    document.getElementById("mail_smtpport").value = '25';
	    document.getElementById("mail_smtpauth_req").checked = true;
	    document.getElementById("mailsettings1").style.display = '';
	    document.getElementById("mailsettings2").style.display = '';
	    document.getElementById("mail_smtppass_label").innerHTML = '{/literal}{$MOD.LBL_MAIL_SMTPPASS}{literal}';
	    document.getElementById("mail_smtpport_label").innerHTML = '{/literal}{$MOD.LBL_MAIL_SMTPPORT}{literal}';
	    document.getElementById("mail_smtpserver_label").innerHTML = '{/literal}{$MOD.LBL_MAIL_SMTPSERVER}{literal}';
	    document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_MAIL_SMTPUSER}{literal}';
    }

    switch (smtptype) {
    case "yahoomail":
        document.getElementById("mail_smtpserver").value = 'smtp.mail.yahoo.com';
        document.getElementById("mail_smtpport").value = '465';
        document.getElementById("mail_smtpauth_req").checked = true;
        var ssl = document.getElementById("mail_smtpssl");
        for(var j=0;j<ssl.options.length;j++) {
            if(ssl.options[j].text == 'SSL') {
                ssl.options[j].selected = true;
                break;
            }
        }
        document.getElementById("mailsettings1").style.display = 'none';
        document.getElementById("mailsettings2").style.display = 'none';
        document.getElementById("mail_smtppass_label").innerHTML =
        document.getElementById("mail_smtppass_label").innerHTML = '{/literal}{$MOD.LBL_YAHOOMAIL_SMTPPASS}{literal}';
        document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_YAHOOMAIL_SMTPUSER}{literal}';
        break;
    case "gmail":
        if(document.getElementById("mail_smtpserver").value == "" || document.getElementById("mail_smtpserver").value == 'smtp.mail.yahoo.com') {
            document.getElementById("mail_smtpserver").value = 'smtp.gmail.com';
            document.getElementById("mail_smtpport").value = '587';
            document.getElementById("mail_smtpauth_req").checked = true;
            var ssl = document.getElementById("mail_smtpssl");
            for(var j=0;j<ssl.options.length;j++) {
                if(ssl.options[j].text == 'TLS') {
                    ssl.options[j].selected = true;
                    break;
                }
            }
        }
        //document.getElementById("mailsettings1").style.display = 'none';
        //document.getElementById("mailsettings2").style.display = 'none';
        document.getElementById("mail_smtppass_label").innerHTML = '{/literal}{$MOD.LBL_GMAIL_SMTPPASS}{literal}';
        document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_GMAIL_SMTPUSER}{literal}';
        break;
    case "exchange":
        if ( document.getElementById("mail_smtpserver").value == 'smtp.mail.yahoo.com'
                || document.getElementById("mail_smtpserver").value == 'smtp.gmail.com' ) {
            document.getElementById("mail_smtpserver").value = '';
        }
        //document.getElementById("mail_smtpport").value = '25';
        //document.getElementById("mail_smtpauth_req").checked = true; bug 40998
        document.getElementById("mailsettings1").style.display = '';
        document.getElementById("mailsettings2").style.display = '';
        document.getElementById("mail_smtppass_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPPASS}{literal}';
        document.getElementById("mail_smtpport_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPPORT}{literal}';
        document.getElementById("mail_smtpserver_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPSERVER}{literal}';
        document.getElementById("mail_smtpuser_label").innerHTML = '{/literal}{$MOD.LBL_EXCHANGE_SMTPUSER}{literal}';
        break;
    }
    setDefaultSMTPPort();
    notify_setrequired(document.ConfigureSettings);
}
var oButtonGroup = new YAHOO.widget.ButtonGroup("smtpButtonGroup");
oButtonGroup.subscribe('checkedButtonChange', function(e)
{
    changeEmailScreenDisplay(e.newValue.get('value'), true);
    document.getElementById('smtp_settings').style.display = '';
    document.getElementById('EditView').mail_smtptype.value = e.newValue.get('value');
});
YAHOO.widget.Button.addHiddenFieldsToForm(document.ConfigureSettings);
if(window.addEventListener){
    window.addEventListener("load", function() { SUGAR.util.setEmailPasswordDisplay('mail_smtppass', {/literal}{$mail_haspass}{literal}); }, false);
}else{
    window.attachEvent("onload", function() { SUGAR.util.setEmailPasswordDisplay('mail_smtppass', {/literal}{$mail_haspass}{literal}); });
}

function toggleLegacyComposeOptions()
{
    var isSelected = $('#legacy_email_behaviour').is(':checked') || false;

    var displayMethod = 'hide';
    if(isSelected) {
        displayMethod = 'show';
    }

    $('.legacy-compose-option')[displayMethod]();
}

$(document).ready(function () {
    toggleLegacyComposeOptions()
    $('#legacy_email_behaviour').on('change', function(){
        toggleLegacyComposeOptions()
    });
});
{/literal}{if !empty($mail_smtptype)}{literal}
changeEmailScreenDisplay("{/literal}{$mail_smtptype}{literal}", false);
{/literal}{/if}{literal}
-->
</script>
{/literal}
