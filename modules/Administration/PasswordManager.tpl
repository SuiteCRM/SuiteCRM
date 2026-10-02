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
<form name="ConfigurePasswordSettings" method="POST" action="index.php" >
<input type='hidden' name='action' value='PasswordManager'/>
<input type='hidden' name='module' value='Administration'/>
<input type='hidden' name='saveConfig' value='1'/>
<span class='error'>{$error.main}</span>
<table width="100%" cellpadding="0" cellspacing="0" border="0" class="actionsContainer">
	<tr>

		<td style="padding-bottom: 2px;" >
			<input title="{$APP.LBL_SAVE_BUTTON_TITLE}" accessKey="{$APP.LBL_SAVE_BUTTON_KEY}" class="button primary" id="btn_save" type="submit" onclick="addcheck(form);return check_form('ConfigurePasswordSettings');"  name="save" value="{$APP.LBL_SAVE_BUTTON_LABEL}" >
			&nbsp;<input title="{$MOD.LBL_CANCEL_BUTTON_TITLE}" id="btn_cancel" onclick="document.location.href='index.php?module=Administration&action=index'" class="button"  type="button" name="cancel" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" >
		</td>
	</tr>
</table>
<table width="100%" border="0" cellspacing="0" cellpadding="0">
	<tr>
		<td>

						<table id="sysGeneratedId" name="sysGeneratedName" width="100%" border="0" cellspacing="1" cellpadding="0" class="edit view">
							<tr>
								<th align="left" scope="row" colspan="4">
									<h4>
										{$MOD.LBL_PASSWORD_SYST_GENERATED_TITLE}
									</h4>
								</th>
							</tr>
										<tr>
									        <td  scope="row" width='25%'>
												{$MOD.LBL_PASSWORD_SYST_GENERATED_PWD_ON}:&nbsp{sugar_help text=$MOD.LBL_PASSWORD_SYST_GENERATED_PWD_HELP WIDTH=400}
											</td>
											<td >
											{if ($config.passwordsetting.SystemGeneratedPasswordON ) == '1'}
												{assign var='SystemGeneratedPasswordON' value='CHECKED'}
											{else}
												{assign var='SystemGeneratedPasswordON' value=''}
											{/if}
												<input type='hidden' name='passwordsetting_SystemGeneratedPasswordON' value='0'>
												<input name='passwordsetting_SystemGeneratedPasswordON' id='SystemGeneratedPassword_checkbox'   type='checkbox' value='1' {$SystemGeneratedPasswordON} onclick='enable_syst_generated_pwd(this);toggleDisplay("SystemGeneratedPassword_warning");'>
											</td>
											{if !($config.passwordsetting.SystemGeneratedPasswordON)}
												{assign var='smtp_warning' value='none'}
											{/if}
										</tr>
										<tr>
											<td colspan="2" id="SystemGeneratedPassword_warning" scope="row" style='display:{$smtp_warning}';>
											<i>{if $SMTP_SERVER_NOT_SET}&nbsp;&nbsp;&nbsp;&nbsp;{$MOD.ERR_SMTP_SERVER_NOT_SET}<br>{/if}
											&nbsp;&nbsp;&nbsp;&nbsp;{$MOD.LBL_EMAIL_ADDRESS_REQUIRED_FOR_FEATURE}</i>
										</td>
									    </tr>
									    <tr>
											<td align="left" scope="row" colspan="4">
													{$MOD.LBL_PASSWORD_SYST_EXPIRATION}
											</td>
										</tr>
										<tr>
											<td colspan='4'>
												<table width="100%" id='syst_generated_pwd_table' border="0" cellspacing="1" cellpadding="0">
													<tr>
												            {assign var='systexplogin' value=''}
			                                                {assign var='systexptime' value=''}
			                                                {assign var='systexpnone' value=''}
			                                            {if ($config.passwordsetting.systexpiration) == '0' || $config.passwordsetting.systexpiration==''}
			                                                {assign var='systexpnone' value='CHECKED'}
			                                            {/if}
			                                            {if ($config.passwordsetting.systexpiration) == '1'}
			                                                {assign var='systexptime' value='CHECKED'}
			                                            {/if}
			                                            {if ($config.passwordsetting.systexpiration) == '2'}
			                                                {assign var='systexplogin' value='CHECKED'}
			                                            {/if}
													    <td width='30%'>
			                                                <input type="radio"  name="passwordsetting_systexpiration"  value='0' {$systexpnone} onclick="form.passwordsetting_systexpirationtime.value='';form.passwordsetting_systexpirationlogin.value='';">
			                                               {$MOD.LBL_UW_NONE}
			                                            </td>
			    										<td  width='30%'>
															<input type="radio"  name="passwordsetting_systexpiration" id="required_sys_pwd_exp_time" value='1' {$systexptime} onclick="form.passwordsetting_systexpirationlogin.value='';">
															{$MOD.LBL_PASSWORD_EXP_IN}
															{assign var='sdays' value=''}
															{assign var='sweeks' value=''}
															{assign var='smonths' value=''}
														{if ($config.passwordsetting.systexpirationtype ) == '1'}
															{assign var='sdays' value='SELECTED'}
														{/if}
														{if ($config.passwordsetting.systexpirationtype ) == '7'}
															{assign var='sweeks' value='SELECTED'}
														{/if}
														{if ($config.passwordsetting.systexpirationtype ) == '30'}
															{assign var='smonths' value='SELECTED'}
														{/if}
															<input type='text' maxlength="3" and style="width:2em"  name='passwordsetting_systexpirationtime' value='{$config.passwordsetting.systexpirationtime}'>
															<SELECT  NAME="passwordsetting_systexpirationtype">
																<OPTION VALUE='1' {$sdays}>{$MOD.LBL_DAYS}
																<OPTION VALUE='7' {$sweeks}>{$MOD.LBL_WEEKS}
																<OPTION VALUE='30' {$smonths}>{$MOD.LBL_MONTHS}
															</SELECT>
														</td>
														<td colspan='2' width='40%'>
															<input type="radio" name="passwordsetting_systexpiration"  id="required_sys_pwd_exp_login" value='2' {$systexplogin} onclick="form.passwordsetting_systexpirationtime.value='';">
															{$MOD.LBL_PASSWORD_EXP_AFTER}
															<input type='text' maxlength="3" and style="width:2em"  name='passwordsetting_systexpirationlogin' value="{$config.passwordsetting.systexpirationlogin}">
															{$MOD.LBL_PASSWORD_LOGINS}
														</td>
													</tr>
												</table>
											</td>
										</tr>
									</table>

			<!-- PASSWORD SECURITY SETTINGS -->
			<table id="pwdsec_table" width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
				<tr>
					<td>{include file="modules/Administration/PasswordManagerSecurity.tpl"}</td>
				</tr>
			</table>
			<!-- END PASSWORD SECURITY SETTINGS -->


			<table id="userResetPassId" name="userResetPassName" width="100%" border="0" cellspacing="1" cellpadding="0" class="edit view">
							<tr>
								<th align="left" scope="row" colspan="2"><h4>{$MOD.LBL_PASSWORD_USER_RESET}</h4>
								</th>
							</tr>
							<tr>

											<td width="25%" scope="row">{$MOD.LBL_PASSWORD_FORGOT_FEATURE}:&nbsp{sugar_help text=$MOD.LBL_PASSWORD_FORGOT_FEATURE_HELP WIDTH=400}</td>
											<td scope="row" width="25%" >
												{if ($config.passwordsetting.forgotpasswordON ) == '1'}
													{assign var='forgotpasswordON' value='CHECKED'}
												{else}
													{assign var='forgotpasswordON' value=''}
												{/if}
												<input type='hidden' name='passwordsetting_forgotpasswordON' value='0'>
												<input name="passwordsetting_forgotpasswordON" id="forgotpassword_checkbox" value="1" class="checkbox" type="checkbox"  onclick='forgot_password_enable(this); toggleDisplay("SystemGeneratedPassword_warning2");' {$forgotpasswordON}>
											</td>
											{if !($config.passwordsetting.forgotpasswordON)}
												{assign var='smtp_warning_2' value='none'}
											{/if}
										</tr>
										<tr><td colspan="4" id="SystemGeneratedPassword_warning2" scope="row" style='display:{$smtp_warning_2}';>
											<i>{if $SMTP_SERVER_NOT_SET}&nbsp;&nbsp;&nbsp;&nbsp;{$MOD.ERR_SMTP_SERVER_NOT_SET}<br>{/if}
											&nbsp;&nbsp;&nbsp;&nbsp;{$MOD.LBL_EMAIL_ADDRESS_REQUIRED_FOR_FEATURE}</i>
											</td>
										</tr>
										<tr>
										<td width="25%" scope="row">{$MOD.LBL_PASSWORD_LINK_EXPIRATION}:&nbsp{sugar_help text=$MOD.LBL_PASSWORD_LINK_EXPIRATION_HELP WIDTH=400}</td>
											<td colspan="3">
												<table width="100%" border="0" cellspacing="0" cellpadding="0" id="forgot_password_table">
													<tr>

											    		{assign var='linkexptime' value=''}
			                                                {assign var='linkexpnone' value=''}
			                                            {if ($config.passwordsetting.linkexpiration) == '0'}
			                                                {assign var='linkexpnone' value='CHECKED'}
			                                            {/if}
			                                            {if ($config.passwordsetting.linkexpiration) == '1'}
			                                                {assign var='linkexptime' value='CHECKED'}
			                                            {/if}
			                                            <td  width='30%'>
			                                                <input type="radio" name="passwordsetting_linkexpiration" value='0'  {$linkexpnone}  onclick="form.passwordsetting_linkexpirationtime.value='';">
			                                               {$MOD.LBL_UW_NONE}
			                                            </td>
			    										<td  width='30%'>
															<input type="radio" name="passwordsetting_linkexpiration" id="required_link_exp_time" value='1'  {$linkexptime}>
															{$MOD.LBL_PASSWORD_LINK_EXP_IN}
															{assign var='ldays' value=''}
															{assign var='lweeks' value=''}
															{assign var='lmonths' value=''}
														{if ($config.passwordsetting.linkexpirationtype ) == '1'}
															{assign var='ldays' value='SELECTED'}
														{/if}
														{if ($config.passwordsetting.linkexpirationtype ) == '60'}
															{assign var='lweeks' value='SELECTED'}
														{/if}
														{if ($config.passwordsetting.linkexpirationtype ) == '1440'}
															{assign var='lmonths' value='SELECTED'}
														{/if}
															<input type='text' maxlength="3" and style="width:2em" name='passwordsetting_linkexpirationtime'  value='{$config.passwordsetting.linkexpirationtime}'>
															<SELECT   NAME="passwordsetting_linkexpirationtype">
																<OPTION VALUE='1' {$ldays}>{$MOD.LBL_MINUTES}
																<OPTION VALUE='60' {$lweeks}>{$MOD.LBL_HOURS}
																<OPTION VALUE='1440' {$lmonths}>{$MOD.LBL_DAYS}
															</SELECT>
														</td width='40%'>
														<td >
														</td>
													</tr>
												</table>
											</td>
										</tr>
										<tr>
										{if !empty($settings.captcha_on) || !($VALID_PUBLIC_KEY)}
											{assign var='captcha_checked' value='CHECKED'}
										{else}
											{assign var='captcha_checked' value=''}
										{/if}
											<td width="25%" scope="row">{$MOD.ENABLE_CAPTCHA}:&nbsp{sugar_help text=$MOD.LBL_CAPTCHA_HELP_TEXT WIDTH=400}</td>
											<td scope="row" width="75%"><input type='hidden' name='captcha_on' value='0'><input name="captcha_on" id="captcha_id" value="1" class="checkbox" tabindex='1' type="checkbox" onclick='document.getElementById("captcha_config_display").style.display=this.checked?"":"none";' {$captcha_checked}></td>
										</tr>
									</table>
									<table width="100%" border="0" cellspacing="0" cellpadding="0">
										<tr>
											<td colspan="4">
												<div id="captcha_config_display" style="display:{$CAPTCHA_CONFIG_DISPLAY}">
													<table width="100%" cellpadding="0" cellspacing="0">
													<tr>
														<td width="10%" scope="row">{$MOD.LBL_PUBLIC_KEY}<span class="required">*</span></td>
														<td width="40%" ><input type="text" name="captcha_public_key" id="captcha_public_key" size="45"  value="{$settings.captcha_public_key}" tabindex='1' onblur="this.value=this.value.replace(/^\s+/,'').replace(/\s+$/,'')"></td>
														<td width="10%" scope="row">{$MOD.LBL_PRIVATE_KEY}<span class="required">*</span></td>
														<td width="40%" ><input type="text" name="captcha_private_key" size="45"  value="{$settings.captcha_private_key}" tabindex='1' onblur="this.value=this.value.replace(/^\s+/,'').replace(/\s+$/,'')"></td>
													</tr>
													</table>
												</div>
											</td>
										</tr>
									{if !($VALID_PUBLIC_KEY)}
										<tr><td scope="row"><span class='error'>{$MOD.ERR_PUBLIC_CAPTCHA_KEY}</span></td></tr>
									{/if}
									</table>




						<table id="emailTemplatesId" name="emailTemplatesName" width="100%" border="0" cellspacing="1" cellpadding="0" class="edit view">
							<tr>
								<th align="left" scope="row" colspan="4">
									<h4>
										{$MOD.LBL_PASSWORD_TEMPLATE}
									</h4>
								</th>
							</tr>

										<tr>
									        <td  scope="row" width="35%">{$MOD.LBL_PASSWORD_GENERATE_TEMPLATE_MSG}: </td>
									        <td  >
										        <span>
									        		<select tabindex='251' id="generatepasswordtmpl" name="passwordsetting_generatepasswordtmpl" {$IE_DISABLED}>{$TMPL_DRPDWN_GENERATE}</select>
													<input type="button" class="button" onclick="javascript:open_email_template_form('generatepasswordtmpl')" value="{$MOD.LBL_PASSWORD_CREATE_TEMPLATE}" {$IE_DISABLED}>
													<input type="button" value="{$MOD.LBL_PASSWORD_EDIT_TEMPLATE}" class="button" onclick="javascript:edit_email_template_form('generatepasswordtmpl')" name='edit_generatepasswordtmpl' id='edit_generatepasswordtmpl' style="{$EDIT_TEMPLATE}">
												</span>
									        </td>
									        <td ></td>
									        <td  ></td>
										</tr>
										<tr>
									        <td  scope="row">{$MOD.LBL_PASSWORD_LOST_TEMPLATE_MSG}: </td>
									        <td  >
							        			<span>
									        		<select tabindex='251' id="lostpasswordtmpl" name="passwordsetting_lostpasswordtmpl" {$IE_DISABLED}>{$TMPL_DRPDWN_LOST}</select>
													<input type="button" class="button" onclick="javascript:open_email_template_form('lostpasswordtmpl')" value="{$MOD.LBL_PASSWORD_CREATE_TEMPLATE}" {$IE_DISABLED}>
													<input type="button" value="{$MOD.LBL_PASSWORD_EDIT_TEMPLATE}" class="button" onclick="javascript:edit_email_template_form('lostpasswordtmpl')" name='edit_lostpasswordtmpl' id='edit_lostpasswordtmpl' style="{$EDIT_TEMPLATE}">
												</span>
							        		 </td>
									        <td ></td>
									        <td ></td>
										</tr>


							<tr>
								<td  scope="row">{$MOD.LBL_TWO_FACTOR_AUTH_EMAIL_TPL}: </td>
								<td>
									<span>
										<select tabindex='251' id="factoremailtmpl"
                                                name="passwordsetting_factoremailtmpl" {$IE_DISABLED}>{$TMPL_DRPDWN_FACTOR}</select>
										<input type="button" class="button"
                                               onclick="open_email_template_form('factoremailtmpl')"
                                               value="{$MOD.LBL_PASSWORD_CREATE_TEMPLATE}" {$IE_DISABLED}>
										<input type="button" value="{$MOD.LBL_PASSWORD_EDIT_TEMPLATE}" class="button"
                                               onclick="edit_email_template_form('factoremailtmpl')"
                                               name='edit_factoremailtmpl' id='edit_factoremailtmpl'
                                               style="{$EDIT_TEMPLATE}">
									</span>
                        </td>
								<td ></td>
								<td ></td>
							</tr>


									</table>


							{if !empty($settings.system_ldap_enabled)}
									{assign var='system_ldap_enabled_checked' value='CHECKED'}
									{assign var='ldap_display' value='inline'}
								{else}
									{assign var='system_ldap_enabled_checked' value=''}
									{assign var='ldap_display' value='none'}
							{/if}
							<table id='ldap_table' width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
								<tr>
									<td>
										<table width="100%" border="0" cellspacing="0" cellpadding="0">
											<tr>
												<th align="left" scope="row" colspan='3'><h4>{$MOD.LBL_LDAP_TITLE}</h4></th>
											</tr>
											<tr>
												<td width="25%" scope="row" valign='middle'>
													{$MOD.LBL_LDAP_ENABLE}{sugar_help text=$MOD.LBL_LDAP_HELP_TXT}
												</td><td valign='middle'><input name="system_ldap_enabled" id="system_ldap_enabled" class="checkbox"  type="checkbox" {$system_ldap_enabled_checked} onclick='toggleDisplay("ldap_display");enableDisablePasswordTable("system_ldap_enabled");'></td><td>&nbsp;</td><td>&nbsp;</td></tr>
											<tr>
												<td colspan='4'>
													<table  cellspacing='0' cellpadding='1' id='ldap_display' style='display:{$ldap_display}' width='100%'>
														<tr>
															<td width='25%' scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_SERVER_HOSTNAME} {sugar_help text=$MOD.LBL_LDAP_SERVER_HOSTNAME_DESC}</td>{$settings.proxy_host}
															<td width='25%' align="left"  valign='top'><input name="ldap_hostname" size='25' type="text" value="{$settings.ldap_hostname}"></td>
															<td width='25%' scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_SERVER_PORT} {sugar_help text=$MOD.LBL_LDAP_SERVER_PORT_DESC}</td>{$settings.proxy_port}
															<td width='25%' align="left"  valign='top' ><input name="ldap_port" size='6' type="text" value="{$settings.ldap_port}"></td>
														</tr>
														<tr>
															<td scope="row" valign='middle' nowrap>{$MOD.LBL_LDAP_USER_DN} {sugar_help text=$MOD.LBL_LDAP_USER_DN_DESC}</td>
															<td align="left"  valign='middle'><input name="ldap_base_dn" size='35' type="text" value="{$settings.ldap_base_dn}"></td>
															<td scope="row" valign='middle' nowrap>{$MOD.LBL_LDAP_USER_FILTER} {sugar_help text=$MOD.LBL_LDAP_USER_FILTER_DESC}</td>
															<td align="left"  valign='middle'><input name="ldap_login_filter" size='25' type="text" value="{$settings.ldap_login_filter}"></td>
														</tr>
														<tr>
															<td scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_BIND_ATTRIBUTE} {sugar_help text=$MOD.LBL_LDAP_BIND_ATTRIBUTE_DESC}</td>
															<td align="left"  valign='top'><input name="ldap_bind_attr" size='25' type="text" value="{$settings.ldap_bind_attr}"> </td>
															<td scope="row" valign='middle' nowrap>{$MOD.LBL_LDAP_LOGIN_ATTRIBUTE} {sugar_help text=$MOD.LBL_LDAP_LOGIN_ATTRIBUTE_DESC}</td>
															<td align="left"  valign='middle'><input name="ldap_login_attr" size='25' type="text" value="{$settings.ldap_login_attr}"></td>
														</tr>
														<tr>
															<td scope="row" valign='top'nowrap>{$MOD.LBL_LDAP_GROUP_MEMBERSHIP} {sugar_help text=$MOD.LBL_LDAP_GROUP_MEMBERSHIP_DESC}</td>
															<td align="left"  valign='top'>
															{if !empty($settings.ldap_group)}
																{assign var='ldap_group_checked' value='CHECKED'}
																{assign var='ldap_group_display' value=''}
															{else}
																{assign var='ldap_group_checked' value=''}
																{assign var='ldap_group_display' value='none'}
															{/if}
																<input name="ldap_group_checkbox" class="checkbox" type="checkbox" {$ldap_group_checked} onclick='toggleDisplay("ldap_group")'>
															</td>
															<td valign='middle' nowrap></td>
															<td align="left"  valign='middle'></td>
														</tr>
														<tr>
															<td></td>
															<td colspan='3'>
																<span id='ldap_group' style='display:{$ldap_group_display}'>
																	<table width='100%'>
																		<tr>
																			<td  width='25%' scope="row" valign='top'nowrap>{$MOD.LBL_LDAP_GROUP_DN} {sugar_help text=$MOD.LBL_LDAP_GROUP_DN_DESC}</td>
																			<td  width='25%' align="left"  valign='top'><input name="ldap_group_dn" size='20' type="text"  value="{$settings.ldap_group_dn}"></td>
																			<td  width='25%' scope="row" valign='top'nowrap>{$MOD.LBL_LDAP_GROUP_NAME} {sugar_help text=$MOD.LBL_LDAP_GROUP_NAME_DESC}</td>
																			<td  width='25%' align="left"  valign='top'><input name="ldap_group_name" size='20' type="text"  value="{$settings.ldap_group_name}"></td>
																		</tr>
																		<tr>
																			<td scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_GROUP_USER_ATTR} {sugar_help text=$MOD.LBL_LDAP_GROUP_USER_ATTR_DESC}</td>
																			<td align="left"  valign='top'><input name="ldap_group_user_attr" size='20' type="text" value="{$settings.ldap_group_user_attr}"> </td>
																			<td scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_GROUP_ATTR} {sugar_help text=$MOD.LBL_LDAP_GROUP_ATTR_DESC}</td>
																			<td align="left"  valign='top'><input name="ldap_group_attr" size='20' type="text" value="{$settings.ldap_group_attr}"> </td>
																		</tr>
																		<tr>
																			<td scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_GROUP_ATTR_REQ_DN} {sugar_help text=$MOD.LBL_LDAP_GROUP_ATTR_REQ_DN_DESC}</td>
																			<td align="left" valign='top'>
																			{if !empty($settings.ldap_group_attr_req_dn)}
																				{assign var='ldap_group_attr_req_dn' value='CHECKED'}
																			{else}
																				{assign var='ldap_group_attr_req_dn' value='none'}
																			{/if}
																			<input name="ldap_group_attr_req_dn" class="checkbox" type="checkbox" {$ldap_group_attr_req_dn}> </td>
																		</tr>
																	</table>
																 <br>
																</span>
															</td>
														</tr>
														<tr>
															<td scope="row" valign='top'nowrap>{$MOD.LBL_LDAP_AUTHENTICATION} {sugar_help text=$MOD.LBL_LDAP_AUTHENTICATION_DESC}</td>
															<td align="left"  valign='top' >
															{if !empty($settings.ldap_authentication)}
																{assign var='ldap_authentication_checked' value='CHECKED'}
																{assign var='ldap_authentication_display' value=''}
															{else}
																{assign var='ldap_authentication_checked' value=''}
																{assign var='ldap_authentication_display' value='none'}
															{/if}
															<input name="ldap_authentication_checkbox" class="checkbox"  type="checkbox" {$ldap_authentication_checked} onclick='toggleDisplay("ldap_authentication")'>
															</td>
															<td valign='middle' nowrap></td>
															<td align="left"  valign='middle'></td>
														</tr>
														<tr>
															<td></td>
															<td colspan='3'>
															<span id='ldap_authentication' style='display:{$ldap_authentication_display}'>
																<table width='100%' >
																	<tr>
																		<td width='25%' scope="row" valign='top'nowrap>{$MOD.LBL_LDAP_ADMIN_USER} {sugar_help text=$MOD.LBL_LDAP_ADMIN_USER_DESC}</td>
																		<td width='25%' align="left"  valign='top'><input name="ldap_admin_user" size='20' type="text" value="{$settings.ldap_admin_user}"></td>
																		<td width='25%' scope="row" valign='middle' nowrap>{$MOD.LBL_LDAP_ADMIN_PASSWORD}</td>
																		<td width='25%' align="left"  valign='middle'><input name="ldap_admin_password" size='20' type="password" value="{$settings.ldap_admin_password}"> </td>
																	</tr>
																</table>
																<br>
															</span>
															</td>
														</tr>
														<tr>
															<td scope="row" valign='top' nowrap>{$MOD.LBL_LDAP_AUTO_CREATE_USERS} {sugar_help text=$MOD.LBL_LDAP_AUTO_CREATE_USERS_DESC}</td>
															{if !empty($settings.ldap_auto_create_users)}
																{assign var='ldap_auto_create_users_checked' value='CHECKED'}
															{else}
																{assign var='ldap_auto_create_users_checked' value=''}
															{/if}
															<td align="left"  valign='top'><input type='hidden' name='ldap_auto_create_users' value='0'><input name="ldap_auto_create_users" value="1" class="checkbox" type="checkbox" {$ldap_auto_create_users_checked}></td>
															<td valign='middle' nowrap></td>
															<td align="left"  valign='middle'></td>
														</tr>
														<tr>
															<td scope="row" valign='middle' nowrap>{$MOD.LBL_LDAP_ENC_KEY} {sugar_help text=$LDAP_ENC_KEY_DESC}</td>
															<td align="left"  valign='middle'><input name="ldap_enc_key" size='35' type="password" value="{$settings.ldap_enc_key}" {$LDAP_ENC_KEY_READONLY}> </td>
															<td valign='middle' nowrap></td>
															<td align="left"  valign='middle'></td>
														</tr>
													</table>
												</td>
											</tr>
										</table>
									</td>
								</tr>
							</table>

						             <!-- start SAML -->
                            {if !empty($config.authenticationClass)
                                && ($config.authenticationClass == 'SAMLAuthenticate'
                                || $config.authenticationClass == 'SAML2Authenticate')}
                           {assign var='saml_enabled_checked' value='CHECKED'}
                           {assign var='saml_display' value='inline'}
                        {else}
                           {assign var='saml_enabled_checked' value=''}
                           {assign var='saml_display' value='none'}
                     {/if}

                     <table id = 'saml_table' width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
                        <tr>
                           <td>
                              <table width="100%" border="0" cellspacing="0" cellpadding="0">
                                 <tr>
                                    <th align="left" scope="row" colspan='3'><h4>{$MOD.LBL_SAML_TITLE}</h4></th>
                                 </tr>
                                 <tr>
                                    <td width="25%" scope="row" valign='middle'>
                                       {$MOD.LBL_SAML_ENABLE}{sugar_help text=$MOD.LBL_SAML_HELP_TXT}
                                    </td><td valign='middle'>

                                    <input name="authenticationClass" id="system_saml_enabled" class="checkbox"
                                       value="SAML2Authenticate" type="checkbox"
                                       {if $saml_enabled_checked}checked="1"{/if}
                                       onclick='toggleDisplay("saml_display");enableDisablePasswordTable("system_saml_enabled");'>
                                    </td><td>&nbsp;</td><td>&nbsp;</td></tr>
                                 <tr>
                                    <td colspan='4'>
                                       <table  cellspacing='0' cellpadding='1' id='saml_display' style='display:{$saml_display}' width='100%'>
                                            <tr>
                                             <td scope="row" valign='middle' nowrap>{$MOD.LBL_SAML_LOGIN_URL} {sugar_help text=$MOD.LBL_SAML_LOGIN_URL_DESC}</td>
                                             <td align="left"  valign='middle'><input name="SAML_loginurl" size='35' type="text" value="{$config.SAML_loginurl}"></td>

                                          </tr>
										   <tr>
											   <td scope="row" valign='middle' nowrap>{$MOD.LBL_SAML_LOGOUT_URL} {sugar_help text=$MOD.LBL_SAML_LOGOUT_URL_DESC}</td>
											   <td align="left"  valign='middle'><input name="SAML_logouturl" size='35' type="text" value="{$config.SAML_logouturl}"></td>

										   </tr>
                                          <tr>
                                             <td width='25%' scope="row" valign='top' nowrap>{$MOD.LBL_SAML_CERT} {sugar_help text=$MOD.LBL_SAML_CERT_DESC}</td>{$settings.proxy_host}
                                             <td width='25%' align="left"  valign='top'><textarea style='height:200px;width:600px' name="SAML_X509Cert" >{$config.SAML_X509Cert}</textarea></td>

                                          </tr>


                     </table>


               </td>
            </tr>
         </table>
         <!-- end SAML -->
					</td>
				</tr>
			</table>
			<div style="padding-top: 2px;">
                     <input title="{$APP.LBL_SAVE_BUTTON_TITLE}" class="button primary" id="btn_save" type="submit" onclick="addcheck(form);return check_form('ConfigurePasswordSettings');" name="save" value="{$APP.LBL_SAVE_BUTTON_LABEL}" />
                     &nbsp;<input title="{$MOD.LBL_CANCEL_BUTTON_TITLE}"  onclick="document.location.href='index.php?module=Administration&action=index'" class="button"  type="button" name="cancel" value="{$APP.LBL_CANCEL_BUTTON_LABEL}" />
                  </div>
		</td>
	</tr>
</table>

      </td>
   </tr>
</table>
</form>
{$JAVASCRIPT}


{if !($VALID_PUBLIC_KEY)}
<script>
document.getElementById('captcha_public_key').focus();
document.getElementById('captcha_id').checked=true;
document.getElementById('forgotpassword_checkbox').checked=true;
</script>
{/if}


{literal}
<script>
function addcheck(form){{/literal}
	addForm('ConfigurePasswordSettings');


	if(document.getElementById('forgotpassword_checkbox').checked){literal}{{/literal}
	addToValidate('ConfigurePasswordSettings', 'passwordsetting_linkexpirationtime', 'int', form.required_link_exp_time.checked,"{$MOD.ERR_PASSWORD_LINK_EXPIRE_TIME} ");
	{literal}}{/literal}

	if(document.getElementById('SystemGeneratedPassword_checkbox').checked){literal}{{/literal}
	addToValidate('ConfigurePasswordSettings', 'passwordsetting_systexpirationtime', 'int', form.required_sys_pwd_exp_time.checked,"{$MOD.ERR_PASSWORD_EXPIRE_TIME}" );
	addToValidate('ConfigurePasswordSettings', 'passwordsetting_systexpirationlogin', 'int', form.required_sys_pwd_exp_login.checked,"{$MOD.ERR_PASSWORD_EXPIRE_LOGIN}" );
	addToValidate('ConfigurePasswordSettings', 'passwordsetting_generatepasswordtmpl', 'string', form.generatepasswordtmpl.value == '',"{$MOD.ERR_SYS_GEN_PWD_TPL_NOT_SELECTED}" );
   {literal}}{/literal}


{literal}	}


function open_email_template_form(fieldToSet) {
	fieldToSetValue = fieldToSet;
	URL="index.php?module=EmailTemplates&action=EditView&inboundEmail=true&show_js=1";
	windowName = 'email_template';
	windowFeatures = 'width=800' + ',height=600' 	+ ',resizable=1,scrollbars=1';

	win = window.open(URL, windowName, windowFeatures);
	if(window.focus)
	{
		// put the focus on the popup if the browser supports the focus() method
		win.focus();
	}
}

function enableDisablePasswordTable(checkbox_id) {
   var other = checkbox_id == "system_saml_enabled" ? "ldap_table" :  "saml_table";
	var enabled = document.getElementById(checkbox_id).checked;
	if (enabled) {
		document.getElementById("emailTemplatesId").style.display = "none";
		document.getElementById("sysGeneratedId").style.display = "none";
		document.getElementById("userResetPassId").style.display = "none";
	} else {
		document.getElementById("emailTemplatesId").style.display = "";
		document.getElementById("sysGeneratedId").style.display = "";
		document.getElementById("userResetPassId").style.display = "";

	}
} // if

function edit_email_template_form(templateField) {
	fieldToSetValue = templateField;
	var field=document.getElementById(templateField);
	URL="index.php?module=EmailTemplates&action=EditView&inboundEmail=true&show_js=1";
	if (field.options[field.selectedIndex].value != 'undefined') {
		URL+="&record="+field.options[field.selectedIndex].value;
	}
	windowName = 'email_template';
	windowFeatures = 'width=800' + ',height=600' 	+ ',resizable=1,scrollbars=1';

	win = window.open(URL, windowName, windowFeatures);
	if(window.focus)
	{
		// put the focus on the popup if the browser supports the focus() method
		win.focus();
	}
}

function refresh_email_template_list(template_id, template_name) {
	var field=document.getElementById(fieldToSetValue);
	var bfound=0;
	for (var i=0; i < field.options.length; i++) {
			if (field.options[i].value == template_id) {
				if (field.options[i].selected==false) {
					field.options[i].selected=true;
				}
				field.options[i].text = template_name;
				bfound=1;
			}
	}
	//add item to selection list.
	if (bfound == 0) {
		var newElement=document.createElement('option');
		newElement.text=template_name;
		newElement.value=template_id;
		field.options.add(newElement);
		newElement.selected=true;
	}

	//enable the edit button.
	var editButtonName = 'edit_generatepasswordtmpl';
	if (fieldToSetValue == 'generatepasswordtmpl') {
		editButtonName = 'edit_lostpasswordtmpl';
	} // if
	var field1=document.getElementById(editButtonName);
	field1.style.visibility="visible";

	var applyListToTemplateField = 'generatepasswordtmpl';
	if (fieldToSetValue == 'generatepasswordtmpl') {
		applyListToTemplateField = 'lostpasswordtmpl';
	} // if
	var field=document.getElementById(applyListToTemplateField);
	if (bfound == 1) {
		for (var i=0; i < field.options.length; i++) {
			if (field.options[i].value == template_id) {
				field.options[i].text = template_name;
			} // if
		} // for

	} else {
		var newElement=document.createElement('option');
		newElement.text=template_name;
		newElement.value=template_id;
		field.options.add(newElement);
	} // else
        
}

function testregex(customregex)
{
try
  {
var string = 'hello';
string.match(customregex.value);
  }
catch(err)
  {
  	alert(SUGAR.language.get("Administration", "ERR_INCORRECT_REGEX"));
  	setTimeout("document.getElementById('customregex').select()",10);
  }
}
function toggleDisplay_2(id){

	if(this.document.getElementById(id).style.display=='none'){
		this.document.getElementById(id).style.display='';
		this.document.getElementById(id+"_lbl").innerHTML='{/literal}{$MOD.LBL_HIDE_ADVANCED_OPTIONS}{literal}';
		this.document.getElementById("regex_config_display_img").src = '{/literal}{sugar_getimagepath file="basic_search.gif"}{literal}';
	}else{
		this.document.getElementById(id).style.display='none'
		this.document.getElementById(id+"_lbl").innerHTML='{/literal}{$MOD.LBL_SHOW_ADVANCED_OPTIONS}{literal}';
		this.document.getElementById("regex_config_display_img").src = '{/literal}{sugar_getimagepath file="advanced_search.gif"}{literal}';
	}
}

function forgot_password_enable(check){
var table_fields=document.getElementById('forgot_password_table');
var forgot_password_input=table_fields.getElementsByTagName('input');
var forgot_password_select=table_fields.getElementsByTagName('select');
	if(check.checked){
		for (i=0;i<forgot_password_input.length;i++)
			forgot_password_input[i].disabled='';
		for (j=0;j<forgot_password_select.length;j++)
			forgot_password_select[j].disabled='';
		document.ConfigurePasswordSettings.captcha_on[1].disabled='';
	}else
		{
		document.ConfigurePasswordSettings.captcha_on[1].disabled='disabled';
		document.ConfigurePasswordSettings.captcha_on[1].checked='';
		document.getElementById("captcha_config_display").style.display='none';
		for (i=0;i<forgot_password_input.length;i++)
			forgot_password_input[i].disabled='disabled';
		for (j=0;j<forgot_password_select.length;j++)
			forgot_password_select[j].disabled='disabled';
	}
}

function enable_syst_generated_pwd(check){
var table_fields=document.getElementById('syst_generated_pwd_table');
var syst_generated_pwd_input=table_fields.getElementsByTagName('input');
var syst_generated_pwd_select=table_fields.getElementsByTagName('select');
	if(check.checked){
		for (i=0;i<syst_generated_pwd_input.length;i++)
			syst_generated_pwd_input[i].disabled='';
		for (j=0;j<syst_generated_pwd_select.length;j++)
			syst_generated_pwd_select[j].disabled='';
	}else
		{
		for (i=0;i<syst_generated_pwd_input.length;i++)
			syst_generated_pwd_input[i].disabled='disabled';
		for (j=0;j<syst_generated_pwd_select.length;j++)
			syst_generated_pwd_select[j].disabled='disabled';
	}
}
forgot_password_enable(document.getElementById('forgotpassword_checkbox'));
enable_syst_generated_pwd(document.getElementById('SystemGeneratedPassword_checkbox'));
if(document.getElementById('system_saml_enabled').checked)enableDisablePasswordTable('system_saml_enabled');
if(document.getElementById('system_ldap_enabled').checked)enableDisablePasswordTable('system_ldap_enabled');

</script>

{/literal}
