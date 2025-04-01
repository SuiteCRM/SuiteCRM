<?php
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
 * THIS CLASS IS FOR DEVELOPERS TO MAKE CUSTOMIZATIONS IN
 */
require_once('modules/OutboundEmailAccounts/OutboundEmailAccounts_sugar.php');
#[\AllowDynamicProperties]
class OutboundEmailAccounts extends OutboundEmailAccounts_sugar
{

    /**
     * @var string
     */
    public $mail_smtppass;

    /**
     * @var string
     */
    public $smtp_from_addr;

    /**
     * @var string
     */
    public $smtp_from_name;

    /**
     * @var string
     */
    public $mail_smtpuser;

    /**
     * @var string
     */
    public $type;

    /**
     * @var string
     */
    public $signature;

    /**
     * @var string
     */
    public $reply_to_addr;

    /**
     * @var string
     */
    public $reply_to_name;

    public function __construct()
    {
        parent::__construct();
    }

    public function save($check_notify = false)
    {
        if (!$this->hasAccessToPersonalAccount()) {
            $this->logPersonalAccountAccessDenied('save');
            throw new RuntimeException('Access Denied');
        }

        $this->keepWriteOnlyFieldValues();

        if (!$this->mail_smtppass && $this->id) {
            $bean = BeanFactory::newBean('OutboundEmailAccounts');
            $bean->retrieve($this->id);
            if (!$bean->mail_smtppass) {
                $GLOBALS['log']->warn("Unable to send email via SMTP using an empty password.");
                $GLOBALS['log']->info("Please ensure that the email settings are configured correctly");
                $this->mail_smtppass = null;
            } else {
                $this->mail_smtppass = $bean->mail_smtppass;
            }
        }
        $this->mail_smtppass = $this->mail_smtppass ? blowfishEncode(blowfishGetKey('OutBoundEmail'), $this->mail_smtppass) : null;

        $this->smtp_from_name = trim($this->smtp_from_name);
        $this->smtp_from_addr = trim($this->smtp_from_addr);
        $this->mail_smtpserver = trim($this->mail_smtpserver);
        $this->mail_smtpuser = trim($this->mail_smtpuser);

        $results = parent::save($check_notify);
        return $results;
    }

    /**
     * @inheritDoc
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        $results = parent::retrieve($id, $encode, $deleted);

        if (!empty($results) && !$this->hasAccessToPersonalAccount()) {
            $this->logPersonalAccountAccessDenied('retrieve');
            return null;
        }

        $this->mail_smtppass = $this->mail_smtppass ? blowfishDecode(blowfishGetKey('OutBoundEmail'), $this->mail_smtppass) : null;
        return $results;
    }

    /**
     * @return array
     */
    public function getUserOutboundAccounts(): array {
        global $current_user, $db;

        $where = '';
        if (is_admin($current_user)) {
            $currentUserId = $db->quote($current_user->id);
            $tableName = $db->quote($this->table_name);
            $where = "(($tableName.type IS NULL) OR ($tableName.type != 'user' ) OR ($tableName.type = 'user' AND $tableName.user_id = '$currentUserId'))";
        }

        return $this->get_list('', $where)['list'] ?? [];
    }

    /**
     * @inheritDoc
     */
    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ) {
        global $current_user, $db;

        $ret_array = parent::create_new_list_query(
            $order_by,
            $where,
            $filter,
            $params ,
            $show_deleted,
            $join_type,
            true,
            $parentbean,
            $singleSelect,
            $ifListForExport
        );

        if(is_admin($current_user)) {
            if ($return_array) {
                return $ret_array;
            }

            return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
        }

        if (is_array($ret_array) && !empty($ret_array['where'])){
            $tableName = $db->quote($this->table_name);
            $currentUserId = $db->quote($current_user->id);

            $showGroupRecords = "($tableName.type IS NULL) OR ($tableName.type != 'user' ) OR ";

            $hasActionAclsDefined = has_group_action_acls_defined('OutboundEmailAccounts', 'list');

            if($hasActionAclsDefined === false) {
                $showGroupRecords = '';
            }

            $ret_array['where'] = $ret_array['where'] . " AND ( $showGroupRecords ($tableName.type = 'user' AND $tableName.user_id = '$currentUserId') )";
        }

        if ($return_array) {
            return $ret_array;
        }

        return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
    }

    /**
     * Check if user has access to personal account
     * @return bool
     */
    public function hasAccessToPersonalAccount() : bool {
        global $current_user;

        if (is_admin($current_user)) {
            return true;
        }

        if (empty($this->type)) {
            return true;
        }

        if ($this->type !== 'user') {
            return true;
        }

        if (empty($this->user_id)) {
            return true;
        }

        if ($this->user_id === $current_user->id) {
            return true;
        }

        return false;
    }

    /**
     * Log personal account access denied
     * @param string $action
     * @return void
     */
    public function logPersonalAccountAccessDenied(string $action) : void {
        global $log, $current_user;

        $log->fatal("OutBoundEmailAccount | Access denied. Non-admin trying to access personal account. Action: '" . $action . "' | Current user id: '" . $current_user->id . "' | record: '" . $this->id . "'" );
    }

    /**
     * @inheritDoc
     */
    public function ACLAccess($view, $is_owner = 'not_set', $in_group = 'not_set')
    {
        global $current_user;

        $isNotAllowAction = $this->isNotAllowedAction($view);
        if ($isNotAllowAction === true) {
            return false;
        }

        if (!$this->hasAccessToPersonalAccount()) {
            $this->logPersonalAccountAccessDenied("ACLAccess-$view");
            return false;
        }

        if (empty($this->type) && $this->assigned_user_id === $current_user->id){
            $this->type = 'user';
        }

        $isPersonal = $this->type === 'user';
        $isAdmin = is_admin($current_user);

        if ($isPersonal === true && $this->hasAccessToPersonalAccount()) {
            return true;
        }

        $isAdminOnlyAction = $this->isAdminOnlyAction($view);
        if (!$isPersonal && !$isAdmin && $isAdminOnlyAction === true) {
            return false;
        }

        $hasActionAclsDefined = has_group_action_acls_defined('OutboundEmailAccounts', 'view');
        $isSecurityGroupBasedAction = $this->isSecurityGroupBasedAction($view);

        if (!$isPersonal && !$isAdmin && !$hasActionAclsDefined && $isSecurityGroupBasedAction === true) {
            return false;
        }

        return parent::ACLAccess($view, $is_owner, $in_group);
    }

    /**
     * Get from address
     * @return string
     */
    public function getFromAddress(): string {
        $fromAddress = $this->smtp_from_addr ?? '';
        if (empty($fromAddress) || isValidEmailAddress($this->mail_smtpuser, '', false, '')) {
            $fromAddress = $this->mail_smtpuser;
        }

        return $fromAddress;
    }

    /**
     * Get from name
     * @return string
     */
    public function getFromName(): string {
        return $this->smtp_from_name ?? '';
    }

    /**
     * Get reply to address
     * @return string
     */
    public function getReplyToAddress(): string {
        $address = $this->reply_to_addr ?? '';
        if (empty($address) && isValidEmailAddress($this->reply_to_addr, '', false, '')) {
            return $this->getFromAddress();
        }

        return $address;
    }

    /**
     * Get reply to name
     * @return string
     */
    public function getReplyToName(): string {
        return $this->reply_to_name ?? '';
    }

    /**
     * @return void
     */
    protected function keepWriteOnlyFieldValues(): void
    {
        if (empty($this->fetched_row)) {
            return;
        }

        foreach ($this->field_defs as $field => $field_def) {
            if (empty($field_def['display']) || $field_def['display'] !== 'writeonly') {
                continue;
            }

            if (empty($this->fetched_row[$field])) {
                continue;
            }

            if (!empty($this->$field)) {
                continue;
            }

            $this->$field = $this->fetched_row[$field];
        }
    }

    public static function getPasswordChange()
    {
        global $mod_strings;
        $html = <<<HTML
<script type="text/javascript">
var passwordToggle = function(elem, sel) {
	$(sel).show();
	$(elem).hide();
}
</script>
<div id="password_toggle" style="display:none;">
	<input type="password" id="mail_smtppass" name="mail_smtppass" />
</div>
<a href="javascript:;" onclick="passwordToggle(this, '#password_toggle');">{$mod_strings['LBL_CHANGE_PASSWORD']}</a>

HTML;
        return $html;
    }

    public static function getEmailProviderChooser($focus, $name, $value, $view)
    {
        global $app_strings, $mod_strings;
        $ss = new Sugar_Smarty();
        $ss->assign('APP', $app_strings);
        $ss->assign('MOD', $mod_strings);
        $ss->assign('mail_smtptype', $focus->mail_smtptype);
        $html = $ss->fetch('modules/OutboundEmailAccounts/smtpPreselection.tpl');
        return $html;
    }

    public static function getSendTestEmailBtn()
    {
        global $app_strings, $current_user;
        $APP = $app_strings;
        $CURRENT_USER_EMAIL = $current_user->email1;
        $admin = BeanFactory::newBean('Administration');
        $admin->retrieveSettings();
        $adminNotifyFromAddress = $admin->settings['notify_fromaddress'];
        isValidEmailAddress($adminNotifyFromAddress);
        $adminNotifyFromName = $admin->settings['notify_fromname'];
        $html = <<<HTML
			<input id="sendTestOutboundEmailSettingsBtn" type="button" class="button" value="{$APP['LBL_EMAIL_TEST_OUTBOUND_SETTINGS']}" onclick="testOutboundSettings();">
			<script type="text/javascript" src="cache/include/javascript/sugar_grp_yui_widgets.js"></script>
			<script type="text/javascript">

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


				var EmailMan = {};

				var testOutboundSettings = function() {
					testOutboundSettingsDialog();
				};

				function testOutboundSettingsDialog() {
					// lazy load dialogue
					if(!EmailMan.testOutboundDialog) {
						EmailMan.testOutboundDialog = new YAHOO.widget.Dialog("testOutboundDialog", {
							modal:true,
							visible:true,
							fixedcenter: false,
							constraintoviewport: false,
							width   : 600,
							shadow  : false
						});
						EmailMan.testOutboundDialog.setHeader("{$APP['LBL_EMAIL_TEST_OUTBOUND_SETTINGS']}");
						YAHOO.util.Dom.removeClass("testOutboundDialog", "yui-hidden");
					} // end lazy load

					EmailMan.testOutboundDialog.render();
					EmailMan.testOutboundDialog.show();
				}

                                function showFullSmtpLogDialog(headerText, bodyHtml, dialogType) {

                                     var config = { };
                                     config.type = dialogType;
                                     config.title = headerText;
                                     config.msg = bodyHtml;
                                     config.modal = false;
                                     config.width = 600;
                                     YAHOO.SUGAR.MessageBox.show(config);
                                }

				function sendTestEmail() {
					var toAddress = document.getElementById("outboundtest_to_address").value;

					if (trim(toAddress) == "")
					{
						overlay("{$APP['ERR_MISSING_REQUIRED_FIELDS']}", "{$APP['LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR']}", 'alert');
						//return;
					}
					else if (!isValidEmail(toAddress)) {
						overlay("{$APP['ERR_INVALID_REQUIRED_FIELDS']}", "{$APP['LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR']}", 'alert');
						return;
					}

					//Hide the email address window and show a message notifying the user that the test email is being sent.
					EmailMan.testOutboundDialog.hide();
					overlay("{$APP['LBL_EMAIL_PERFORMING_TASK']}", "{$APP['LBL_EMAIL_ONE_MOMENT']}", 'alert');

					var callbackOutboundTest = {
						success	: function(o) {
							hideOverlay();
							var responseObject = YAHOO.lang.JSON.parse(o.responseText);
							if (responseObject.status)
								overlay("{$APP['LBL_EMAIL_TEST_OUTBOUND_SETTINGS']}", "{$APP['LBL_EMAIL_TEST_NOTIFICATION_SENT']}", 'alert');
       							else {

                                                           var dialogBody =
                                                            "<div style='padding: 10px'>" +
                                                               "<div class='well'>" + responseObject.errorMessage + "</div>" +
                                                               "<div >" +
                                                                   "<button class='btn btn-primary' type='button' data-toggle='collapse' data-target='#fullSmtpLog' aria-expanded='false' aria-controls='fullSmtpLog'>" +
                                                                       "{$APP['LBL_EMAIL_TEST_SEE_FULL_SMTP_LOG']}" +
                                                                  "</button>" +
                                                                   "<div class='collapse' id='fullSmtpLog'>" +
                                                                       "<pre style='height: 300px; overflow: scroll;'>" +
                                                                           responseObject.fullSmtpLog +
                                                                       "</pre>" +
                                                                   "</div>" +
                                                               "</div>" +
                                                           "</div>";
                                                           showFullSmtpLogDialog("{$APP['LBL_EMAIL_TEST_OUTBOUND_SETTINGS']}", dialogBody, 'alert');
                                                        }
						}
					};

					var smtpServer = document.getElementById('mail_smtpserver').value;
					var smtpPort = document.getElementById('mail_smtpport').value;
					var smtpssl  = document.getElementById('mail_smtpssl').value;
					var mailsmtpauthreq = document.getElementById('mail_smtpauth_req');
					var mail_sendtype = 'SMTP';
                                                                var adminNotifyFromAddress = document.getElementById('smtp_from_addr').value ? document.getElementById('smtp_from_addr').value :'$adminNotifyFromName';
                                                                var adminNotifyFromName = document.getElementById('smtp_from_name').value ? document.getElementById('smtp_from_name').value : '$adminNotifyFromAddress';
					var postDataString =
						'mail_type=system&' +
						'mail_sendtype=' + mail_sendtype + '&' +
						'mail_smtpserver=' + smtpServer + "&" +
						"mail_smtpport=" + smtpPort + "&mail_smtpssl=" + smtpssl + "&" +
						"mail_smtpauth_req=" + mailsmtpauthreq.checked + "&" +
						"mail_smtpuser=" + trim(document.getElementById('mail_smtpuser').value) + "&" +
						"mail_smtppass=" + trim(document.getElementById('mail_smtppass').value) + "&" +
						"outboundtest_to_address=" + toAddress + '&' +
						'outboundtest_from_address=' + adminNotifyFromAddress + '&' +
						'mail_from_name=' + adminNotifyFromName;
					//YAHOO.util.Connect.asyncRequest("POST", "index.php?action=EmailUIAjax&module=Emails&emailUIAction=testOutbound&to_pdf=true&sugar_body_only=true", callbackOutboundTest, postDataString);
					YAHOO.util.Connect.asyncRequest("POST", "index.php?action=testOutboundEmail&module=EmailMan&to_pdf=true&sugar_body_only=true", callbackOutboundTest, postDataString);
				}

			</script>

			<div id="testOutboundDialog" class="yui-hidden">
				<div id="testOutbound">
					<form>
					<table width="100%" border="0" cellspacing="0" cellpadding="0" class="edit view">
						<tr>
							<td scope="row">
								{$APP['LBL_EMAIL_SETTINGS_FROM_TO_EMAIL_ADDR']}
								<span class="required">
								</span>
							</td>
							<td>
								<input type="text" id="outboundtest_to_address" name="outboundtest_to_address" size="35" maxlength="64" value="{$CURRENT_USER_EMAIL}">
							</td>
						</tr>
						<tr>
							<td scope="row" colspan="2">
								<input type="button" class="button" value="   {$APP['LBL_EMAIL_SEND']}   " onclick="javascript:sendTestEmail();">&nbsp;
								<input type="button" class="button" value="   {$APP['LBL_CANCEL_BUTTON_LABEL']}   " onclick="javascript:EmailMan.testOutboundDialog.hide();">&nbsp;
							</td>
						</tr>
					</table>
					</form>
				</div>
			</div>
HTML;
        return $html;
    }

    /**
     * Check if its admin only action
     * @param string $view
     * @return bool
     */
    protected function isAdminOnlyAction(string $view): bool
    {
        $adminOnlyAction = ['edit', 'delete', 'editview', 'save'];
        return in_array(strtolower($view), $adminOnlyAction);
    }

    /**
     * Check if its a security based action
     * @param string $view
     * @return bool
     */
    protected function isSecurityGroupBasedAction(string $view): bool
    {
        $securityBasedActions = ['detail', 'detailview', 'view'];
        return in_array(strtolower($view), $securityBasedActions);
    }

    /**
     * Get not allowed action
     * @param string $view
     * @return bool
     */
    protected function isNotAllowedAction(string $view): bool
    {
        $notAllowed = ['export', 'import', 'massupdate', 'duplicate'];
        return in_array(strtolower($view), $notAllowed);
    }
}
