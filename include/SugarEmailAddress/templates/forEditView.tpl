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
<script type="text/javascript" language="javascript">
var emailAddressWidgetLoaded = false;
</script>
	<script type="text/javascript" src="include/SugarEmailAddress/SugarEmailAddress.js"></script>
<script type="text/javascript">
	var module = '{$module}';
</script>

<div class="col-xs-12">
	<div class="col-xs-12 email-address-add-line-container emailaddresses" id="{$module}emailAddressesTable{$index}">
		{capture assign="other_attributes"}id="{$module}{$index}_email_widget_add" onclick="SUGAR.EmailAddressWidget.instances.{$module}{$index}.addEmailAddress('{$module}emailAddressesTable{$index}','', false);"{/capture}
		<button type="button" class="btn btn-danger email-address-add-button" title="{$app_strings.LBL_ID_FF_ADD_EMAIL} " {$other_attributes}>
			<span class="suitepicon suitepicon-action-plus"></span><span></span>
		</button>
	</div>
	<div class="col-xs-12 email-address-lines-container">
		{*
		@version > SuiteCRM 7.7.5
		@description Template represents a single email line item

		To customise the layout:
		 ** keep the .template class in the most parent container of a line item
		 ** keep the elements with id's
		 ** don't change the id's of the elements.
		 ** don't add js events inline. Instead bind the event in javascript.
	 	*}
		<div class="col-xs-12 template email-address-line-container hidden">
			<div class="col-xs-12 col-sm-6  email-address-input-container {if $module == "Users"} email-address-users-profile{/if}">
				<div class="input-group email-address-input-group">
					<input type="email" id="email-address-input" class="form-control" placeholder="email@example.com" title="{$app_strings.LBL_EMAIL_TITLE}">
					<input type="hidden" id="record-id">
					<input type="hidden" id="verified-flag" class="verified-flag" value="true"/>
					<input type="hidden" id="verified-email-value" class="verified-email-value" value=""/>
					<input type=hidden id="{$module}_email_widget_id" name="{$module}_email_widget_id" value="">
					<input type=hidden id='emailAddressWidget' name='emailAddressWidget' value='1'>
					<span class="input-group-btn">
					<button type="button" id="email-address-remove-button" class="btn btn-danger email-address-remove-button" name="" title="{$app_strings.LBL_ID_FF_REMOVE_EMAIL}">
						<span class="suitepicon suitepicon-action-minus"></span>
					</button>
				</span>
				</div>
			</div>
			<div class="col-xs-12 col-sm-6 email-address-options-container">


				<div class="col-xs-3 col-sm-2 col-md-2 col-lg-2 text-center email-address-option">
					<label class="text-sm col-xs-12">{$app_strings.LBL_EMAIL_PRIMARY}</label>
					<div><input type="radio" name="" id="email-address-primary-flag" class="email-address-primary-flag" value="" enabled="true" tabindex="0" checked="true" title="{$app_strings.LBL_EMAIL_PRIM_TITLE}"></div>
				</div>

				{if $useReplyTo == true}
				<div class="col-xs-3 col-sm-2 col-md-2 col-lg-2 text-center email-address-option">
					<label class="text-sm  col-xs-12">{$app_strings.LBL_EMAIL_REPLY_TO}</label>
					<div><input type="checkbox" name="" id="email-address-reply-to-flag" class="email-address-reply-to-flag" value="" enabled="true"></div>
				</div>
				{/if}

				{if $useOptOut == true}
				<div class="col-xs-3 col-sm-2 col-md-2 col-lg-2 text-center email-address-option">
					<label class="text-sm col-xs-12">{$app_strings.LBL_EMAIL_OPT_OUT}</label>
					<div><input type="checkbox" name="" title="{$app_strings.LBL_ID_FF_OPT_OUT}" id="email-address-opt-out-flag" class="email-address-opt-out-flag" value="" enabled="true"></div>
				</div>
				{/if}

				{if $useInvalid == true}
				<div class="col-xs-3 col-sm-2 col-md-2 col-lg-2 text-center email-address-option">
					<label class="text-sm col-xs-12">{$app_strings.LBL_EMAIL_INVALID}</label>
					<div><input type="checkbox" name="" title="{$app_strings.LBL_EMAIL_INVALID}" id="email-address-invalid-flag" class="email-address-invalid-flag" value="" enabled="true"></div>
				</div>
				{/if}

                {if $useOptIn == true}
					<div class="col-xs-3 col-sm-2 col-md-2 col-lg-2 text-center email-address-option">
						<label class="text-sm col-xs-12">{$app_strings.LBL_OPT_IN}</label>
						<div>
                                                    <input type="hidden" name="shouldSaveOptInFlag" value="1">
                                                    <input type="checkbox" name="" title="{$app_strings.LBL_OPT_IN}" 
                                                           id="email-address-opted-in-flag" class="email-address-opted-in-flag" value="" enabled="true">
                                                </div>
					</div>
                {/if}
			</div>
		</div>

	</div>
</div>
<input type="hidden" name="useEmailWidget" value="true">
<script type="text/javascript" language="javascript">
SUGAR_callsInProgress++;
var eaw = SUGAR.EmailAddressWidget.instances.{$module}{$index} = new SUGAR.EmailAddressWidget("{$module}");
eaw.emailView = '{$emailView}';
eaw.emailIsRequired = "{$required}";
eaw.tabIndex = '{$tabindex}';
var addDefaultAddress = '{$addDefaultAddress}';
var prefillEmailAddress = '{$prefillEmailAddresses}';
var prefillData = {$prefillData};
if(prefillEmailAddress == 'true') {ldelim}
	eaw.prefillEmailAddresses('{$module}emailAddressesTable{$index}', prefillData);
{rdelim} else if(addDefaultAddress == 'true') {ldelim}
	eaw.addEmailAddress('{$module}emailAddressesTable{$index}', '',true);
{rdelim}
if('{$module}_email_widget_id') {ldelim}
   document.getElementById('{$module}_email_widget_id').value = eaw.count;
{rdelim}
SUGAR_callsInProgress--;
</script>
