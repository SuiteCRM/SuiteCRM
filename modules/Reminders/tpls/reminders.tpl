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

{* REMINDERS_DISABLED - prevents duplicate display on the detail view *}
{if $remindersDisabled == 'false' || !$remindersDisabled}
	{assign var=REMINDERS_DISABLED value=false}
{else}
	{assign var=REMINDERS_DISABLED value=true}
{/if}

{literal}

<style type="text/css">
#reminders #reminder_view .col {float: left; padding-right: 15px;}
#reminders #reminder_view .btns {float: left;}
</style>
{/literal}

<!-- Template for reminders  -->

<div style="display:none;">

	{if !$REMINDERS_DISABLED}

	<div id="reminder_template">

		<span class="error-msg"></span>

		<div class="clear"></div>

		<div class="col">
			<label>{$MOD.LBL_REMINDERS_ACTIONS}</label>&nbsp;
        </div>

        <div class="clear"></div>

        <div class="col">
			<input type="checkbox" class="popup_chkbox" onclick="Reminders.onPopupChkboxClick(this);"><label>{$MOD.LBL_REMINDERS_POPUP}</label>&nbsp;
			<!-- <label>{$MOD.LBL_REMINDERS_WHEN}</label> -->
			<select tabindex="0" class="timer_sel_popup" onchange="Reminders.onPopupTimerSelChange(this);">
				{html_options options=$fields.reminder_time.options}
			</select>
		</div>

        <div class="col">
			<input type="checkbox" class="email_chkbox" onclick="Reminders.onEmailChkboxClick(this);"><label>{$MOD.LBL_REMINDERS_EMAIL}</label>&nbsp;
			<!-- <label>{$MOD.LBL_REMINDERS_WHEN}</label> -->
			<select tabindex="0" class="timer_sel_email" onchange="Reminders.onEmailTimerSelChange(this);">
				{html_options options=$fields.reminder_time.options}
			</select>
		</div>

		<div class="clear"></div>

		<div class="col">
			<ul class="invitees_list"></ul>
		</div>

		<div class="clear"></div>

		<div class="btns">
			<button class="add-btn btn btn-info" type="button" onclick="Reminders.onAddAllClick(this); return false;">
				<span class="suitepicon suitepicon-action-plus"></span>
                {$MOD.LBL_REMINDERS_ADD_ALL_INVITEES}
			</button>
			<button class="remove-reminder-btn btn btn-danger" type="button" onclick="Reminders.onRemoveClick(this); return false;">
				<span class="suitepicon suitepicon-action-minus"></span>
                {$MOD.LBL_REMINDERS_REMOVE_REMINDER}
			</button>
		</div>

		<div class="clear"></div>
	</div>

	{else}

	<div id="reminder_template">

		<span class="error-msg"></span>

		<div class="clear"></div>

		<div class="col">
			<span>{$MOD.LBL_REMINDERS_ACTIONS}</span>&nbsp;
        </div>

        <div class="clear"></div>

        <div class="col">
			<input type="checkbox" class="popup_chkbox" disabled="disabled"><span>{$MOD.LBL_REMINDERS_POPUP}</span>&nbsp;
			<!-- <span>{$MOD.LBL_REMINDERS_WHEN}</span> -->
			<span type="text" class="reminder_when_value" /></span>
			<select tabindex="0" class="timer_sel_popup" disabled="disabled" style="-webkit-appearance: none; -webkit-border-radius: 0px; border: none;">
				{html_options options=$reminder_time_options}
			</select>
		</div>

		<div class="col">
			<input type="checkbox" class="email_chkbox" disabled="disabled"><span>{$MOD.LBL_REMINDERS_EMAIL}</span>&nbsp;
			<!-- <span>{$MOD.LBL_REMINDERS_WHEN}</span> -->
			<span type="text" class="reminder_when_value" /></span>
			<select tabindex="0" class="timer_sel_email" disabled="disabled" style="-webkit-appearance: none; -webkit-border-radius: 0px; border: none;">
				{html_options options=$reminder_time_options}
			</select>
		</div>

		<div class="clear"></div>

		<div class="col">
			<ul class="invitees_list disabled"></ul>
		</div>

		<div class="clear"></div>
	</div>

	{/if}

</div>
<!-- Reminders field in EditViews -->
<div id="reminders">
	<input type="hidden" id="reminders_data" name="reminders_data" />
	<ul id="reminder_view">
	{if $REMINDERS_DISABLED}

        {foreach from=$remindersData item=reminder}

            <ul class="reminder_item" data-reminder-id="{$reminder.id}">

                <span class="error-msg"></span>

                <div class="clear"></div>

                <div class="col">
                    <span>{$MOD.LBL_REMINDERS_ACTIONS}</span>&nbsp;
                </div>

                <div class="clear"></div>

                <div class="col">
                    <input type="checkbox" class="popup_chkbox" disabled="disabled"{if $reminder.popup} checked="checked"{/if}><span>{$MOD.LBL_REMINDERS_POPUP}</span>&nbsp;
                    <!-- <span>{$MOD.LBL_REMINDERS_WHEN}</span> -->
                    <span type="text" class="reminder_when_value" /></span>
                    <select tabindex="0" class="timer_sel_popup" disabled="disabled" style="-webkit-appearance: none; -webkit-border-radius: 0px; border: none;">
                        {html_options options=$reminder_time_options selected=$reminder.timer_popup}
                    </select>
                </div>

                <div class="col">
                    <input type="checkbox" class="email_chkbox" disabled="disabled"{if $reminder.email} checked="checked"{/if}><span>{$MOD.LBL_REMINDERS_EMAIL}</span>&nbsp;
                    <!-- <span>{$MOD.LBL_REMINDERS_WHEN}</span> -->
                    <span type="text" class="reminder_when_value" /></span>
                    <select tabindex="0" class="timer_sel_email" disabled="disabled" style="-webkit-appearance: none; -webkit-border-radius: 0px; border: none;">
                        {html_options options=$reminder_time_options selected=$reminder.timer_email}
                    </select>
                </div>

                <div class="clear"></div>

                <div class="col">
                    <ul class="invitees_list disabled">
                    {foreach from=$reminder.invitees item=invitee}
                        <li class="invitees_item">
                            <button class="invitee_btn btn btn-danger" data-invitee-id="{$invitee.id}" data-id="{$invitee.module_id}" data-module="{$invitee.module}" disabled="disabled">
								<span class="suitepicon suitepicon-module-users"></span>
                                <span class="related-value"> {$invitee.value}</span>
                            </button>
                        </li>
                    {/foreach}
                    </ul>
                </div>

                <div class="clear"></div>

            </ul>

        {/foreach}

	{/if}
    </ul>
	{if !$REMINDERS_DISABLED}
		<button id="reminder_add_btn" class="add-btn btn btn-info" type="button" onclick="Reminders.onAddClick(this);return false">
			<span class="suitepicon suitepicon-action-plus"></span>
            {$MOD.LBL_REMINDERS_ADD_REMINDER}
		</button>
	{/if}
</div>


{if $remindersDataJson && $remindersDefaultValuesDataJson && $remindersDisabled}

	{if !$REMINDERS_DISABLED}
	{literal}
	<script type="text/javascript">

		$(function(){
			Reminders.loadDefaultsAndInit({/literal}{$remindersDataJson}, {$remindersDefaultValuesDataJson}, {$remindersDisabled}{literal}, '{/literal}{$module}{literal}', [{'personModule':'{/literal}{$returnModule}{literal}', 'personModuleId':'{/literal}{$returnId}{literal}'}], '{/literal}{$returnAction}{literal}');
		});

	</script>
	{/literal}
	{/if}

{else}

	{if !$REMINDERS_DISABLED}
	{literal}
	<script type="text/javascript">

		$(function(){
			Reminders.loadDefaultsAndInit(null, null, null, '{/literal}{$module}{literal}', [{'personModule': '{/literal}{$current_user->module_dir}{literal}', 'personModuleId':'{/literal}{$current_user->id}{literal}', 'personName':'{/literal}{$current_user->name}{literal}'}, {'personModule':'{/literal}{$returnModule}{literal}', 'personModuleId':'{/literal}{$returnId}{literal}'}], '{/literal}{$returnAction}{literal}');
		});

	</script>
	{/literal}
	{/if}

{/if}
