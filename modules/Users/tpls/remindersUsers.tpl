{*

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2023 SalesAgility Ltd.
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
	{if $fields.reminder_time}

            	{assign var="REMINDER_TIME_OPTIONS" value=$fields.reminder_time.options}
            	{assign var="EMAIL_REMINDER_TIME_OPTIONS" value=$fields.reminder_time.options}

            	{if !$fields.reminder_checked.value}
            		{assign var="REMINDER_TIME" value=-1}
            	{else}
            		{assign var="REMINDER_TIME" value=$fields.reminder_time.value}
            	{/if}
            	{if !$fields.email_reminder_checked.value}
            		{assign var="EMAIL_REMINDER_TIME" value=-1}
            	{else}
            		{assign var="EMAIL_REMINDER_TIME" value=$fields.email_reminder_time.value}
            	{/if}
	{/if}

	{assign var="REMINDER_TIME_DISPLAY" value="none"}
	{assign var="EMAIL_REMINDER_TIME_DISPLAY" value="none"}
	{if $REMINDER_TIME != -1}
            	{assign var="REMINDER_TIME_DISPLAY" value="inline"}
	{/if}
        {if $EMAIL_REMINDER_TIME != -1}
            	{assign var="EMAIL_REMINDER_TIME_DISPLAY" value="inline"}
        {/if}


{if $view == "EditView" || $view == "QuickCreate" || $view == "QuickEdit"}
{else}
	{assign var="disabled" value="disabled"}
{/if}


<!-- Reminders Profile -> tab Advanced -->

<div id="reminders">

	{if !$disabled}
		<div class="col">
			<slot>{$MOD.LBL_REMINDER}:</slot>&nbsp;{sugar_help text=$MOD.LBL_REMINDER_TEXT }
		</div>
	{/if}
	<td>
	<div class="col">
		<input name="reminder_checked" type="hidden" value="0">
		<input name="reminder_checked" type="checkbox" class="popup_chkbox" value="1"{if $REMINDER_CHECKED} checked="checked"{/if} {$disabled}>
		<span>{$MOD.LBL_REMINDERS_POPUP}</span>&nbsp;

		{if !$disabled}
			<br>
			<select tabindex="0" class="popup_timer_sel" name="reminder_time"{$disabled}>
				{html_options options=$REMINDER_TIME_OPTIONS selected=$REMINDER_TIME}
			</select>
			<br>
			&nbsp;
		{else}
			{$REMINDER_TIME_OPTIONS.$REMINDER_TIME}
		{/if}
	</div>
</td>
<td>
	<div class="col">
		<input name="email_reminder_checked" type="hidden" value="0">
		<input name="email_reminder_checked" type="checkbox" class="email_chkbox" value="1"{if $EMAIL_REMINDER_CHECKED} checked="checked"{/if} {$disabled}>
		<span>{$MOD.LBL_REMINDERS_EMAIL}</span>&nbsp;

		{if !$disabled}
			<br>
			<select tabindex="0" class="email_timer_sel" name="email_reminder_time"{$disabled}>
				{html_options options=$REMINDER_TIME_OPTIONS selected=$EMAIL_REMINDER_TIME}
			</select>
			<br>
			&nbsp;
		{else}
			{$REMINDER_TIME_OPTIONS.$EMAIL_REMINDER_TIME}
		{/if}
	</div>
</td>
</div>
