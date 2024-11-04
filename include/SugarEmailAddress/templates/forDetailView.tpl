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

			<table cellpadding="0" cellspacing="0" border="0" width="100%">
				{foreach from=$emailAddresses item=address}
				<tr>
					<td style="border:none;">
						{if $address.key === 'opt_out' || $address.key === 'invalid' || $address.key === 'opt_out_invalid'}
							<span style="text-decoration: line-through;">
						{elseif $address.key === 'primary'}
							<b>
						{elseif $address.key === 'reply_to' && (isset($item.key) && $item.key != 0)}
							<i>
						{/if}

						{$address.address}

						{if $address.key === 'primary'}
							</b>&nbsp;<i>&#x28;{$app_strings.LBL_EMAIL_PRIMARY}&#x29;&#x200E;</i>
						{elseif $address.key === 'reply_to'}
							&nbsp;<i>&#x28;{$app_strings.LBL_EMAIL_REPLY_TO}&#x29;&#x200E;</i>
						{elseif $address.key === 'opt_out'}
							</span>&nbsp;<i class='error'>&#x28;{$app_strings.LBL_EMAIL_OPT_OUT}&#x29;&#x200E;</i>
						{elseif $address.key === 'invalid'}
							</span>&nbsp;<i>({$app_strings.LBL_EMAIL_INVALID}&#x29;&#x200E;</i>
						{elseif $address.key === 'opt_out_invalid'}
						    </span>&nbsp;<i class='error'>&#x28;{$app_strings.LBL_EMAIL_OPT_OUT_AND_INVALID}&#x29;&#x200E;</i>
						{/if}
					</td>
				</tr>
				{foreachelse}
				<tr>
					<td>
						<i>{$app_strings.LBL_NONE}</i>
					</td>
				</tr>
				{/foreach}
			</table>
