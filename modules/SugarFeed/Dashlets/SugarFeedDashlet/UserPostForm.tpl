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

<form name='form_{$id}' id='form_{$id}'>
<div class="dashletNonTable" style='white-space:nowrap;'>
  <table border=0 cellspacing=0 cellpadding=2>
    <tr>
      <td nowrap="nowrap">{if !empty($link_types)}<span id='more_img_{$id}'>{$more_img}</span><span id='less_img_{$id}' style="display:none;">{$less_img}</span>{/if} <b>{$user_name}</b>&nbsp;</td>
      <td style="padding-right: 5px;"><input id="text" name="text" type="text" size='25' maxlength='100' value="" title="{sugar_translate label='LBL_POST_TITLE' module='SugarFeed'} {$user_name} "/></td>
      <td nowrap="nowrap">
      <input type="submit" value="{$LBL_POST}" class="button" style="vertical-align:top" onclick="SugarFeed.pushUserFeed('{$id}'); return false;"></td>
        <td>{$facebook}</td>
    </tr>
</table>
<div id='more_{$id}' style='display:none;padding-top:5px'>
<table>
{if !empty($link_types)}
<tr>
    <td>{html_options name='link_type' options=$link_types}</td>
    <td><input type='text' name='link_url' title="{sugar_translate label='LBL_URL_LINK_TITLE' module='SugarFeed'}"  size='30'/></td>

</tr>
{/if}
</table>
</div>
</div>

</form>

<form name='SugarFeedReplyForm_{$id}' id='SugarFeedReplyForm_{$id}'>
<input type='hidden' name='parentFeed' value=''>
<div style='white-space:nowrap; display: none;'>
 <table border=0 cellspacing=0 cellpadding=2>
    <tr>
      <td nowrap="nowrap"><b>{$user_name}</b>&nbsp;</td>
      <td style="padding-right: 5px;"><input id="text" name="text" type="text" size='25' maxlength='100' value="" /></td>
      <td nowrap="nowrap">
      <input type="submit" value="{$LBL_POST}" class="button" style="vertical-align:top" onclick="SugarFeed.replyToFeed('{$id}'); return false;"></td>
    </tr>
</table>
</div>
</form>

