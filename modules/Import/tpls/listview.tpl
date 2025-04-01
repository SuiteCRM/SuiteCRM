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
<style type="text/css">{literal}
.warn { font-style:italic;
        font-weight:bold;
        color:red;
}{/literal}
</style>

<script type='text/javascript' src='{sugar_getjspath file='include/javascript/popup_helper.js'}'></script>

<div id="{$tableID}_content">
    <table cellpadding='0' cellspacing='0' width='50%' border='0' class='list view'>
        {include file='modules/Import/tpls/listviewpaginator.tpl'}
        <tr height='20'>
            {counter start=0 name="colCounter" print=false assign="colCounter"}
            {if $displayColumns eq false}
                <th scope='col'  style="text-align: left;" nowrap="nowrap" colspan="{$maxColumns}">{$MOD.LBL_MISSING_HEADER_ROW}</th>
            {else}
                {foreach from=$displayColumns key=colHeader item=label}
                    <th scope='col' nowrap="nowrap">
                        <div style='white-space: nowrap; width:100%; text-align:left;'>
                        {$label}
                        </div>
                    </th>
                    {counter name="colCounter"}
                {/foreach}
            {/if}
        </tr>
        {counter start=$pageData.offsets.current print=false assign="offset" name="offset"}
        {foreach name=rowIteration from=$data key=id item=rowData}
            {counter name="offset" print=false}

            {if $smarty.foreach.rowIteration.iteration is odd}
                {assign var='_rowColor' value=$rowColor[0]}
            {else}
                {assign var='_rowColor' value=$rowColor[1]}
            {/if}
            <tr height='20' class='{$_rowColor}S1'>
                {counter start=0 name="colCounter" print=false assign="colCounter"}
                {foreach from=$rowData key=col item=params}
                    {strip}
                    <td align='left' valign="top" scope="record">
                        {$params}
                    </td>
                    {/strip}
                    {counter name="colCounter"}
                {/foreach}
                </tr>
        {foreachelse}
        <tr height='20' class='{$rowColor[0]}S1'>
            <td colspan="{$colCounter}">
                <em>{$APP.LBL_NO_DATA}</em>
            </td>
        </tr>
        {/foreach}
    {include file='modules/Import/tpls/listviewpaginator.tpl'}
    </table>
</div>
