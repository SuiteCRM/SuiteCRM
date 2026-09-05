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

{assign var="alt_start" value=$navStrings.start}
{assign var="alt_next" value=$navStrings.next}
{assign var="alt_prev" value=$navStrings.previous}
{assign var="alt_end" value=$navStrings.end}

<table id="dashletPanel" cellpadding='0' cellspacing='0' width='100%' border='0' class='list view default dashletPanel'>
	<thead>
    <tr class="pagination" role=”presentation”>
        <td colspan='{$colCount+1}' align='right'>
            <table border='0' cellpadding='0' cellspacing='0' width='100%'>
                <tr>
                    <td align='left'>&nbsp;</td>
                    <td align='right' nowrap='nowrap'>
                        {if isset($pageData.urls.startPage) && $pageData.urls.startPage}
							<button title='{$navStrings.start}' class='button' onclick='return SUGAR.mySugar.retrieveDashlet("{$dashletId}", "{$pageData.urls.startPage}", false, false, true, $(this).closest("div[id^=pageNum_][id$=_div]").parent().parent())'>
								<span class="suitepicon suitepicon-action-first"></span>
							</button>

                        {else}
                            <!--{sugar_getimage name="start_off" ext=".png" alt=$navStrings.start other_attributes='align="absmiddle" border="0" '}&nbsp;{$navStrings.start}&nbsp;&nbsp;-->
							<button title='{$navStrings.start}' class='button' disabled>
								<span class="suitepicon suitepicon-action-first"></span>
							</button>

                        {/if}
                        {if $pageData.urls.prevPage}
							<button title='{$navStrings.previous}' class='button' onclick='return SUGAR.mySugar.retrieveDashlet("{$dashletId}", "{$pageData.urls.prevPage}", false, false, true, $(this).closest("div[id^=pageNum_][id$=_div]").parent().parent())'>
								<span class="suitepicon suitepicon-action-left"></span>
							</button>

                        {else}
                            <!--{sugar_getimage name="previous_off" ext=".png" width="8" height="13" alt=$navStrings.previous other_attributes='align="absmiddle" border="0" '}&nbsp;{$navStrings.previous}&nbsp;-->
							<button class='button' disabled title='{$navStrings.previous}'>
								<span class="suitepicon suitepicon-action-left"></span>
							</button>
                        {/if}
                            <span class='pageNumbers'>({if $pageData.offsets.lastOffsetOnPage == 0}0{else}{$pageData.offsets.current+1}{/if} - {$pageData.offsets.lastOffsetOnPage} {$navStrings.of} {if $pageData.offsets.totalCounted}{$pageData.offsets.total}{else}{$pageData.offsets.total}{if $pageData.offsets.lastOffsetOnPage != $pageData.offsets.total}+{/if}{/if})</span>
                        {if $pageData.urls.nextPage}
							<button title='{$navStrings.next}' class='button' onclick='return SUGAR.mySugar.retrieveDashlet("{$dashletId}", "{$pageData.urls.nextPage}", false, false, true, $(this).closest("div[id^=pageNum_][id$=_div]").parent().parent())'>
								<span class="suitepicon suitepicon-action-right"></span>
							</button>

                        {else}
							<button class='button' title='{$navStrings.next}' disabled>
								<span class="suitepicon suitepicon-action-right"></span>
							</button>

                        {/if}
						{if $pageData.urls.endPage  && $pageData.offsets.total != $pageData.offsets.lastOffsetOnPage}
							<button title='{$navStrings.end}' class='button' onclick='return SUGAR.mySugar.retrieveDashlet("{$dashletId}", "{$pageData.urls.endPage}", false, false, true, $(this).closest("div[id^=pageNum_][id$=_div]").parent().parent())'>
								<span class="suitepicon suitepicon-action-last"></span>
							</button>

						{elseif !$pageData.offsets.totalCounted || $pageData.offsets.total == $pageData.offsets.lastOffsetOnPage}
							<button class='button' disabled title='{$navStrings.end}'>
								<span class="suitepicon suitepicon-action-last"></span>
							</button>

                        {/if}
                    </td>
                </tr>
            </table>
        </td>
    </tr>
    <tr height='20'>
        {counter start=0 name="colCounter" print=false assign="colCounter"}
        {assign var='datahide' value=""}
        {foreach from=$displayColumns key=colHeader item=params}
            {if $colCounter == '1'}{assign var='datahide' value="phone"}{/if}
            {if $colCounter == '3'}{assign var='datahide' value="phone,phonelandscape"}{/if}
            {if $colCounter == '5'}{assign var='datahide' value="phone,phonelandscape,tablet"}{/if}
            {if $colHeader == 'NAME' || $params.bold}<th scope='col' data-toggle="true">
            {else}<th scope='col' data-hide="{$datahide}">{/if}
				<div style='white-space: normal; width:100%; text-align:{$params.align|default:'left'}'>
                {if $params.sortable|default:true}
					<!-- dashlet: {$dashletId} -->
	                <a href='#' onclick='return SUGAR.mySugar.retrieveDashlet("{$dashletId}", "{$pageData.urls.orderBy}{$params.orderBy|default:$colHeader|lower}&sugar_body_only=1&id={$dashletId}", false, false, true, $(this).closest("div[id^=pageNum_][id$=_div]").parent().parent())' class='listViewThLinkS1' title="{$arrowAlt}">{sugar_translate label=$params.label module=$pageData.bean.moduleDir}</a>&nbsp;&nbsp;
	                {if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy}
	                    {if $pageData.ordering.sortOrder == 'ASC'}
                            {capture assign="imageName"}arrow_down.{$arrowExt}{/capture}
                            {capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT_DESC'}{/capture}
							<span class="suitepicon suitepicon-action-sorting-descending" title="{$alt_sort}"></span>
	                    {else}
                            {capture assign="imageName"}arrow_up.{$arrowExt}{/capture}
                            {capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT_ASC'}{/capture}
							<span class="suitepicon suitepicon-action-sorting-ascending" title="{$alt_sort}"></span>
	                    {/if}
	                {else}
                        {capture assign="imageName"}arrow.{$arrowExt}{/capture}
                        {capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT'}{/capture}
						<span class="suitepicon suitepicon-action-sorting-none" title="{$alt_sort}"></span>
	                {/if}
	           {else}
                    {if !isset($params.noHeader) || $params.noHeader == false}
	           		  {sugar_translate label=$params.label module=$pageData.bean.moduleDir}
                    {/if}
	           {/if}
			   </div>
            </th>
            {counter name="colCounter"}
        {/foreach}
		{if !empty($quickViewLinks)}
		<td  class='td_alt' nowrap="nowrap" width='1%'>&nbsp;</td>
		{/if}
    </tr>
	</thead>
	{foreach name=rowIteration from=$data key=id item=rowData}
		{if $smarty.foreach.rowIteration.iteration is odd}
			{assign var='_rowColor' value=$rowColor[0]}
		{else}
			{assign var='_rowColor' value=$rowColor[1]}
		{/if}
		<tr height='20' class='{$_rowColor}S1'>
			{if $prerow}
			<td width='1%' nowrap='nowrap'>
					<input onclick='sListView.check_item(this, document.MassUpdate)' type='checkbox' class='checkbox' name='mass[]' value='{$rowData[$params.id]|default:$rowData.ID}'>
			</td>
			{/if}
			{counter start=0 name="colCounter" print=false assign="colCounter"}
			{foreach from=$displayColumns key=col item=params}
			    {strip}
				<td scope='row' align='{$params.align|default:'left'}' valign="top" {if ($params.type == 'teamset')}class="nowrap" {else} class="text-wrap" {/if}>
					{if $col == 'NAME' || $params.bold}<b>{/if}
				    {if $params.link && !$params.customCode}
						<{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN} href='index.php?action={$params.action|default:'DetailView'}&module={if $params.dynamic_module}{$rowData[$params.dynamic_module]}{else}{$params.module|default:$pageData.bean.moduleDir}{/if}&record={$rowData[$params.id]|default:$rowData.ID}&offset={$pageData.offsets.current+$smarty.foreach.rowIteration.iteration}&stamp={$pageData.stamp}'>
					{/if}
					{if $params.customCode}
						{sugar_evalcolumn_old var=$params.customCode rowData=$rowData}
					{else}
                       {sugar_field parentFieldArray=$rowData vardef=$params displayType=ListView field=$col}
					{/if}
					{if empty($rowData.$col) && empty($params.customCode)}&nbsp;{/if}
					{if $params.link && !$params.customCode}
						</{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN}>
                    {/if}
                    {if $col == 'NAME' || $params.bold}</b>{/if}
				</td>
				{/strip}
				{counter name="colCounter"}
			{/foreach}
			{if !empty($quickViewLinks)}
			<td width='1%' class='{$_rowColor}S1' bgcolor='{$_bgColor}' nowrap>
				{if $pageData.rowAccess[$id].edit}
                    {capture name='tmp1' assign='alt_edit'}{sugar_translate label="LNK_EDIT"}{/capture}
                    {capture name='tmp1' assign='alt_view'}{sugar_translate label="LBL_VIEWINLINE"}{/capture}
                    <a title='{$editLinkString}' class="list-view-data-icon" href='index.php?action=EditView&module={$pageData.bean.moduleDir}&record={$rowData.ID}&offset={$pageData.offsets.current+$smarty.foreach.rowIteration.iteration}&stamp={$pageData.stamp}&return_module=Home&return_action=index'> <span class="suitepicon suitepicon-action-edit"></span></a>
				{/if}
				{if $pageData.access.view}
                <a title='{$viewLinkString}' class="list-view-data-icon" href='index.php?action=DetailView&module={$pageData.bean.moduleDir}&record={$rowData[$params.parent_id]|default:$rowData.ID}&offset={$pageData.offsets.current+$smarty.foreach.rowIteration.iteration}&stamp={$pageData.stamp}&return_module=Home&return_action=index' title="{sugar_translate label="LBL_VIEW_INLINE"}"> <span class="suitepicon suitepicon-action-view-record"></span></a>
				{/if}
			</td>
			{/if}
	    	</tr>
	{foreachelse}
	<tr height='20' class='{$rowColor[0]}S1'>
	    <td colspan="{$colCount}">
	        <em>{$APP.LBL_NO_DATA}</em>
	    </td>
	</tr>
	{/foreach}
</table>
