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

{{include file=$headerTpl}}
{$jsLang}
{$LIST_HEADER}
{if $should_process}
	<table cellpadding='0' cellspacing='0' width='100%' border='0' class='list view'>

		<thead>
		<tr height='20'>
			{if $prerow}
				<th nowrap="nowrap" width="43px" class="selectCol td_alt"></th>
				<th class='td_alt' nowrap="nowrap" width='1%'>&nbsp;</th>
			{/if}
			{counter start=0 name="colCounter" print=false assign="colCounter"}
			{foreach from=$displayColumns key=colHeader item=params}
				{if $colCounter == '0'}<th scope='col'  data-toggle="true">
				{else}<th scope='col'  data-hide="phone,tablet">{/if}
					<div style='white-space: normal; width:100%; text-align:{$params.align|default:'left'}'>
	                {if $params.sortable|default:true}
                                <a href="{$pageData.urls.orderBy}{$params.orderBy|default:$colHeader|lower}" onclick='sListView.save_checks(0, "{$moduleString}");' class='listViewThLinkS1'>{sugar_translate label=$params.label module=$pageData.bean.moduleDir}&nbsp;&nbsp;
						{if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy}
							{if $pageData.ordering.sortOrder == 'ASC'}
                                {capture assign="arrowAlt"}{sugar_translate label='LBL_ALT_SORT_DESC'}{/capture}
								<span class="suitepicon suitepicon-action-sorting-descending"></span>
							{else}
                                {capture assign="arrowAlt"}{sugar_translate label='LBL_ALT_SORT_ASC'}{/capture}
                                {capture assign="imageName"}arrow_up.{$arrowExt}{/capture}
								<span class="suitepicon suitepicon-action-sorting-ascending"></span>
							{/if}
						{else}
                            {capture assign="arrowAlt"}{sugar_translate label='LBL_ALT_SORT'}{/capture}
                            {capture assign="imageName"}arrow.{$arrowExt}{/capture}
							<span class="suitepicon suitepicon-action-sorting-none"></span>
						{/if}
					{else}
						{sugar_translate label=$params.label module=$pageData.bean.moduleDir}
					{/if}
					</div>
				</th>
				{counter name="colCounter"}
			{/foreach}
			{if !empty($quickViewLinks)}
			<th class='td_alt' nowrap="nowrap" width='1%'>&nbsp;</th>
			{/if}
		</tr>
		<tr class='pagination'  role='presentation'>
			<td colspan='{$colCount+1}' align='right'>
				<table border='0' cellpadding='0' cellspacing='0' width='100%'>
					<tr>
						<td align='left' >
							<div>
								{sugar_action_menu id=$link_select_id params=$selectLink}
							</div>
						</td>
						<td  align='right' nowrap='nowrap'>
							{if $pageData.urls.startPage}
								<button type='button' id='popupViewStartButton' title='{$navStrings.start}' class='button' {if $prerow}onclick='return sListView.save_checks(0, "{$moduleString}");'{else} onClick='location.href="{$pageData.urls.startPage}"' {/if}>
									<span class="suitepicon suitepicon-action-first"></span>
								</button>
							{else}
								<button type='button' id='popupViewStartButton' title='{$navStrings.start}' class='button' disabled>
									<span class="suitepicon suitepicon-action-first"></span>
								</button>
							{/if}
							{if $pageData.urls.prevPage}
								<button type='button' id='popupViewPrevButton' title='{$navStrings.previous}' class='button' {if $prerow}onclick='return sListView.save_checks({$pageData.offsets.prev}, "{$moduleString}")' {else} onClick='location.href="{$pageData.urls.prevPage}"'{/if}>
									<span class="suitepicon suitepicon-action-left"></span>
								</button>
							{else}
								<button type='button' id='popupViewPrevButton' class='button' disabled title='{$navStrings.previous}'>
									<span class="suitepicon suitepicon-action-left"></span>
								</button>
							{/if}
							<span class='pageNumbers'>({if $pageData.offsets.lastOffsetOnPage == 0}0{else}{$pageData.offsets.current+1}{/if} - {$pageData.offsets.lastOffsetOnPage} {$navStrings.of} {if $pageData.offsets.totalCounted}{$pageData.offsets.total}{else}{$pageData.offsets.total}{if $pageData.offsets.lastOffsetOnPage != $pageData.offsets.total}+{/if}{/if})</span>
							{if $pageData.urls.nextPage}
								<button type='button' id='popupViewNextButton' title='{$navStrings.next}' class='button' {if $prerow}onclick='return sListView.save_checks({$pageData.offsets.next}, "{$moduleString}")' {else} onClick='location.href="{$pageData.urls.nextPage}"'{/if}>
									<span class="suitepicon suitepicon-action-right"></span>
								</button>
							{else}
								<button type='button' id='popupViewNextButton' class='button' title='{$navStrings.next}' disabled>
									<span class="suitepicon suitepicon-action-right"></span>
								</button>
							{/if}
							{if $pageData.urls.endPage  && $pageData.offsets.total != $pageData.offsets.lastOffsetOnPage}
								<button type='button' id='popupViewEndButton' title='{$navStrings.end}' class='button' {if $prerow}onclick='return sListView.save_checks("end", "{$moduleString}")' {else} onClick='location.href="{$pageData.urls.endPage}"'{/if}>
									<span class="suitepicon suitepicon-action-last"></span>
								</button>
							{elseif !$pageData.offsets.totalCounted || $pageData.offsets.total == $pageData.offsets.lastOffsetOnPage}
								<button type='button' id='popupViewEndButton' class='button' disabled title='{$navStrings.end}'>
									<span class="suitepicon suitepicon-action-last"></span>
								</button>
							{/if}
						</td>
					</tr>
				</table>
			</td>
		</tr>
		</thead>
		<tbody>
		{foreach name=rowIteration from=$data key=id item=rowData}
			{if $smarty.foreach.rowIteration.iteration is odd}
				{assign var='_rowColor' value=$rowColor[0]}
			{else}
				{assign var='_rowColor' value=$rowColor[1]}
			{/if}
			<tr height='20' class='{$_rowColor}S1'>
				{if $prerow}
				<td width='1%' nowrap='nowrap'>
						<input onclick='sListView.check_item(this, document.MassUpdate)' type='checkbox' class='checkbox' name='mass[]' value='{$rowData.ID}'>
				</td>
				<td width='1%' nowrap='nowrap'>
						{$pageData.additionalDetails.$id}
				</td>
				{/if}
				{counter start=0 name="colCounter" print=false assign="colCounter"}
				{foreach from=$displayColumns key=col item=params}
					<td scope='row' align='{$params.align|default:'left'}' valign=top class='{$_rowColor}S1' bgcolor='{$_bgColor}'>
						{if $params.link && !$params.customCode}

							<{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN} href='javascript:void(0)' onclick="send_back('{if $params.dynamic_module}{$rowData[$params.dynamic_module]}{else}{$params.module|default:$pageData.bean.moduleDir}{/if}','{$rowData[$params.id]|default:$rowData.ID}');">{$rowData.$col}</{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN}>

						{elseif $params.customCode}
							{sugar_evalcolumn_old var=$params.customCode rowData=$rowData}
						{elseif $params.currency_format}
							{sugar_currency_format
								id=$rowData.ID
								var=$rowData.$col
								field_name=$params.name
	                            round=$params.currency_format.round
	                            decimals=$params.currency_format.decimals
	                            symbol=$params.currency_format.symbol
	                            convert=$params.currency_format.convert
	                            currency_symbol=$params.currency_format.currency_symbol
							}
						{elseif $params.type == 'bool'}
								<input type='checkbox' disabled=disabled class='checkbox'
								{if !empty($rowData[$col])}
									checked=checked
								{/if}
								/>
                        {elseif $params.type == 'multienum' }
								{counter name="oCount" assign="oCount" start=0}
								{multienum_to_array string=$rowData.$col assign="vals"}
								{foreach from=$vals item=item}
									{counter name="oCount"}
									{sugar_translate label=$params.options select=$item}{if $oCount !=  count($vals)},{/if}
								{/foreach}
                        {else}
                            {sugar_field parentFieldArray=$rowData vardef=$params displayType=ListView field=$col}
						{/if}
					</td>
					{counter name="colCounter"}
				{/foreach}

		{/foreach}
		</tbody>
		<tfoot>
		<tr class='pagination' role='presentation'>
			<td colspan='{$colCount+1}' align='right'>
				<table border='0' cellpadding='0' cellspacing='0' width='100%'>
					<tr>
						<td align='left' >
							<div>
								{sugar_action_menu id=$link_select_id params=$selectLink}
							</div>
						</td>
						<td  align='right' nowrap='nowrap'>
							{if $pageData.urls.startPage}
								<button type='button' title='{$navStrings.start}' class='button' {if $prerow}onclick='return sListView.save_checks(0, "{$moduleString}");'{else} onClick='location.href="{$pageData.urls.startPage}"' {/if}>
									<span class="suitepicon suitepicon-action-first"></span>
								</button>
							{else}
								<button type='button' title='{$navStrings.start}' class='button' disabled>
									<span class="suitepicon suitepicon-action-first"></span>
								</button>
							{/if}
							{if $pageData.urls.prevPage}
								<button type='button' title='{$navStrings.previous}' class='button' {if $prerow}onclick='return sListView.save_checks({$pageData.offsets.prev}, "{$moduleString}")' {else} onClick='location.href="{$pageData.urls.prevPage}"'{/if}>
									<span class="suitepicon suitepicon-action-left"></span>
								</button>
							{else}
								<button type='button' class='button' disabled title='{$navStrings.previous}'>
									<span class="suitepicon suitepicon-action-left"></span>
								</button>
							{/if}
							<span class='pageNumbers'>({if $pageData.offsets.lastOffsetOnPage == 0}0{else}{$pageData.offsets.current+1}{/if} - {$pageData.offsets.lastOffsetOnPage} {$navStrings.of} {if $pageData.offsets.totalCounted}{$pageData.offsets.total}{else}{$pageData.offsets.total}{if $pageData.offsets.lastOffsetOnPage != $pageData.offsets.total}+{/if}{/if})</span>
							{if $pageData.urls.nextPage}
								<button type='button' title='{$navStrings.next}' class='button' {if $prerow}onclick='return sListView.save_checks({$pageData.offsets.next}, "{$moduleString}")' {else} onClick='location.href="{$pageData.urls.nextPage}"'{/if}>
									<span class="suitepicon suitepicon-action-right"></span>
								</button>
							{else}
								<button type='button' class='button' title='{$navStrings.next}' disabled>
									<span class="suitepicon suitepicon-action-right"></span>
								</button>
							{/if}
							{if $pageData.urls.endPage  && $pageData.offsets.total != $pageData.offsets.lastOffsetOnPage}
								<button type='button' title='{$navStrings.end}' class='button' {if $prerow}onclick='return sListView.save_checks({$pageData.offsets.end}, "{$moduleString}")' {else} onClick='location.href="{$pageData.urls.endPage}"'{/if}>
									<span class="suitepicon suitepicon-action-last"></span>
								</button>
							{elseif !$pageData.offsets.totalCounted || $pageData.offsets.total == $pageData.offsets.lastOffsetOnPage}
								<button type='button' class='button' disabled title='{$navStrings.end}'>
									<span class="suitepicon suitepicon-action-last"></span>
								</button>
							{/if}
						</td>
					</tr>
				</table>
			</td>
		</tr>
		</tfoot>
	</table>
	{if $prerow}
	<script>
	{literal}function lvg_dtails(id){return SUGAR.util.getAdditionalDetails( '{/literal}{$module}{literal}',id, 'adspan_'+id);}{/literal}
	</script>
	{/if}
{else}
	{$APP.LBL_SEARCH_POPULATE_ONLY}
{/if}
{{include file=$footerTpl}}
