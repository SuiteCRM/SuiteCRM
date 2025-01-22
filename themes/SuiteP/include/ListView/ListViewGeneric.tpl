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
{sugar_include include=$includes}
{include file='include/ListView/ListViewColumnsFilterDialog.tpl'}
<script type='text/javascript' src='{sugar_getjspath file='include/javascript/popup_helper.js'}'></script>


<script>
{literal}
	$(document).ready(function(){
	    $("ul.clickMenu").each(function(index, node){
	  		$(node).sugarActionMenu();
	  	});

        $('.selectActionsDisabled').children().each(function(index) {
            $(this).attr('onclick','').unbind('click');
        });

        var selectedTopValue = $("#selectCountTop").attr("value");
        if(typeof(selectedTopValue) != "undefined" && selectedTopValue != "0"){
        	sugarListView.prototype.toggleSelected();
        }
	});
{/literal}
</script>
{assign var="currentModule" value = $pageData.bean.moduleDir}
{assign var="singularModule" value = $moduleListSingular.$currentModule}
{assign var="moduleName" value = $moduleList.$currentModule}
{assign var="hideTable" value=false}

{if $form.headerTpl}
    {sugar_include type="smarty" file=$form.headerTpl}
{/if}

{if !isset($data) || (count($data) == 0)}
	{assign var="hideTable" value=true}
	<div class="list view listViewEmpty">
        {if $showFilterIcon}
			<div class="filterContainer">
                {include file='include/ListView/ListViewSearchLink.tpl'}
			</div>
        {/if}
		{if $displayEmptyDataMesssages}
        {if strlen($query) == 0}
                {capture assign="createLink"}<a href="?module={$pageData.bean.moduleDir}&action=EditView&return_module={$pageData.bean.moduleDir}&return_action=DetailView">{$APP.LBL_CREATE_BUTTON_LABEL}</a>{/capture}
                {capture assign="importLink"}<a href="?module=Import&action=Step1&import_module={$pageData.bean.moduleDir}&return_module={$pageData.bean.moduleDir}&return_action=index">{$APP.LBL_IMPORT}</a>{/capture}
                {capture assign="helpLink"}<a target="_blank" href='?module=Administration&action=SupportPortal&view=documentation&version={$sugar_info.sugar_version}&edition={$sugar_info.sugar_flavor}&lang=&help_module={$currentModule}&help_action=&key='>{$APP.LBL_CLICK_HERE}</a>{/capture}

			{if !(isset($options.hide_edit_link) && $options.hide_edit_link === true) && !empty($quickViewLinks)}
				<p class="msg"> {$APP.MSG_EMPTY_LIST_VIEW_NO_RESULTS|replace:"<item2>":$createLink|replace:"<item3>":$importLink} </p>
			{/if}

        {elseif $query == "-advanced_search"}
            <p class="msg emptyResults">
                {$APP.MSG_LIST_VIEW_NO_RESULTS_CHANGE_CRITERIA}
            </p>
        {else}
            <p class="msg">
                {capture assign="quotedQuery"}"{$query}"{/capture}
                {$APP.MSG_LIST_VIEW_NO_RESULTS|replace:"<item1>":$quotedQuery}
            </p>
			{if $pageData}
            <p class="submsg">
                <a href="?module={$pageData.bean.moduleDir}&action=EditView&return_module={$pageData.bean.moduleDir}&return_action=DetailView">
                    {$APP.MSG_LIST_VIEW_NO_RESULTS_SUBMSG|replace:"<item1>":$quotedQuery|replace:"<item2>":$singularModule}
                </a>
            </p>
			{/if}
        {/if}
    {else}
        <p class="msg">
            {$APP.LBL_NO_DATA}
        </p>
	{/if}
	</div>
{/if}
{$multiSelectData}
{if $hideTable == false}
	<div class="list-view-rounded-corners">
		<table cellpadding='0' cellspacing='0' border='0' class='list view table-responsive'>
	<thead>
		{assign var="link_select_id" value="selectLinkTop"}
		{assign var="link_action_id" value="actionLinkTop"}
		{assign var="actionsLink" value=$actionsLinkTop}
		{assign var="selectLink" value=$selectLinkTop}
		{assign var="action_menu_location" value="top"}

		<tr height='20'>
			{if $prerow}
				<th class="td_alt">&nbsp;</th>
			{/if}
			{if !(isset($options.hide_edit_link) && $options.hide_edit_link === true) && !empty($quickViewLinks)}
				<th class='td_alt quick_view_links'>&nbsp;</th>
			{/if}
			{counter start=0 name="colCounter" print=false assign="colCounter"}
            {assign var='datahide' value="xs sm"}
			{foreach from=$displayColumns key=colHeader item=params}
                {if $colCounter == '3'}{assign var='datahide' value="xs sm"}{/if}
                {if $colCounter == '5'}{assign var='datahide' value="md"}{/if}

				{if $colCounter == '0'}
					{assign var='hide' value=""}
				{elseif $colHeader  == 'NAME' }
					{assign var='hide' value=""}
				{elseif $colCounter  > '10' }
					{assign var='hide' value="hidden-xs hidden-sm hidden-md"}
				{elseif $colCounter > '4' }
					{assign var='hide' value="hidden-xs hidden-sm"}
				{elseif $colCounter > '0' }
					{assign var='hide' value="hidden-xs"}
				{else}
					{assign var='hide' value=""}
				{/if}
                {if $colHeader == 'NAME' || $params.bold}
					<th scope='col' data-toggle="true" class="{$hide}">
				{else}
					<th scope='col' data-breakpoints="{$datahide}" class="{$hide}">
				{/if}
						<div>
						{if $params.sortable|default:true}
							{if $params.url_sort}
								<a href='{$pageData.urls.orderBy}{$params.orderBy|default:$colHeader|lower}' class='listViewThLinkS1'>
							{else}
								{if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy}
									<a href='javascript:sListView.order_checks("{$pageData.ordering.sortOrder|default:ASCerror}", "{$params.orderBy|default:$colHeader|lower}" , "{$pageData.bean.moduleDir}{"2_"}{$pageData.bean.objectName|upper}{"_ORDER_BY"}")' class='listViewThLinkS1'>
								{else}
									<a href='javascript:sListView.order_checks("ASC", "{$params.orderBy|default:$colHeader|lower}" , "{$pageData.bean.moduleDir}{"2_"}{$pageData.bean.objectName|upper}{"_ORDER_BY"}")' class='listViewThLinkS1'>
								{/if}
							{/if}
							{if isset($params.hide_header_label) && $params.hide_header_label == true}
							{else}
							{sugar_translate label=$params.label module=$pageData.bean.moduleDir}
						&nbsp;&nbsp;  {/if}
							{if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy}
								{if $pageData.ordering.sortOrder == 'ASC'}
									{capture assign="imageName"}arrow_down.{$arrowExt}{/capture}
									{capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT_DESC'}{/capture}
									<span class="suitepicon suitepicon-action-sorting-ascending" title="{$alt_sort}"></span>
								{else}
									{capture assign="imageName"}arrow_up.{$arrowExt}{/capture}
									{capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT_ASC'}{/capture}
									<span class="suitepicon suitepicon-action-sorting-descending" title="{$alt_sort}"></span>
								{/if}
							{else}
								{capture assign="imageName"}arrow.{$arrowExt}{/capture}
								{capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT'}{/capture}
								<span class="suitepicon suitepicon-action-sorting-none" title="{$alt_sort}"></span>
							{/if}
							</a>
						{else}
							{if !isset($params.noHeader) || $params.noHeader == false}
                                {if isset($params.hide_header_label) && $params.hide_header_label == true}
                                {else}
                                    {sugar_translate label=$params.label module=$pageData.bean.moduleDir}
									{if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy && $params.force_show_sort_direction}
										{if $pageData.ordering.sortOrder == 'ASC'}
											{capture assign="imageName"}arrow_down.{$arrowExt}{/capture}
											{capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT_DESC'}{/capture}
											<span class="suitepicon suitepicon-action-sorting-ascending" title="{$alt_sort}"></span>
										{else}
											{capture assign="imageName"}arrow_up.{$arrowExt}{/capture}
											{capture assign="alt_sort"}{sugar_translate label='LBL_ALT_SORT_ASC'}{/capture}
											<span class="suitepicon suitepicon-action-sorting-descending" title="{$alt_sort}"></span>
										{/if}
									{/if}
							&nbsp;&nbsp;  {/if}
							{/if}
						{/if}
						</div>
					</th>
				{counter name="colCounter"}
			{/foreach}
			{* add extra column for icons*}
			<th>{$pageData.additionalDetails.$id}</th>
		</tr>
		{include file='themes/SuiteP/include/ListView/ListViewPaginationTop.tpl'}
	</thead>
	<tbody>
		{counter start=$pageData.offsets.current print=false assign="offset" name="offset"}
		{foreach name=rowIteration from=$data key=id item=rowData}
		    {counter name="offset" print=false}
	        {assign var='scope_row' value=true}

			{if $smarty.foreach.rowIteration.iteration is odd}
				{assign var='_rowColor' value=$rowColor[0]}
			{else}
				{assign var='_rowColor' value=$rowColor[1]}
			{/if}
			<tr height='20' class='{$_rowColor}S1'>
				{if $prerow}
				<td>
				 {if !$is_admin && $is_admin_for_user && $rowData.IS_ADMIN==1}
						<input type='checkbox' disabled="disabled" class='listview-checkbox' value='{$rowData.ID}'>
				 {else}
	                    <input title="{sugar_translate label='LBL_SELECT_THIS_ROW_TITLE'}" onclick='sListView.check_item(this, document.MassUpdate)' type='checkbox' class='listview-checkbox' name='mass[]' value='{$rowData.ID}'>
				 {/if}
				</td>
				{/if}
				{if !empty($quickViewLinks)}
	            {capture assign=linkModule}{if $params.dynamic_module}{$rowData[$params.dynamic_module]}{else}{$pageData.bean.moduleDir}{/if}{/capture}
	            {capture assign=action}{if $act}{$act}{else}EditView{/if}{/capture}
				{if isset($options.hide_edit_link) && $options.hide_edit_link === true}
				{else}
					<td>
                        {if $pageData.rowAccess[$id].edit && !empty($quickViewLinks)}
							<a class="edit-link" title='{$editLinkString}' id="edit-{$rowData.ID}"
							   href="index.php?module={$linkModule}&offset={$offset}&stamp={$pageData.stamp}&return_module={$linkModule}&action={$action}&record={$rowData.ID}"
							>
                                {capture name='tmp1' assign='alt_edit'}{sugar_translate label="LNK_EDIT"}{/capture}
								<span class="suitepicon suitepicon-action-edit"></span></a>
                        {/if}
					</td>
				{/if}

				{/if}
				{counter start=0 name="colCounter" print=false assign="colCounter"}
				{foreach from=$displayColumns key=col item=params}
					{if $colCounter == '0'}
						{assign var='hide' value=""}
					{elseif $col  == 'NAME' }
						{assign var='hide' value=""}
					{elseif $colCounter  > '10' }
						{assign var='hide' value="hidden-xs hidden-sm hidden-md"}
					{elseif $colCounter > '4' }
						{assign var='hide' value="hidden-xs hidden-sm"}
					{elseif $colCounter > '0' }
						{assign var='hide' value="hidden-xs"}
					{else}
						{assign var='hide' value=""}
					{/if}
                    {$displayColumns[type]}
				    {strip}
					<td {if $scope_row} scope='row' {/if} align='{$params.align|default:'left'}' valign="top" type="{$displayColumns.$col.type}" field="{$col|lower}" class="{$hide} {if $inline_edit && ($displayColumns.$col.inline_edit == 1 || !isset($displayColumns.$col.inline_edit))}inlineEdit{/if}{if ($params.type == 'teamset')}nowrap{/if}{if preg_match('/PHONE/', $col)} phone{/if}">
						{if $col == 'NAME' || $params.bold}<b>{/if}
					    {if $params.link && !$params.customCode}
							{capture assign=linkModule}{if $params.dynamic_module}{$rowData[$params.dynamic_module]}{else}{$params.module|default:$pageData.bean.moduleDir}{/if}{/capture}
							{capture assign=action}{if $act}{$act}{else}DetailView{/if}{/capture}
							{capture assign=record}{$rowData[$params.id]|default:$rowData.ID}{/capture}
							{capture assign=url}index.php?module={$linkModule}&offset={$offset}&stamp={$pageData.stamp}&return_module={$linkModule}&action={$action}&record={$record}{/capture}
													<{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN} href="{sugar_ajax_url url=$url}">
						{/if}

						{if $params.customCode}
							{sugar_evalcolumn_old var=$params.customCode rowData=$rowData}
						{else}
	                       {sugar_field parentFieldArray=$rowData vardef=$params displayType=ListView field=$col}

						{/if}
						{if empty($rowData.$col) && empty($params.customCode)}{/if}
						{if $params.link && !$params.customCode}
							</{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN}>
	                    {/if}
                        {if $inline_edit && ($displayColumns.$col.inline_edit == 1 || !isset($displayColumns.$col.inline_edit))}<div class="inlineEditIcon">{sugar_getimage name="inline_edit_icon.svg" attr='border="0" ' alt="$alt_edit"}</div>{/if}
					</td>
					{/strip}
	                {assign var='scope_row' value=false}
					{counter name="colCounter"}

				{/foreach}
				<td align='right'>{$pageData.additionalDetails.$id}</td>
		    	</tr>
		{foreachelse}
		<tr height='20' class='{$rowColor[0]}S1'>
		    <td colspan="{$colCount}">
		        <em>{$APP.LBL_NO_DATA}</em>
		    </td>
		</tr>
		{/foreach}
    {assign var="link_select_id" value="selectLinkBottom"}
    {assign var="link_action_id" value="actionLinkBottom"}
    {assign var="selectLink" value=$selectLinkBottom}
    {assign var="actionsLink" value=$actionsLinkBottom}
    {assign var="action_menu_location" value="bottom"}
	</tbody>
	<tfoot>
    {include file='themes/SuiteP/include/ListView/ListViewPaginationBottom.tpl'}
	</tfoot>
	</table></div>
{/if}
{if $contextMenus}
<script type="text/javascript">
{$contextMenuScript}
{literal}
function lvg_nav(m,id,act,offset,t){
  if (t.href.search(/#/) < 0) {
  }
    else{
        if(act=='pte'){
            act='ProjectTemplatesEditView';
        }
        else if(act=='d'){
            act='DetailView';
        }else if( act =='ReportsWizard'){
            act = 'ReportsWizard';
        }else{
            act='EditView';
        }
    {/literal}
        url = 'index.php?module='+m+'&offset=' + offset + '&stamp={$pageData.stamp}&return_module='+m+'&action='+act+'&record='+id;
        t.href=url;
    {literal}
    }
}{/literal}
{literal}
    function lvg_dtails(id){{/literal}
        return SUGAR.util.getAdditionalDetails( '{$pageData.bean.moduleDir|default:$params.module}',id, 'adspan_'+id);{literal}}{/literal}
</script>
<script type="text/javascript" src="include/InlineEditing/inlineEditing.js"></script>
{/if}


{if $form.footerTpl}
    {sugar_include type="smarty" file=$form.headerTpl}
{/if}
