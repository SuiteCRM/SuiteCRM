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

{include file='include/ListView/ListViewColumnsFilterDialog.tpl'}
<script type='text/javascript' src='{sugar_getjspath file='include/javascript/popup_helper.js'}'></script>


<script>
    {literal}
    $(document).ready(function () {
      $("ul.clickMenu").each(function (index, node) {
        $(node).sugarActionMenu();
      });

      $('.selectActionsDisabled').children().each(function (index) {
        $(this).attr('onclick', '').unbind('click');
      });

      var selectedTopValue = $("#selectCountTop").attr("value");
      if (typeof(selectedTopValue) != "undefined" && selectedTopValue != "0") {
        sugarListView.prototype.toggleSelected();
      }
    });
    {/literal}
</script>
{assign var="currentModule" value = $pageData.bean.moduleDir}
{assign var="singularModule" value = $moduleListSingular.$currentModule}
{assign var="moduleName" value = $moduleList.$currentModule}
{assign var="hideTable" value=false}

{if !isset($data) || $data|count == 0}
    {assign var="hideTable" value=true}
    <div class="list view listViewEmpty">
        {if $displayEmptyDataMesssages}
            {if strlen($query) == 0}
                {capture assign="createLink"}<a
                    href="?module={$pageData.bean.moduleDir}&action=EditView&return_module={$pageData.bean.moduleDir}&return_action=DetailView">{$APP.LBL_CREATE_BUTTON_LABEL}</a>{/capture}
                {capture assign="importLink"}<a
                    href="?module=Import&action=Step1&import_module={$pageData.bean.moduleDir}&return_module={$pageData.bean.moduleDir}&return_action=index">{$APP.LBL_IMPORT}</a>{/capture}
                {capture assign="helpLink"}<a target="_blank"
                                              href='?module=Administration&action=SupportPortal&view=documentation&version={$sugar_info.sugar_version}&edition={$sugar_info.sugar_flavor}&lang=&help_module={$currentModule}&help_action=&key='>{$APP.LBL_CLICK_HERE}</a>{/capture}
                <p class="msg">
                    {$APP.MSG_EMPTY_LIST_VIEW_NO_RESULTS|replace:"<item2>":$createLink|replace:"<item3>":$importLink}
                </p>
            {elseif $query == "-advanced_search"}
                <p class="msg emptyResults">
                    {$APP.MSG_LIST_VIEW_NO_RESULTS_CHANGE_CRITERIA}
                </p>
            {else}
                <p class="msg">
                    {capture assign="quotedQuery"}"{$query}"{/capture}
                    {$APP.MSG_LIST_VIEW_NO_RESULTS|replace:"<item1>":$quotedQuery}
                </p>
                <p class="submsg">
                    <a href="?module={$pageData.bean.moduleDir}&action=EditView&return_module={$pageData.bean.moduleDir}&return_action=DetailView">
                        {$APP.MSG_LIST_VIEW_NO_RESULTS_SUBMSG|replace:"<item1>":$quotedQuery|replace:"<item2>":$singularModule}
                    </a>
                </p>
            {/if}
        {else}
            <p class="msg">
                {$APP.LBL_NO_DATA}
            </p>
        {/if}
        {if $showFilterIcon}
            {include file='include/ListView/ListViewSearchLink.tpl'}
        {/if}
    </div>
{/if}
{$multiSelectData}
{if $hideTable == false}
    <table cellpadding='0' cellspacing='0' width='100%' border='0' class='list view table'>
        <thead>
        {assign var="link_select_id" value="selectLinkTop"}
        {assign var="link_action_id" value="actionLinkTop"}
        {assign var="actionsLink" value=$actionsLinkTop}
        {assign var="selectLink" value=$selectLinkTop}
        {assign var="action_menu_location" value="top"}
        {include file='include/ListView/ListViewPagination.tpl'}
        <tr height='20'>
            {if $prerow}
                <td width='1%' class="td_alt">
                    &nbsp;
                </td>
            {/if}
            {if !empty($quickViewLinks)}
                <td class='td_alt' width='1%' style="padding: 0px;">&nbsp;</td>
            {/if}
            {counter start=0 name="colCounter" print=false assign="colCounter"}
            {assign var='datahide' value="phone"}
            {foreach from=$displayColumns key=colHeader item=params}
                {if $colCounter == '3'}{assign var='datahide' value="phone,phonelandscape"}{/if}
                {if $colCounter == '5'}{assign var='datahide' value="phone,phonelandscape,tablet"}{/if}
                {if $colCounter == '0'}
                    {assign var='hiddenclass' value=""}
                {elseif $colCounter < '5'}
                    {assign var='hiddenclass' value="hidden-xs"}
                {elseif $colCounter >= '5'}
                    {assign var='hiddenclass' value="hidden-xs hidden-sm hidden-md"}
                {/if}
                {if $colHeader == 'NAME' || $params.bold}
                <th scope='col' data-toggle="true" class="{$hiddenclass}">
                {else}<th scope='col' data-hide="{$datahide}" class="{$hiddenclass}">{/if}
                <div style='white-space: normal;' width='100%' align='{$params.align|default:'left'}'>
                    {if $params.sortable|default:true}
                    {if $params.url_sort}
                    <a href='{$pageData.urls.orderBy}{$params.orderBy|default:$colHeader|lower}'
                       class='listViewThLinkS1'>
                        {else}
                        {if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy}
                        <a href='javascript:sListView.order_checks("{$pageData.ordering.sortOrder|default:ASCerror}", "{$params.orderBy|default:$colHeader|lower}" , "{$pageData.bean.moduleDir}{"2_"}{$pageData.bean.objectName|upper}{"_ORDER_BY"}")'
                           class='listViewThLinkS1'>
                            {else}
                            <a href='javascript:sListView.order_checks("ASC", "{$params.orderBy|default:$colHeader|lower}" , "{$pageData.bean.moduleDir}{"2_"}{$pageData.bean.objectName|upper}{"_ORDER_BY"}")'
                               class='listViewThLinkS1'>
                                {/if}
                                {/if}
                                {sugar_translate label=$params.label module=$pageData.bean.moduleDir}
                                &nbsp;&nbsp;
                                {if $params.orderBy|default:$colHeader|lower == $pageData.ordering.orderBy}
                                    {if $pageData.ordering.sortOrder == 'ASC'}
                                        {capture assign="imageName"}arrow_down.{$arrowExt}{/capture}
                                        <span class="suitepicon suitepicon-action-sorting-ascending"></span>
                                    {else}
                                        {capture assign="imageName"}arrow_up.{$arrowExt}{/capture}
                                        <span class="suitepicon suitepicon-action-sorting-descending"></span>
                                    {/if}
                                {else}
                                    {capture assign="imageName"}arrow.{$arrowExt}{/capture}
                                    <span class="suitepicon suitepicon-action-sorting-none"></span>
                                {/if}
                            </a>
                            {else}
                            {if !isset($params.noHeader) || $params.noHeader == false}
                                {sugar_translate label=$params.label module=$pageData.bean.moduleDir}
                            {/if}
                            {/if}
                </div>
                </th>
                {counter name="colCounter"}
            {/foreach}
            <th width='1%' class="td_alt">
                &nbsp;
            </th>
        </tr>
        </thead>
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
                    <td width='1%' class='nowrap'>
                        {if !$is_admin && $is_admin_for_user && $rowData.IS_ADMIN==1}
                            <input type='checkbox' disabled="disabled" class='checkbox' value='{$rowData.ID}'>
                        {else}
                            <input title="{sugar_translate label='LBL_SELECT_THIS_ROW_TITLE'}"
                                   onclick='sListView.check_item(this, document.MassUpdate)' type='checkbox'
                                   class='checkbox' name='mass[]' value='{$rowData.ID}'>
                        {/if}
                    </td>
                {/if}
                {if !empty($quickViewLinks)}
                    {capture assign=linkModule}{if $params.dynamic_module}{$rowData[$params.dynamic_module]}{else}{$pageData.bean.moduleDir}{/if}{/capture}
                    {capture assign=action}{if $act}{$act}{else}EditView{/if}{/capture}
                    <td width='2%' nowrap>
                        {if $pageData.rowAccess[$id].edit}
                            {if $linkModule != 'AOR_Reports'}
                                <a title='{$editLinkString}' id="edit-{$rowData.ID}"
                                   href="index.php?module={$linkModule}&offset={$offset}&stamp={$pageData.stamp}&return_module={$linkModule}&action={$action}&record={$rowData.ID}"
                                >
                                    {capture name='tmp1' assign='alt_edit'}{sugar_translate label="LNK_EDIT"}{/capture}
                                    {sugar_getimage name="edit_inline.gif" attr='border="0" ' alt="$alt_edit"}</a>
                            {/if}
                        {/if}
                    </td>
                {/if}
                {counter start=0 name="colCounter" print=false assign="colCounter"}
                {foreach from=$displayColumns key=col item=params}

                    {if $colCounter == '0'}
                        {assign var='hiddenclass' value=""}
                    {elseif $colCounter < '5'}
                        {assign var='hiddenclass' value="hidden-xs"}
                    {elseif $colCounter >= '5'}
                        {assign var='hiddenclass' value="hidden-xs hidden-sm hidden-md"}
                    {/if}
                    {$displayColumns[type]}
                    {strip}
                        <td {if $scope_row} scope='row' {/if} align='{$params.align|default:'left'}' valign="top"
                                                              type="{$displayColumns.$col.type}" field="{$col|lower}"
                                                              class="{$hiddenclass} {if $inline_edit && ($displayColumns.$col.inline_edit == 1 || !isset($displayColumns.$col.inline_edit))}inlineEdit{/if}{if ($params.type == 'teamset')}nowrap{/if}{if preg_match('/PHONE/', $col)} phone{/if}">
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
                                {if empty($rowData.$col) && empty($params.customCode)}&nbsp;{/if}
                                {if $params.link && !$params.customCode}
                            </{$pageData.tag.$id[$params.ACLTag]|default:$pageData.tag.$id.MAIN}>
                            {/if}
                            {if $inline_edit && ($displayColumns.$col.inline_edit == 1 || !isset($displayColumns.$col.inline_edit))}
                                <div class="inlineEditIcon"><span class="suitepicon suitepicon-action-edit"></span></div>{/if}
                        </td>
                    {/strip}
                    {assign var='scope_row' value=false}
                    {counter name="colCounter"}

                {/foreach}
                <td align='right'>{$pageData.additionalDetails.$id|default:''}</td>
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
        {include file='include/ListView/ListViewPagination.tpl'}
    </table>
{/if}
{if $contextMenus}
    <script type="text/javascript">
        {$contextMenuScript}
        {literal}
        function lvg_nav(m, id, act, offset, t) {
          if (t.href.search(/#/) < 0) {
            return;
          }
          else {
            if (act == 'pte') {
              act = 'ProjectTemplatesEditView';
            }
            else if (act == 'd') {
              act = 'DetailView';
            } else if (act == 'ReportsWizard') {
              act = 'ReportsWizard';
            } else {
              act = 'EditView';
            }
              {/literal}
            url = 'index.php?module=' + m + '&offset=' + offset + '&stamp={$pageData.stamp}&return_module=' + m + '&action=' + act + '&record=' + id;
            t.href = url;
              {literal}
          }
        }{/literal}
        {literal}
        function lvg_dtails(id) {{/literal}
          return SUGAR.util.getAdditionalDetails('{$pageData.bean.moduleDir|default:$params.module}', id, 'adspan_' + id);{literal}}{/literal}
    </script>
    <script type="text/javascript" src="include/InlineEditing/inlineEditing.js"></script>
{/if}
