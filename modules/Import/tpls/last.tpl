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

{literal}
<style>
    #tabListContainer ul.subpanelTablist li a.current
    {
        padding-left: 10px;
    }
div.resultsTable {
    overflow: auto;
    padding-top: 20px;
    position: relative;
}
</style>
{/literal}

<h2>
	<p>{$MOD.LBL_SUMMARY}</p>
</h2>
<br/>
<span style="font-size: 14px">
{if $createdCount > 0}
<b>{$createdCount}</b>&nbsp;{$MOD.LBL_SUCCESSFULLY_IMPORTED}<br />
{/if}
{if $updatedCount > 0}
<b>{$updatedCount}</b>&nbsp;{$MOD.LBL_UPDATE_SUCCESSFULLY}<br />
{/if}
{if $errorCount > 0}
<b>{$errorCount}</b>&nbsp;{$MOD.LBL_RECORDS_SKIPPED_DUE_TO_ERROR}<br />
{/if}
{if $dupeCount > 0}
<b>{$dupeCount}</b>&nbsp;{$MOD.LBL_DUPLICATES}<br />
{/if}
</span>
<form name="importlast" id="importlast" method="POST" action="index.php">
<input type="hidden" name="module" value="Import">
<input type="hidden" name="action" value="Undo">
<input type="hidden" name="has_header" value="{$smarty.request.has_header}">
<input type="hidden" name="import_module" value="{$IMPORT_MODULE}">

<br />

<table width="100%" cellpadding="0" cellspacing="0" border="0">
    <tr>
        <td align="left" style="padding-bottom: 2px;">
        {if $showUndoButton}
            <input title="{$MOD.LBL_UNDO_LAST_IMPORT}"  class="button"
                type="submit" name="undo" id="undo" value="  {$MOD.LBL_UNDO_LAST_IMPORT}  ">
        {/if}
        <input title="{$MOD.LBL_IMPORT_MORE}"  class="button" type="submit" name="importmore" id="importmore" value="  {$MOD.LBL_IMPORT_MORE}  ">
        <input title="{$MOD.LBL_FINISHED}{$MODULENAME}"  class="button" type="submit" name="finished" id="finished" value="  {$MOD.LBL_IMPORT_COMPLETE}  ">
            {$PROSPECTLISTBUTTON}
        </td>
    </tr>
</table>
</form>

<br/>

<table width="100%" id="tabListContainerTable" cellpadding="0" cellspacing="0" border="0">
    <tr>
        <td nowrap id="tabListContainerTD">
            <div id="tabListContainer" class="yui-module yui-scroll">
                <div class="yui-hd">
                    <span class="yui-scroll-controls" style="visibility:hidden">
                        <a title="scroll left" class="yui-scrollup"><em>scroll left</em></a>
                        <a title="scroll right" class="yui-scrolldown"><em>scroll right</em></a>
                    </span>
                </div>
                <div class="yui-bd">
                    <ul class="subpanelTablist" id="tabList">
                        <li id="pageNumIW_0" class="active" >
                            <a id="pageNumIW_0_anchor" class="current" href="javascript:SUGAR.IV.togglePages('0');">
                            <span id="pageNum_0_input_span" style="display:none;">
                            <input type="hidden" id="pageNum_0_name_hidden_input" value="{$pageData.pageTitle}"/>
                            <input type="text" id="pageNum_0_name_input" value="Testing" size="10"/>
                            </span>
                            <span id="pageNum_0_link_span" class="tabText">
                            <span id="pageNum_0_title_text">{$MOD.LBL_CREATED_TAB}</span>
                            </span>
                            </a>
                        </li>
                        <li id="pageNumIW_1" >
                            <a id="pageNumIW_1_anchor" class="" href="javascript:SUGAR.IV.togglePages('1');">
                            <span id="pageNum_1_input_span" style="display:none;">
                            <input type="hidden" id="pageNum_1_name_hidden_input" value="{$pageData.pageTitle}"/>
                            <input type="text" id="pageNum_1_name_input" value="Testing" size="10"/>
                            </span>
                            <span id="pageNum_1_link_span" class="tabText">
                            <span id="pageNum_1_title_text">{$MOD.LBL_DUPLICATE_TAB}</span>
                            </span>
                            </a>
                        </li>
                        <li id="pageNumIW_2" >
                            <a id="pageNumIW_2_anchor" class="" href="javascript:SUGAR.IV.togglePages('2');">
                            <span id="pageNum_2_input_span" style="display:none;">
                            <input type="hidden" id="pageNum_2_name_hidden_input" value="{$pageData.pageTitle}"/>
                            <input type="text" id="pageNum_2_name_input" value="Testing" size="10" />
                            </span>
                            <span id="pageNum_2_link_span" class="tabText">
                            <span id="pageNum_2_title_text">{$MOD.LBL_ERROR_TAB}</span>
                            </span>
                            </a>
                        </li>
                    </ul>
                </div>
            </div>
            <div id="addPage" style="visibility: hidden">
                <a href='javascript:void(0)' id="add_page"></a>
            </div>
        </td>
        <td nowrap id="dashletCtrlsTD">
            <div id="dashletCtrls" style="margin:0;padding:0;"></div>
        </td>
    </tr>
</table>

<div style='width:100%'>
    <div id="pageNumIW_0_div">{$RESULTS_TABLE}</div>
    <div id="pageNumIW_1_div" style="display:none;" ><br/>
        {if $dupeCount > 0}
            <a href ="{$dupeFile}" target='_blank'>{$MOD.LNK_DUPLICATE_LIST}</a><br />
        {/if}
        <br/>
        {$MOD.LBL_DUP_HELP}
        <div id="dup_table" class="resultsTable">
            {$DUP_TABLE}
        </div>
    </div>
    <div id="pageNumIW_2_div" style="display: none;" ><br/>
        {$MOD.LBL_ERROR_HELP}
        {if $errorCount > 0}
            <br/><br/>
            <a href="{$errorFile}" target='_blank'>{$MOD.LNK_ERROR_LIST}</a><br />
            <a href ="{$errorrecordsFile}" target='_blank'>{$MOD.LNK_RECORDS_SKIPPED_DUE_TO_ERROR}</a><br />
        {/if}
        <div id="errors_table" class="resultsTable">
            {$ERROR_TABLE}
        </div>
    </div>
</div>

{if $PROSPECTLISTBUTTON != ''}
<form name="DetailView">
    <input type="hidden" name="module" value="Prospects">
    <input type="hidden" name="record" value="id">
</form>
{/if}
