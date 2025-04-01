{*
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
 *}
<h2 class="moduleTitle">{$APP.LBL_SEARCH_REAULTS_TITLE}</h2>
{if !empty($total)}{$APP.LBL_SEARCH_TOTAL}{$total}{/if}
{if !empty($error)}
    <p class="error">{$APP.ERR_SEARCH_INVALID_QUERY}</p>
{else}

    {if $pagination}
        <ul class="nav nav-tabs">
            <li class="tab-inline-pagination">
                <table border="0" cellpadding="0" cellspacing="0" width="100%">
                    <tbody>
                        <tr>
                            <td nowrap class="paginationWrapper">
                                <span class="pagination">
                                    <button type="button" class="button btn-pagination" title="{$APP.LBL_SEARCH_PREV}"{if !$pagination.prev}disabled="true"{else}onclick="pagination.onClick('prev');"{/if}>
                                        <span class="suitepicon suitepicon-action-left"> </span><span class="pagination-label">{$APP.LBL_SEARCH_PREV}</span>
                                    </button>
                                    &nbsp;&nbsp;
                                    <span class="pagination-range-label">{$APP.LBL_SEARCH_PAGE}{$pagination.page}{$APP.LBL_SEARCH_OF}{$pagination.last}</span>
                                    &nbsp;&nbsp;
                                    <button type="button" class="button btn-pagination" title="{$APP.LBL_SEARCH_NEXT}"{if !$pagination.next}disabled="true"{else}onclick="pagination.onClick('next');"{/if}>
                                        <span class="pagination-label">{$APP.LBL_SEARCH_NEXT}</span><span class="suitepicon suitepicon-action-right"> </span>
                                    </button>
                                </span>
                            </td>
                        </tr>
                    </tbody>
                </table>
            </li>
        </ul>
        <script>
            {literal}
            var pagination = {
                onClick: function(dir) {
                    var from = {/literal}{$pagination.from}{literal};
                    var size = {/literal}{$pagination.size}{literal};
                    var string = "{/literal}{$pagination.string}{literal}";
                    if (dir === 'prev') {
                        from -= size;
                    } else if (dir === 'next') {
                        from += size;
                    } else {
                        throw 'Invalid direction';
                    }
                    // keep search form values
                    $('input[name="search-query-from"]').val(from);
                    $('select[name="search-query-size"]').val(size);
                    $('input[name="search-query-string"').val(string);
                    $('#search-wrapper-form').submit();
                }
            };
            {/literal}
        </script>
    {/if}

    {foreach from=$resultsAsBean item=beans key=module}
    <h3>{$module}</h3>
    <table class="list view">
        <thead>
            <tr>
                <th></th>
                {foreach from=$headers[$module] item=header}
                <th title="{$header.comment}">{$header.label}</th>
                {/foreach}
            </tr>
        </thead>
        <tbody>
            {foreach from=$beans item=bean}
            <tr class="{cycle values="oddListRowS1,evenListRowS1"}">
                <td><a href="{$APP_CONFIG.site_url}/index.php?action=EditView&module={$module}&record={$bean->id}&offset=1"><span class="suitepicon suitepicon-action-edit"></span></a></td>
                {foreach from=$headers[$module] item=header}
                {assign var="headerField" value=$header.field|default:''}
                <td>{$bean->$headerField}
                </td>
                {/foreach}
            </tr>
            {/foreach}
        </tbody>
        </thead>
    </table>
    {foreachelse}
    <p class="error">{$APP.ERR_SEARCH_NO_RESULTS}</p>
    {/foreach}

    {if !empty($results->getSearchTime())}
        <p class="text-muted text-right" id="search-time">
            {$APP.LBL_SEARCH_PERFORMED_IN} {$results->getSearchTime()*1000|string_format:"%.2f"} ms
        </p>
    {/if}
{/if}
