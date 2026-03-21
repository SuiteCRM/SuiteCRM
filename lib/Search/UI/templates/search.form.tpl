{*
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

<div class="moduleTitle">
    <h2 class="module-title-text">{$APP.LBL_SEARCH_TITLE}</h2>
</div>
<div style="clear:both;">
    <form id="search-wrapper-form">
        {*hidden input to handle actions*}
        {search_controller}

        <table>
            <tbody>
            <tr style="padding-bottom: 10px">
                <td class="submitButtons" colspan="8" nowrap="">
                    <label for="searchFieldMain" class="text-muted hide">{$APP.LBL_SEARCH_QUERY}</label>
                    <input id="searchFieldMain" title="{$APP.LBL_SEARCH_TEXT_FIELD_TITLE_ATTR}" class="searchField"
                           type="text" size="80" name="search-query-string" value="{$searchQueryString}" autofocus>
                    <input type="submit" onclick="searchForm.onSubmitClick(this);"
                           title="{$APP.LBL_SEARCH_SUBMIT_FIELD_TITLE_ATTR}" class="button primary"
                           value="{$APP.LBL_SEARCH_SUBMIT_FIELD_VALUE}">&nbsp;
                </td>
            <tr>
            <tr style="padding-top: 10px;">
                <td colspan="6" style="padding-left: 20px;" nowrap="">
                    <div id="inlineGlobalSearch">
                        <table style="margin-bottom:0;">
                            <tbody>
                            <label for="search-query-size"
                                   class="text-muted">{$APP.LBL_SEARCH_RESULTS_PER_PAGE}</label>
                            {html_options options=$sizeOptions selected=$searchQuerySize id="search-query-size" name="search-query-size"}
                            &nbsp;&nbsp;
                            <label for="search-query-modules"
                                   class="text-muted">{$APP.LBL_SEARCH_MODULES}:</label>
                            {html_options options=$moduleOptions selected=$searchQueryModules id="search-query-modules" name="search-query-modules[]" multiple="1"}

                            <input type="hidden" name="search-query-from" value="{$searchQueryFrom}">
                            &nbsp;&nbsp;
                            {if $engineOptions|@count gt 1}
                                <label for="search-query-size" class="text-muted">{$APP.LBL_SEARCH_ENGINE}</label>
                                {html_options options=$engineOptions selected=$searchQueryEngine id="search-engine" name="search-engine"}
                            {else}
                                {assign var=firstRow value=$engineOptions|@key}
                                <input type="hidden" name="search-engine" value="{$firstRow}" />
                            {/if}
                            </tbody>
                        </table>
                    </div>
                </td>
            </tr>
            </tbody>
        </table>
    </form>
    <script>
        {literal}
        var searchForm = {
          onSubmitClick: function (e) {
            // jump to the first page on new results list
            $('input[name="search-query-from"]').val(0);
            return true;
          }
        };
        {/literal}
    </script>
</div>
