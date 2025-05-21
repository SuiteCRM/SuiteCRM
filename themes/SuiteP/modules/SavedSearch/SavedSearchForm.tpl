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
<div>
    <div class="col-xs-12 col-sm-6 col-md-4 col-lg-4 col-advanced-search">
        <div{if $orderBySelectOnly} style="display:none;"{/if}>
            <input id='displayColumnsDef' type='hidden' name='displayColumns'>
            <input id='hideTabsDef' type='hidden' name='hideTabs'>
            {$columnChooser}
            <br>
        </div>
        <div class="col-xs-12 saved-search-sort-column-config-row">
            <label>{sugar_translate label='LBL_ORDER_BY_COLUMNS' module='SavedSearch'}</label>

        </div>
        <div class="form-item saved-search-sort-column-config-row">
            <select name='orderBy' id='orderBySelect'>
            </select>
        </div>
    </div>
    <div class="col-xs-12 col-sm-6 col-md-4 col-lg-4 col-advanced-search">
        <div class="col-xs-12 saved-search-sort-column-config-row">
            <label>{sugar_translate label='LBL_DIRECTION' module='SavedSearch'}</label>
        </div>
        <div class="form-item radios saved-search-sort-column-config-row">
            <div><input id='sort_order_desc_radio' type='radio' name='sortOrder' value='DESC'
                        {if $selectedSortOrder == 'DESC'}checked{/if}>&nbsp;<span
                        onclick='document.getElementById("sort_order_desc_radio").checked = true'
                        style="cursor: pointer; cursor: hand">{$MOD.LBL_DESCENDING}</span></div>

            <div><input id='sort_order_asc_radio' type='radio' name='sortOrder' value='ASC'
                        {if $selectedSortOrder == 'ASC'}checked{/if}>&nbsp;<span
                        onclick='document.getElementById("sort_order_asc_radio").checked = true'
                        style="cursor: pointer; cursor: hand">{$MOD.LBL_ASCENDING}</span>
            </div>
        </div>
    </div>

</div>
<script>
    SUGAR.savedViews.columnsMeta = {$columnsMeta};
    columnsMeta = {$columnsMeta};
    saved_search_select = "{$SAVED_SEARCH_SELECT}";
    selectedSortOrder = "{$selectedSortOrder|default:'DESC'}";
    selectedOrderBy = "{$selectedOrderBy}";


    {literal}
    //this populates the label that shows the name of the current saved view
    //The label is located under the update/delete buttons
    function fillInLabels() {
        //this javascript runs and populates values in savedSearchForm.tpl
        x = document.getElementById('saved_search_select');
        if ((typeof(x) != 'undefined' && x != null) && x.selectedIndex != 0) {
            curr_search_name = document.getElementById('curr_search_name');
            curr_search_name.innerHTML = '';
            curr_search_name.appendChild(document.createTextNode('"' + x.options[x.selectedIndex].text + '"'));
            document.getElementById('ss_update').disabled = false;
            document.getElementById('ss_delete').disabled = false;
            $('.hideUnusedSavedSearchElements').show();
        } else {
            document.getElementById('ss_update').disabled = true;
            document.getElementById('ss_delete').disabled = true;
            document.getElementById('curr_search_name').innerHTML = '';
            $('.hideUnusedSavedSearchElements').hide();
        }
    }
    //call scripts that need to get run onload of this form.  This function is called when image
    //to collapse/show subpanels is loaded
    function loadSSL_Scripts() {
        //this will fill in the name of the current module, and enable/disable update/delete buttons
        fillInLabels();
        //this populates the order by dropdown, and activates the chooser widget.
        SUGAR.savedViews.handleForm();
    }

    {/literal}
</script>
