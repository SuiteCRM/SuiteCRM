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

{if !$columnsFilterDialogAdded}
    <div id="columnsFilterDialog" class="modal fade modal-columns-filter" tabindex="-1" role="dialog">
        <div class="modal-dialog modal-lg">
            <div class="modal-content">
                <div class="modal-header">
                    <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span
                                aria-hidden="true">&times;</span></button>
                    <h4 class="modal-title">{$APP.LBL_COLUMNS_FILTER_HEADER_TITLE}</h4>
                </div>
                <div class="modal-body" id="columnsFilterList">

                </div>
                <div class="modal-footer">
                    <input title="{$APP.LBL_CLOSE_BUTTON_TITLE}" type="button" class="button"
                           data-dismiss="modal" value="{$APP.LBL_CLOSE_BUTTON_TITLE}">
                    <input title="{$APP.LBL_SAVE_CHANGES_BUTTON_TITLE}" type="submit" onclick="columnsFilter.onSaveClick();"
                           class="button primary" value="{$APP.LBL_SAVE_CHANGES_BUTTON_TITLE}">
                </div>
            </div><!-- /.modal-content -->
        </div><!-- /.modal-dialog -->
    </div>
    <!-- /.modal -->
    <div id="chooserTemplate" class="hidden">
        <div class="chooserContent">
            <h1>{$APP.LBL_DISPLAYED}</h1>
            <ul class="chooserList green"></ul>
            <div id="error-displayed-columns" class="error"></div>
            <h1>{$APP.LBL_HIDDEN}</h1>
            <ul class="chooserList red"></ul>
        </div>
    </div>
{literal}
    <script>

        // TODO add it to sListView
        if (typeof columnsFilter == 'undefined') {

            var columnsFilter = {

                onOpen: function () {
                    this.loadColumnsSettings();
                },

                showContents: function (contents) {
                    $('#columnsFilterList').html(contents);
                },

                showPreload: function () {
                    this.showContents('<p class="preloading"></p>');
                },

                loadColumnsSettings: function () {
                    var _this = this;
                    this.showPreload();

                    if (typeof module_sugar_grp1 != 'undefined' && module_sugar_grp1) {

                        var url = 'index.php?module=' + module_sugar_grp1 + '&action=index&search_form_only=true&to_pdf=true&search_form_view=advanced_search&columnsFilter=true';

                        var cObj = YAHOO.util.Connect.asyncRequest('GET', url, {
                            success: function (e) {
                                _this.showContents(_this.getDragDropChooserHTML(JSON.parse($('<div></div>').html(e.responseText).find('#responseData').html())));
                                _this.initDragDropChooser();
                            }, failure: function () {
                                _this.showContents('ERR_COMMUNICATION_ERROR');
                            }
                        });
                    }
                    else {
                        _this.showContents('ERR_NO_MODULE_SELECTED');
                    }

                },

                getDragDropChooserHTML: function (chooserData) {

                    $('#chooserTemplate .chooserList.green').html('');
                    $.each(chooserData.args.values_array[0], function (key, value) {
                        $('#chooserTemplate .chooserList.green').append('<li data-key="' + key + '">' + value + '</li>');
                    });

                    $('#chooserTemplate .chooserList.red').html('');
                    $.each(chooserData.args.values_array[1], function (key, value) {
                        $('#chooserTemplate .chooserList.red').append('<li data-key="' + key + '">' + value + '</li>');
                    });

                    return $('#chooserTemplate').html();
                },

                initDragDropChooser: function () {
                    var _this = this;
                    $("#columnsFilterList .chooserContent .chooserList.green").sortable({
                        connectWith: "#columnsFilterList .chooserContent .chooserList.red",
                        stop: function () {
                            _this.isValid();
                        }
                    });
                    $("#columnsFilterList .chooserContent .chooserList.green").disableSelection();

                    $("#columnsFilterList .chooserContent .chooserList.red").sortable({
                        connectWith: "#columnsFilterList .chooserContent .chooserList.green",
                        stop: function () {
                            _this.isValid();
                        }
                    });
                    $("#columnsFilterList .chooserContent .chooserList.red").disableSelection();

                },

                onSaveClick: function () {
                    if (this.isValid()) {
                        this.save();
                    }
                },

                // validation (return true if valid, otherwise show error(s) and return false)
                isValid: function () {
                    // clear error message
                    $('#error-displayed-columns').html('');
                    // check validation for empty list
                    var v = $('#columnsFilterList > div > ul.chooserList.green.ui-sortable li').length > 0;
                    if (!v) {
                        // show error
                        $('#error-displayed-columns').html('{/literal}{$APP.ERR_EMPTY_COLUMNS_LIST}{literal}');
                        // scroll to error message
                        $('#columnsFilterDialog').animate({
                            scrollTop: $("#error-displayed-columns").offset().top - 100
                        });
                    }
                    // return validation result
                    return v;
                },

                // send it to server to save user preferences (refresh the page to show changes)
                save: function () {

                    // TODO : show loading message...

                    // make query columns list
                    var cols = [];
                    $('#columnsFilterList > div > ul.chooserList.green.ui-sortable > li').each(function (i, e) {
                        cols.push($(e).attr('data-key'));
                    });

                    $.post($('#search_form').attr('action'), {
                        displayColumns: cols.join('|'),
                        query: 'true',
                        use_stored_query: 'true',
                        update_stored_query: 'true',
                        update_stored_query_key: 'displayColumns',
                        last_search_tab: listViewSearchIcon.getLatestSearchDialogType(),
                        save_columns_order: 'true'
                    }, function () {
                        //close form and refresh page after save
                        $('#columnsFilterDialog > div > div > div.modal-footer > button.btn.button.purple.btn-default').click();
                        if ($('#search_form').length > 0) {
                            document.location.href = $('#search_form').attr('action');
                        } else {
                            if (typeof module_sugar_grp1 != 'undefined' && module_sugar_grp1) {
                                document.location.href = 'index.php?module=' + module_sugar_grp1 + '&action=index';
                            } else {
                                document.location.href = document.location.href;
                            }
                        }
                    });

                }

            };

        }

    </script>
{/literal}

    {assign var="columnsFilterDialogAdded" value="true"}
{/if}
