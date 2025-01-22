/*
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

/* global SUGAR */

$("#es-test-connection").click(function () {
    var url = "index.php?module=Administration&action=ElasticSearchSettings&do=TestConnection";
    var host = $("#es-host").val();
    var user = $("#es-user").val();
    var pass = $("#es-password").val();

    $.ajax({
        url: url,
        method: "POST",
        data: {
            host: host,
            user: user,
            pass: pass
        }
    }).done(function (data) {
        if (data.status === "success") {
            alert(
                SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_TEST_CONNECTION_SUCCESS")
                + "\n\nPing: " + data.ping / 1000 + " ms\nElasticsearch v" + data.info.version.number
            );
        } else {
            alert(
                SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_TEST_CONNECTION_FAIL")
                + "\n\n" + data.error + "."
            );
        }
    }).error(function () {
        alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_TEST_CONNECTION_ERROR"));
    });
});

$("#es-full-index").click(function () {
    var url = "index.php?module=Administration&action=ElasticSearchSettings&do=FullIndex";

    $.ajax(url).done(function (data) {
        if (data.status === "success") {
            alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_INDEX_SCHEDULE_FULL_SUCCESS"));
        }
        else {
            alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_INDEX_SCHEDULE_FULL_FAIL_NO_SUCCESS"));
        }
    }).error(function () {
        alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_INDEX_SCHEDULE_FULL_FAIL"));
    });
});

$("#es-partial-index").click(function () {
    var url = "index.php?module=Administration&action=ElasticSearchSettings&do=PartialIndex";

    $.ajax(url).done(function (data) {
        if (data.status === "success") {
            alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_INDEX_SCHEDULE_PART_SUCCESS"));
        }
        else {
            alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_INDEX_SCHEDULE_PART_FAIL_NO_SUCCESS"));
        }
    }).error(function () {
        alert(SUGAR.language.get("Administration", "LBL_ELASTIC_SEARCH_INDEX_SCHEDULE_PART_FAIL"));
    });
});
