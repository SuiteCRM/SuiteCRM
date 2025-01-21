{*
/**
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 * 
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo, "Supercharged by SuiteCRM" logo and “Nonprofitized by SinergiaCRM” logo. 
 * If the display of the logos is not reasonably feasible for technical reasons, 
 * the Appropriate Legal Notices must display the words "Powered by SugarCRM", 
 * "Supercharged by SuiteCRM" and “Nonprofitized by SinergiaCRM”. 
 */

*}
<!-- Include SmartMenus core CSS -->
<link href="SticInclude/vendor/smartmenus/dist/css/sm-core-css.css" rel="stylesheet">
<!-- Include SticCRM custom SmartMenus theme -->
<link href="SticInclude/vendor/smartmenus/dist/css/sm-stic/sm-stic.css" rel="stylesheet">

<!-- Main toolbar container for desktop view -->
<div class="desktop-toolbar" id="stic-toolbar">

    {$renderedMenu}
    {literal}
        <script>
            $(document).ready(function() {
                // Remove empty dropdown menus
                $('#main-menu li.dropdown').each(function() {
                    // Check if the inner ul is empty (has no li elements)
                    if ($(this).find('ul').children().length === 0) {
                        // Remove the li.dropdown if its ul is empty
                        $(this).remove();
                    }
                });

                // Implement live search functionality for the "All" menu
                $("#search-all").on("keyup", function() {
                    var searchTerm = $(this).val().toLocaleLowerCase();
                    var normalizedSearchTerm = searchTerm.normalize("NFD").replace(/[\u0300-\u036f]/g, "");
                
                    $("#search-all").parent().siblings().each(function() {
                        var text = $(this).text().toLocaleLowerCase();
                        var normalizedText = text.normalize("NFD").replace(/[\u0300-\u036f]/g, "");
                        var matchFound = normalizedText.indexOf(normalizedSearchTerm) > -1;
                        $(this).toggle(matchFound);
                    });
                });
            });
        </script>
    {/literal}
</div>