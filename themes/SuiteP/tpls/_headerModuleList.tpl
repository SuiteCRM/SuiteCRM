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
<!--Start Responsive Top Navigation Menu -->
<nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
    <div class="container-fluid">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="dropdown">
                <span class="sr-only">Toggle navigation</span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
                <span class="icon-bar"></span>
            </button>
            <ul class="dropdown-menu mobile_menu" role="menu" id="mobile_menu">
                {foreach from=$groupTabs item=modules key=group name=groupList}
                    {if $smarty.foreach.groupList.last}
                        {capture name=extraparams assign=extraparams}parentTab={$group}{/capture}
                        {foreach from=$modules.modules item=module key=modulekey}
                            <li role="presentation" data-test="1">
                                {capture name=moduleTabId assign=moduleTabId}moduleTab_{$smarty.foreach.moduleList.index}_{$module}{/capture}
                                <a href="javascript:void(0)" onclick="window.location.href = '{sugar_link id=$moduleTabId module=$modulekey link_only=1 data=$module extraparams=$extraparams}'">
                                    {$module}
                                    {if $modulekey !='Home' && $modulekey !='Calendar'}
                                        <span class="glyphicon glyphicon-plus"  onclick="window.location.href = 'index.php?action=EditView&module={$modulekey}'"></span>
                                        {*<span class="glyphicon glyphicon-plus"  onclick="window.location.href = 'http://google.com'"></span>*}
                                    {/if}
                                </a>
                            </li>
                        {/foreach}
                        {foreach from=$modules.extra item=submodulename key=submodule}
                            <li role="presentation" data-test="2">
                                <a href="javascript:void(0)" onclick="window.location.href = '{sugar_link module=$submodule link_only=1 extraparams=$extraparams}'">
                                    {$submodulename}
                                    <span class="glyphicon glyphicon-plus"  onclick="window.location.href = 'index.php?action=EditView&module={$submodule}'"></span>
                                    {*<span class="glyphicon glyphicon-plus"  onclick="window.location.href = 'http://google.com'"></span>*}
                                </a>
                            </li>
                        {/foreach}
                    {/if}
                {/foreach}
            </ul>
            <div id="mobileheader" class="mobileheader">
                <div id="modulelinks" class="modulelinks">
                    {foreach from=$moduleTopMenu item=module key=name name=moduleList}
                        {if $name == $MODULE_TAB}
                            <span class="modulename" data-toggle="dropdown" aria-expanded="false">
                                {sugar_link id="moduleTab_$name" module=$name data=$module caret=true}
                            </span>
                                <ul class="dropdown-menu" role="menu">
                                {if $name !='Home'}
                                    {if is_array($shortcutTopMenu.$name) && count($shortcutTopMenu.$name) > 0}
                                        <li class="mobile-current-actions" role="presentation">
                                           <ul class="mobileCurrentTab">
                                               {foreach from=$shortcutTopMenu.$name item=item}
                                                   {if $item.URL == "-"}
                                                       <li class="mobile-action"><a></a><span>&nbsp;</span></li>
                                                   {else}
                                                       <li class="mobile-action"><a href="{$item.URL}">{$item.LABEL}</a></li>
                                                   {/if}
                                               {/foreach}
                                           </ul>
                                        </li>
                                    {else}
                                        <li class="mobile-action"><a>{$APP.LBL_NO_SHORTCUT_MENU}</a></li>
                                    {/if}
                            {/if}

                                    {if is_array($recentRecords) && count($recentRecords) > 0}
                                        <li class="recent-links-title" role="presentation">
                                            <a><strong>{$APP.LBL_LAST_VIEWED}</strong></a>
                                        </li>
                                        <li role="presentation">
                                            <ul class="recent-list">
                                                {foreach from=$recentRecords item=item name=lastViewed }
                                                    {if $smarty.foreach.lastViewed.iteration < 4} {* limit to 3 results *}
                                                        <li class="recentlinks" role="presentation">
                                                            <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                                               accessKey="{$smarty.foreach.lastViewed.iteration}"
                                                               href="{sugar_link module=$item.module_name action='DetailView' record=$item.item_id link_only=1}" class="recent-links-detail">
                                                                <span class="suitepicon suitepicon-module-{$item.module_name|lower|replace:'_':'-'}"></span>
                                                                <span aria-hidden="true">{$item.item_summary_short}</span>
                                                            </a>
                                                            {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.item_id}{/capture}
                                                            {if $access}
                                                                 <a href="{sugar_link module=$item.module_name action='EditView' record=$item.item_id link_only=1}" class="recent-links-edit"><span class=" glyphicon glyphicon-pencil"></a>
                                                            {/if}
                                                        </li>
                                                    {/if}
                                                {/foreach}
                                            </ul>
                                         </li>
                                    {/if}

                                    {if is_array($favoriteRecords) && count($favoriteRecords) > 0}
                                        <li class="favorite-links-title" role="presentation">
                                            <a><strong>{$APP.LBL_FAVORITES}</strong></a>
                                        </li>
                                        <li>
                                            <ul class="favorite-list">
                                                {foreach from=$favoriteRecords item=item name=lastViewed}
                                                    {if $smarty.foreach.lastViewed.iteration < 4} {* limit to 3 results *}
                                                        <li class="favoritelinks" role="presentation">
                                                            <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                                               accessKey="{$smarty.foreach.lastViewed.iteration}"
                                                               href="{sugar_link module=$item.module_name action='DetailView' record=$item.id link_only=1}"  class="favorite-links-detail">
                                                                <span class="suitepicon suitepicon-module-{$item.module_name|lower|replace:'_':'-'}"></span>
                                                                <span aria-hidden="true">{$item.item_summary_short}</span>
                                                            </a>
                                                            {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.id}{/capture}
                                                            {if $access}
                                                                <a href="{sugar_link module=$item.module_name action='EditView' record=$item.id link_only=1}" class="favorite-links-edit"><span class="glyphicon glyphicon-pencil" aria-hidden="true"></a>
                                                            {/if}
                                                        </li>
                                                    {/if}
                                                {/foreach}
                                            </ul>
                                        </li>
                                    {/if}
                                </ul>

                        {/if}
                    {/foreach}
                </div>
            </div>
        </div>
        <div class="desktop-toolbar" id="toolbar">
            {if $USE_GROUP_TABS}
                <ul class="nav navbar-nav">
                    <li class="navbar-brand-container">
                            <a class="navbar-brand with-home-icon suitepicon suitepicon-action-home" href="index.php?module=Home&action=index"></a>
                    </li>
                    {assign var="groupSelected" value=false}
                    {foreach from=$moduleTopMenu item=module key=name name=moduleList}
                        {if $name == $MODULE_TAB}
                            {if $name != 'Home'}
                                <li class="topnav">
                                    <span class="currentTabLeft">&nbsp;</span>
                                    <span class="currentTab">{sugar_link id="moduleTab_$name" module=$name data=$module}</span>
                                    <span>&nbsp;</span>
                                    {* check, is there any recent items *}
                                    {assign var=foundRecents value=false}
                                    {foreach from=$recentRecords item=item name=lastViewed}
                                        {if $item.module_name == $name}
                                            {assign var=foundRecents value=true}
                                        {/if}
                                    {/foreach}


                                    {* check, is there any favorite items *}
                                    {assign var=foundFavorits value=false}
                                    {foreach from=$favoriteRecords item=item name=lastViewed}
                                        {if $item.module_name == $name}
                                            {assign var=foundFavorits value=true}
                                        {/if}
                                    {/foreach}
                                    {if $foundRecents || $foundFavorits
                                        || (is_array($shortcutTopMenu.$name) && count($shortcutTopMenu.$name) > 0)}

                                        <ul class="dropdown-menu" role="menu">
                                            <li class="current-module-action-links">
                                                <ul>
                                                    {if is_array($shortcutTopMenu.$name)
                                                        && count($shortcutTopMenu.$name) > 0}
                                                        {foreach from=$shortcutTopMenu.$name item=item}
                                                            {if $item.URL == "-"}
                                                                {*<li><a></a><span>&nbsp;</span></li>*}
                                                            {else}
                                                                <li><a href="{$item.URL}"><span class="topnav-fake-icon">{* fakes the space the icon takes *}</span><span aria-hidden="true">{$item.LABEL}</span></a></li>
                                                            {/if}
                                                        {/foreach}
                                                    {/if}
                                                </ul>
                                            </li>

                                            {* when records are found for the current submodule show recent header *}
                                            {counter start=0 name="submoduleRecentRecordsTotal" assign="submoduleRecentRecordsTotal"  print=false}
                                            {foreach from=$recentRecords item=item name=lastViewed}
                                                {if $item.module_name == $name and $submoduleRecentRecordsTotal == 0}
                                                    <li class="recent-links-title"><a><strong>{$APP.LBL_LAST_VIEWED}</strong></a></li>
                                                    {counter name="submoduleRecentRecordsTotal" print=false}
                                                {/if}
                                            {/foreach}
                                                <li class="current-module-recent-links">
                                                    <ul>
                                                        {* when records are found for the current submodule show the first 3 records *}
                                                        {counter start=0 name="submoduleRecentRecords" assign="submoduleRecentRecords"  print=false}
                                                        {foreach from=$recentRecords item=item name=lastViewed}
                                                            {if $item.module_name == $name and $submoduleRecentRecords < 3}
                                                                <li class="recentlinks" role="presentation">
                                                                    <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                                                       accessKey="{$smarty.foreach.lastViewed.iteration}"
                                                                       href="{sugar_link module=$item.module_name action='DetailView' record=$item.item_id link_only=1}" class="recent-links-detail">

                                                                        <span aria-hidden="true">{$item.item_summary_short}</span>
                                                                    </a>
                                                                    {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.item_id }{/capture}
                                                                    {if $access}
                                                                        <a href="{sugar_link module=$item.module_name action='EditView' record=$item.item_id link_only=1}" class="recent-links-edit"><span class=" glyphicon glyphicon-pencil"></a>
                                                                    {/if}
                                                                </li>
                                                                {counter name="submoduleRecentRecords" print=false}
                                                            {/if}
                                                        {/foreach}
                                                    </ul>
                                                </li>





                                            {counter start=0 name="submoduleFavoriteRecordsTotal" assign="submoduleFavoriteRecordsTotal"  print=false}
                                            {foreach from=$favoriteRecords item=item name=lastViewed}
                                                {if $item.module_name == $name and $submoduleFavoriteRecordsTotal == 0}
                                                    <li class="favorite-links-title"><a><strong>{$APP.LBL_FAVORITES}</strong></a></li>
                                                    {counter name="submoduleFavoriteRecordsTotal" print=false}
                                                {/if}
                                            {/foreach}
                                            <li class="current-module-favorite-links">
                                                <ul>
                                                    {* when records are found for the current submodule show the first 3 records *}
                                                    {counter start=0 name="submoduleFavoriteRecords" assign="submoduleFavoriteRecords" print=false}
                                                    {foreach from=$favoriteRecords item=item name=lastViewed}
                                                        {if $item.module_name == $name and $submoduleFavoriteRecords < 3}
                                                            <li class="favoritelinks" role="presentation">
                                                                <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                                                   accessKey="{$smarty.foreach.lastViewed.iteration}"
                                                                   href="{sugar_link module=$item.module_name action='DetailView' record=$item.id link_only=1}" class="favorite-links-detail">
                                                                    <span class="suitepicon suitepicon-module-{$item.module_name|lower|replace:'_':'-'}"></span>
                                                                    <span aria-hidden="true">{$item.item_summary_short}</span>
                                                                </a>
                                                                {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.id }{/capture}
                                                                {if $access}
                                                                    <a href="{sugar_link module=$item.module_name action='EditView' record=$item.id link_only=1}" class="favorite-links-edit"><span class=" glyphicon glyphicon-pencil" aria-hidden="true"></a>
                                                                {/if}
                                                            </li>
                                                            {counter name="submoduleFavoriteRecords" print=false}
                                                        {/if}
                                                    {/foreach}
                                                </ul>
                                            </li>
                                        </ul>

                                    {/if}
                                 </li>
                            {/if}

                        {/if}
                    {/foreach}
                    {foreach from=$groupTabs item=modules key=group name=groupList}
                        {capture name=extraparams assign=extraparams}parentTab={$group}{/capture}
                        <li class="topnav {if $smarty.foreach.groupList.last}all{/if}">
                            <span class="notCurrentTabLeft">&nbsp;</span><span class="notCurrentTab">
                            <a href="#" id="grouptab_{$smarty.foreach.groupList.index}" class="dropdown-toggle grouptab">{$group}</a>
                            <span class="notCurrentTabRight">&nbsp;</span>
                            <ul class="dropdown-menu" role="menu" {if $smarty.foreach.groupList.last} class="All"{/if}>
                                {foreach from=$modules.modules item=module key=modulekey}
                                    <li>
                                        {capture name=moduleTabId assign=moduleTabId}moduleTab_{$smarty.foreach.moduleList.index}_{$module}{/capture}
                                        {sugar_link id=$moduleTabId module=$modulekey data=$module extraparams=$extraparams}
                                    </li>
                                {/foreach}
                                {foreach from=$modules.extra item=submodulename key=submodule}
                                    <li>
                                        <a href="{sugar_link module=$submodule link_only=1 extraparams=$extraparams}">{$submodulename}</a>
                                    </li>
                                {/foreach}
                            </ul>
                        </li>
                    {/foreach}
                </ul>
                {* 7.8 Hide filter menu items when the window is too small to display them *}
            {literal}
                <script>
                  var windowResize = function() {
                    // Since the height can be changed in Sass.
                    // Take a measurement of the initial desktop navigation bar height with just one menu item
                    $('.desktop-toolbar ul.navbar-nav > li').not('.all').addClass('hidden');
                    var dth = $('.desktop-toolbar').outerHeight();

                    // Show all desktop menu items
                    $('.desktop-toolbar ul.navbar-nav > li.hidden').removeClass('hidden');

                    // Remove the each menu item from the end of the toolbar until
                    // the navigation bar is the matches the initial height.
                    while($('.desktop-toolbar').outerHeight() > dth) {
                      ti = $('.desktop-toolbar ul.navbar-nav > li').not('.hidden').not('.all');
                      $(ti).last().addClass('hidden');
                    }
                  };
                  $(window).resize(windowResize);
                  $(document).ready(windowResize);
                </script>
            {/literal}
            {else}

                <ul class="nav navbar-nav navbar-horizontal-fluid">
                    <li class="navbar-brand-container">
                        <a class="navbar-brand with-home-icon" href="index.php?module=Home&action=index">
                            <span class="suitepicon suitepicon-action-home"></span>
                        </a>
                    </li>
                    {foreach from=$groupTabs item=modules key=group name=groupList}
                        {capture name=extraparams assign=extraparams}parentTab={$group}{/capture}
                    {/foreach}

                    <!--nav items with actions -->
                    {foreach from=$modules.modules item=submodulename key=submodule}
                        {if $submodule != "Home"}
                            <li class="topnav with-actions">
                                <span class="notCurrentTabLeft">&nbsp;</span>
                                <span class="dropdown-toggle headerlinks notCurrentTab"> <a href="{sugar_link module=$submodule link_only=1 extraparams=$extraparams}">{$submodulename}</a> </span>
                                <span class="notCurrentTabRight">&nbsp;</span>
                                <ul class="dropdown-menu" role="menu">
                                    <li>
                                        <ul>
                                            {if is_array($shortcutTopMenu) && count($shortcutTopMenu) > 0}
                                                {foreach from=$shortcutTopMenu.$submodule item=item}
                                                    {if $item.URL == "-"}
                                                        {*<li><a></a><span>&nbsp;</span></li>*}
                                                    {else}
                                                        <li><a href="{$item.URL}"><span class="topnav-fake-icon">{* fakes the space the icon takes *}</span><span aria-hidden="true">{$item.LABEL}</span></a></li>
                                                    {/if}
                                                {/foreach}
                                            {/if}
                                        </ul>
                                    </li>
                                    {* when records are found for the current submodule show recent header *}
                                    {counter start=0 name="submoduleRecentRecordsTotal" assign="submoduleRecentRecordsTotal"  print=false}
                                    {foreach from=$recentRecords item=item name=lastViewed}
                                        {if $item.module_name == $submodule and $submoduleRecentRecordsTotal == 0}
                                            <li class="recent-links-title"><a><strong>{$APP.LBL_LAST_VIEWED}</strong></a></li>
                                            {counter name="submoduleRecentRecordsTotal" print=false}
                                        {/if}
                                    {/foreach}
                                    <li>
                                        <ul>
                                            {* when records are found for the current submodule show the first 3 records *}
                                            {counter start=0 name="submoduleRecentRecords" assign="submoduleRecentRecords"  print=false}
                                            {foreach from=$recentRecords item=item name=lastViewed}
                                                {if $item.module_name == $submodule and $submoduleRecentRecords < 3}
                                                    <li class="recentlinks" role="presentation">
                                                        <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                                           accessKey="{$smarty.foreach.lastViewed.iteration}"
                                                           href="{sugar_link module=$item.module_name action='DetailView' record=$item.item_id link_only=1}" class="recent-links-detail">
                                                            <span aria-hidden="true">{$item.item_summary_short}</span>
                                                        </a>
                                                        {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.item_id }{/capture}
                                                        {if $access}
                                                            <a href="{sugar_link module=$item.module_name action='EditView' record=$item.item_id link_only=1}" class="recent-links-edit"><span class=" glyphicon glyphicon-pencil"></a>
                                                        {/if}
                                                    </li>
                                                    {counter name="submoduleRecentRecords" print=false}
                                                {/if}
                                            {/foreach}
                                        </ul>
                                    </li>
                                    {* when records are found for the current submodule show favorites header *}
                                    {counter start=0 name="submoduleFavoriteRecordsTotal" assign="submoduleFavoriteRecordsTotal"  print=false}
                                    {foreach from=$favoriteRecords item=item name=lastViewed}
                                        {if $item.module_name == $submodule and $submoduleFavoriteRecordsTotal == 0}
                                            <li class="favorite-links-title"><a><strong>{$APP.LBL_FAVORITES}</strong></a></li>
                                            {counter name="submoduleFavoriteRecordsTotal" print=false}
                                        {/if}
                                    {/foreach}
                                    <li>
                                        <ul>
                                        {* when records are found for the current submodule show the first 3 records *}
                                        {counter start=0 name="submoduleFavoriteRecords" assign="submoduleFavoriteRecords" print=false}
                                        {foreach from=$favoriteRecords item=item name=lastViewed}
                                            {if $item.module_name == $submodule and $submoduleFavoriteRecords < 3}
                                                <li class="favoritelinks" role="presentation">
                                                    <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                                       accessKey="{$smarty.foreach.lastViewed.iteration}"
                                                       href="{sugar_link module=$item.module_name action='DetailView' record=$item.id link_only=1}" class="favorite-links-detail">
                                                        <span aria-hidden="true">{$item.item_summary_short}</span>
                                                    </a>
                                                    {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.id }{/capture}
                                                    {if $access}
                                                        <a href="{sugar_link module=$item.module_name action='EditView' record=$item.id link_only=1}" class="favorite-links-edit"><span class=" glyphicon glyphicon-pencil" aria-hidden="true"></a>
                                                    {/if}
                                                </li>
                                                {counter name="submoduleFavoriteRecords" print=false}
                                            {/if}
                                        {/foreach}
                                        </ul>
                                    </li>
                                </ul>
                            </li>
                        {/if}
                    {/foreach}
                    {if count($moduleExtraMenu) > 0}
                        <li class="topnav overflow-toggle-menu">
                            <span class="notCurrentTabLeft">&nbsp;</span>
                            <span class="dropdown-toggle headerlinks notCurrentTab"><a href="#">{$APP.LBL_MORE}</a></span>
                            <span class="notCurrentTabRight">&nbsp;</span>
                            <ul id="overflow-menu" class="dropdown-menu" role="menu">
                                <!--nav items without actions -->
                                {foreach from=$modules.extra item=submodulename key=submodule}
                                    <li class="topnav without-actions">
                                        <span class=" notCurrentTab"> <a href="{sugar_link module=$submodule link_only=1 extraparams=$extraparams}">{$submodulename}</a> </span>
                                    </li>
                                {/foreach}
                            </ul>
                        </li>
                    {/if}
                </ul>
                <div class="hidden hidden-actions"></div>
                {* Hide nav items when the window size is too small to display them *}
                {literal}
                    <script>
                        var windowResize = function() {
                            // reset navbar
                            var $navCollapsedItems = $('ul#overflow-menu > li.with-actions');
                            if(typeof $navCollapsedItems !== "undefined") {
                                $($navCollapsedItems).each(function() {
                                    $(this).addClass('topnav');
                                    $(this).insertBefore('.overflow-toggle-menu');
                                });
                            }



                            var $navItemMore = $('.navbar-horizontal-fluid > li.overflow-toggle-menu'),
                                    $navItems = $('.navbar-horizontal-fluid > li.with-actions'),
                                    navItemMoreWidth = navItemWidth = $navItemMore.width(),
                                    windowWidth = $(window).width() - ($(window).width()  * 0.55),
                                    navItemMoreLeft, offset, navOverflowWidth;

                            $navItems.each(function() {
                                navItemWidth += $(this).width();
                            });

                            // Remove nav items that are cause the right hand nav-bar items to wrap
                            while (navItemWidth > windowWidth) {
                                navItemWidth -= $navItems.last().width();
                                $navItems.last().removeClass('topnav');
                                $navItems.last().prependTo('#overflow-menu');
                                $navItems.splice(-1,1);
                            }
                            if(typeof $navItemMoreLeft !== "undefined") {
                                navItemMoreLeft = $('.navbar-horizontal-fluid .overflow-toggle-menu').offset().left;
                                navOverflowWidth = $('#overflow-menu').width();
                                offset = navItemMoreLeft + navItemMoreWidth - navOverflowWidth;
                            }
                        };
                        $(window).resize(windowResize);
                        windowResize();
                    </script>
                {/literal}

            {/if}
        </div>

        <!-- Right side of the main navigation -->
        <div class="mobile-bar">
            <ul id="toolbar" class="toolbar">
                <li id="quickcreatetop" class="create dropdown nav navbar-nav quickcreatetop">
                    <a class="dropdown-toggle" data-toggle="dropdown" aria-expanded="false">
                        {$APP.LBL_CREATE_BUTTON_LABEL}<span class="suitepicon suitepicon-action-caret"></span>
                    </a>
                    <ul class="dropdown-menu" role="menu">
                        <li><a href="index.php?module=Accounts&action=EditView&return_module=Accounts&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Accounts" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Contacts&action=EditView&return_module=Contacts&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Contacts" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Opportunities&action=EditView&return_module=Opportunities&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Opportunities" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Leads&action=EditView&return_module=Leads&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Leads" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Documents&action=EditView&return_module=Documents&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Documents" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Calls&action=EditView&return_module=Calls&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Calls" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Tasks&action=EditView&return_module=Tasks&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Tasks" label="LBL_MODULE_NAME"}</a></li>
                    </ul>
                </li>
                <li id="" class="dropdown nav navbar-nav navbar-search">
                    <button id="searchbutton" class="dropdown-toggle btn btn-default searchbutton suitepicon suitepicon-action-search" data-toggle="dropdown" aria-expanded="true">
                    </button>
                    <div class="dropdown-menu" role="menu" aria-labelledby="searchbutton">
                        <form id="searchformdropdown" class="searchformdropdown" name='UnifiedSearch' action='index.php'
                              onsubmit='return SUGAR.unifiedSearchAdvanced.checkUsaAdvanced()'>
                            {search_controller}
                            <input type="hidden" class="form-control" name="module" value="Home">
                            <input type="hidden" class="form-control" name="search_form" value="false">
                            <input type="hidden" class="form-control" name="advanced" value="false">
                            <div class="input-group">
                                <input type="text" class="form-control query_string" name="query_string" id="query_string"
                                       placeholder="{$APP.LBL_SEARCH_BUTTON_TITLE}..." value="{$SEARCH}"/>
                            <span class="input-group-btn">
                                <button type="submit" class="btn btn-default suitepicon suitepicon-action-search"></button>
                            </span>
                            </div>
                        </form>
                    </div>
                </li>
                <li id="desktop_notifications" class="dropdown nav navbar-nav desktop_notifications">
                    <button class="alertsButton btn dropdown-toggle suitepicon suitepicon-action-alerts" data-toggle="dropdown"
                            aria-expanded="false">
                        <span class="alert_count hidden">0</span>
                    </button>
                    <div id="alerts" class="dropdown-menu" role="menu">{$APP.LBL_EMAIL_ERROR_VIEW_RAW_SOURCE}</div>
                </li>
                <li>
                    <form id="searchform" class="navbar-form searchform" name='UnifiedSearch' action='index.php'
                          onsubmit='return SUGAR.unifiedSearchAdvanced.checkUsaAdvanced()'>
                        {search_controller}
                        <input type="hidden" class="form-control" name="module" value="Home">
                        <input type="hidden" class="form-control" name="search_form" value="false">
                        <input type="hidden" class="form-control" name="advanced" value="false">
                        <div class="input-group">
                            <input type="text" class="form-control query_string " name="query_string" id="query_string"
                                   placeholder="{$APP.LBL_SEARCH}..." value="{$SEARCH}"/>
                    <span class="input-group-btn">
                        <button type="submit" class="btn btn-default suitepicon suitepicon-action-search"></button>
                    </span>
                        </div>
                    </form>
                </li>
                <li id="globalLinks" class="dropdown nav navbar-nav globalLinks-mobile">

                    <button id="usermenucollapsed" class="dropdown-toggle btn btn-default usermenucollapsed" data-toggle="dropdown" aria-expanded="true">
                        <span class="suitepicon suitepicon-action-user-small"></span>
                    </button>
                    <ul class="dropdown-menu user-dropdown user-menu" role="menu" aria-labelledby="dropdownMenu2">
                        <li role="presentation">
                            <a href='index.php?module=Users&action=EditView&record={$CURRENT_USER_ID}'>
                                {$APP.LBL_PROFILE}
                            </a>
                        </li>
                        {foreach from=$GCLS item=GCL name=gcl key=gcl_key}
                            <li role="presentation">
                                <a id="{$gcl_key}_link"
                                   href="{$GCL.URL}"
                                   {if !empty($GCL.ONCLICK)} 
                                   onclick="{$GCL.ONCLICK}"
                                   {/if}
                                   {if !empty($GCL.TARGET)} 
                                   target="{$GCL.TARGET}"
                                   {/if}
                                   >{$GCL.LABEL}</a>
                            </li>
                        {/foreach}
                        <li role="presentation"><a role="menuitem" id="logout_link" href='{$LOGOUT_LINK}'
                                                   class='utilsLink'>{$LOGOUT_LABEL}</a></li>
                    </ul>
                </li>
            </ul>
        </div>
        <div class="tablet-bar">
            <ul id="toolbar" class="toolbar">
                <li id="quickcreatetop" class="create dropdown nav navbar-nav quickcreatetop">
                    <a class="dropdown-toggle" data-toggle="dropdown" aria-expanded="false">
                        {$APP.LBL_CREATE_BUTTON_LABEL}<span class="suitepicon suitepicon-action-caret"></span>
                    </a>
                    <ul class="dropdown-menu" role="menu">
                        <li><a href="index.php?module=Accounts&action=EditView&return_module=Accounts&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Accounts" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Contacts&action=EditView&return_module=Contacts&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Contacts" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Opportunities&action=EditView&return_module=Opportunities&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Opportunities" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Leads&action=EditView&return_module=Leads&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Leads" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Documents&action=EditView&return_module=Documents&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Documents" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Calls&action=EditView&return_module=Calls&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Calls" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Tasks&action=EditView&return_module=Tasks&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Tasks" label="LBL_MODULE_NAME"}</a></li>
                    </ul>
                </li>
                <li id="" class="dropdown nav navbar-nav navbar-search">
                    <button id="searchbutton" class="dropdown-toggle btn btn-default searchbutton suitepicon suitepicon-action-search" data-toggle="dropdown" aria-expanded="true">
                    </button>
                    <div class="dropdown-menu" role="menu" aria-labelledby="searchbutton">
                        <form id="searchformdropdown" class="searchformdropdown" name='UnifiedSearch' action='index.php'
                              onsubmit='return SUGAR.unifiedSearchAdvanced.checkUsaAdvanced()'>
                            {search_controller}
                            <input type="hidden" class="form-control" name="module" value="Home">
                            <input type="hidden" class="form-control" name="search_form" value="false">
                            <input type="hidden" class="form-control" name="advanced" value="false">
                            <div class="input-group">
                                <input type="text" class="form-control query_string" name="query_string" id="query_string"
                                       placeholder="{$APP.LBL_SEARCH}..." value="{$SEARCH}"/>
                            <span class="input-group-btn">
                                <button type="submit" class="btn btn-default suitepicon suitepicon-action-search"></button>
                            </span>
                            </div>
                        </form>
                    </div>
                </li>
                <li>
                    <form id="searchform" class="navbar-form searchform" name='UnifiedSearch' action='index.php'
                          onsubmit='return SUGAR.unifiedSearchAdvanced.checkUsaAdvanced()'>
                        {search_controller}
                        <input type="hidden" class="form-control" name="module" value="Home">
                        <input type="hidden" class="form-control" name="search_form" value="false">
                        <input type="hidden" class="form-control" name="advanced" value="false">
                        <div class="input-group">
                            <input type="text" class="form-control query_string" name="query_string" id="query_string"
                                   placeholder="{$APP.LBL_SEARCH}..." value="{$SEARCH}"/>
                    <span class="input-group-btn">
                        <button type="submit" class="btn btn-default suitepicon suitepicon-action-search"></button>
                    </span>
                        </div>
                    </form>
                </li>
                <li id="desktop_notifications" class="dropdown nav navbar-nav desktop_notifications">
                    <button class="alertsButton btn dropdown-toggle suitepicon suitepicon-action-alerts" data-toggle="dropdown"
                            aria-expanded="false">
                        <span class="alert_count hidden">0</span>
                    </button>
                    <div id="alerts" class="dropdown-menu" role="menu">{$APP.LBL_EMAIL_ERROR_VIEW_RAW_SOURCE}</div>
                </li>
                <li id="globalLinks" class="dropdown nav navbar-nav globalLinks-mobile">

                    <button id="usermenucollapsed" class="dropdown-toggle btn btn-default usermenucollapsed" data-toggle="dropdown"
                            aria-expanded="true">
                        <span class="suitepicon suitepicon-action-current-user"></span>
                    </button>
                    <ul class="dropdown-menu user-dropdown user-menu" role="menu" aria-labelledby="dropdownMenu2">
                        <li role="presentation">
                            <a href='index.php?module=Users&action=EditView&record={$CURRENT_USER_ID}'>
                                {$APP.LBL_PROFILE}
                            </a>
                        </li>
                        {foreach from=$GCLS item=GCL name=gcl key=gcl_key}
                            <li role="presentation">
                                <a id="{$gcl_key}_link"
                                   href="{$GCL.URL}"
                                   {if !empty($GCL.ONCLICK)} 
                                   onclick="{$GCL.ONCLICK}"
                                   {/if}
                                   {if !empty($GCL.TARGET)} 
                                   target="{$GCL.TARGET}"
                                   {/if}
                                   >{$GCL.LABEL}</a>
                            </li>
                        {/foreach}
                        <li role="presentation"><a role="menuitem" id="logout_link" href='{$LOGOUT_LINK}'
                                                   class='utilsLink'>{$LOGOUT_LABEL}</a></li>
                    </ul>
                </li>
            </ul>
        </div>
        <div class="desktop-bar">
            <ul id="toolbar" class="toolbar">
                <li id="quickcreatetop" class="create dropdown nav navbar-nav quickcreatetop">
                    <a class="dropdown-toggle" data-toggle="dropdown" aria-expanded="false">
                        {$APP.LBL_CREATE_BUTTON_LABEL}<span class="suitepicon suitepicon-action-caret"></span>
                    </a>
                    <ul class="dropdown-menu" role="menu">
                        <li><a href="index.php?module=Accounts&action=EditView&return_module=Accounts&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Accounts" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Contacts&action=EditView&return_module=Contacts&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Contacts" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Opportunities&action=EditView&return_module=Opportunities&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Opportunities" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Leads&action=EditView&return_module=Leads&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Leads" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Documents&action=EditView&return_module=Documents&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Documents" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Calls&action=EditView&return_module=Calls&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Calls" label="LBL_MODULE_NAME"}</a></li>
                        <li><a href="index.php?module=Tasks&action=EditView&return_module=Tasks&return_action=DetailView">{$APP.LBL_QUICK_CREATE}{sugar_translate module="Tasks" label="LBL_MODULE_NAME"}</a></li>
                    </ul>
                </li>
                <li id="" class="dropdown nav navbar-nav navbar-search">
                    <button id="searchbutton" class="dropdown-toggle btn btn-default searchbutton suitepicon suitepicon-action-search" data-toggle="dropdown" aria-expanded="true">
                    </button>
                    <div class="dropdown-menu" role="menu" aria-labelledby="searchbutton">
                        <form id="searchformdropdown" class="searchformdropdown" name='UnifiedSearch' action='index.php'
                              onsubmit='return SUGAR.unifiedSearchAdvanced.checkUsaAdvanced()'>
                            {search_controller}
                            <input type="hidden" class="form-control" name="module" value="Home">
                            <input type="hidden" class="form-control" name="search_form" value="false">
                            <input type="hidden" class="form-control" name="advanced" value="false">
                            <div class="input-group">
                                <input type="text" class="form-control query_string" name="query_string" id="query_string"
                                       placeholder="{$APP.LBL_SEARCH}..." value="{$SEARCH}"/>
                            <span class="input-group-btn">
                                <button type="submit" class="btn btn-default suitepicon suitepicon-action-search"></button>
                            </span>
                            </div>
                        </form>
                    </div>
                </li>
                <li>
                    <form id="searchform" class="navbar-form searchform" name='UnifiedSearch' action='index.php'
                          onsubmit='return SUGAR.unifiedSearchAdvanced.checkUsaAdvanced()'>
                        {search_controller}
                        <input type="hidden" class="form-control" name="module" value="Home">
                        <input type="hidden" class="form-control" name="search_form" value="false">
                        <input type="hidden" class="form-control" name="advanced" value="false">
                        <div class="input-group">
                            <input type="text" class="form-control query_string" name="query_string" id="query_string"
                                   placeholder="{$APP.LBL_SEARCH}..." value="{$SEARCH}"/>
                    <span class="input-group-btn">
                        <button type="submit" class="btn btn-default suitepicon suitepicon-action-search"></button>
                    </span>
                        </div>
                    </form>
                </li>
                <li id="desktop_notifications" class="dropdown nav navbar-nav desktop_notifications">
                    <button class="alertsButton btn dropdown-toggle suitepicon suitepicon-action-alerts" data-toggle="dropdown"
                            aria-expanded="false">
                        <span class="alert_count hidden">0</span>
                    </button>
                    <div id="alerts" class="dropdown-menu" role="menu">{$APP.LBL_EMAIL_ERROR_VIEW_RAW_SOURCE}</div>
                </li>
                <li id="globalLinks" class="dropdown nav navbar-nav globalLinks-desktop">
                    <button id="with-label" class="dropdown-toggle user-menu-button" title="{$CURRENT_USER}"data-toggle="dropdown" aria-expanded="true">
                        <span class="suitepicon suitepicon-action-current-user"></span>
                        <span class="globallabel-user">{$CURRENT_USER}</span>
                        <span class="suitepicon suitepicon-action-caret"></span>
                    </button>
                    <ul class="dropdown-menu user-dropdown user-menu" role="menu" aria-labelledby="with-label">
                        <li role="presentation">
                            <a href='index.php?module=Users&action=EditView&record={$CURRENT_USER_ID}'>
                                {$APP.LBL_PROFILE}
                            </a>
                        </li>
                        {foreach from=$GCLS item=GCL name=gcl key=gcl_key}
                            <li role="presentation">
                                <a id="{$gcl_key}_link"
                                   href="{$GCL.URL}"
                                   {if !empty($GCL.ONCLICK)} 
                                   onclick="{$GCL.ONCLICK}"
                                   {/if}
                                   {if !empty($GCL.TARGET)}
                                   target="{$GCL.TARGET}"
                                   {/if}
                                   >{$GCL.LABEL}</a>
                            </li>
                        {/foreach}
                        <li role="presentation"><a role="menuitem" id="logout_link" href='{$LOGOUT_LINK}'
                                                   class='utilsLink'>{$LOGOUT_LABEL}</a></li>
                    </ul>
                </li>
            </ul>

        </div>
</nav>
<!--End Responsive Top Navigation Menu -->
{if $THEME_CONFIG.display_sidebar}
    <!--Start Page Container and Responsive Sidebar -->
    <div id='sidebar_container' class="container-fluid sidebar_container">

        <a id="buttontoggle" class="buttontoggle"><span></span></a>

        <div {if isset($smarty.cookies.sidebartoggle) && $smarty.cookies.sidebartoggle == 'collapsed'}style="display:none"{/if}
             class="sidebar">

                <div id="actionMenuSidebar" class="actionMenuSidebar">
                    {foreach from=$moduleTopMenu item=module key=name name=moduleList}
                        {if $name == $MODULE_TAB}
                            <ul>
                                {if isset($shortcutTopMenu.$name) && is_array($shortcutTopMenu)
                                    && count($shortcutTopMenu.$name) > 0}
                                    <h2 class="recent_h3">{$APP.LBL_LINK_ACTIONS}</h2>
                                    {foreach from=$shortcutTopMenu.$name item=item}
                                        {if $item.URL == "-"}
                                            <li><a></a><span>&nbsp;</span></li>
                                        {else}
                                            <li class="actionmenulinks" role="presentation">
                                                <a href="{$item.URL}" data-action-name="{$item.MODULE_NAME}">
                                                    <div class="side-bar-action-icon">
                                                        <span class="suitepicon suitepicon-action-{$item.MODULE_NAME|lower|replace:'_':'-'}"></span>
                                                    </div>
                                                    <div class="actionmenulink">{$item.LABEL}</div>
                                                </a>
                                            </li>
                                        {/if}
                                    {/foreach}
                                {/if}
                            </ul>
                        {/if}
                    {/foreach}
                </div>

                <div id="recentlyViewedSidebar" class="recentlyViewedSidebar">
                    {if is_array($recentRecords) && count($recentRecords) > 0}
                    <h2 class="recent_h3">{$APP.LBL_LAST_VIEWED}</h2>
                    {/if}
                    <ul class="nav nav-pills nav-stacked">
                        {foreach from=$recentRecords item=item name=lastViewed}
                            {if $item.module_name != 'Emails' && $item.module_name != 'InboundEmail' && $item.module_name != 'EmailAddresses'}<!--Check to ensure that recently viewed emails or email addresses are not displayed in the recently viewed panel.-->
                                {if $smarty.foreach.lastViewed.index < 5}
                                    <div class="recently_viewed_link_container_sidebar">
                                        <li class="recentlinks" role="presentation">
                                            <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}"
                                               accessKey="{$smarty.foreach.lastViewed.iteration}"
                                               href="{sugar_link module=$item.module_name action='DetailView' record=$item.item_id link_only=1}"
                                               class="recent-links-detail">
                                                <span class="suitepicon suitepicon-module-{$item.module_name|lower|replace:'_':'-'}"></span>
                                                <span>{$item.item_summary_short}</span>
                                            </a>
                                            {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.item_id }{/capture}
                                            {if $access}
                                                <a href="{sugar_link module=$item.module_name action='EditView' record=$item.item_id link_only=1}" class="recent-links-edit"><span class="glyphicon glyphicon-pencil" aria-hidden="true"></span></a>
                                            {/if}
                                        </li>
                                    </div>
                                {/if}
                            {/if}
                        {/foreach}
                    </ul>
                </div>
                 <div id="favoritesSidebar" class="favoritesSidebar">
                {if is_array($favoriteRecords) && count($favoriteRecords) > 0}
                    <h2 class="recent_h3">{$APP.LBL_FAVORITES}</h2>
                {/if}
                    <ul class="nav nav-pills nav-stacked">
                        {foreach from=$favoriteRecords item=item name=lastViewed}
                            {if $smarty.foreach.lastViewed.index < 5}
                            <div class="recently_viewed_link_container_sidebar">
                                <li class="recentlinks" role="presentation">
                                    <a title="{sugar_translate module=$item.module_name label=LBL_MODULE_NAME}" accessKey="{$smarty.foreach.lastViewed.iteration}" href="{sugar_link module=$item.module_name action='DetailView' record=$item.id link_only=1}" class="favorite-links-detail">
                                        <span class="suitepicon suitepicon-module-{$item.module_name|lower|replace:'_':'-'}"></span>
                                        <span aria-hidden="true">{$item.item_summary_short}</span>
                                    </a>
                                    {capture assign='access'}{suite_check_access module=$item.module_name action='edit' record=$item.id }{/capture}
                                    {if $access}
                                        <a href="{sugar_link module=$item.module_name action='EditView' record=$item.id link_only=1}" class="favorite-links-edit"><span class="glyphicon glyphicon-pencil" aria-hidden="true"></span></a>
                                    {/if}
                                </li>
                            </div>
                            {/if}
                        {/foreach}
                    </ul>
                </div>
            </div>
        <!--</div>-->
    </div>
    <!--End Responsive Sidebar -->
{/if}
<!--Start Page content -->
