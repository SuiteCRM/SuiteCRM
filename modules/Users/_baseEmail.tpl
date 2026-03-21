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
<link rel="stylesheet" type="text/css" href="{sugar_getjspath file='modules/Emails/EmailUI.css'}"/>
{include file="modules/Emails/templates/_baseJsVars.tpl"}
<script type="text/javascript" src='{sugar_getjspath file='vendor/tinymce/tinymce/tinymce.min.js'}'></script>
<script type="text/javascript" src='{sugar_getjspath file='cache/include/javascript/sugar_grp_emails.js'}'></script>
<script type="text/javascript"
        src='{sugar_getjspath file='cache/include/javascript/sugar_grp_yui_widgets.js'}'></script>
<script type="text/javascript" src="include/javascript/jsclass_base.js"></script>
<script type="text/javascript" src="include/javascript/jsclass_async.js"></script>

<script type="text/javascript" language="Javascript">

    {include file="modules/Emails/templates/_baseConfigData.tpl"}

    var calFormat = '{$calFormat}';
    var theme = "{$theme}";

    {$quickSearchForAssignedUser}

    SUGAR.email2.detailView.qcmodules = {$qcModules};


    var isAdmin = {$is_admin};
    var loadingSprite = app_strings.LBL_EMAIL_LOADING + " <img src='include/javascript/yui/build/assets/skins/sam/wait.gif' alt=$mod_strings.LBL_WAIT height='14' align='absmiddle'>";
</script>
<div class="email">
    <form id="emailUIForm" name="emailUIForm">
        <input type="hidden" id="module" name="module" value="Emails">
        <input type="hidden" id="action" name="action" value="EmailUIAjax">
        <input type="hidden" id="to_pdf" name="to_pdf" value="true">
        <input type="hidden" id="emailUIAction" name="emailUIAction">
        <input type="hidden" id="mbox" name="mbox">
        <input type="hidden" id="uid" name="uid">
        <input type="hidden" id="ieId" name="ieId">
        <input type="hidden" id="forceRefresh" name="forceRefresh">
        <input type="hidden" id="focusFolder" name="focusFolder">
        <input type="hidden" id="focusFolderOpen" name="focusFolderOpen">
        <input type="hidden" id="sortBy" name="sortBy">
        <input type="hidden" id="reverse" name="reverse">
    </form>
    <div id="overDiv" style="position:absolute; visibility:hidden; z-index:1000;"></div>

    <table cellpadding="0" cellspacing="0" border="0" width="100%" style="display: none;">
        <tr>
            <td NOWRAP style="padding-bottom: 2px;">
                <button class="button" id="checkEmailButton" onclick="SUGAR.email2.folders.startEmailAccountCheck();">
                    <img src="themes/default/images/icon_email_check.gif" alt=$mod_strings.LBL_CHECKEMAIL
                         align="absmiddle" border="0"> {$app_strings.LBL_EMAIL_CHECK}</button>
                <button class="button" id="composeButton" onclick="SUGAR.email2.composeLayout.c0_composeNewEmail();">
                    <img src="themes/default/images/icon_email_compose.gif" alt=$mod_strings.LBL_COMPOSEEMAIL
                         align="absmiddle" border="0"> {$mod_strings.LNK_NEW_SEND_EMAIL}</button>
                {if $ID}
                    <button class="button" id="settingsButton"
                            onclick="SUGAR.email2.settings.showSettings(getUserEditViewUserId());"><img
                                src="themes/default/images/icon_email_settings.gif" alt=$mod_strings.LBL_EMAILSETTINGS
                                align="absmiddle" border="0"> {$app_strings.LBL_EMAIL_SETTINGS}</button>
                {/if}
            </td>
            <td NOWRAP align="right" style="padding-bottom: 2px;">
                <a href="index.php?module=Administration&action=SupportPortal&view=documentation&version={$sugar_version}&edition={$sugar_flavor}&lang={$current_language}&help_module=Emails&help_action=index&key={$server_unique_key}"
                   width='13' height='13' alt='{$app_strings.LNK_HELP}' border='0' align='absmiddle'
                   target="_blank"></a>
                &nbsp;
                <a href="index.php?module=Administration&action=SupportPortal&view=documentation&version={$sugar_version}&edition={$sugar_flavor}&lang={$current_language}&help_module=Emails&help_action=index&key={$server_unique_key}"
                   class='utilsLink' target="_blank">{$app_strings.LNK_HELP}</a>
            </td>
        </tr>
    </table>


    {include file="modules/Emails/templates/overlay.tpl"}

    <div id="emailContextMenu"></div>
    <div id="folderContextMenu"></div>
    <div id="container" class="email" style="position:relative; height:550px; overflow:hidden; display: none;"></div>
    <div id="innerLayout" class="yui-hidden"></div>
    <div id="listViewLayout" class="yui-hidden"></div>
    <div id="settingsDialog"></div>

    <!-- Hidden Content -->
    <div class="yui-hidden">
        <div id="searchTab" style="padding:5px">
            {include file="modules/Emails/templates/advancedSearch.tpl"}
        </div>
        <div id="settings">
            {include file="modules/Emails/templates/emailSettings.tpl"}
        </div>

        <div id="footerLinks" class="yui-hidden"></div>
    </div>


    <div id="editContact" class="yui-hidden"></div>
    <div id="editContactTab" class="yui-hidden"></div>
    <div id="editMailingList" class="yui-hidden"></div>
    <div id="editMailingListTab" class="yui-hidden"></div>


    <!-- for detailView quickCreate() calls -->
    <div id="quickCreateForEmail"></div>
    <div id="quickCreateContent"></div>


    <div id="importDialog"></div>
    <div id="importDialogContent"></div>


    <div id="relateDialog"></div>
    <div id="relateDialogContent"></div>


    <div id="assignmentDialog"></div>
    <div id="assignmentDialogContent"></div>


    <div id="emailDetailDialog"></div>
    <div id="emailDetailDialogContent"></div>


    <!-- for detailView views -->
    <div id="viewDialog"></div>
    <div id="viewDialogContent"></div>

    <!-- addressBook select -->
    {include file="modules/Emails/templates/addressSearchContent.tpl"}

    <!-- accounts outbound server dialogue -->
    <div id="outboundDialog" class="yui-hidden">
        {include file="modules/Emails/templates/outboundDialog.tpl"}
    </div>

    <!-- accounts edit dialogue -->
    <div id="editAccountDialogue" class="yui-hidden">
        {include file="modules/Emails/templates/editAccountDialogue.tpl"}
    </div>

    <div id="testOutboundDialog" class="yui-hidden">
        {include file="modules/Emails/templates/outboundDialogTest.tpl"}
    </div>

    <div id="assignToDiv" class="yui-hidden">
        {include file="modules/Emails/templates/assignTo.tpl"}
    </div>


    <script type="text/javascript" language="Javascript">
      enableQS(true);
    </script>

</div>