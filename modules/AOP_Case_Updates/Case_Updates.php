<?php
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

/**
 * @param $focus
 *
 * @return string
 */
function display_updates($focus)
{
    if (empty($focus->id)) {
        return '';
    }

    global $mod_strings;

    $hideImage = SugarThemeRegistry::current()->getImageURL('basic_search.gif');
    $showImage = SugarThemeRegistry::current()->getImageURL('advanced_search.gif');

    //Javascript for Asynchronous update
    $html = <<<A
<script>
var hideUpdateImage = '$hideImage';
var showUpdateImage = '$showImage';
function collapseAllUpdates(){
    $('.caseUpdateImage').attr("src",showUpdateImage);
    $('.caseUpdate').slideUp('fast');
}
function expandAllUpdates(){
    $('.caseUpdateImage').attr("src",hideUpdateImage);
    $('.caseUpdate').slideDown('fast');
}
function toggleCaseUpdate(updateId){
    var id = 'caseUpdate'+updateId;
    var updateElem = $('#'+id);
    var imageElem = $('#'+id+"Image");

    if(updateElem.is(":visible")){
        imageElem.attr("src",showUpdateImage);
    }else{
        imageElem.attr("src",hideUpdateImage);
    }
    updateElem.slideToggle('fast');
}
function caseUpdates(record){
    loadingMessgPanl = new YAHOO.widget.SimpleDialog('loading', {
                    width: '200px',
                    close: true,
                    modal: true,
                    visible: true,
                    fixedcenter: true,
                    constraintoviewport: true,
                    draggable: false
                });
    loadingMessgPanl.setHeader(SUGAR.language.get('app_strings', 'LBL_EMAIL_PERFORMING_TASK'));
    loadingMessgPanl.setBody(SUGAR.language.get('app_strings', 'LBL_EMAIL_ONE_MOMENT'));
    loadingMessgPanl.render(document.body);
    loadingMessgPanl.show();

    var update_data = encodeURIComponent(document.getElementById('update_text').value);
    var checkbox = document.getElementById('internal').checked;
    var internal = "";
    if(checkbox){
        internal=1;
    }

    //Post parameters

    var params =
        "record="+record+"&module=Cases&return_module=Cases&action=Save&return_id="+record+"&return_action=DetailView&relate_to=Cases&relate_id="+record+"&offset=1&update_text="
        + update_data + "&internal=" + internal;

    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", "index.php", true);


    xmlhttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlhttp.setRequestHeader("Content-length", params.length);
    xmlhttp.setRequestHeader("Connection", "close");

    //When button is clicked
    xmlhttp.onreadystatechange = function() {

        if(xmlhttp.readyState == 4 && xmlhttp.status == 200) {


            showSubPanel('history', null, true);
            //Reload the case updates stream and history panels
		    $("#aop_case_updates_threaded_span").load("index.php?module=Cases&action=DetailView&record="+record + " #aop_case_updates_threaded_span", function(){


            //Collapse all except newest update
            $('.caseUpdateImage').attr("src",showUpdateImage);
            $('.caseUpdate').slideUp('fast');

            var id = $('.caseUpdate').last().attr('id');
            if(id){
            toggleCaseUpdate(id.replace('caseUpdate',''));
            }


            loadingMessgPanl.hide();

            }

        );
	}
}

        xmlhttp.send(params);



}
</script>
A;

    $updates = $focus->get_linked_beans('aop_case_updates', 'AOP_Case_Updates');
    if (!$updates || is_null($focus->id)) {
        $html .= quick_edit_case_updates($focus);

        return $html;
    }

    $html .= <<<EOD
<script>
$(document).ready(function(){
    collapseAllUpdates();
    var id = $('.caseUpdate').last().attr('id');
    if(id){
        toggleCaseUpdate(id.replace('caseUpdate',''));
    }
});
</script>
<a href='' onclick='collapseAllUpdates(); return false;'>{$mod_strings['LBL_CASE_UPDATES_COLLAPSE_ALL']}</a>
<a href='' onclick='expandAllUpdates(); return false;'>{$mod_strings['LBL_CASE_UPDATES_EXPAND_ALL']}</a>
<div>
EOD;

    usort(
        $updates,
        function ($a, $b) {
            $aDate = $a->fetched_row['date_entered'];
            $bDate = $b->fetched_row['date_entered'];
            if ($aDate < $bDate) {
                return -1;
            } elseif ($aDate > $bDate) {
                return 1;
            }

            return 0;
        }
    );

    foreach ($updates as $update) {
        $html .= display_single_update($update, $hideImage);
    }
    $html .= '</div>';
    $html .= quick_edit_case_updates($focus);

    return $html;
}

/**
 * @return mixed|string|void
 */
function display_update_form()
{
    global $mod_strings, $app_strings;
    $sugar_smarty = new Sugar_Smarty();
    $sugar_smarty->assign('MOD', $mod_strings);
    $sugar_smarty->assign('APP', $app_strings);

    return $sugar_smarty->fetch('modules/AOP_Case_Updates/tpl/caseUpdateForm.tpl');
}

/**
 * @param SugarBean $update
 *
 * @return string - html to be displayed
 */
function getUpdateDisplayHead(SugarBean $update)
{
    global $mod_strings;
    if ($update->contact_id) {
        $name = $update->getUpdateContact()->name;
    } elseif ($update->assigned_user_id) {
        $name = $update->getUpdateUser()->name;
    } else {
        $name = $mod_strings['LBL_UNKNOWN_CONTACT'];
    }
    $html = "<a href='' onclick='toggleCaseUpdate(\"" . $update->id . "\");return false;'>";
    $html .= "<img  id='caseUpdate" .
             $update->id .
             "Image' class='caseUpdateImage' src='" .
             SugarThemeRegistry::current()->getImageURL('basic_search.gif') .
             "'>";
    $html .= '</a>';
    $html .= '<span>' .
             ($update->internal ? '<strong>' . $mod_strings['LBL_INTERNAL'] . '</strong> ' : '') .
             $name .
             ' ' .
             $update->date_entered .
             '</span><br>';
    $notes = $update->get_linked_beans('notes', 'Notes');
    if ($notes) {
        $html .= $mod_strings['LBL_AOP_CASE_ATTACHMENTS'];
        foreach ($notes as $note) {
            $html .= "<a href='index.php?module=Notes&action=DetailView&record={$note->id}'>{$note->filename}</a>&nbsp;";
        }
    }

    return $html;
}

/**
 * Gets a single update and returns it.
 *
 * @param AOP_Case_Updates $update
 *
 * @return string - the html for the update
 */
function display_single_update(AOP_Case_Updates $update)
{

    /*if assigned user*/
    if ($update->assigned_user_id) {
        /*if internal update*/
        if ($update->internal) {
            $html = "<div id='caseStyleInternal'>" . getUpdateDisplayHead($update);
            $html .= "<div id='caseUpdate" . $update->id . "' class='caseUpdate'>";
            $html .= nl2br(html_entity_decode(purify_html((string) $update->description, ['HTML.ForbiddenElements' => ['iframe' => true]])));
            $html .= '</div></div>';

            return $html;
        } /*if standard update*/ else {
            $html = "<div id='lessmargin'><div id='caseStyleUser'>" . getUpdateDisplayHead($update);
            $html .= "<div id='caseUpdate" . $update->id . "' class='caseUpdate'>";
            $html .= nl2br(html_entity_decode(purify_html((string) $update->description, ['HTML.ForbiddenElements' => ['iframe' => true]])));
            $html .= '</div></div></div>';

            return $html;
        }
    }

    /*if contact user*/
    $html = "<div id='extramargin'><div id='caseStyleContact'>" . getUpdateDisplayHead($update);
    $html .= "<div id='caseUpdate" . $update->id . "' class='caseUpdate'>";
    $html .= html_entity_decode(purify_html((string) $update->description, ['HTML.ForbiddenElements' => ['iframe' => true]]));
    $html .= '</div></div></div>';

    return $html;
}

/**
 * Displays case attachments.
 *
 * @param $case
 *
 * @return string - html link
 */
function display_case_attachments($case)
{
    $html = '';
    if(empty($case->id)){
        return '';
    }
    $notes = $case->get_linked_beans('notes', 'Notes');
    if ($notes) {
        foreach ($notes as $note) {
            $html .= "<a href='index.php?module=Notes&action=DetailView&record={$note->id}'>{$note->filename}</a>&nbsp;";
        }
    }

    return $html;
}

/**
 * The Quick edit for case updates which appears under update stream
 * Also includes the javascript for AJAX update.
 *
 * @param $case
 *
 * @return string - the html to be displayed and javascript
 */
function quick_edit_case_updates($case)
{
    global $action;
    global $app_strings;
    global $currentModule;
    global $current_language;
    $mod_strings = return_module_language($current_language, 'Cases');
    #
    //on DetailView only
    if ($action !== 'DetailView') {
        return;
    }

    //current record id
    $record = $_GET['record'];

    //Get Users roles
    require_once 'modules/ACLRoles/ACLRole.php';
    $user = $GLOBALS['current_user'];
    $id = $user->id;
    $acl = BeanFactory::newBean('ACLRoles');
    $roles = $acl->getUserRoles($id);

    //Return if user cannot edit cases
    if ($roles === 'no edit cases' || in_array('no edit cases', $roles)) {
        return '';
    }
    $internalChecked = '';
    if (isset($case->internal) && $case->internal) {
        $internalChecked = "checked='checked'";
    }
    $saveBtn = $app_strings['LBL_SAVE_BUTTON_LABEL'];
    $saveTitle = $app_strings['LBL_SAVE_BUTTON_TITLE'];

    $html = <<< EOD
    <form id='case_updates' enctype="multipart/form-data">

    <div><label for="update_text">{$mod_strings['LBL_UPDATE_TEXT']}</label></div>
    <textarea id="update_text" name="update_text" cols="80" rows="4"></textarea>

    <div><label>{$mod_strings['LBL_INTERNAL']}</label>
    <input id='internal' type='checkbox' name='internal' tabindex=0 title='' value='1' $internalChecked ></input>
    </div>
    <input type='button' value='$saveBtn' onclick="caseUpdates('$record')" title="$saveTitle" name="button"> </input>


    </br>
    </form>


EOD;

    return $html;
}
