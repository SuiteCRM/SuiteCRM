/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2011 - 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

var currentln;
var emailln = new Array();

function show_edit_template_link(field, ln) {
    var field1 = document.getElementById('aow_actions_edit_template_link' + ln);

    if (field.selectedIndex == 0) {
        field1.style.visibility = "hidden";
    } else {
        field1.style.visibility = "visible";
    }
}

function refresh_email_template_list(template_id, template_name) {
    refresh_template_list(template_id, template_name,currentln);
}

function refresh_template_list(template_id, template_name, ln) {
    var field = document.getElementById('aow_actions_param_email_template' + ln);
    var bfound = 0;
    for (var i = 0; i < field.options.length; i++) {
        if (field.options[i].value == template_id) {
            if (field.options[i].selected == false) {
                field.options[i].selected = true;
            }
            field.options[i].text = template_name;
            bfound = 1;
        }
    }
    //add item to selection list.
    if (bfound == 0) {
        var newElement = document.createElement('option');
        newElement.text = template_name;
        newElement.value = template_id;
        field.options.add(newElement);
        newElement.selected = true;
    }

    //enable the edit button.
    var field1 = document.getElementById('aow_actions_edit_template_link' + ln);
    field1.style.visibility = "visible";
}

function open_email_template_form(ln) {
    currentln = ln;
    URL = "index.php?module=EmailTemplates&action=EditView&inboundEmail=1&return_module=AOW_WorkFlow&base_module=AOW_WorkFlow";
    URL += "&show_js=1";

    windowName = 'email_template';
    windowFeatures = 'width=800' + ',height=600' + ',resizable=1,semailollbars=1';

    win = window.open(URL, windowName, windowFeatures);
    if (window.focus) {
        // put the focus on the popup if the browser supports the focus() method
        win.focus();
    }
}

function edit_email_template_form(ln) {
    currentln = ln;
    var field = document.getElementById('aow_actions_param_email_template' + ln);
    URL = "index.php?module=EmailTemplates&action=EditView&inboundEmail=1&return_module=AOW_WorkFlow&base_module=AOW_WorkFlow";
    if (field.options[field.selectedIndex].value != 'undefined') {
        URL += "&record=" + field.options[field.selectedIndex].value;
    }
    URL += "&show_js=1";

    windowName = 'email_template';
    windowFeatures = 'width=800' + ',height=600' + ',resizable=1,semailollbars=1';

    win = window.open(URL, windowName, windowFeatures);
    if (window.focus) {
        // put the focus on the popup if the browser supports the focus() method
        win.focus();
    }
}

function show_emailField(ln, cln, value){
    if (typeof value === 'undefined') { value = ''; }

    flow_module = document.getElementById('flow_module').value;
    var aow_emailtype = document.getElementById('aow_actions_param'+ln+'_email_target_type'+cln).value;
    if(aow_emailtype != ''){
        var callback = {
            success: function(result) {
                document.getElementById('emailLine'+ln+'_field'+cln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
            },
            failure: function(result) {
                document.getElementById('emailLine'+ln+'_field'+cln).innerHTML = '';
            }
        }

        var aow_field_name = "aow_actions_param["+ln+"][email]["+cln+"]";

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getEmailField&aow_module="+flow_module+"&aow_newfieldname="+aow_field_name+"&aow_type="+aow_emailtype+"&aow_value="+value,callback);
    }
    else {
        document.getElementById('emailLine'+ln+'_field'+cln).innerHTML = '';
    }
}

function load_emailline(ln, to, type, value){
    cln = add_emailLine(ln);
    document.getElementById("aow_actions_param"+ln+"_email_to_type"+cln).value = to;
    document.getElementById("aow_actions_param"+ln+"_email_target_type"+cln).value = type;
    show_emailField(ln, cln, value);
}

function add_emailLine(ln){

    var aow_email_type_list = document.getElementById("aow_email_type_list").value;
    var aow_email_to_list = document.getElementById("aow_email_to_list").value;

    if(emailln[ln] == null){emailln[ln] = 0}

    tablebody = document.createElement("tbody");
    tablebody.id = 'emailLine'+ln+'_body' + emailln[ln];
    document.getElementById('emailLine'+ln+'_table').appendChild(tablebody);

    var x = tablebody.insertRow(-1);
    x.id = 'emailLine'+ln+'_line' + emailln[ln];

    var a = x.insertCell(0);
    a.innerHTML = "<button type='button' id='emailLine"+ln+"_delete" + emailln[ln]+"' class='button' value='Remove Line' tabindex='116' onclick='clear_emailLine(" + ln + ",this);'><span class='suitepicon suitepicon-action-minus'></span></button> ";

    a.innerHTML += "<select tabindex='116' name='aow_actions_param["+ln+"][email_to_type]["+emailln[ln]+"]' id='aow_actions_param"+ln+"_email_to_type"+emailln[ln]+"'>" + aow_email_to_list + "</select> ";

    a.innerHTML += "<select tabindex='116' name='aow_actions_param["+ln+"][email_target_type]["+emailln[ln]+"]' id='aow_actions_param"+ln+"_email_target_type"+emailln[ln]+"' onchange='show_emailField(" + ln + "," + emailln[ln] + ");'>" + aow_email_type_list + "</select> ";

    a.innerHTML += "<span id='emailLine"+ln+"_field"+emailln[ln]+"'><input id='aow_actions_param["+ln+"][email]["+emailln[ln]+"]' type='text' tabindex='116' size='25' name='aow_actions_param["+ln+"][email]["+emailln[ln]+"]'></span>";


    emailln[ln]++;

    return emailln[ln] -1;

}

function clear_emailLine(ln, cln){

    document.getElementById('emailLine'+ln+'_table').deleteRow(cln.parentNode.parentNode.rowIndex);
}

function clear_emailLines(ln){

    var email_rows = document.getElementById('emailLine'+ln+'_table').getElementsByTagName('tr');
    var email_row_length = email_rows.length;
    var i;
    for (i=0; i < email_row_length; i++) {
        document.getElementById('emailLine'+ln+'_table').deleteRow(email_rows[i]);
    }
}

function hideElem(id){
    if(document.getElementById(id)){
        document.getElementById(id).style.display = "none";
        document.getElementById(id).value = "";
    }
}

function showElem(id){
    if(document.getElementById(id)){
        document.getElementById(id).style.display = "";
    }
}

function targetTypeChanged(ln){
    var elem = document.getElementById("aow_actions_param_email_target_type"+ln);
    if(elem.value === 'Email Address'){
        showElem("aow_actions_param_email"+ln);
        hideElem("aow_actions_param_email_target"+ln);
        hideElem("aow_actions_email_user_span"+ln);
    }else if(elem.value === 'Specify User'){
        hideElem("aow_actions_param_email"+ln);
        hideElem("aow_actions_param_email_target"+ln);
        showElem("aow_actions_email_user_span"+ln);
    }else if(elem.value === 'Related Field'){
        hideElem("aow_actions_param_email"+ln);
        showElem("aow_actions_param_email_target"+ln);
        hideElem("aow_actions_email_user_span"+ln);
    }else if(elem.value === 'Record Email'){
        hideElem("aow_actions_param_email"+ln);
        hideElem("aow_actions_param_email_target"+ln);
        hideElem("aow_actions_email_user_span"+ln);
    }
}
