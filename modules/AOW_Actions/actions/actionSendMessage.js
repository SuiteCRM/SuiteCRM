/**
* This file is part of SinergiaCRM.
* SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
* Copyright (C) 2013 - 2023 SinergiaTIC Association
*
* This program is free software; you can redistribute it and/or modify it under
* the terms of the GNU Affero General Public License version 3 as published by the
* Free Software Foundation.
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
* You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
*/


var currentln;
var phoneln = new Array();

function show_edit_template_link(field, ln) {
    var field1 = document.getElementById('aow_actions_edit_template_link' + ln);

    if (field.selectedIndex == 0) {
        field1.style.visibility = "hidden";
    } else {
        field1.style.visibility = "visible";
    }
}

function filterTemplatesByType(lineNum) {
    var typeSelect = document.getElementById('aow_actions_param_type_' + lineNum);
    var templateSelect = document.getElementById('aow_actions_param_email_template' + lineNum);
    
    if (!typeSelect || !templateSelect) {
        return;
    }
    
    var selectedType = typeSelect.value;
    
    // Get template arrays from hidden fields
    var smsTemplates = {};
    var whatsappTemplates = {};
    
    try {
        var smsField = document.getElementById('aow_sms_templates');
        var waField = document.getElementById('aow_whatsapp_templates');
        
        if (smsField && smsField.value) smsTemplates = JSON.parse(smsField.value);
        if (waField && waField.value) whatsappTemplates = JSON.parse(waField.value);
    } catch(e) {
        return;
    }
    
    // Determine which templates to show based on type (only 2 generic types: SMS and WhatsApp)
    var templatesToShow = {};
    var typeLower = selectedType.toLowerCase();
    
    if (typeLower.indexOf('whatsapp') !== -1) {
        templatesToShow = whatsappTemplates;
    } else {
        templatesToShow = smsTemplates;
    }
    
    // Store current selection before clearing
    var currentValue = templateSelect.value;
    
    // Clear all options
    templateSelect.innerHTML = '';
    
    // Add "None" option first
    var noneOpt = document.createElement('option');
    noneOpt.value = 'None';
    var noneLabelField = document.getElementById('aow_none_label');
    noneOpt.text = noneLabelField ? noneLabelField.value : '--None--';
    templateSelect.add(noneOpt);
    
    // Add filtered options (skip empty keys from PHP templates)
    for (var id in templatesToShow) {
        if (templatesToShow.hasOwnProperty(id) && id !== '' && templatesToShow[id] !== '') {
            var opt = document.createElement('option');
            opt.value = id;
            opt.text = templatesToShow[id];
            templateSelect.add(opt);
        }
    }
    
    // Try to restore previous selection if still valid
    if (currentValue && templatesToShow.hasOwnProperty(currentValue)) {
        templateSelect.value = currentValue;
    }
    
    // Update edit link visibility
    show_edit_template_link(templateSelect, lineNum);

    // Auto-populate sender with workflow assigned user when type is WhatsApp
    var senderField = document.getElementById('aow_actions_param[' + lineNum + '][sender_name]');
    var assignedUser = document.getElementById('assigned_user_name');
    if (senderField && assignedUser && typeLower.indexOf('whatsapp') !== -1) {
        senderField.value = assignedUser.value;
    }
}

function initTemplateFilter(lineNum) {
    var typeSelect = document.getElementById('aow_actions_param_type_' + lineNum);
    
    if (typeSelect) {
        typeSelect.addEventListener('change', function() {
            filterTemplatesByType(lineNum);
        });
        // Run on page load with delay to ensure hidden fields are rendered
        setTimeout(function() {
            filterTemplatesByType(lineNum);
        }, 500);
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

function show_PhoneField(ln, cln, value){
    if (typeof value === 'undefined') { value = ''; }

    flow_module = document.getElementById('flow_module').value;
    var aow_phonetype = document.getElementById('aow_actions_param'+ln+'_phone_target_type'+cln).value;
    if(aow_phonetype != ''){
        var callback = {
            success: function(result) {
                document.getElementById('phoneLine'+ln+'_field'+cln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
            },
            failure: function(result) {
                document.getElementById('phoneLine'+ln+'_field'+cln).innerHTML = '';
            }
        }

        var aow_field_name = "aow_actions_param["+ln+"][phone]["+cln+"]";

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=stic_Messages&action=getPhoneField&aow_module="+flow_module+"&aow_newfieldname="+aow_field_name+"&aow_type="+aow_phonetype+"&aow_value="+encodeURIComponent(value),callback);
    }
    else {
        document.getElementById('phoneLine'+ln+'_field'+cln).innerHTML = '';
    }
}

function load_phoneline(ln, type, value){
    cln = add_phoneLine(ln);
    document.getElementById("aow_actions_param"+ln+"_phone_target_type"+cln).value = type;
    show_PhoneField(ln, cln, value);
}

function add_phoneLine(ln){
    var aow_phone_type_list = document.getElementById("aow_message_type_list").value;

    if(phoneln[ln] == null){phoneln[ln] = 0}

    tablebody = document.createElement("tbody");
    tablebody.id = 'phoneLine'+ln+'_body' + phoneln[ln];
    document.getElementById('phoneLine'+ln+'_table').appendChild(tablebody);

    var x = tablebody.insertRow(-1);
    x.id = 'phoneLine'+ln+'_line' + phoneln[ln];

    var a = x.insertCell(0);
    a.innerHTML = "<button type='button' id='phoneLine"+ln+"_delete" + phoneln[ln]+"' class='button' value='Remove Line' tabindex='116' onclick='clear_phoneLine(" + ln + ",this);'><span class='suitepicon suitepicon-action-minus'></span></button> ";

    a.innerHTML += "<select tabindex='116' name='aow_actions_param["+ln+"][phone_target_type]["+phoneln[ln]+"]' id='aow_actions_param"+ln+"_phone_target_type"+phoneln[ln]+"' onchange='show_PhoneField(" + ln + "," + phoneln[ln] + ");'>" + aow_phone_type_list + "</select> ";

    a.innerHTML += "<span id='phoneLine"+ln+"_field"+phoneln[ln]+"'><input id='aow_actions_param["+ln+"][phone]["+phoneln[ln]+"]' type='text' tabindex='116' size='25' name='aow_actions_param["+ln+"][phone]["+phoneln[ln]+"]'></span>";


    phoneln[ln]++;

    return phoneln[ln] -1;

}

function clear_phoneLine(ln, cln){

    document.getElementById('phoneLine'+ln+'_table').deleteRow(cln.parentNode.parentNode.rowIndex);
}

function clear_phoneLines(ln){

    var phone_rows = document.getElementById('phoneLine'+ln+'_table').getElementsByTagName('tr');
    var phone_row_length = phone_rows.length;
    var i;
    for (i=0; i < phone_row_length; i++) {
        document.getElementById('phoneLine'+ln+'_table').deleteRow(phone_rows[i]);
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
