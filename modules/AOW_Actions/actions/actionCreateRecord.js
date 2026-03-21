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

var cr_fields = [];
var cr_relationships = [];
var cr_module = [];
var crln = [];
var crreln = [];

function show_crModuleFields(ln){

    clear_crLines(ln);
    cr_module[ln] = document.getElementById('aow_actions_param_record_type'+ln).value;

    if(cr_module[ln] != ''){

        var callback = {
            success: function(result) {

                cr_fields[ln] = result.responseText;

                //add_crLine(ln);
                document.getElementById('addcrline'+ln).style.display = '';

                //document.getElementById('aow_action_div'+ln).innerHTML ="<select tabindex='116' name='service_vat[]' id='service_vat'>" + cr_fields[ln] + "</select>";
            }
        };

        var callback2 = {
            success: function(result) {

                cr_relationships[ln] = result.responseText;

                //add_crLine(ln);
                document.getElementById('addcrrelline'+ln).style.display = '';

                //document.getElementById('aow_action_div'+ln).innerHTML ="<select tabindex='116' name='service_vat[]' id='service_vat'>" + cr_fields[ln] + "</select>";
            }
        };

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&view=EditView&action=getModuleFields&override[]=email1&override[]=email2&aow_module="+cr_module[ln],callback);
        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getModuleRelationships&aow_module="+cr_module[ln],callback2);

    }

}

function show_crModuleField(ln, cln, value, type_value){
    if (typeof value === 'undefined') { value = ''; }
    if (typeof type_value === 'undefined') { type_value = ''; }

    flow_module = document.getElementById('flow_module').value;
    var aow_field = document.getElementById('aow_actions_param'+ln+'_field'+cln).value;
    if(aow_field != ''){
        var callback = {
            success: function(result) {
                document.getElementById('crLine'+ln+'_fieldType'+cln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
                document.getElementById('crLine'+ln+'_fieldType'+cln).onchange = function(){show_crModuleFieldType(ln, cln);};
            },
            failure: function(result) {
                document.getElementById('crLine'+ln+'_fieldType'+cln).innerHTML = '';
            }
        };
        var callback2 = {
            success: function(result) {
                document.getElementById('crLine'+ln+'_field'+cln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
            },
            failure: function(result) {
                document.getElementById('crLine'+ln+'_field'+cln).innerHTML = '';
            }
        };

        var aow_field_name = "aow_actions_param["+ln+"][value]["+cln+"]";
        var aow_field_type_name = "aow_actions_param["+ln+"][value_type]["+cln+"]";

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getActionFieldTypeOptions&aow_module="+cr_module[ln]+"&aow_fieldname="+aow_field+"&aow_newfieldname="+aow_field_type_name+"&aow_value="+type_value,callback);
        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getModuleFieldTypeSet&aow_module="+cr_module[ln]+"&alt_module="+flow_module+"&aow_fieldname="+aow_field+"&aow_newfieldname="+aow_field_name+"&aow_value="+value+"&aow_type="+type_value,callback2);
    }
    else {
        document.getElementById('crLine'+ln+'_fieldType'+cln).innerHTML = '';
        document.getElementById('crLine'+ln+'_field'+cln).innerHTML = '';
    }
}

function show_crModuleFieldType(ln, cln, value){
    if (typeof value === 'undefined') { value = ''; }

    var callback = {
        success: function(result) {
            document.getElementById('crLine'+ln+'_field'+cln).innerHTML = result.responseText;
            SUGAR.util.evalScript(result.responseText);
            enableQS(false);
        },
        failure: function(result) {
            document.getElementById('crLine'+ln+'_field'+cln).innerHTML = '';
        }
    };

    flow_module = document.getElementById('flow_module').value;
    var aow_field = document.getElementById('aow_actions_param'+ln+'_field'+cln).value;
    var type_value = document.getElementById("aow_actions_param["+ln+"][value_type]["+cln+"]").value;
    var aow_field_name = "aow_actions_param["+ln+"][value]["+cln+"]";

    YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getModuleFieldTypeSet&aow_module="+cr_module[ln]+"&alt_module="+flow_module+"&aow_fieldname="+aow_field+"&aow_newfieldname="+aow_field_name+"&aow_value="+value+"&aow_type="+type_value,callback);

}

function load_crline(ln, field, value, value_type){
    document.getElementById('addcrline'+ln).style.display = '';
    document.getElementById('addcrrelline'+ln).style.display = '';
    cln = add_crLine(ln);
    document.getElementById("aow_actions_param"+ln+"_field"+cln).value = field;
    show_crModuleField(ln, cln, value, value_type);
}

function show_crRelField(ln, cln, value, type_value){
    if (typeof value === 'undefined') { value = ''; }
    if (typeof type_value === 'undefined') { type_value = ''; }

    flow_module = document.getElementById('flow_module').value;
    var aow_field = document.getElementById('aow_actions_param'+ln+'_rel'+cln).value;
    if(aow_field != ''){
        var callback = {
            success: function(result) {
                document.getElementById('crRelLine'+ln+'_fieldType'+cln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
                document.getElementById('crRelLine'+ln+'_fieldType'+cln).onchange = function(){show_crRelFieldType(ln, cln);};
            },
            failure: function(result) {
                document.getElementById('crRelLine'+ln+'_fieldType'+cln).innerHTML = '';
            }
        };
        var callback2 = {
            success: function(result) {
                document.getElementById('crRelLine'+ln+'_field'+cln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
            },
            failure: function(result) {
                document.getElementById('crRelLine'+ln+'_field'+cln).innerHTML = '';
            }
        };


        var aow_field_name = "aow_actions_param["+ln+"][rel_value]["+cln+"]";
        var aow_field_type_name = "aow_actions_param["+ln+"][rel_value_type]["+cln+"]";

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getActionFieldTypeOptions&aow_module="+cr_module[ln]+"&alt_module="+flow_module+"&aow_fieldname="+aow_field+"&aow_newfieldname="+aow_field_type_name+"&aow_value="+type_value,callback);
        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getRelFieldTypeSet&aow_module="+cr_module[ln]+"&alt_module="+flow_module+"&aow_fieldname="+aow_field+"&aow_newfieldname="+aow_field_name+"&aow_value="+value+"&aow_type="+type_value,callback2);
    }
    else {
        document.getElementById('crRelLine'+ln+'_fieldType'+cln).innerHTML = '';
        document.getElementById('crRelLine'+ln+'_field'+cln).innerHTML = '';
    }

}

function show_crRelFieldType(ln, cln, value){
    if (typeof value === 'undefined') { value = ''; }

    var callback = {
        success: function(result) {
            document.getElementById('crRelLine'+ln+'_field'+cln).innerHTML = result.responseText;
            SUGAR.util.evalScript(result.responseText);
            enableQS(false);
        },
        failure: function(result) {
            document.getElementById('crRelLine'+ln+'_field'+cln).innerHTML = '';
        }
    };

    flow_module = document.getElementById('flow_module').value;
    var aow_field = document.getElementById('aow_actions_param'+ln+'_rel'+cln).value;
    var type_value = document.getElementById("aow_actions_param["+ln+"][rel_value_type]["+cln+"]").value;
    var aow_field_name = "aow_actions_param["+ln+"][rel_value]["+cln+"]";

    YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getModuleFieldTypeSet&aow_module="+cr_module[ln]+"&alt_module="+flow_module+"&aow_fieldname="+aow_field+"&aow_newfieldname="+aow_field_name+"&aow_value="+value+"&aow_type="+type_value,callback);

}

function load_crrelline(ln, field, value, value_type){
    document.getElementById('addcrline'+ln).style.display = '';
    document.getElementById('addcrrelline'+ln).style.display = '';
    cln = add_crRelLine(ln);
    document.getElementById("aow_actions_param"+ln+"_rel"+cln).value = field;
    show_crRelField(ln, cln, value, value_type);
}

function add_crLine(ln){

    if(crln[ln] == null){crln[ln] = 0}

    /*if (document.getElementById(tableid + '_head') != null) {
        document.getElementById(tableid + '_head').style.display = "";
    }*/

    tablebody = document.createElement("tbody");
    tablebody.id = 'crLine'+ln+'_body' + crln[ln];
    document.getElementById('crLine'+ln+'_table').appendChild(tablebody);

    var x = tablebody.insertRow(-1);
    x.id = 'crLine'+ln+'_line' + crln[ln];
  x.setAttribute('data-workflow-action-line', '');

    var a = x.insertCell(0);
    a.innerHTML = "<button type='button' id='crLine"+ln+"_delete" + crln[ln]+"' class='button' value='Remove Line' tabindex='116' onclick='clear_crLine(" + ln + ",this);'><span class='suitepicon suitepicon-action-minus'></span></button>";

    var b = x.insertCell(1);
    b.innerHTML = "<select tabindex='116' name='aow_actions_param["+ln+"][field]["+crln[ln]+"]' id='aow_actions_param"+ln+"_field"+crln[ln]+"' onchange='show_crModuleField(" + ln + "," + crln[ln] + ");'>" + cr_fields[ln] + "</select>";

    var c = x.insertCell(2);
    c.id = 'crLine'+ln+'_fieldType' + crln[ln];

    var d = x.insertCell(3);
    d.id = 'crLine'+ln+'_field' + crln[ln];

    crln[ln]++;

    return crln[ln] -1;

}

function clear_crLine(ln, cln){

    document.getElementById('crLine'+ln+'_table').deleteRow(cln.parentNode.parentNode.rowIndex);
}

function add_crRelLine(ln){

    if(crreln[ln] == null){crreln[ln] = 0}

    /*if (document.getElementById(tableid + '_head') != null) {
     document.getElementById(tableid + '_head').style.display = "";
     }*/

    tablebody = document.createElement("tbody");
    tablebody.id = 'crRelLine'+ln+'_body' + crreln[ln];
    document.getElementById('crRelLine'+ln+'_table').appendChild(tablebody);

    var x = tablebody.insertRow(-1);
    x.id = 'crRelLine'+ln+'_line' + crreln[ln];

    var a = x.insertCell(0);
    a.innerHTML = "<button type='button' id='crRelLine"+ln+"_delete" + crreln[ln]+"' class='button' value='Remove Line' tabindex='116' onclick='clear_crRelLine(" + ln + ",this);'><span class='suitepicon suitepicon-action-minus'></span></button>";

    var b = x.insertCell(1);
    b.innerHTML = "<select tabindex='116' name='aow_actions_param["+ln+"][rel]["+crreln[ln]+"]' id='aow_actions_param"+ln+"_rel"+crreln[ln]+"' onchange='show_crRelField(" + ln + "," + crreln[ln] + ");'>" + cr_relationships[ln] + "</select>";

    var c = x.insertCell(2);
    c.id = 'crRelLine'+ln+'_fieldType' + crreln[ln];

    var d = x.insertCell(3);
    d.id = 'crRelLine'+ln+'_field' + crreln[ln];

    crreln[ln]++;

    return crreln[ln] -1;

}

function show_mrModuleFields(ln){

    clear_crLines(ln);
    cr_module[ln] = document.getElementById('aow_actions_param_record_type'+ln).value;
    rel_module = document.getElementById('aow_actions_param_rel_type'+ln).value;

    if(cr_module[ln] != ''){

        var callback = {
            success: function(result) {
                cr_module[ln] = result.responseText;
            }
        };

        var callback2 = {
            success: function(result) {
                cr_fields[ln] = result.responseText;
                document.getElementById('addcrline'+ln).style.display = '';
            }
        };

        var callback3 = {
            success: function(result) {
                cr_relationships[ln] = result.responseText;
                document.getElementById('addcrrelline'+ln).style.display = '';
            }
        };

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getRelatedModule&aow_module="+cr_module[ln]+"&rel_field="+rel_module,callback);
        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&view=EditView&action=getModuleFields&override[]=email1&override[]=email2&aow_module="+cr_module[ln]+"&rel_field="+rel_module,callback2);
        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getModuleRelationships&aow_module="+cr_module[ln]+"&rel_field="+rel_module,callback3);

    }

}

function clear_crRelLine(ln, cln){

    document.getElementById('crRelLine'+ln+'_table').deleteRow(cln.parentNode.parentNode.rowIndex);
}

function clear_crLines(ln){

    var cr_rows = document.getElementById('crLine'+ln+'_table').getElementsByTagName('tr');
    var cr_row_length = cr_rows.length;
    var i;
    for (i=0; i < cr_row_length; i++) {
        document.getElementById('crLine'+ln+'_table').deleteRow(cr_rows[i]);
    }

    var cr_rel_rows = document.getElementById('crRelLine'+ln+'_table').getElementsByTagName('tr');
    var cr_rel_row_length = cr_rel_rows.length;
    var i;
    for (i=0; i < cr_rel_row_length; i++) {
        document.getElementById('crRelLine'+ln+'_table').deleteRow(cr_rel_rows [i]);
    }

    document.getElementById('addcrline'+ln).style.display = 'none';
    document.getElementById('addcrrelline'+ln).style.display = 'none';
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

function assign_field_change(field){
    hideElem(field + '[1]');
    hideElem(field + '[2]');

    if(document.getElementById(field + '[0]').value == 'role'){
        showElem(field + '[2]');
    }
    else if(document.getElementById(field + '[0]').value == 'security_group'){
        showElem(field + '[1]');
        showElem(field + '[2]');
    }
}

function date_field_change(field){
    if(document.getElementById(field + '[1]').value == 'now'){
        hideElem(field + '[2]');
        hideElem(field + '[3]');
    } else {
        showElem(field + '[2]');
        showElem(field + '[3]');
    }
}
