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


var actln = 0;
var actln_count = 0;

document.getElementById('flow_module').addEventListener("change", showActions, false);

function showActions(){

    clearActionLines();

    var flow_module = document.getElementById('flow_module').value;

    if(flow_module != ''){
        document.getElementById('btn_ActionLine').disabled = '';
    } else{
        document.getElementById('btn_ActionLine').disabled = 'disabled';
    }
}


function loadActionLine(action){

    var prefix = 'aow_actions_';
    var ln = 0;

    ln = insertActionLine();

    for(var a in action){
        if(document.getElementById(prefix + a + ln) != null){
            document.getElementById(prefix + a + ln).value = action[a];
        }
    }

    getView(ln,action['id']);

}

function insertActionLine(){

    /*if (document.getElementById(actionLines_head') != null) {
     document.getElementById('actionLines_head').style.display = "";
     }*/

    var app_list_actions=document.getElementById("app_list_actions").value;

    var tableBody = document.createElement("tr");
    tableBody.id = "aow_actions_group_body"+actln;
    document.getElementById('actionLines').appendChild(tableBody);

    var a=tableBody.insertCell(0);
    var table = document.createElement("table");
    table.id = "aow_actions_table" + actln;
    table.className = "action-table";
    a.appendChild(table);

    var tablebody = document.createElement("tbody");
    tablebody.id = "aow_actions_body" + actln;
    table.appendChild(tablebody);


    var x = tablebody.insertRow(-1);
    x.id = 'action_line' + actln;

    var a1 = x.insertCell(0);
    a1.scope="row";
    a1.setAttribute("field", "action");
    a1.innerHTML= "<label>"+ SUGAR.language.get('AOW_Actions', 'LBL_SELECT_ACTION') +":</label><select name='aow_actions_action[" + actln + "]' id='aow_actions_action" + actln + "' onchange='getView(" + actln + ");'>"+ app_list_actions +"</select>";

    var b1 = x.insertCell(1);
    b1.scope="row";
    b1.setAttribute("field", "name");
    b1.innerHTML= "<label>"+SUGAR.language.get('AOW_Actions', 'LBL_NAME')+":</label><input name='aow_actions_name[" + actln + "]' id='aow_actions_name" + actln + "' type='text'>";

    var c1 = x.insertCell(2);
    c1.scope="row";
    c1.setAttribute("field", "delete");
  c1.innerHTML = "<span class='delete-btn'><a style='cursor: pointer;' id='aow_actions_delete_line" + actln + "' tabindex='116' onclick='markActionLineDeleted(" + actln + ")' class='btn btn-danger'><span class=\"suitepicon suitepicon-action-clear\"></span></a></span>";
    c1.innerHTML += "<input type='hidden' name='aow_actions_deleted[" + actln + "]' id='aow_actions_deleted" + actln + "' value='0'><input type='hidden' name='aow_actions_id[" + actln + "]' id='aow_actions_id" + actln + "' value=''>";


    var y = tablebody.insertRow(-1);
    y.id = 'action_parameter_line' + actln;
  y.setAttribute('data-workflow-action-parameter', '');

    var a2 = y.insertCell(0);
    a2.colSpan = 2;
  a2.innerHTML = "<div id ='action_parameter" + actln + "' ></div>";



    actln++;
    actln_count++;

    return actln -1;
}

function markActionLineDeleted(ln){

    // collapse line; update deleted value
    document.getElementById('aow_actions_group_body' + ln).style.display = 'none';
    document.getElementById('aow_actions_deleted' + ln).value = '1';
    document.getElementById('aow_actions_delete_line' + ln).onclick = '';

    actln_count--;
    /*if(actln_count == 0){
     document.getElementById('actionLines_head').style.display = "none";
     }*/
}


function getView(ln, id){
    if (typeof id === 'undefined') { id = ''; }

    var callback = {
        success: function(result) {
            document.getElementById('action_parameter' + ln).innerHTML = result.responseText;
            if(document.getElementById('aow_script'+ln) != null){
              SUGAR.util.evalScript(result.responseText);
            }
        }
    };

    var action = document.getElementById('aow_actions_action' + ln).value;
    var module = document.getElementById('flow_module').value;

    YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOW_WorkFlow&action=getAction&id="+id+"&aow_action="+action+"&line="+ln+"&aow_module="+module,callback);
}

function clearActionLines(){

    if(document.getElementById('actionLines') != null){
        var actionLines_rows = document.getElementById('actionLines').getElementsByTagName('tr');
        for (var i = 0; i < actionLines_rows.length; i++) {
            if(document.getElementById('aow_actions_delete_line' + i) != null){
                document.getElementById('aow_actions_delete_line' + i).click();
            }
        }
    }
}
