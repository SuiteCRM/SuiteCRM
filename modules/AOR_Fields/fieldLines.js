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


var fieldln = 0;
var fieldln_count = 0;
var report_rel_modules =  new Array();
var report_fields =  new Array();
var report_module = '';

var FieldLineHandler = {

    makeGroupDisplaySelectOptions: function(selectedField, selectedField2) {
        var found = false;
        var value = $('#group_display').val();
        if(selectedField) {
            value = this.getFieldNth(selectedField);
        }
        var foundValues = [];
        $('#group_display').html('<option value="-1">' + SUGAR.language.get('app_strings', 'LBL_NONE') + '</option>');
        $('#fieldLines input[type="text"]').each(function(i,e){
            var _value = $(this).attr('id').substr('aor_fields_label'.length);
            if($(this).attr('id').substr(0, 'aor_fields_label'.length)=='aor_fields_label' && $('#aor_fields_deleted' + _value).val() != 1) {
                $('#group_display').append('<option value="' + _value + '">' + $(this).val() + '</option>');
                found = true;
                foundValues.push(_value);
            }
        });

        if(found) {
            $('#group_display_table').show();
            if($.inArray(value, foundValues) != -1) {
                $('#group_display').val(value);
            }
            else {
                $('#group_display').val(-1);
            }
        }
        else {
            $('#group_display_table').hide();
            $('#group_display').val(-1);
        }


        var value2 = $('#group_display_1').val();
        $('#group_display_1').html($('#group_display').html());
        $('#group_display_1').val(value2);

        if(selectedField2) {
            $('#group_display_1').val(this.getFieldNth(selectedField2));
        }

        if($('#group_display_1').val() == null || $('#group_display_1 option[value="' + $('#group_display_1').val() + '"]').css('display')=='none') {
            $('#group_display_1').val(-1);
        }
    },

    getFieldNth: function(field) {
        var ret = false;
        $('input[value="' + field.id + '"]').each(function(i,e){
            var id = $(this).attr('id');
            if(id.substr(0, 'aor_fields_id'.length)=='aor_fields_id') {
                ret = id.substr('aor_fields_id'.length);
                return ;
            }
        });
        return ret;
    }

};

function loadFieldLine(field){

    var prefix = 'aor_fields_';
    var ln = 0;

    ln = insertFieldLine();

    for(var a in field){

        var elem = document.getElementById(prefix + a + ln);

        if(elem != null){
            if(a === 'field_order'){
                $('#'+prefix+a+ln).val(ln);
            }else if(elem.nodeName != 'INPUT' && elem.nodeName != 'SELECT'){
                elem.innerHTML = field[a];
            }else if(elem.type == 'checkbox'){
                elem.checked = field[a] == 1;
            } else {
                elem.value = field[a];
            }
        }
    }
    showFieldOptions(field, ln);
    showFieldModuleField(ln, field['field_function'], field['label']);
    FieldLineHandler.makeGroupDisplaySelectOptions(parseInt(field.group_display) == 1 ? field : null, parseInt(field.group_display) == 2 ? field : null);
}

function showFieldOptions(field, ln){
    if(field.field_type == "datetime" || field.field_type == "date" || field.field_type == "datetimecombo"){
        showElem("aor_fields_format" + ln);
    }
}
function showFieldCurrentModuleFields(ln, value){

    if (typeof value === 'undefined') { value = ''; }

    report_module = document.getElementById('report_module').value;
    var rel_field = document.getElementById('aor_fields_module_path' + ln).value;

    if(report_module != '' && rel_field != ''){

        var callback = {
            success: function(result) {
                var fields = JSON.parse(result.responseText);

                document.getElementById('aor_fields_field'+ln).innerHTML = '';

                var selector = document.getElementById('aor_fields_field'+ln);
                for (var i in fields) {
                    selector.options[selector.options.length] = new Option(fields[i], i);
                }

                if(fields[value] != null ){
                    document.getElementById('aor_fields_field'+ln).value = value;
                }
                if(value == '') showFieldModuleField(ln);
            }
        }

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleFields&aor_module="+report_module+"&view=JSON&rel_field="+rel_field+"&aor_value="+value,callback);

    }

}

function showFieldModuleField(ln, function_value, label_value){
    if (typeof function_value === 'undefined') { function_value = ''; }
    if (typeof label_value === 'undefined') { label_value = ''; }

    var aor_field = document.getElementById('aor_fields_field'+ln).value;
    if(aor_field != ''){

        var callback = {
            success: function(result) {
                document.getElementById('aor_fields_fieldFunction'+ln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);

                var select_field = document.getElementById('aor_fields_field'+ln);
                if(label_value === ''){
                    document.getElementById('aor_fields_label'+ln).value = select_field.options[select_field.selectedIndex].text;
                }

            },
            failure: function(result) {
                fieldResetLine(ln);
            }
        }

        var aor_field_name = "aor_fields_field_function["+ln+"]";

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleFunctionField&view="+action_sugar_grp1+"&aor_module="+report_module+"&aor_fieldname="+aor_field+"&aor_newfieldname="+aor_field_name+"&aor_value="+function_value,callback);

    } else {
        fieldResetLine(ln);
    }
}

function fieldResetLine(ln){
    document.getElementById('aor_fields_display'+ln).checked = true;
    document.getElementById('aor_fields_link'+ln).checked = false;
    document.getElementById('aor_fields_label'+ln).value = '';
    document.getElementById('aor_fields_fieldFunction'+ln).innerHTML = '';
    document.getElementById('aor_fields_sort_by'+ln).value = '';
    document.getElementById('aor_fields_group_by'+ln).checked = false;
}

function showFieldModuleFieldType(ln, value){
    if (typeof value === 'undefined') { value = ''; }

    var callback = {
        success: function(result) {
            document.getElementById('aor_fields_fieldInput'+ln).innerHTML = result.responseText;
            SUGAR.util.evalScript(result.responseText);
            enableQS(false);
        },
        failure: function(result) {
            document.getElementById('aor_fields_fieldInput'+ln).innerHTML = '';
        }
    }

    var aor_field = document.getElementById('aor_fields_field'+ln).value;
    var type_value = document.getElementById("aor_fields_value_type["+ln+"]").value;
    var aor_field_name = "aor_fields_value["+ln+"]";

    YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleFieldType&view="+action_sugar_grp1+"&aor_module="+report_module+"&aor_fieldname="+aor_field+"&aor_newfieldname="+aor_field_name+"&aor_value="+value+"&aor_type="+type_value,callback);

}


/**
 * Insert Header
 */

function insertFieldHeader(){
    tablehead = document.createElement("thead");
    tablehead.id = "fieldLines_head";
    document.getElementById('fieldLines').appendChild(tablehead);

    var x=tablehead.insertRow(-1);
    x.id='fieldLines_head';

    var a=x.insertCell(0);
    //a.style.color="rgb(68,68,68)";

    var b=x.insertCell(1);
    b.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_MODULE_PATH');

    var b1=x.insertCell(2);
    b1.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_FIELD');

    var c=x.insertCell(3);
    c.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_DISPLAY');

    var d=x.insertCell(4);
    d.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_LINK');

    var e=x.insertCell(5);
    e.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_LABEL');

    var f=x.insertCell(6);
    f.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_FUNCTION');

    var g=x.insertCell(7);
    g.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_SORT');

    var h=x.insertCell(8);
    h.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_GROUP');

    var i=x.insertCell(9);
    i.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_FORMAT');

    var h=x.insertCell(10);
    h.innerHTML=SUGAR.language.get('AOR_Fields', 'LBL_TOTAL');

    tablebody = document.createElement("tbody");
    document.getElementById('fieldLines').appendChild(tablebody);

    $('#fieldLines tbody').sortable({
        stop: fieldSort,
        axis: 'y',
        containment: "#fieldLines"
    });
}

function insertFieldLine(){

    if (document.getElementById('fieldLines_head') == null) {
        insertFieldHeader();
    } else {
        document.getElementById('fieldLines_head').style.display = '';
    }

    tablebody = document.getElementById('fieldLines').getElementsByTagName('tbody')[0];

    var x = tablebody.insertRow(-1);
    x.id = 'field_line' + fieldln;

    var a = x.insertCell(0);
    if(action_sugar_grp1 == 'EditView'){
        a.innerHTML = "<button type='button' id='aor_fields_delete_line" + fieldln + "' class='button' value='' tabindex='116' onclick='markFieldLineDeleted(" + fieldln + ")'>-</button><br>";
        a.innerHTML += "<input type='hidden' name='aor_fields_deleted[" + fieldln + "]' id='aor_fields_deleted" + fieldln + "' value='0'><input type='hidden' name='aor_fields_id[" + fieldln + "]' id='aor_fields_id" + fieldln + "' value=''>";
    } else{
        a.innerHTML = fieldln +1;
    }
    a.innerHTML += "<input type='hidden' name='aor_fields_field_order[" + fieldln + "]' id='aor_fields_field_order" + fieldln + "' value='"+fieldln+"'>"
    a.style.width = '5%';

    var b = x.insertCell(1);
    b.style.width = '12%';
    var viewStyle = 'display:none';
    if(action_sugar_grp1 == 'EditView'){viewStyle = '';}
    b.innerHTML = "<input type='hidden' name='aor_fields_module_path["+ fieldln +"]' id='aor_fields_module_path" + fieldln + "' value=''>";
    if(action_sugar_grp1 == 'EditView'){viewStyle = 'display:none';}else{viewStyle = '';}
    b.innerHTML += "<span id='aor_fields_module_path_display" + fieldln + "'></span>";

    var b1 = x.insertCell(2);
    b1.style.width = '12%';
    var viewStyle = 'display:none';
    if(action_sugar_grp1 == 'EditView'){viewStyle = '';}
    b1.innerHTML = "<input type='hidden' name='aor_fields_field["+ fieldln +"]' id='aor_fields_field" + fieldln + "' value=''>";
    if(action_sugar_grp1 == 'EditView'){viewStyle = 'display:none';}else{viewStyle = '';}
    b1.innerHTML += "<span style='width:178px;' id='aor_fields_field_label" + fieldln + "' ></span>";

    var c = x.insertCell(3);
    c.innerHTML = "<input name='aor_fields_display["+ fieldln +"]' value='0' type='hidden'>";
    c.innerHTML += "<input id='aor_fields_display" + fieldln + "' name='aor_fields_display["+ fieldln +"]' value='1' type='checkbox' CHECKED>";
    c.style.width = '5%';

    var d = x.insertCell(4);
    d.innerHTML = "<input name='aor_fields_link["+ fieldln +"]' value='0' type='hidden'>";
    d.innerHTML += "<input id='aor_fields_link" + fieldln + "' name='aor_fields_link["+ fieldln +"]' value='1' type='checkbox'>";
    d.style.width = '5%';

    var e = x.insertCell(5);
    e.innerHTML = "<input name='aor_fields_label["+ fieldln +"]' id='aor_fields_label" + fieldln + "' size='20' maxlength='150' value='' type='text'>";
    e.style.width = '12%';

    var f = x.insertCell(6);
    f.id='aor_fields_fieldFunction'+fieldln;
    f.style.width = '10%';

    var g=x.insertCell(7);
    g.innerHTML = "<select type='text' name='aor_fields_sort_by["+ fieldln +"]' id='aor_fields_sort_by" + fieldln + "'>"+sort_by_values+"</select>";
    g.style.width = '10%';

    var h=x.insertCell(8);
    h.innerHTML = "<input name='aor_fields_group_by["+ fieldln +"]' value='0' type='hidden'>";
    h.innerHTML += "<input id='aor_fields_group_by" + fieldln + "' name='aor_fields_group_by["+ fieldln +"]' value='1' type='checkbox'>";
    h.style.width = '10%';

    var i=x.insertCell(9);
    i.innerHTML = "<select type='text' name='aor_fields_format["+ fieldln +"]' id='aor_fields_format" + fieldln + "' style='display:none;'>" + format_values + "</select>";
    i.style.width = '10%';

    var h=x.insertCell(10);
    h.innerHTML = "<select type='text' name='aor_fields_total["+ fieldln +"]' id='aor_fields_total" + fieldln + "'>"+total_values+"</select>";
    h.style.width = '10%';

    fieldln++;
    fieldln_count++;

    return fieldln -1;
}

function markFieldLineDeleted(ln)
{
    // collapse line; update deleted value
    document.getElementById('field_line' + ln).style.display = 'none';
    document.getElementById('aor_fields_deleted' + ln).value = '1';
    document.getElementById('aor_fields_delete_line' + ln).onclick = '';

    fieldln_count--;
    if(fieldln_count == 0){
        document.getElementById('fieldLines_head').style.display = "none";
    }
    FieldLineHandler.makeGroupDisplaySelectOptions();


    // remove fields header if doesn't exists any more field in area
    var found = false;
    $('#fieldLines tbody').each(function(i,e){
        if($(e).css('display') != 'none') {
            found = true;
        }
    });
    if(!found) {
        $('#fieldLines_head').remove();
    }
}

function clearFieldLines(){

    if(document.getElementById('fieldLines') != null){
        var cond_rows = document.getElementById('fieldLines').getElementsByTagName('tr');
        var cond_row_length = cond_rows.length;
        var i;
        for (i=0; i < cond_row_length; i++) {
            if(document.getElementById('aor_fields_delete_line'+i) != null){
                document.getElementById('aor_fields_delete_line'+i).click();
            }
        }
    }
}

function fieldSort(){
    if(document.getElementById('fieldLines') != null){
        var cond_rows = document.getElementById('fieldLines').getElementsByTagName('tr');
        var cond_row_length = cond_rows.length;
        var i;
        for (i=0; i < cond_row_length; i++) {
            var input = cond_rows[i].getElementsByTagName('input');
            var j;
            for (j=0; j < input.length; j++) {
                if (input[j].id.indexOf('aor_fields_field_order') != -1) {
                    $('.chartDimensionSelect').each(function(){
                        if($(this).data('value') == input[j].value){
                            $(this).data('value',(i-1)+'tmp');
                        }
                    });
                    input[j].value = i-1;
                }
            }
        }
        $('.chartDimensionSelect').each(function(){
            var suffix = 'tmp';
            var val = $(this).data('value')+'';
            if(val && val.indexOf(suffix, val.length - suffix.length) !== -1){
                $(this).data('value',val.slice(0,-3));
            }
        });
        updateChartDimensionSelects();
        FieldLineHandler.makeGroupDisplaySelectOptions();
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

function date_field_change(field){
    if(document.getElementById(field + '[1]').value == 'now'){
        hideElem(field + '[2]');
        hideElem(field + '[3]');
    } else {
        showElem(field + '[2]');
        showElem(field + '[3]');
    }
}

function addNodeToFields(node){
    if(node.type == "field"){
        //do ajax on moudule & name
        $.getJSON('index.php',
            {
                'module' : 'AOR_Reports',
                'action' : 'getVarDefs',
                'aor_module' : node.module,
                'aor_request' : node.id,
                'view' : 'JSON'
            },
            function(relData){
                //var result = JSON.parse(relData);
                loadFieldLine(
                    {
                        'label' : node.name,
                        'module' : node.module,
                        'module_path' : node.module_path,
                        'module_path_display' : node.module_path_display,
                        'field' : node.id,
                        'field_label' : node.name,
                        'field_type' : relData.type
                    });
            }
        );
    }else{
        loadFieldLine(
            {
                'label' : node.name,
                'module' : node.module,
                'module_path' : node.module_path,
                'module_path_display' : node.module_path_display,
                'field' : node.id,
                'field_label' : node.name});
    }
}

