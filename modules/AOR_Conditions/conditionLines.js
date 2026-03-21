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


var condln = 0;
var condln_count = 0;
var report_fields =  new Array();
var report_module = '';

var LogicalOperatorHandler = {
    //logicSelectCounter: 0.

    getLogicalOperatorSelectHTML: function(value, _condln, forcedValue) {

        // default set to 'AND'!!
        if(typeof forcedValue == 'undefined' || forcedValue === true) forcedValue = 'AND';
        if(!forcedValue) forcedValue = null;

        if(_condln===0)_condln = '0';

        if (typeof value === 'undefined' || !value) {
            value = forcedValue ? forcedValue : null;
        }

        var selecteds = {};
        selecteds.none = value == null ? ' selected="selected"' : '';
        selecteds.AND = value == 'AND' ? ' selected="selected"' : '';
        selecteds.OR = value == 'OR' ? ' selected="selected"' : '';
        selectHTML =
            '<select class="logic-select" name="aor_conditions_logic_op[' + (_condln ? _condln : condln) + ']" onchange="LogicalOperatorHandler.onLogicSelectChange(this, ' + (_condln ? _condln : condln) + ');" style="display:none;">' +

            (!value && !forcedValue ? ('   <option value=""' + selecteds.none + '></option>') : '')  +
            '   <option value="AND"' + selecteds.AND + '>' + SUGAR.language.get('AOR_Conditions', 'LBL_CONDITION_AND') + '</option>' +
            '   <option value="OR"' + selecteds.OR + '>' + SUGAR.language.get('AOR_Conditions', 'LBL_CONDITION_OR') + '</option>' +
            '</select>';

        //logicSelectCounter++;

        return selectHTML;
    },

    hideUnnecessaryLogicSelects: function() {
        var isPrevParenthesisOpen = true;
        $('#aor_conditions_body tr').each(function (i, e) {
            if ($(this).css('display') != 'none') {
                if (isPrevParenthesisOpen) {
                    $(this).find('.logic-select').prop('disabled', 'disabled').hide();
                }
                else {
                    $(this).find('.logic-select').prop('disabled', false).show();
                }
                isPrevParenthesisOpen = $(this).hasClass('parenthesis-line') && $(this).hasClass('parenthesis-open');
            }
        });
    },

    onLogicSelectChange: function(elem, counter) {
        // console.log('logic select changed... ', elem, counter);
    }

};

var ConditionOrderHandler = {

    //conditionOrderInputCounter: 0,

    getConditionOrderHiddenInput: function(value, _condln) {

        if(_condln===0)_condln = '0';

        if (typeof value === 'undefined' || !value) { value = '0'; }

        //conditionOrderInputCounter++

        inputHTML = '<input type="hidden" class="aor_condition_order_input" name="aor_conditions_order[' + (_condln ? _condln : condln) + ']" value="' + value + '">';

        return inputHTML;
    },

    setConditionOrders: function() {
        var ord = 0;
        $('#aor_conditions_body tr').each(function (i, e) {
            if ($(this).css('display') != 'none') {
                $(this).find('.aor_condition_order_input').val(ord++);
            }
            else {
                $(this).find('.aor_condition_order_input').val(-1);
            }
        });
    },

    getConditionLineByPageEvent: function(event) {
        var closest = $(document.elementFromPoint(event.pageX - window.pageXOffset, event.pageY - window.pageYOffset)).closest('tr');
        if((closest.attr('id') && closest.attr('id').search('product_line') === 0) || (closest.attr('class') && closest.attr('class').search('parenthesis-line') !== -1)) {
            return closest;
        }
        return false;
    },

    putPositionedConditionLines: function(elemTarget, elemNew) {
        elemTarget.before(elemNew);
    }

};

var ParenthesisHandler = {

    //parenthesisCounter: 0,

    getParenthesisStartHtml: function(condition_id, logic_op, condition_order, _condln) {

        if(!condition_id) condition_id = '';
        if(!logic_op) logic_op = '';
        if(!condition_order) condition_order = '';
        if(_condln===0)_condln = '0';

        var html =
            '<tr class="parenthesis-line parenthesis-open" parenthesis-counter="' + (_condln ? _condln : condln) + '" data-condition-id="' + condition_id + '">' +
            '   <td>' +
            '       <input type="hidden" name="aor_conditions_parenthesis[' + ((_condln ? _condln : condln)) + ']" value="START">' +
            '       <button type="button" class="button parenthesis-remove-btn" value="" onclick="ParenthesisHandler.deleteParenthesisPair(this, ' + ((_condln ? _condln : condln)) + ');">' +
            '          -' +
            '       </button>' +
            '       <input type="hidden" name="aor_conditions_deleted[' + (_condln ? _condln : condln) + ']" id="aor_conditions_deleted' + (_condln ? _condln : condln) + '" value="0" data-delete-id="' + condition_id + '">' +
            '       <input type="hidden" name="aor_conditions_id[' + (_condln ? _condln : condln) + ']" id="aor_conditions_id' + (_condln ? _condln : condln) + '" value="' + condition_id + '">' +
            '       <input type="hidden" name="aor_conditions_field[' + ((_condln ? _condln : condln)) + ']" value="">' +
            '   </td>' +
            '   <td>' + LogicalOperatorHandler.getLogicalOperatorSelectHTML(logic_op, ((_condln ? _condln : condln))) + ConditionOrderHandler.getConditionOrderHiddenInput(condition_order, ((_condln ? _condln : condln))) + '</td>' +
            '   <td>' +
            '       <strong>(</strong> ' +
            '   </td>' +
            '</tr>';

        return html;
    },

    getParenthesisEndHtml: function(condition_id, condition_order, condition_parenthesis_start, _condln, _start_condln) {

        if(!condition_id) condition_id = '';
        if(!condition_order) condition_order = '';
        if(!condition_parenthesis_start || condition_parenthesis_start == 'END') condition_parenthesis_start = '';
        if(_condln===0)_condln = '0';
        if(_start_condln===0)_start_condln = '0';

        var html =
            '<tr class="parenthesis-line parenthesis-close" parenthesis-counter="' + ((_condln ? _condln : condln+1)) + '" data-condition-id="' + condition_id + '" data-parenthesis-start="' + (condition_parenthesis_start) + '" data-parenthasis-start-condln="' + (_start_condln ? _start_condln : condln) + '">' +
            '   <td>' +
            '       <input type="hidden" name="aor_conditions_parenthesis[' + ((_condln ? _condln : condln+1)) + ']" value="' + (condition_parenthesis_start ? condition_parenthesis_start : 'END') + '">' +
            '       <input type="hidden" class="parenthesis-close-deleted-input" name="aor_conditions_deleted[' + (_condln ? _condln : condln+1) + ']" id="aor_conditions_deleted' + (_condln ? _condln : condln+1) + '" value="0" data-delete-id="' + condition_id + '">' +
            '       <input type="hidden" name="aor_conditions_id[' + (_condln ? _condln : condln+1) + ']" id="aor_conditions_id' + (_condln ? _condln : condln+1) + '" value="' + condition_id + '">' +
            '       <input type="hidden" name="aor_conditions_field[' + ((_condln ? _condln : condln+1)) + ']" value="">' +
            '       &nbsp;' +
            '   </td>' +
            '   <td>' + ConditionOrderHandler.getConditionOrderHiddenInput(condition_order, ((_condln ? _condln : condln+1))) + '</td>' +
            '   <td>' +
            '       <strong>)</strong> ' +
            '   </td>' +
            '</tr>';

        return html;
    },

    replaceParenthesisBtns: function() {

        $( ParenthesisHandler.getParenthesisStartHtml() + ParenthesisHandler.getParenthesisEndHtml() ).replaceAll('#aor_conditions_body .parentheses-btn');

        condln+=2;
        condln_count+=2;

        //parenthesisCounter++;
    },

    deleteParenthesisPair: function(elem, counter) {
        condition_id = $('#aor_conditions_id'+counter).val();
        if(condition_id) {
            $('input[data-delete-id="' + condition_id + '"]').val(1);
            $('tr[data-condition-id="' + condition_id + '"]').hide();
            $('tr.parenthesis-line.parenthesis-close').each(function(i,e){
                if($(this).attr('data-parenthesis-start') && $(this).attr('data-parenthesis-start') == condition_id) {
                    $(this).find('input.parenthesis-close-deleted-input').val(1);
                    $(this).hide();
                }
            });
        }
        else {
            $('.parenthesis-line[parenthesis-counter=' + counter + ']').remove();
            $('.parenthesis-line[data-parenthasis-start-condln=' + counter + ']').remove();
        }
        LogicalOperatorHandler.hideUnnecessaryLogicSelects();
        ConditionOrderHandler.setConditionOrders();
        ParenthesisHandler.addParenthesisLineIdent();
    },

    deleteParenthesisPairs: function() {
        $('.parenthesis-remove-btn').click();
    },

    addParenthesisLineIdent: function() {
        var identDeep = 0;
        $('.condition-ident').remove();
        $('#aor_conditions_body tr').each(function (i, e) {
            if($(this).css('display') != 'none') {
                if ($(this).hasClass('parenthesis-close')) {
                    identDeep--;
                }
                if ($(this).css('display') != 'none') {
                    for (var i = 0; i < identDeep; i++) {
                        $(this).find('td:nth-child(3)').prepend('<span class="condition-ident">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span>');
                    }
                }
                if ($(this).hasClass('parenthesis-open')) {
                    identDeep++;
                }
            }
        });
        $('.condition-ident').closest('td').css('white-space', 'nowrap');
    }

};

function loadConditionLine(condition, overrideView){

    showConditionLines();

    var prefix = 'aor_conditions_';
    var ln = 0;

    ln = insertConditionLine(condition);

    for(var a in condition){
        var elem = document.getElementById(prefix + a + ln);
        if(elem != null){
            if(elem.nodeName !== 'INPUT') {
                elem.innerHTML = condition[a];
            }else if(elem.getAttribute('type') === 'checkbox'){
                elem.checked = condition[a] == 1;
            }else {
                elem.value = condition[a];
            }
        }
    }

    if (condition['value'] instanceof Array) {
        condition['value'] = JSON.stringify(condition['value'])
    }

    if(!condition['parenthesis']) {
        showConditionModuleField(ln, condition['operator'], condition['value_type'], condition['value'],overrideView, condition['logic_op'], condition['condition_order'], condition['parenthesis']);
    }

    return $('#product_line'+ln);
}

function showConditionLines() {
    $('#detailpanel_parameters').removeClass('hidden');
}

function showConditionCurrentModuleFields(ln, value){

    if (typeof value === 'undefined') { value = ''; }

    report_module = document.getElementById('report_module').value;
    var rel_field = document.getElementById('aor_conditions_module_path' + ln).value;

    if(report_module != '' && rel_field != ''){

        var callback = {
            success: function(result) {
                var fields = JSON.parse(result.responseText);

                document.getElementById('aor_conditions_field'+ln).innerHTML = '';

                var selector = document.getElementById('aor_conditions_field'+ln);
                for (var i in fields) {
                    selector.options[selector.options.length] = new Option(fields[i], i);
                }

                if(fields[value] != null ){
                    document.getElementById('aor_conditions_field'+ln).value = value;
                }

                if(value == '') showConditionModuleField(ln);

            }
        }

        YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleFields&aor_module="+report_module+"&view=JSON&rel_field="+rel_field+"&aor_value="+value,callback);

    }

}

var moduleFieldsPendingFinished = 0;

var setModuleFieldsPendingFinishedCallback = function(callback) {
	moduleFieldsPendingFinishedCallback = callback;
}

var testModuleFieldsPendingFinished = function () {
  moduleFieldsPendingFinished--;
  if (moduleFieldsPendingFinished <= 0) {
    setModuleFieldsPendingFinishedCallback();
  }
};

function showConditionModuleField(ln, operator_value, type_value, field_value, overrideView, logic_value, condition_order, parenthesis){
    if(overrideView === undefined){
        overrideView = action_sugar_grp1;
    }


    if (typeof operator_value === 'undefined') { operator_value = ''; }
    if (typeof type_value === 'undefined') { type_value = ''; }
    if (typeof field_value === 'undefined') { field_value = ''; }

    var rel_field = document.getElementById('aor_conditions_module_path'+ln).value;
    var aor_field = document.getElementById('aor_conditions_field'+ln).value;
    if(aor_field != ''){

        var callback = {
            success: function(result) {
                document.getElementById('aor_conditions_operatorInput'+ln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                testModuleFieldsPendingFinished();
            },
            failure: function(result) {
                document.getElementById('aor_conditions_operatorInput'+ln).innerHTML = '';
                testModuleFieldsPendingFinished();
            }
        }
        var callback2 = {
            success: function(result) {
                document.getElementById('aor_conditions_fieldTypeInput'+ln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                document.getElementById('aor_conditions_fieldTypeInput'+ln).onchange = function(){showConditionModuleFieldType(ln, undefined, overrideView);};
                testModuleFieldsPendingFinished();
            },
            failure: function(result) {
                document.getElementById('aor_conditions_fieldTypeInput'+ln).innerHTML = '';
                testModuleFieldsPendingFinished();
            }
        }
        var callback3 = {
            success: function(result) {
                document.getElementById('aor_conditions_fieldInput'+ln).innerHTML = result.responseText;
                SUGAR.util.evalScript(result.responseText);
                enableQS(false);
                testModuleFieldsPendingFinished();
            },
            failure: function(result) {
                document.getElementById('aor_conditions_fieldInput'+ln).innerHTML = '';
                testModuleFieldsPendingFinished();
            }
        }

        var aor_operator_name = "aor_conditions_operator["+ln+"]";
        var aor_field_type_name = "aor_conditions_value_type["+ln+"]";
        var aor_field_name = "aor_conditions_value["+ln+"]";

        moduleFieldsPendingFinished++; YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleOperatorField&view="+overrideView+"&aor_module="+report_module+"&aor_fieldname="+aor_field+"&aor_newfieldname="+aor_operator_name+"&aor_value="+operator_value+"&rel_field="+rel_field,callback);
        moduleFieldsPendingFinished++; YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getFieldTypeOptions&view="+overrideView+"&aor_module="+report_module+"&aor_fieldname="+aor_field+"&aor_newfieldname="+aor_field_type_name+"&aor_value="+type_value+"&rel_field="+rel_field,callback2);
        moduleFieldsPendingFinished++; YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleFieldType&view="+overrideView+"&aor_module="+report_module+"&aor_fieldname="+aor_field+"&aor_newfieldname="+aor_field_name+"&aor_value="+field_value+"&aor_type="+type_value+"&rel_field="+rel_field,callback3);

    } else {
        document.getElementById('aor_conditions_logicInput'+ln).innerHTML = ''
        document.getElementById('aor_conditions_operatorInput'+ln).innerHTML = ''
        document.getElementById('aor_conditions_fieldTypeInput'+ln).innerHTML = '';
        document.getElementById('aor_conditions_fieldInput'+ln).innerHTML = '';
    }
}

function showConditionModuleFieldType(ln, value, overrideView){
    if(overrideView === undefined){
        overrideView = action_sugar_grp1;
    }
    if (typeof value === 'undefined') { value = ''; }

    var callback = {
        success: function(result) {
            document.getElementById('aor_conditions_fieldInput'+ln).innerHTML = result.responseText;
            SUGAR.util.evalScript(result.responseText);
            enableQS(false);
        },
        failure: function(result) {
            document.getElementById('aor_conditions_fieldInput'+ln).innerHTML = '';
        }
    }

    var rel_field = document.getElementById('aor_conditions_module_path'+ln).value;
    var aor_field = document.getElementById('aor_conditions_field'+ln).value;
    var type_value = document.getElementById("aor_conditions_value_type["+ln+"]").value;
    var aor_field_name = "aor_conditions_value["+ln+"]";

    YAHOO.util.Connect.asyncRequest ("GET", "index.php?module=AOR_Reports&action=getModuleFieldType&view="+overrideView+"&aor_module="+report_module+"&aor_fieldname="+aor_field+"&aor_newfieldname="+aor_field_name+"&aor_value="+value+"&aor_type="+type_value+"&rel_field="+rel_field,callback);

}


/**
 * Insert Header
 */

function insertConditionHeader(){
    var nxtCell = 0;
    var view = action_sugar_grp1;
    tablehead = document.createElement("thead");
    tablehead.id = "aor_conditionLines_head";
    document.getElementById('aor_conditionLines').appendChild(tablehead);

    var x=tablehead.insertRow(-1);
    x.id='aor_conditionLines_head';

    var a=x.insertCell(nxtCell++);

    if(view === 'EditView') {
        var cellLogic = x.insertCell(nxtCell++);
        cellLogic.innerHTML = SUGAR.language.get('AOR_Conditions', 'LBL_LOGIC_OP');
    }

    var b=x.insertCell(nxtCell++);
    b.innerHTML=SUGAR.language.get('AOR_Conditions', 'LBL_MODULE_PATH');

    var c=x.insertCell(nxtCell++);
    c.innerHTML=SUGAR.language.get('AOR_Conditions', 'LBL_FIELD');

    var d=x.insertCell(nxtCell++);
    d.innerHTML=SUGAR.language.get('AOR_Conditions', 'LBL_OPERATOR');

    var e=x.insertCell(nxtCell++);
    e.innerHTML=SUGAR.language.get('AOR_Conditions', 'LBL_VALUE_TYPE');

    var f=x.insertCell(nxtCell++);
    f.innerHTML=SUGAR.language.get('AOR_Conditions', 'LBL_VALUE');

    if(view === 'EditView') {
        var h = x.insertCell(-1);
        h.innerHTML = SUGAR.language.get('AOR_Conditions', 'LBL_PARAMETER');
    }
}

function insertConditionLine(condition){

    var nxtCell = 0;
    var view = action_sugar_grp1;
    if (document.getElementById('aor_conditionLines_head') == null) {
        insertConditionHeader();
    } else {
        document.getElementById('aor_conditionLines_head').style.display = '';
    }

    var tablebody = document.getElementById('aor_conditions_body');
    if(tablebody == null) {
        tablebody = document.createElement("tbody");
        tablebody.id = "aor_conditions_body";
        tablebody.className = "connectedSortableConditions";
        document.getElementById('aor_conditionLines').appendChild(tablebody);
    }

    if(view == 'EditView' && condition.parenthesis) {
        if(condition.parenthesis == 'START') {
            $(tablebody).append(ParenthesisHandler.getParenthesisStartHtml(condition.id, condition.logic_op, condition.condition_order, condln));
        }
        else {
            $(tablebody).append(ParenthesisHandler.getParenthesisEndHtml(condition.id, condition.condition_order, condition.parenthesis, condln));
        }
    }
    else {

        var x = tablebody.insertRow(-1);
        x.id = 'product_line' + condln;

        var a = x.insertCell(nxtCell++);
        if(action_sugar_grp1 == 'EditView'){
            a.innerHTML = "<button type='button' id='aor_conditions_delete_line" + condln + "' class='button' value='' tabindex='116' onclick='markConditionLineDeleted(" + condln + ")'>-</button><br>";
            a.innerHTML += "<input type='hidden' name='aor_conditions_deleted[" + condln + "]' id='aor_conditions_deleted" + condln + "' value='0'><input type='hidden' name='aor_conditions_id[" + condln + "]' id='aor_conditions_id" + condln + "' value=''>";
        } else{
            a.innerHTML = condln +1 + "<input class='aor_conditions_id' type='hidden' name='aor_conditions_id[" + condln + "]' id='aor_conditions_id" + condln + "' value=''>";
        }
        a.style.width = '5%';


        if(view == 'EditView') {
            var cellLogic = x.insertCell(nxtCell++);
            cellLogic.id = 'aor_conditions_logicInput' + condln;
            cellLogic.innerHTML = LogicalOperatorHandler.getLogicalOperatorSelectHTML(condition.logic_op ? condition.logic_op : null, condln) + ConditionOrderHandler.getConditionOrderHiddenInput(condition.condition_order ? condition.condition_order : null, condln);
        }

        var b = x.insertCell(nxtCell++);
        b.style.width = '15%';
        b.className = 'condition-sortable-handle';
        var viewStyle = 'display:none';
        if (action_sugar_grp1 == 'EditView') {
            viewStyle = '';
        }
        b.innerHTML = "<input type='hidden' name='aor_conditions_module_path[" + condln + "]' id='aor_conditions_module_path" + condln + "' value=''>";
        if (action_sugar_grp1 == 'EditView') {
            viewStyle = 'display:none';
        } else {
            viewStyle = '';
        }
        b.innerHTML += "<span style='width:178px;' id='aor_conditions_module_path_display" + condln + "' ></span>";


        var c = x.insertCell(nxtCell++);
        c.style.width = '15%';
        c.className = 'condition-sortable-handle';
        var viewStyle = 'display:none';
        if (action_sugar_grp1 == 'EditView') {
            viewStyle = '';
        }
        c.innerHTML = "<input type='hidden' name='aor_conditions_field[" + condln + "]' id='aor_conditions_field" + condln + "' value=''>";
        if (action_sugar_grp1 == 'EditView') {
            viewStyle = 'display:none';
        } else {
            viewStyle = '';
        }
        c.innerHTML += "<span style='width:178px;' id='aor_conditions_field_label" + condln + "' ></span>";


        var d = x.insertCell(nxtCell++);
        d.id = 'aor_conditions_operatorInput' + condln;
        d.style.width = '15%';

        var e = x.insertCell(nxtCell++);
        e.id = 'aor_conditions_fieldTypeInput' + condln;
        e.style.width = '15%';

        var f = x.insertCell(nxtCell++);
        f.id = 'aor_conditions_fieldInput' + condln;
        f.style.width = '30%';


        if (view === 'EditView') {
            var h = x.insertCell(-1);
            h.innerHTML += "<input id='aor_conditions_parameter" + condln + "' name='aor_conditions_parameter[" + condln + "]' value='1' type='checkbox'>";
            h.style.width = '10%';
        }

    }

    condln++;
    condln_count++;

    return condln -1;
}

function markConditionLineDeleted(ln)
{
    // collapse line; update deleted value
    document.getElementById('product_line' + ln).style.display = 'none';
    document.getElementById('aor_conditions_deleted' + ln).value = '1';
    document.getElementById('aor_conditions_delete_line' + ln).onclick = '';

    condln_count--;
    if(condln_count == 0){
        document.getElementById('aor_conditionLines_head').style.display = "none";
    }

    // remove condition header if doesn't exists any more condition in area
    var found = false;
    $('#aor_conditions_body tr').each(function(i,e){
        if($(e).css('display') != 'none') {
            found = true;
        }
    });
    if(!found) {
        $('#aor_conditionLines_head').remove();
    }

    LogicalOperatorHandler.hideUnnecessaryLogicSelects();
    ConditionOrderHandler.setConditionOrders();
    ParenthesisHandler.addParenthesisLineIdent();
}

function clearConditionLines(){

    if(document.getElementById('aor_conditionLines') != null){
        var cond_rows = document.getElementById('aor_conditionLines').getElementsByTagName('tr');
        var cond_row_length = cond_rows.length;
        var i;
        for (i=0; i < cond_row_length; i++) {
            if(document.getElementById('aor_conditions_delete_line'+i) != null){
                document.getElementById('aor_conditions_delete_line'+i).click();
            }
        }
    }
    ParenthesisHandler.deleteParenthesisPairs();
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

function addNodeToConditions(node){
    return loadConditionLine(
        {
            'label' : node.name,
            'module_path' : node.module_path,
            'module_path_display' : node.module_path_display,
            'field' : node.id,
            'field_label' : node.name});
}