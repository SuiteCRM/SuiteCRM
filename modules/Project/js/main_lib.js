/**
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU AFFERO GENERAL PUBLIC LICENSE
 * along with this program; if not, see http://www.gnu.org/licenses
 * or write to the Free Software Foundation,Inc., 51 Franklin Street,
 * Fifth Floor, Boston, MA 02110-1301  USA
 * @Package Gantt chart
 * @copyright Andrew Mclaughlan 2014
 * @author Andrew Mclaughlan <andrew@mclaughlan.info>
 */


/**
 * General javascript for view.ganttchart.php
 */

// unblock when ajax activity stops
$(document).ajaxStop(function(){
    setTimeout($.unblockUI, 1000);
});
//Get the default sugar page loading message
try{
	var loading = SUGAR.language.languages.app_strings['LBL_LOADING_PAGE'];
}catch(err){
	var loading = ""; 
}

$(function() {

    var project_id = $('#project_id').val();

    //generate the chart on page load
    gen_chart(0);

    //Default to tasks
    var milestone_flag ='Task';

    //If its a milestone not a full task then disable the duration and duration unit fields
    //by making them read only.

    $('[name="Milestone"]').change(function(){

        if ($(this).val() == 'Milestone'){
            $('#Duration').val('0').attr('readonly', true);
            $('#Duration_unit').attr('readonly', true);

            milestone_flag = 'Milestone';
        }
        else {
            $('#Duration').val('').attr('readonly', false);
            $('#Duration_unit').attr('readonly', false);

            milestone_flag = 'Task';
        }

    });

    //Ajax call to the action_get_end_date() function in controller.php
    //Returns the end date of predecessor with the lag added
    $("#Predecessor").on('change', function() {

        if($('#task_id').val().length == 0){
            var Id = $(this).find(":selected").attr('rel');
            var Lag = $('#Lag').val();
            var dataString = '&task_id=' + Id + '&lag=' + Lag;
            $.ajax({
                type: "POST",
                url: "index.php?module=Project&action=get_end_date",
                data: dataString,
                success: function(data) {
                    $("#Start").val(data);

                }
            });
        }
    });

    //Ajax call to the action_get_end_date() function in controller.php
    //Returns the end date of predecessor with the lag added
    $("#Lag").on('change', function() {
        var Id = $("#Predecessor").find(":selected").attr('rel');
        var Lag = $(this).val();
        var dataString = '&task_id=' + Id + '&lag=' + Lag;
        $.ajax({
            type: "POST",
            url: "index.php?module=Project&action=get_end_date",
            data: dataString,
            success: function(data) {
                $("#Start").val(data);
            }
        });

    });

    //some basic validation on add task popup
    $("#popup_form").validate({

        rules: {
            task_name: "required",
            Start: {
                required: true
            },
            Duration: {
                required: true,
                number: true
            },
            Complete: {
                required: true,
                number: true,
                max: 100
            }
        },
        messages: {
            task_name: "The task name is required",
            Start: "Start date is required",
            Duration: {
                required: "The duration is required",
                number: "Enter a number"
            },
            Complete: {
                required: "Percentage complete is required",
                number:"Enter a number",
                max: "Maximum value is 100"
            }
        }

    });

    $('#add_button').button({
        text: true,
        icons: {
                primary: 'ui-icon-plusthick'
            }
        }).click(function( event ) {
            event.preventDefault();
            var form = $("#popup_form");

            //make sure duration is not readonly on create new task
            $('#Duration').val('0').attr('readonly', false);
            $('#Duration_unit').attr('readonly', false);

            $( "#dialog" ).dialog({
                autoOpen: true,
                show: {
                    effect: "none",
                    duration: 0
                },
                hide: {
                    effect: "none",
                    duration: 0
                },
                width: 700,
                modal: false,
                buttons: {
                    "Add": function() {
                        var Project_id = $('#project_id').val();
			var override_business_hours = $('#consider_business_hours').val();
                        //var Parent_task = $('#parent_task').val();
                        var Task_name = $('#task_name').val();
                        var milestone = milestone_flag;
                        var Task_pre = $('#Predecessor').val();
                        var rel_type = $('#relation_type').val();
                        var Task_Start = $('#Start').val();
                        var Task_Duration = $('#Duration').val();
                        var Task_Duration_unit = $('#Duration_unit').val();
                        var Task_Resource = $('#Resources').val();
                        var Task_Percent = $('#Complete').val();
                        var Task_Notes = $('#Notes').val();
                        var rowCount = $('#Task_table tr').length -1;
                        var dateStart = "Start_date_"+rowCount ;
			var actual_duration = $('#Actual_duration').val();
			    
                        if($("#popup_form").valid()){

                            var dataString = '&project_id=' + Project_id + '&override_business_hours=' + override_business_hours + '&milestone=' + milestone + '&task_name=' +Task_name + '&predecessor=' + Task_pre + '&rel_type=' + rel_type + '&start=' + Task_Start + '&duration=' + Task_Duration + '&unit=' + Task_Duration_unit + '&resource=' + Task_Resource + '&percent=' + Task_Percent + '&note=' + Task_Notes + '&actual_duration=' + actual_duration;
                            //block();
                            $.ajax({
                                type: "POST",
                                url: "index.php?module=Project&action=update_GanttChart",
                                data: dataString,
                                success: function() {
                                    //close and clear form
                                    $("#popup_form").trigger("reset");
                                    $( "#dialog" ).dialog( "close" );
                                }
                            });
                        }
                    },
                    Cancel: function() {
                        //close and clear form
                        $("#popup_form").validate().resetForm();
                        $("#popup_form").trigger("reset");
                        $(this).dialog( "close" );
                    }
                },
                close: function () {
                    $(this).parent().promise().done(function () {
                        gen_chart(1);
                    });
                }
            });
        });

});



 /*For date fields sugar overwrites the onchange function that was set on onchange event.In this case you have to assign
 the function to the event after it is loaded on screen. (I could not get jquery to pick up the date field values after
 they were changed by the sugar date picker.
 */


function gen_chart(blockui){
    //Get the chart properties
    var pid = $('#project_id').val();

    //Put the properties into a string
    var dataString = '&pid=' + pid;

    var msg = '<div><br />' +
        '<h1><img align="absmiddle" src="themes/default/images/img_loading.gif"> ' + loading + '</h1>' + '</div>';
    //call blockui
    if(blockui == '1'){
        block();
    }

    //Pass the properties to the controller function via ajax
    $.ajax({
        type: "POST",
        url: "index.php?module=Project&action=generate_chart",
        data: dataString,
        success: function(data) { // On success generate the tasks for the chart
            $('#project_wrapper').html(data);
        },
        complete: function(){
            animate_percentages();
            add_arrows();
            remove_button();
            rows_sortable();
            get_predecessors();
        }

    });
}
//Used to animate the percentage bars on gantt chart tasks
function animate_percentages(){

    $('#gantt .task_percent').each(function() {

        var width = $(this).attr('rel')+'%';

        $(this).animate(
            { width: width },
            {
                duration:1000,
                step: function(now, fx) {
                    var data= Math.round(now);
                    $(this).html(data + '%');
                }
                //easing: "linear"
            }
        );
    });
}

function remove_button(){

    $('.remove_button').button({
        text: false,
        icons: {
            primary: 'ui-icon-circle-close'
        }
    }).click(function( event ) {
        event.preventDefault();
        var Id = $(this).val();


        $( "#delete_dialog" ).dialog({
            autoOpen: true,
            show: {
                effect: "none",
                duration: 0
            },
            hide: {
                effect: "none",
                duration: 0
            },
            width: 700,
            modal: true,
            buttons: {
                "Delete": function() {
                    var dataString = '&task_id=' + Id;

                    $.ajax({
                        type: "POST",
                        url: "index.php?module=Project&action=delete_task",
                        data: dataString,
                        success: function() {
                            //close and clear form
                            $( "#delete_dialog" ).dialog( "close" );
                        }
                    });

                },
                Cancel: function() {
                    //close and clear form
                    $( this ).dialog( "close" );
                    $("#popup_form").validate().resetForm();
                }
            },
            close: function () {
                $(this).parent().promise().done(function () {
                    gen_chart(1);
                });
            }
        });

    });

}


function rows_sortable(){
    if ($('#is_editable').length > 0) {
        //Helper function used to prevent table rows with from collapsing when using sortable
        var fixHelperModified = function (e, tr) {
            var $originals = tr.children();
            var $helper = tr.clone();
            $helper.children().each(function (index) {
                $(this).width($originals.eq(index).width())
            });
            return $helper;
        };

        //Make project task table rows sortable
        $('#Task_table tbody').sortable({
            items: "tr:not(.disable_sort)",//disable sortable on header row
            helper: fixHelperModified, //call helper function
            update: function (event, ui) {
                var order = {};//create object
                $('#Task_table tr.row_sortable').each(function () {//loop through rows
                    //get new row indexes
                    var rowIndex = $(this).closest('tr').prevAll().length;
                    var id = $(this).children('td:first-child').find(".order_number").attr("rel");
                    //var order_number = $(this).children('td:first-child').find(".order_number").val();
                    var order_number = rowIndex;
                    //fill object array with keys(task->id) and values (task->order_number)
                    order[id] = order_number;
                });

                //convert array to json
                var jsonArray = JSON.stringify(order);
                //prepare POST data
                var dataString = {'orderArray': jsonArray};
                $.ajax({
                    type: "POST",
                    url: "index.php?module=Project&action=update_order",
                    data: dataString,
                    success: function () {
                        gen_chart(1);
                    }
                });
            }
        });
    }
}
//Gets tasks via ajax call from controller functions get_predecessors
//Used in the add task pop-up form.
function get_predecessors(){
    var project_id = $('#project_id').val();
    var dataString = '&project_id=' + project_id;
    $.ajax({
        type: "POST",
        url: "index.php?module=Project&action=get_predecessors",
        data: dataString,
        success: function(data) {
            $("#Predecessor").html(data);
        }
    });
}

//Used to create ajax loading effect using the blockUI jquery plugin
function block(){
    var msg = '<div><br />' +
        '<h1><img align="absmiddle" src="themes/default/images/img_loading.gif"> ' + loading + '</h1>' + '</div>';

    $.blockUI({//ajax loading screen
        message:msg,
        css: {
            height: 'auto',
            width: '240px',
            // top:  ($(window).height() - 50) /2 + 'px',
            left: ($(window).width() - 240) /2 + 'px'//centre box
        }
    });
}
//Adds relationship link arrows between tasks
function add_arrows(){

    $(".link").each(function(i) {
        var task = $(this);
        var pre = $(this).attr('pre');

        if(i && task.attr('pre') != '0'){
            drawlink($('#'+ pre), task, task.attr('link'));
        }
    });
}

//Edit project tasks
function edit_task(task){

    var data = task.attr('data').split(",");
    var milestone_flag ='Task';


    $('#task_id').val(data[0]);
    $('#task_name').val(task.text());
    $('#Start').val(data[3]);
    if(data[7] == '1'){
        $('#Subtask').prop('checked', false);
        $('#Milestone').prop('checked', true);
        $('#Duration').val('0').attr('readonly', true);
        $('#Duration_unit').attr('readonly', true);
    }
    else{
        $('#Subtask').prop('checked', true);
        $('#Milestone').prop('checked', false);
        $('#Duration').val('0').attr('readonly', false);
        $('#Duration_unit').attr('readonly', false);
    }
    $('#Predecessor').val(data[1]);
    $('#relation_type').val(data[2]);
    $('#Duration').val(data[4]);
    $('#Duration_unit').val(data[5]);
    $('#Resources').val(data[6]);
    $('#Complete').val(data[8]);
    $('#Notes').val(data[9]);
    $('#Actual_duration').val(data[10]);

    $( "#dialog" ).dialog({
        autoOpen: true,
        show: {
            effect: "none",
            duration: 0
        },
        hide: {
            effect: "none",
            duration: 0
        },
        width: 700,
        modal: true,
        buttons: {
            "Update": function() {
                var Project_id = $('#project_id').val();
                var override_business_hours = $('#consider_business_hours').val();
		var Task_id = $('#task_id').val();
                //var Parent_task = $('#parent_task').val();
                var Task_name = $('#task_name').val();

                if($('[name="Milestone"]:checked').val() == 'Milestone'){
                    milestone_flag = 'Milestone'
                }

                var milestone = milestone_flag;
                var Task_pre = $('#Predecessor').val();
                var rel_type = $('#relation_type').val();
                var Task_Start = $('#Start').val();
                var Task_Duration = $('#Duration').val();
                var Task_Duration_unit = $('#Duration_unit').val();
                var Task_Resource = $('#Resources').val();
                var Task_Percent = $('#Complete').val();
                var Task_Notes = $('#Notes').val();
                var Actual_duration = $('#Actual_duration').val();
                var rowCount = $('#Task_table tr').length -1;
                var dateStart = "Start_date_"+rowCount ;

                get_predecessors();

                if($("#popup_form").valid()){

                    var dataString = '&project_id=' + Project_id + '&override_business_hours=' + override_business_hours + '&task_id=' + Task_id + '&milestone=' + milestone + '&task_name=' +Task_name + '&predecessor=' + Task_pre + '&rel_type=' + rel_type + '&start=' + Task_Start + '&duration=' + Task_Duration + '&unit=' + Task_Duration_unit + '&resource=' + Task_Resource + '&percent=' + Task_Percent + '&note=' + Task_Notes + '&actual_duration=' + Actual_duration;
                    //block();
                    $.ajax({
                        type: "POST",
                        url: "index.php?module=Project&action=update_GanttChart",
                        data: dataString,
                        success: function() {
                            //close and clear form
                            $("#popup_form").trigger("reset");
                            $( "#dialog" ).dialog( "close" );
                        }
                    });
                }
            },
            Cancel: function() {
                //close and clear form
                $("#popup_form").validate().resetForm();
                $("#popup_form").trigger("reset");
                $(this).dialog( "close" );
            }
        },
        close: function () {
            $(this).parent().promise().done(function () {
                get_predecessors();
                gen_chart(1);
            });
        }
    });
}

/*********************************** Draw Link Elements **************************************/

/*
 Copyright (c) 2012-2014 Open Lab
 Written by Roberto Bicchierai and Silvia Chelazzi http://roberto.open-lab.com
 Permission is hereby granted, free of charge, to any person obtaining
 a copy of this software and associated documentation files (the
 "Software"), to deal in the Software without restriction, including
 without limitation the rights to use, copy, modify, merge, publish,
 distribute, sublicense, and/or sell copies of the Software, and to
 permit persons to whom the Software is furnished to do so, subject to
 the following conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */



var peduncolusSize = 8;
var lineSize = 0;


function drawlink (from, to, type) {

    var rectFrom = buildRect(from);
    var rectTo = buildRect(to);

    // Dispatch to the correct renderer
    if (type == 'SS') {
        $("#gantt").append(
            drawStartToStart(rectFrom, rectTo, peduncolusSize)
        );
    } else if (type == 'FS') {
        $("#gantt").append(
            drawStartToEnd(rectFrom, rectTo, peduncolusSize)
        );
    }

}

/**
 * A representation of a Horizontal line
 */
HLine = function(width, top, left) {
    var hl = $("<div>").addClass("taskDepLine");

    hl.css({
        height: lineSize,
        left: left,
        width: width,
        top: top - lineSize / 2 -2 //added - 1
    });
    return hl;
};

/**
 * A representation of a Vertical line
 */
VLine = function(height, top, left) {
    var vl = $("<div>").addClass("taskDepLine");

    vl.css({
        height: height -2,//added -2
        left:left - lineSize / 2,
        width: lineSize,
        top: top
    });
    return vl;
};

/**
 * Given an item, extract its rendered position
 * width and height into a structure.
 */
function buildRect(item) {
    var rect = item.position();
    rect.width = item.width();
    rect.height = item.height();

    return rect;
}

/**
 * The default rendering method, which paints a start to end dependency.
 *
 * @see buildRect
 */
function drawStartToEnd(rectFrom, rectTo, peduncolusSize) {
    var left, top;
    var gheight = $('.main_table').innerHeight();
    var gleft = -5;

    var ndo = $("<div style='position: relative;'> </div>").css({
        "bottom":gheight,
        "left":-5
    });

    var currentX = rectFrom.left + rectFrom.width;
    var currentY = rectFrom.height / 2 + rectFrom.top;

    var useThreeLine = (currentX + 2 * peduncolusSize) < rectTo.left;

    if (!useThreeLine) {
        // L1
        if (peduncolusSize > 0) {
            var l1 = new HLine(peduncolusSize, currentY, currentX);
            currentX = currentX + peduncolusSize;
            ndo.append(l1);
        }

        // L2
        var l2_4size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2)) / 2;
        var l2;
        if (l2_4size < 0) {
            l2 = new VLine(-l2_4size, currentY + l2_4size, currentX);
        } else {
            l2 = new VLine(l2_4size, currentY, currentX);
        }
        currentY = currentY + l2_4size;

        ndo.append(l2);

        // L3
        var l3size = rectFrom.left + rectFrom.width + peduncolusSize - (rectTo.left - peduncolusSize);
        currentX = currentX - l3size;
        var l3 = new HLine(l3size, currentY, currentX);
        ndo.append(l3);

        // L4
        var l4;
        if (l2_4size < 0) {
            l4 = new VLine(-l2_4size, currentY + l2_4size, currentX);
        } else {
            l4 = new VLine(l2_4size, currentY, currentX);
        }
        ndo.append(l4);

        currentY = currentY + l2_4size;

        // L5
        if (peduncolusSize > 0) {
            var l5 = new HLine(peduncolusSize, currentY, currentX);
            currentX = currentX + peduncolusSize;
            ndo.append(l5);

        }
    } else {
        //L1
        var l1_3Size = (rectTo.left - currentX) / 2;
        var l1 = new HLine(l1_3Size, currentY, currentX);
        currentX = currentX + l1_3Size;
        ndo.append(l1);

        //L2
        var l2Size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2));
        var l2;
        if (l2Size < 0) {
            l2 = new VLine(-l2Size, currentY + l2Size, currentX);
        } else {
            l2 = new VLine(l2Size, currentY, currentX);
        }
        ndo.append(l2);

        currentY = currentY + l2Size;

        //L3
        var l3 = new HLine(l1_3Size, currentY, currentX);
        currentX = currentX + l1_3Size;
        ndo.append(l3);
    }

    //arrow
    var arr = $("<img src='modules/Project/images/linkArrow.png'>").css({
        position: 'absolute',
        top: rectTo.top + rectTo.height / 2 - 6,//added -6
        left: rectTo.left - 4
    });

    ndo.append(arr);

    return ndo;
}

/**
 * A rendering method which paints a start to start dependency.
 *
 * @see buildRect
 */
function drawStartToStart(rectFrom, rectTo, peduncolusSize) {
    var left, top;
    var gheight = $('.main_table').innerHeight();
    var ndo = $("<div style='position: relative;'> </div>").css({
        "bottom":gheight,
        "left":-5
    });

    var currentX = rectFrom.left;
    var currentY = rectFrom.height / 2 + rectFrom.top;

    var useThreeLine = (currentX + 2 * peduncolusSize) < rectTo.left;

    if (!useThreeLine) {
        // L1
        if (peduncolusSize > 0) {
            var l1 = new HLine(peduncolusSize, currentY, currentX - peduncolusSize);
            currentX = currentX - peduncolusSize;
            ndo.append(l1);
        }

        // L2
        var l2_4size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2)) / 2;
        var l2;
        if (l2_4size < 0) {
            l2 = new VLine(-l2_4size, currentY + l2_4size, currentX);
        } else {
            l2 = new VLine(l2_4size, currentY, currentX);
        }
        currentY = currentY + l2_4size;

        ndo.append(l2);

        // L3
        var l3size = (rectFrom.left - peduncolusSize) - (rectTo.left - peduncolusSize);
        currentX = currentX - l3size;
        var l3 = new HLine(l3size, currentY, currentX);
        ndo.append(l3);

        // L4
        var l4;
        if (l2_4size < 0) {
            l4 = new VLine(-l2_4size, currentY + l2_4size, currentX);
        } else {
            l4 = new VLine(l2_4size, currentY, currentX);
        }
        ndo.append(l4);

        currentY = currentY + l2_4size;

        // L5
        if (peduncolusSize > 0) {
            var l5 = new HLine(peduncolusSize, currentY, currentX);
            currentX = currentX + peduncolusSize;
            ndo.append(l5);
        }
    } else {
        //L1

        var l1 = new HLine(peduncolusSize, currentY, currentX - peduncolusSize);
        currentX = currentX - peduncolusSize;
        ndo.append(l1);

        //L2
        var l2Size = ((rectTo.top + rectTo.height / 2) - (rectFrom.top + rectFrom.height / 2));
        var l2;
        if (l2Size < 0) {
            l2 = new VLine(-l2Size, currentY + l2Size, currentX);
        } else {
            l2 = new VLine(l2Size, currentY, currentX);
        }
        ndo.append(l2);

        currentY = currentY + l2Size;

        //L3
        var l3 = new HLine((rectTo.left - rectFrom.left ), currentY, currentX);
        currentX = currentX + peduncolusSize + (rectTo.left - rectFrom.left);
        ndo.append(l3);
    }

    //arrow
    var arr = $("<img src='modules/Project/images/linkArrow.png'>").css({
        position: 'absolute',
        top: rectTo.top + rectTo.height / 2 - 6,//changed to -6
        left: rectTo.left - 5
    });

    ndo.append(arr);

    return ndo;
}

