/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2014 - 2026 SuiteCRM Ltd.
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

// unblock when ajax activity stops
$(document).ajaxStop($.unblockUI);
//Get the default sugar page loading message
var loading = SUGAR.language.languages.app_strings['LBL_LOADING_PAGE'];

$(function() {
   
   //generate the chart on page load
    gen_chart('0');

  //message for ajax loading screen
    var msg = '<div><br />' +
        '<h1><img align="absmiddle" src="themes/default/images/img_loading.gif"> ' + loading + '</h1>' + '</div>';
    //on button click re-generate the chart
    $(document.body).on('click','#create_link', function(e) {
        $(".qtip").remove(); //Clear all tooltips before re-generating the chart

        $.blockUI({//ajax loading screen
            message:msg,
            css: {
                height: '50px',
                width: '240px',
               // top:  ($(window).height() - 50) /2 + 'px',
                left: ($(window).width() - 240) /2 + 'px'//centre box
            }
        });
        setTimeout(gen_chart(0,true),800); //call the ajax generate chart function with delay
    });
    //Gets the previous month
    $(document.body).on('click','#prev_month', function(e) {
        e.preventDefault();
        $(".qtip").remove(); //Clear all tooltips before re-generating the chart

        $.blockUI({//ajax loading screen
            message:msg,
            css: {
                height: '50px',
                width: '240px',
                // top:  ($(window).height() - 50) /2 + 'px',
                left: ($(window).width() - 240) /2 + 'px'//centre box
            }
        });
        setTimeout(gen_chart('-1'),800); //call the ajax generate chart function with delay
    });
    //Gets the next month
    $(document.body).on('click','#next_month', function(e) {
        e.preventDefault();
        $(".qtip").remove(); //Clear all tooltips before re-generating the chart

        $.blockUI({//ajax loading screen
            message:msg,
            css: {
                height: '50px',
                width: '240px',
                // top:  ($(window).height() - 50) /2 + 'px',
                left: ($(window).width() - 240) /2 + 'px'//centre box
            }
        });
        setTimeout(gen_chart('+1'),800); //call the ajax generate chart function with delay
    });

});
/*
* This function generates the html chart grid by calling the update_chart function in the controller using ajax.
* The jquery block UI plugin is used for the loading screen
* */

 function gen_chart(month, flag){

    //Get the chart properties
    var start = $('#date_start').val();
    var end = $('#date_end').val();
	var projects = $('#projects').val();
    var users = $('#users').val();
	var contacts = $('#contacts').val();
	var chart_type = $('#chart_type').val();
    //var type = 'all';


    if(!start){
        start = '';
    }

	if(!end){
        end = '';
    }

    if(!projects){
        projects = '';
    }

    if(!users){
        users = '';
    }

    if(!contacts){
        contacts = '';
    }

    if(!chart_type){
        chart_type = '';
    }

    if(flag){
        flag = '1';
    }
    else {
        flag = '0';
    }

    //Put the properties into a string
    var dataString = '&start=' + start + '&end=' + end +  '&projects=' + projects + '&users=' + users + '&contacts=' + contacts + '&month=' + month + '&flag=' + flag  + '&chart_type=' + chart_type  ;
    
	//Pass the properties to the controller function via ajax
    $.ajax({
        type: "POST",
        url: "index.php?module=Project&action=update_chart",
        data: dataString,
        success: function(data) { // On success generate the tasks for the chart
            // data is ur summary
            $('#gantt_chart').html(data);
            add_tooltips();
        }
    });
}

/*
 * This function generates the tooltips on the resource chart by calling the tooltip function in the controller using ajax.
 * qTips tooltip jquery plugin is used for the tooltips
 * */

 function add_tooltips(){
    //loop through each task and add tooltips
    $('.h,.d').each(function(event) {

        //set tooltip title
        var title = SUGAR.language.get('Project', 'LBL_TOOLTIP_TITLE');
        
	//get start and end date of the task to pass via ajax to the tooltip controller function
        var rel = $(this).attr('rel');
        var dates = rel.split("|");
		var projects = $("#projects").val();
		if(projects != null)
			projects = projects.join();
		else
			projects = '';
        var dataString = 'start_date=' + dates[0] + '&end_date=' + dates[1] +'&resource_id=' + dates[2] + '&type=' + dates[3] + '&projects=' + projects;
        var url = 'index.php?module=Project&action=Tooltips';

        $(this).qtip({
            content: {
                text: function(event, api) {
                    $.ajax({
			type: "POST",
                        url: url,
                        data: dataString
                    }).then(function(content) {
                            // Set the tooltip content upon successful retrieval
                            api.set('content.text', content);
                        },
                        function(xhr, status, error) {
                            // Upon failure... set the tooltip content to the status and error value
                            api.set('content.text', status + ': ' + error);
                        });

                    return 'Loading...'; // Set some initial text
                },
                title: {
                    //button: true,
                    text: title
                }
            },
            position: {
                my: 'bottom center',
                at: 'top center',
                target: 'mouse',
                adjust: {
                    mouse: false,
                    scroll: false,
                    y: -10
                }
            },
            show: {
                event: 'mouseover'
            },
            hide: {
                event: 'mouseout'
            },
            style: {
                classes : 'qtip-green qtip-shadow qtip_box', //qtip-rounded'
                tip: {

                    offset: 10

                }
            }
        });
    });
}


$(document.body).on('change','#users', function(e) {

	$('#users option:selected').each(function(i, selected){
		if($(selected).val() == '' || $(selected).val() == 'none' ){
			$('#users option:selected').removeAttr("selected");	
			$("#users").val(new Array($(selected).val()));
			return 0;
		}

	});

});


$(document.body).on('change','#contacts', function(e) {

	$('#contacts option:selected').each(function(i, selected){
		if($(selected).val() == ''  || $(selected).val() == 'none' ){
			$('#contacts option:selected').removeAttr("selected");
			$("#contacts").val(new Array($(selected).val()));
			return 0;
		}

	});

});
