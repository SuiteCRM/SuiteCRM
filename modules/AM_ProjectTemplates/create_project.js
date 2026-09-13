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


function checkLength( o, n, min, max ) {
    if ( o.val().length > max || o.val().length < min ) {
        o.addClass( "ui-state-error" );
        updateTips( "Length of " + n + " must be between " +
            min + " and " + max + "." );
        return false;
    } else {
        return true;
    }
}

function checkRegexp( o, regexp, n ) {
    if ( !( regexp.test( o.val() ) ) ) {
        o.addClass( "ui-state-error" );
        updateTips( n );
        return false;
    } else {
        return true;
    }
}

function updateTips( t ) {
   var tips = $( ".validateTips" );
    tips
        .text( t )
        .addClass( "ui-state-highlight" );
    setTimeout(function() {
        tips.removeClass("ui-state-highlight", 1500);
    }, 500 );
}

function confirmation(id){

    var CreateProject = SUGAR.language.get('AM_ProjectTemplates', 'LBL_NEW_PROJECT');
    var Cancel = SUGAR.language.get('AM_ProjectTemplates', 'LBL_CANCEL_PROJECT');

    $( "#dialog-confirm" ).dialog({
        height: 400,
        width: 350,
        modal: false,
        buttons: [
            {
                text: CreateProject,
                click: function () {
                    var name = $("#p_name");
                    var start_date = $("#start_date"),
                        allFields = $([]).add(name).add(start_date),
                        tips = $(".validateTips");

                    if (check_form('project_form')) {
                        $("#users tbody").append("<tr>" +
                            "<td>" + name.val() + "</td>" +
                            "</tr>");
                        $("#project_form").submit()
                        $(this).dialog("close");
                        name.removeClass("ui-state-error");
                    }
                },
            },
            {
                text: Cancel,
                click : function() {
                    var name = $( "#p_name" );
                    var start_date = $("#start_date");
                    name.val('');
                    start_date.val('');
                    $( this ).dialog( "close" );
                    name.removeClass( "ui-state-error" );
                }
            }
        ]
    });


	$('#copy_all_tasks').click(function() {

		var $this = $(this);
		if ($this.is(':checked')) {
			 $('#tasks').hide();
			 $('#tasks_label').hide();
		} else {
			$('#tasks').show();
			$('#tasks_label').show();
		}
	});

}





 //html =  '<a id="create_link" onclick="' + $("#view_gantt").attr('onclick') + '" class="utilsLink">' + $("#view_gantt").attr('value') + '</a>&nbsp;&nbsp;' + $(".moduleTitle .utils").html();
 //$(".moduleTitle .utils").html(html);
