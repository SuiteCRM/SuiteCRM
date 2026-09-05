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
 * Remainers handling
 * Use Calls/Meatings module EditView.
 * The Remainders need a Scheduler table.
 */
var Reminders = {
	
	// override this by user preferences on reminder init
	defaultValues: {
		popup: true,
		email: true,
		timer_popup: 1800,
        timer_email: 3600
	},

    // we have to disabled the reminders on details view - override this on initialization
    disabled: false,

    errors: [],

    addDefaultReminderInterval: null,

    defaultInvitees: [],

    getInviteeView: function(id, module, moduleId, relatedValue) {
        if(!id) id = '';
        // TODO: add a template for this
        if(!Reminders.disabled) {
            var inviteeView = '<!-- enabled --><li class="invitees_item"><button class="invitee_btn btn btn-danger" data-invitee-id="' + id + '" data-id="' + moduleId + '" data-module="' + module + '" onclick="Reminders.onInviteeClick(this);"><span class="suitepicon suitepicon-module-' + module.toLowerCase().replace('_', '-') + '"></span><span class="related-value">' + relatedValue + '</span></button></li>';
        }
        else {
            var inviteeView = '<!-- disabled --><li class="invitees_item"><button class="invitee_btn btn btn-danger" data-invitee-id="' + id + '" data-id="' + moduleId + '" data-module="' + module + '" disabled="disabled"><span class="suitepicon suitepicon-module-' + module.toLowerCase().replace('_', '-') + '"></span></span></button></li>';
        }
        return inviteeView;
    },

    addAllInvitees: function(e) {
        var inviteesList = '';
        var alreadyInvited = new Array();

        // add all default invitees
        $.each(Reminders.defaultInvitees, function(i,e){
            var dataModule = e.personModule;
            var dataId = e.personModuleId;
            var relatedValue = e.personName;

            // Fix for issue #1161: dont add duplicate invitees
            var associativeArray = new Array();
            associativeArray['dataModule'] = dataModule;
            associativeArray['dataId'] = dataId;
            associativeArray['relatedValue'] = relatedValue;
            alreadyInvited.push(associativeArray);

            inviteesList += Reminders.getInviteeView(false, dataModule, dataId, relatedValue);
        });

        // this function need a scheduler table!
        $('table#schedulerTable tr.schedulerAttendeeRow').each(function(i,e){
            var dataModule = $(e).attr('data-module');
            var dataId = $(e).attr('data-id');
            var relatedValue = $(e).find('td[scope="row"]').first().text();

            // Fix for issue #1161: dont add duplicate invitees
            var result = new Array();
            for( var i = 0, len = alreadyInvited.length; i < len; i++ ) {
                if(alreadyInvited[i]['dataModule'] === dataModule) {
                    if(alreadyInvited[i]['dataId'] === dataId) {
                        if(alreadyInvited[i]['relatedValue'].trim() === relatedValue.trim()) {
                            result = alreadyInvited[i];
                            break;
                        }
                    }
                }
            }
            if(typeof result.dataModule == 'undefined') {
                inviteesList += Reminders.getInviteeView(false, dataModule, dataId, relatedValue);
            }
            
        });
        $(e).find('.invitees_list').first().html(inviteesList);
    },

    addInvitees: function(e, invitees) {
        if(!e) e = document.getElementById('reminder_template');
        var inviteesList = '';
        $.each(invitees, function(i,e){
            inviteesList += Reminders.getInviteeView(e.id, e.module, e.module_id, e.value);
        });
        $(e).find('.invitees_list').first().html(inviteesList);
    },

    getBool: function(value) {
        if(!value || value === false || value === 0 || value === '0' || value === '' || (typeof value == 'string' && value.toLowerCase() === 'false') ) {
            value = false;
        }
        else {
            value = true;
        }
        return value;
    },

    setCheckboxValue: function(sel, value) {
        value = Reminders.getBool(value);
        sel.prop('checked', value);
        sel.attr('checked', value);
    },

    setSelectValue: function(sel, value) {
        sel.val(value);
        sel.find('option').prop('selected', false);
        sel.find('option[value="' + value + '"]').attr('selected', 'selected');
        sel.find('option[value="' + value + '"]').prop('selected', true);
    },

    setReminderPopupChkbox: function(e, value) {
        Reminders.setCheckboxValue($(e).find('.popup_chkbox'), value);
    },

    setReminderEmailChkbox: function(e, value) {
        Reminders.setCheckboxValue($(e).find('.email_chkbox'), value);
    },

    setPopupTimerSelectValue: function(e, value) {
        Reminders.setSelectValue($(e).find('.timer_sel_popup'), value);
    },

    setEmailTimerSelectValue: function(e, value) {
        Reminders.setSelectValue($(e).find('.timer_sel_email'), value);
    },

    addReminder: function(e, popup, email, timer_popup, timer_email, reminderId, invitees) {
        if(!reminderId) reminderId = '';
        let reminderIdX = Reminders.getRemindersData().length;
        Reminders.setReminderPopupChkbox($('#reminder_template'), popup);
        Reminders.setReminderEmailChkbox($('#reminder_template'), email);
        Reminders.setPopupTimerSelectValue($('#reminder_template'), timer_popup);
        Reminders.setEmailTimerSelectValue($('#reminder_template'), timer_email);
        if(!invitees) {
            Reminders.addAllInvitees($('#reminder_template'));
        }
        else {
            Reminders.addInvitees(e, invitees);
        }
        $('#reminder_view').append('<li class="reminder_item" data-reminder-id="' + reminderId + '" data-reminder-idx="' + reminderIdX + '">' + $('#reminder_template').html() + '</li>');
    },

    onAddClick: function(e){
        Reminders.addReminder(e, Reminders.defaultValues.popup, Reminders.defaultValues.email, Reminders.defaultValues.timer_popup, Reminders.defaultValues.timer_email);
        Reminders.createRemindersPostData();
    },
    onRemoveClick: function(e) {
        $(e).closest('.reminder_item').remove();
        Reminders.createRemindersPostData();
    },
    onAddAllClick: function(e) {
        Reminders.addAllInvitees($(e).closest('.reminder_item'));
        Reminders.createRemindersPostData();
    },
    onInviteeClick: function(e) {
        var parentReminderItem = $(e).closest('.reminder_item');
        var parentReminderId = parentReminderItem.attr('data-reminder-id');
        var parentReminderIdX = parentReminderItem.attr('data-reminder-idx');
        var reminders = Reminders.getRemindersData();
        var _e = e;
        $.each(reminders, function(i, reminder) {
            if((reminder.id != '' && reminder.id == parentReminderId || i == parentReminderIdX) && reminder.invitees.length == 1) {
                var confirmDeletePopup = new YAHOO.widget.SimpleDialog("Confirm ", {
                    //width: "400px",
                    draggable: false,
                    constraintoviewport: true,
                    modal: true,
                    fixedcenter: true,
                    text: SUGAR.language.get('app_strings', 'LBL_DELETE_REMINDER_CONFIRM'),
                    bodyStyle: "padding:5px",
                    buttons: [{
                        text: SUGAR.language.get('app_strings', 'LBL_OK'),
                        handler: function(){
                            // YES
                            confirmDeletePopup.hide();
                            parentReminderItem.remove();
                            Reminders.createRemindersPostData();
                            return false;
                        },
                        isDefault:true
                    }, {
                        text: SUGAR.language.get('app_strings', 'LBL_CANCEL_BUTTON_LABEL'),
                        handler: function() {
                            // NO
                            confirmDeletePopup.hide();
                            Reminders.createRemindersPostData();
                            return false;
                        }
                    }]
                });
                confirmDeletePopup.setHeader(SUGAR.language.get('app_strings', 'LBL_DELETE_REMINDER'));
                confirmDeletePopup.render(document.body);
            }
        });
        $(e).closest('.invitees_item').remove();
        Reminders.createRemindersPostData();
    },

    getInviteesData: function(e) {
        var ret = [];
        $(e).find('.invitee_btn').each(function(i,e){
            ret.push({
                id: $(e).attr('data-invitee-id'),
                module: $(e).attr('data-module'),
                module_id: $(e).attr('data-id')
            });
        });
        return ret;
    },

    getRemindersData: function() {
        var reminders = [];
        $('#reminder_view .reminder_item').each(function (i, e) {
            reminders.push({
                idx: i,
                id: $(e).attr('data-reminder-id'),
                popup: $(e).find('.popup_chkbox').prop('checked'),
                email: $(e).find('.email_chkbox').prop('checked'),
                timer_popup: $(e).find('.timer_sel_popup').val(),
                timer_email: $(e).find('.timer_sel_email').val(),
                invitees: Reminders.getInviteesData(e)
            });
        });
        return reminders;
    },

    createRemindersPostData: function() {
        // do not create post data if disabled and/or it is not an EditView!
        if(!Reminders.disabled) {
            var reminders = Reminders.getRemindersData();
            //document.EditView.reminders_data.value = JSON.stringify(reminders);
            document.getElementById('reminders_data').value = JSON.stringify(reminders);
            //Reminders.isValid('EditView', 'reminders_data');
            Reminders.isValid('reminders_data');
        }
    },

    loadDefaultsAndInit: function(data, defaultValues, disabled, eventModule, defaultInvitees, returnAction) {
        var _data = data;
        var _defaultValues = defaultValues;
        var _disabled = disabled;
        var _eventModule = eventModule;
        var _returnAction = returnAction;
        $.getJSON('index.php?module=Reminders&action=getInviteesPersonName', {invitees: defaultInvitees}, function(resp){
            var _defaultInvitees = resp;
            if(!_defaultValues) {
                $.getJSON('index.php?module=Reminders&action=getUserPreferencesForReminders', function(resp) {
                    _defaultValues = resp;
                    Reminders.init(_data, _defaultValues, _disabled, _eventModule, _defaultInvitees, _returnAction);
                });
            }
            else {
                Reminders.init(_data, _defaultValues, _disabled, _eventModule, _defaultInvitees, _returnAction);
            }
        });
    },

    init: function(data, defaultValues, disabled, eventModule, defaultInvitees, returnAction) {

        Reminders.initViaData(eventModule, defaultInvitees, returnAction);

        Reminders.disabled = disabled ? true : false;
        if(data) {
            $.each(data, function(i,e){
                Reminders.addReminder(false, e.popup, e.email, e.timer_popup, e.timer_email, e.id, e.invitees);
            });
        }
		if(defaultValues) {
            Reminders.defaultValues = defaultValues;
		}

        Reminders.createRemindersPostData();

        // on edit view
        if(!Reminders.disabled) {
            // add validations on edit view
            addToValidateCallback('EditView', 'reminders_data', 'function', false, SUGAR.language.get('app_strings', 'ERR_A_REMINDER_IS_EMPTY_OR_INCORRECT'), function (formname, nameIndex) {
                //return Reminders.isValid(formname, nameIndex);
                return Reminders.isValid(nameIndex);
            });
            // add one reminder by default into the edit view if we don't have any reminders BUT we checked any remainders in user preferences!
            if(Reminders.getRemindersData().length == 0 && (Reminders.getBool(Reminders.defaultValues.popup) || Reminders.getBool(Reminders.defaultValues.email))) {
                Reminders.addDefaultReminderInterval = setInterval(function () {
                    // we have to wait for the scheduler table loaded
                    if ($('#schedulerTable').length != 0 && $('#schedulerTable .schedulerAttendeeRow').length > 0) {
                        clearInterval(Reminders.addDefaultReminderInterval);
                        Reminders.addReminder(null, Reminders.defaultValues.popup, Reminders.defaultValues.email, Reminders.defaultValues.timer_popup, Reminders.defaultValues.timer_email);
                        Reminders.createRemindersPostData();
                    }
                }, 1);
            }
        }

    },

    initViaData: function(eventModule, defaultInvitees, returnAction) {
        Reminders.defaultInvitees = defaultInvitees;
    },

    onPopupChkboxClick: function(e) {
        Reminders.createRemindersPostData();
    },

    onEmailChkboxClick: function(e) {
        Reminders.createRemindersPostData();
    },

    onPopupTimerSelChange: function(e) {
        Reminders.createRemindersPostData();
    },

    onEmailTimerSelChange: function(e) {
        Reminders.createRemindersPostData();
    },

    //isValid:function (formname, nameIndex) {
    isValid:function (nameIndex) {
        Reminders.clearErrors();
        try {
            //var remindersData = JSON.parse(document[formname][nameIndex].value);
            var remindersData = JSON.parse(document.getElementById(nameIndex).value);
            $.each(remindersData, function(i,e){
                if(!e.popup && !e.email) {
                    Reminders.addError(e, SUGAR.language.get('app_strings', 'ERR_REMINDER_IS_NOT_SET_POPUP_OR_EMAIL'));
                }
                if(e.invitees.length == 0) {
                    Reminders.addError(e, SUGAR.language.get('app_strings', 'ERR_NO_INVITEES_FOR_REMINDER'));
                }
            });
        }
        catch(e) {
            throw e;
        }

        Reminders.showErrors();

        return Reminders.getErrors().length > 0 ? false : true;
    },

    clearErrors: function() {
        Reminders.errors = [];
    },

    addError: function(elem, msg) {
        Reminders.errors.push({'elem':elem, 'msg':msg});
    },

    getErrors: function() {
        return Reminders.errors;
    },

    showErrors: function() {
        $('.reminder_item').removeClass('reminder-error');
        $('.reminder_item .error-msg').html('');
        $.each(Reminders.getErrors(), function(i,err){
            var _err = err;
            $('.reminder_item').each(function(i,elem){
                if( (_err.elem.id && $(elem).attr('data-reminder-id')==_err.elem.id) || (_err.elem.idx==i) ) {
                    $(elem).find('.error-msg').html(_err.msg);
                    $(elem).addClass('reminder-error');
                }
            });
        });
    }

};
