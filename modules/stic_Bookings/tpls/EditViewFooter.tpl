
{*
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
 *}
<input type="hidden" id="periodic_action" name="periodic_action" value="createPeriodicBookingsRecords">
<input type="hidden" name="repeat_parent_id">

<div class="right-aligned-container">
  <div id="repeat_options" style="display: none;">
    <h2 id="repeatTitle">{$MOD.LBL_PERIODIC_BOOKING}</h2>
    <table class="BookingRepeatForm" width="100%" border="0" cellpadding="0" cellspacing="0">
        <tr>
            <td width="12.5%" valign="top" scope="row">{$MOD.LBL_REPEAT_TYPE}:</td>
            <td width="37.5%" valign="top">
                <select name="repeat_type" id="repeat_type" onchange="toggle_repeat_type();">
                    {html_options options=$APPLIST.repeat_type_dom selected=$selected_repeat_type|default:''}
                </select>
            </td>
        </tr>

        <tr id="repeat_interval_row">
            <td valign="top" scope="row">{$MOD.LBL_REPEAT_INTERVAL}:</td>
            <td valign="top">
                <select name="repeat_interval">
                    {html_options options=$repeat_intervals selected=$selected_repeat_interval|default:'1'}
                </select>
                <span id="repeat-interval-text"></span>
            </td>
        </tr>
        <tr id="repeat_end_row">
            <td width="12.5%" valign="top" scope="row">{$MOD.LBL_REPEAT_END}:</td>
            <td width="37.5%" valign="top">
                <div>
                	<input type="radio" name="repeat_end_type" value="number" id="repeat_count_radio" checked
							onclick="toggle_repeat_end();" style="position: relative; top: -5px;">
                    {$MOD.LBL_REPEAT_END_AFTER}
                    <input type="number" size="3" name="repeat_count" value="{$selected_repeat_count|default:'1'}"> {$MOD.LBL_REPEAT_OCCURRENCES}
                </div>

                <div>
                    <input type="radio" name="repeat_end_type" id="repeat_until_radio" value="until"
                           {if $selected_repeat_end_type eq 'until'}checked{/if}
                           onclick="toggle_repeat_end();" style="position: relative; top: -5px;">
                    {$MOD.LBL_REPEAT_END_BY}
                    <input type="text" class="date_input" size="11" maxlength="10" id="repeat_until_input"
                           name="repeat_until" value="{$selected_repeat_until|default:''}" 
                           {if $selected_repeat_end_type neq 'until'}disabled{/if}>
                    <img border="0" src="index.php?entryPoint=getImage&imageName=jscalendar.gif"
                         alt="{$APP.LBL_ENTER_DATE}" id="repeat_until_trigger" align="absmiddle"
                         {if $selected_repeat_end_type neq 'until'}style="display: none;"{else}style="display: inline;"{/if}>

                    <script type="text/javascript">
                        {literal}
                            Calendar.setup({
                                inputField: "repeat_until_input",
                                ifFormat: "%d/%m/%Y",
                                daFormat: "%d/%m/%Y",
                                button: "repeat_until_trigger",
                                singleClick: true,
                                dateStr: "",
                                step: 1,
                                startWeekday: {/literal}{$CALENDAR_FDOW|default:'1'}{literal},
                                weekNumbers: false
                            });
                        {/literal}
                    </script>
                </div>
            </td>
        </tr>
        <tr id="repeat_dow_row">
            <td width="12.5%" valign="top" scope="row">{$MOD.LBL_REPEAT_DOW}:</td>
            <td width="37.5%" valign="top">
                {foreach name=dow from=$dow key=i item=d}
                    {$d.label} <input type="checkbox" name="repeat_dow_{$d.index}" id="repeat_dow_{$d.index}"
                        {if isset($selected_repeat_dow[$d.index]) and $selected_repeat_dow[$d.index]}checked{/if}
                        style="margin-right: 10px;">
                {/foreach}
            </td>
        </tr>
    </table>
  </div>
</div>

<h2 id="resourcesTitle">{$RESOURCES_MOD.LBL_MODULE_NAME}  <button id="openCenterPopup" type="button" class="button">{$RESOURCES_MOD.LBL_CENTERS_BUTTON}</button></h2>
<div class="filter-box">
    <div id="resourceSearchFields" class="filter-content">
        <div id="selectedCentersContainer">
            <div id="selectedCentersList"></div>
        </div>
        
        <div class="filter-row">
            <div class="filter-item">
                <label for="resourcePlaceUserType">{$RESOURCES_MOD.LBL_USER_TYPE}</label>
                <select id="resourcePlaceUserType" name="resourcePlaceUserType" multiple></select>
            </div>
            
            <div class="filter-item">
                <label for="resourcePlaceType">{$RESOURCES_MOD.LBL_PLACE_TYPE}</label>
                <select id="resourcePlaceType" name="resourcePlaceType" multiple></select>
            </div>
        </div>
        
        <div class="filter-row">
            <div class="filter-item">
                <label for="resourceGender">{$RESOURCES_MOD.LBL_GENDER}</label>
                <select id="resourceGender" name="resourceGender" multiple></select>
            </div>
            
            <div class="filter-item">
                <label for="resourceName">{$RESOURCES_MOD.LBL_NAME}</label>
                <input type="text" id="resourceName" name="resourceName">
            </div>
        </div>
        
        <div class="filter-row">
            <div class="filter-item">
                <label for="numberOfPlaces">{$RESOURCES_MOD.LBL_NUMBER_OF_PLACES}</label>
                <input type="number" id="numberOfPlaces" name="numberOfPlaces">
            </div>
        </div>
        <div class="filter-actions grouped-buttons">
            <button id="loadCenterResourcesButton" type="button" class="button">
                {$MOD.LBL_ADD_BUTTON}
            </button>
            <button id="resetResourcesButton" type="button" class="button">
                {$MOD.LBL_UNDO_BUTTON}
            </button>
            <button id="deleteResourcesButton" type="button" class="button">
                {$APP.LBL_DELETE_BUTTON}
            </button>
            <span id="resourceCount"></span>
        </div>
    </div>
</div>
<br>
<table id="resourceLine" class="resource-table">
    <tr>
        {foreach from=$config_resource_fields key=field item=label}
            <th class="resource_column {if $field eq 'name'}resource_name{/if}
                {if $field eq 'hourly_rate' || $field eq 'daily_rate'}hidden-xs hidden-sm{/if}">
                {$label}
            </th>
        {/foreach}
        <th class="resource_column"></th>
    </tr>
</table>
<div style="padding-top: 2px">
    <input type="button" class="button" value="{$MOD.LBL_RESOURCES_ADD}" id="addResourceLine" />
</div>
<br>
{literal}
<script>
$(document).ready(function() {
    // Initialize Selectize on all multiple select elements
    $('#stic_resources_places_gender_list, #stic_resources_places_type_list, #stic_resources_places_users_list').selectize({
        plugins: ['remove_button'],
        delimiter: ',',
        persist: false,
        allowEmptyOption: false,
        create: false
    });
    
    {/literal}
    {if $is_session_reload}
    {literal}
    
    if (!typeFieldDisabled) {
        $('#periodic_booking').prop('checked', true);
    }
    
    $('#repeat_options').show();
    
    // Set the repeat_end_type radio button based on session data
    {/literal}
    {if $selected_repeat_end_type eq 'until'}
    {literal}
    $('#repeat_until_radio').prop('checked', true);
    $('#repeat_count_radio').prop('checked', false);
    {/literal}
    {else}
    {literal}
    $('#repeat_count_radio').prop('checked', true);
    $('#repeat_until_radio').prop('checked', false);
    {/literal}
    {/if}
    {literal}
    
    // Set the repeat_until input value
    {/literal}
    {if $selected_repeat_until}
    {literal}
    $('#repeat_until_input').val('{/literal}{$selected_repeat_until}{literal}');
    {/literal}
    {/if}
    {literal}
    
    // Call the toggle functions to set up the UI properly
    if (typeof toggle_repeat_type === 'function') {
        toggle_repeat_type();
    }
    
    if (typeof toggle_repeat_end === 'function') {
        toggle_repeat_end();
    }
    
    // Set day of week checkboxes for weekly repeats
    {/literal}
    {if $selected_repeat_type eq 'Weekly'}
    {foreach from=$selected_repeat_dow key=index item=value}
    {if $value}
    {literal}
    $('#repeat_dow_{/literal}{$index}{literal}').prop('checked', true);
    {/literal}
    {/if}
    {/foreach}
    {/if}
    {literal}
    {/literal}
    {/if}
    {literal}
});
</script>
<style>
        .BookingRepeatForm {
            width: 60% !important;
            table-layout: fixed;
        }

        .BookingRepeatForm td:first-child {
            width: 26% !important;
            white-space: nowrap;
            padding-right: 15px;
            text-align: left;
        }

        .BookingRepeatForm td:not(:first-child) {
            width: 75% !important;
        }

        .right-aligned-container {
            margin-left: 35px;
            width: 100%;
        }

        .resource-table .resouce_data_group>input {
            width: calc(100% - 85px);
        }

        .resource-table {
            width: 100%;
        }

        #resourceLine th {
            white-space: initial;
        }

        #resourceLine input.resource_color,
        #resourceLine input.resource_status,
        #resourceLine input.resource_hourly_rate,
        #resourceLine input.resource_daily_rate {
            max-width: 90px !important;
        }

        .resource-table th.resource_name {
            width: calc(20% + 85px);
            min-width: 250px;
        }

        .resource_data {
            color: grey;
            width: 95%;
            border-color: grey !important;
        }

        .filter-box {
            border: 1px solid #ddd;
            border-radius: 4px;
            background-color: #f9f9f9;
            padding: 15px;
            margin-bottom: 20px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .filter-content {
            display: flex;
            flex-direction: column;
            gap: 10px;
        }
        
        .filter-row {
            display: flex;
            flex-wrap: wrap;
            gap: 15px;
            margin-bottom: 10px;
        }
        
        .filter-item {
            flex: 1;
            min-width: 200px;
        }
        
        .filter-item label {
            display: block;
            margin-bottom: 5px;
            font-weight: bold;
        }
        
        .filter-item select,
        .filter-item input {
            width: 100%;
            padding: 6px 8px;
            border: 1px solid #ccc;
            border-radius: 4px;
        }
        .filter-actions {
            display: flex;
            align-items: center;
            gap: 8px;
            margin-top: 5px;
        }
        
        #resourceCount {
            font-weight: bold;
        }
        #numberOfPlaces {
            width: 100px;
        }
        #resourceName {
            height: 32px; 
            box-sizing: border-box;
            padding: 8px 10px;
            border: 1px solid #d0d0d0;
            border-radius: 3px;
            width: 90%;
            font-size: 13px;
        }

        #selectedCentersContainer {
            margin-bottom: 10px;
        }
</style>

<script>

    // Function to toggle repeat options based on repeat_booking checkbox
    window.toggle_repeat_booking = function() {
        var repeatBookingCheckbox = document.getElementById('periodic_booking');
        var repeatOptionsDiv = document.getElementById('repeat_options');
        
        if (!repeatBookingCheckbox || !repeatOptionsDiv) {
            return;
        }
        
        if (repeatBookingCheckbox.checked) {
            repeatOptionsDiv.style.display = '';
            // Reset repeat_type to empty so user must select one (unless loading from session)
            var repeatType = document.getElementById('repeat_type');
            if (repeatType && !repeatType.value) {
                repeatType.value = '';
                toggle_repeat_type();
            } else if (repeatType && repeatType.value) {
                toggle_repeat_type();
            }
        } else {
            repeatOptionsDiv.style.display = 'none';
            // Clear repeat_type value when hiding
            var repeatType = document.getElementById('repeat_type');
            if (repeatType) {
                repeatType.value = '';
            }
            // Clear validation if hiding options
            if (typeof validate != "undefined" && typeof validate['EditView'] != "undefined") {
                removeFromValidate('EditView', 'repeat_type');
                removeFromValidate('EditView', 'repeat_count');
                removeFromValidate('EditView', 'repeat_until');
            }
        }
    };


    window.toggle_repeat_type = function() {
        var repeatVal = document.getElementById('repeat_type').value;

        if (typeof validate != "undefined" && typeof validate['EditView'] != "undefined") {
            validate['EditView'] = undefined;
        }

        // Show/hide interval and end rows based on repeat type
        var repeatIntervalRow = document.getElementById("repeat_interval_row");
        var repeatEndRow = document.getElementById("repeat_end_row");
        var repeatDowRow = document.getElementById("repeat_dow_row");
        
        if (repeatVal == "") {
            // Hide all repeat detail rows when no type is selected
            if (repeatIntervalRow) repeatIntervalRow.style.display = "none";
            if (repeatEndRow) repeatEndRow.style.display = "none";
            if (repeatDowRow) repeatDowRow.style.display = "none";
        } else {
            // Show interval and end rows for any valid repeat type
            if (repeatIntervalRow) repeatIntervalRow.style.display = "";
            if (repeatEndRow) repeatEndRow.style.display = "";
            toggle_repeat_end();
            
            // Show/hide DOW row specifically for Weekly repeat type
            if (repeatVal == "Weekly") {
                if (repeatDowRow) repeatDowRow.style.display = "";
            } else {
                if (repeatDowRow) repeatDowRow.style.display = "none";
            }
        }

        // Set interval text
        var intervalTextElm = document.getElementById('repeat-interval-text');
        if (intervalTextElm && typeof SUGAR.language.languages.app_list_strings['repeat_intervals'] != 'undefined') {
            intervalTextElm.innerHTML = SUGAR.language.languages.app_list_strings['repeat_intervals'][repeatVal];
        }
    };

    window.toggle_repeat_end = function() {
    var repeatCountRadio = document.getElementById("repeat_count_radio");
    var repeatUntilRadio = document.getElementById("repeat_until_radio");
    var repeatUntilInput = document.getElementById("repeat_until_input");
    var repeatCountInput = document.querySelector("input[name='repeat_count']");
    var repeatUntilTrigger = document.getElementById("repeat_until_trigger");

    if (!repeatCountRadio || !repeatUntilRadio || !repeatUntilInput || !repeatCountInput || !repeatUntilTrigger) {
      return; // Exit if elements don't exist
    }

    if (repeatCountRadio.checked) {
      // Enable count, disable until
      repeatUntilInput.disabled = true;
      repeatCountInput.disabled = false;
      repeatUntilTrigger.style.display = "none";

      if (typeof validate != "undefined" && typeof validate['EditView'] != "undefined") {
        removeFromValidate('EditView', 'repeat_until');
      }
      if (typeof addToValidateMoreThan === 'function') {
        addToValidateMoreThan('EditView', 'repeat_count', 'int', true, '{/literal}{$MOD.LBL_REPEAT_COUNT}{literal}', 1);
      }
    } else if (repeatUntilRadio.checked) {
      // Enable until, disable count
      repeatCountInput.disabled = true;
      repeatUntilInput.disabled = false;
      repeatUntilTrigger.style.display = "";

      if (typeof validate != "undefined" && typeof validate['EditView'] != "undefined") {
        removeFromValidate('EditView', 'repeat_count');
      }
      if (typeof addToValidate === 'function') {
        addToValidate('EditView', 'repeat_until', 'date', true, '{/literal}{$MOD.LBL_REPEAT_UNTIL}{literal}');
      }
    }

    // Prevent an issue when a calendar date picker is hidden under a dialog
    var editContainer = document.getElementById('cal-edit_c');
    if (editContainer) {
      var pickerContainer = document.getElementById('container_repeat_until_trigger_c');
      if (pickerContainer) {
        pickerContainer.style.zIndex = editContainer.style.zIndex + 1;
      }
    }
    };

window.toggle_repeat_booking = function() {
    var repeatBookingCheckbox = document.getElementById('periodic_booking');
    var repeatOptionsDiv = document.getElementById('repeat_options');
    
    if (!repeatBookingCheckbox || !repeatOptionsDiv) {
        return;
    }
    
    if (repeatBookingCheckbox.checked) {
        repeatOptionsDiv.style.display = '';
        var repeatType = document.getElementById('repeat_type');
        if (repeatType && !repeatType.value) {
            repeatType.value = '';
            toggle_repeat_type();
        } else if (repeatType && repeatType.value) {
            toggle_repeat_type();
        }
    } else {
        repeatOptionsDiv.style.display = 'none';
        var repeatType = document.getElementById('repeat_type');
        if (repeatType) {
            repeatType.value = '';
        }
    }
};

window.toggle_repeat_type = function() {
    var repeatVal = document.getElementById('repeat_type').value;

    var repeatIntervalRow = document.getElementById("repeat_interval_row");
    var repeatEndRow = document.getElementById("repeat_end_row");
    var repeatDowRow = document.getElementById("repeat_dow_row");
    
    if (repeatVal == "") {
        if (repeatIntervalRow) repeatIntervalRow.style.display = "none";
        if (repeatEndRow) repeatEndRow.style.display = "none";
        if (repeatDowRow) repeatDowRow.style.display = "none";
    } else {
        if (repeatIntervalRow) repeatIntervalRow.style.display = "";
        if (repeatEndRow) repeatEndRow.style.display = "";
        toggle_repeat_end();
        
        if (repeatVal == "Weekly") {
            if (repeatDowRow) repeatDowRow.style.display = "";
        } else {
            if (repeatDowRow) repeatDowRow.style.display = "none";
        }
    }

    var intervalTextElm = document.getElementById('repeat-interval-text');
    if (intervalTextElm && typeof SUGAR.language.languages.app_list_strings['repeat_intervals'] != 'undefined') {
        intervalTextElm.innerHTML = SUGAR.language.languages.app_list_strings['repeat_intervals'][repeatVal];
    }
};

window.toggle_repeat_end = function() {
    var repeatCountRadio = document.getElementById("repeat_count_radio");
    var repeatUntilRadio = document.getElementById("repeat_until_radio");
    var repeatUntilInput = document.getElementById("repeat_until_input");
    var repeatCountInput = document.querySelector("input[name='repeat_count']");
    var repeatUntilTrigger = document.getElementById("repeat_until_trigger");

    if (!repeatCountRadio || !repeatUntilRadio || !repeatUntilInput || !repeatCountInput || !repeatUntilTrigger) {
        return;
    }

    if (repeatCountRadio.checked) {
        repeatUntilInput.disabled = true;
        repeatCountInput.disabled = false;
        repeatUntilTrigger.style.display = "none";

        if (typeof validate != "undefined" && typeof validate['EditView'] != "undefined") {
            removeFromValidate('EditView', 'repeat_until');
        }
        if (typeof addToValidateMoreThan === 'function') {
            addToValidateMoreThan('EditView', 'repeat_count', 'int', true, 'Repeat Count', 1);
        }
    } else if (repeatUntilRadio.checked) {
        repeatCountInput.disabled = true;
        repeatUntilInput.disabled = false;
        repeatUntilTrigger.style.display = "";

        if (typeof validate != "undefined" && typeof validate['EditView'] != "undefined") {
            removeFromValidate('EditView', 'repeat_count');
        }
        if (typeof addToValidate === 'function') {
            addToValidate('EditView', 'repeat_until', 'date', true, 'Repeat Until');
        }
    }

    var editContainer = document.getElementById('cal-edit_c');
    if (editContainer) {
        var pickerContainer = document.getElementById('container_repeat_until_trigger_c');
        if (pickerContainer) {
            pickerContainer.style.zIndex = editContainer.style.zIndex + 1;
        }
    }
};

document.addEventListener('DOMContentLoaded', function () {
    var repeatBookingCheckbox = document.getElementById('periodic_booking');
    if (repeatBookingCheckbox) {
        repeatBookingCheckbox.addEventListener('change', toggle_repeat_booking);
        toggle_repeat_booking();
    }
    
    var repeatType = document.getElementById('repeat_type');
    if (repeatType) {
        repeatType.addEventListener('change', toggle_repeat_type);
        toggle_repeat_type();
    }
    
    var form = document.getElementById('EditView');
    
    if (form) {
        form.addEventListener('submit', function(e) {
            e.preventDefault();
            
            if (typeof check_form === 'function') {
                var validationResult = check_form('EditView');
                
                if (!validationResult) {
                    return false;
                }
            } else {
                console.error('check_form no está disponible');
            }
            
            var repeatBookingCheckbox = document.getElementById('periodic_booking');
            var repeatType = document.getElementById('repeat_type');
            var isPeriodic = repeatBookingCheckbox && 
                             repeatBookingCheckbox.checked && 
                             repeatType && 
                             repeatType.value && 
                             repeatType.value !== "" && 
                             repeatType.value.toLowerCase() !== "none";
            
            
            var resourceLines = document.querySelectorAll('#resourceLine tr.resource_row');
            if (resourceLines.length > 0) {
                resourceLines.forEach(function(row, index) {
                    var resourceId = row.getAttribute('data-id');
                    var resourceName = row.querySelector('.resource_name input').value;
                    
                    if (!document.getElementById('resource_id_' + index)) {
                        var hiddenIdInput = document.createElement('input');
                        hiddenIdInput.type = 'hidden';
                        hiddenIdInput.name = 'resource_ids[]';
                        hiddenIdInput.id = 'resource_id_' + index;
                        hiddenIdInput.value = resourceId;
                        form.appendChild(hiddenIdInput);
                        
                        var hiddenNameInput = document.createElement('input');
                        hiddenNameInput.type = 'hidden';
                        hiddenNameInput.name = 'resource_names[]';
                        hiddenNameInput.id = 'resource_name_' + index;
                        hiddenNameInput.value = resourceName;
                        form.appendChild(hiddenNameInput);
                    }
                });
            }
            
            if (isPeriodic) {
                
                if (!document.querySelector('input[name="action"][value="editview"]')) {
                    $('<input>').attr({type: 'hidden', name: 'action', value: 'editview'}).appendTo(form);
                    $('<input>').attr({type: 'hidden', name: 'module', value: 'stic_Bookings'}).appendTo(form);
                    $('<input>').attr({type: 'hidden', name: 'periodic_action', value: 'createPeriodicBookingsRecords'}).appendTo(form);
                }
                
                if (document.getElementById('repeat_count_radio') && document.getElementById('repeat_count_radio').checked) {
                    var repeatCount = document.querySelector('input[name="repeat_count"]');
                    if (repeatCount && !document.querySelector('input[name="repeat_count_hidden"]')) {
                        $('<input>').attr({type: 'hidden', name: 'repeat_count_hidden', value: repeatCount.value}).appendTo(form);
                    }
                }
                
                if (document.getElementById('repeat_until_radio') && document.getElementById('repeat_until_radio').checked) {
                    var repeatUntil = document.querySelector('input[name="repeat_until"]');
                    if (repeatUntil && !document.querySelector('input[name="repeat_until_hidden"]')) {
                        $('<input>').attr({type: 'hidden', name: 'repeat_until_hidden', value: repeatUntil.value}).appendTo(form);
                    }
                }
                
                var repeatIntervalSelect = document.querySelector('select[name="repeat_interval"]');
                if (repeatIntervalSelect && !document.querySelector('input[name="repeat_interval_hidden"]')) {
                    $('<input>').attr({type: 'hidden', name: 'repeat_interval_hidden', value: repeatIntervalSelect.value}).appendTo(form);
                }
                
                if (repeatType.value === 'Weekly') {
                    var dowCheckboxes = document.querySelectorAll('input[id^="repeat_dow_"]');
                    dowCheckboxes.forEach(function(checkbox) {
                        if (checkbox.checked) {
                            var dowIndex = checkbox.id.replace('repeat_dow_', '');
                            if (!document.querySelector('input[name="repeat_dow_' + dowIndex + '_hidden"]')) {
                                $('<input>').attr({type: 'hidden', name: 'repeat_dow_' + dowIndex + '_hidden', value: '1'}).appendTo(form);
                            }
                        }
                    });
                }
                
                if (!document.querySelector('input[name="repeat_type_hidden"]')) {
                    $('<input>').attr({type: 'hidden', name: 'repeat_type_hidden', value: repeatType.value}).appendTo(form);
                }
            }
            
            form.removeEventListener('submit', arguments.callee);
            HTMLFormElement.prototype.submit.call(form);
        });
    }
});
</script>
{/literal}
{{include file='include/EditView/footer.tpl'}}