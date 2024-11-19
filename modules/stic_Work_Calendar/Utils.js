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

/* HEADER */
// Set module name
var module = "stic_Work_Calendar";

/* VALIDATION DEPENDENCIES */
var validationDependencies = {
  end_date: "start_date",
};

/* VALIDATION CALLBACKS */
addToValidateCallback(getFormName(), "type", "type", false, SUGAR.language.get(module, "LBL_INCOMPATIBLE_TYPE_WITH_EXISTING_RECORDS"), function () {
  return checkIfExistsOtherTypesIncompatibleRecords("start_date", "end_date", "type", "assigned_user_id");
});

/* VIEWS CUSTOM CODE */
var allDayTypes = ['vacation', 'holiday', 'personal', 'sick', 'leave'];
switch (viewType()) {
  case "edit":
  case "quickcreate":
  case "popup":    
    // Set autofill mark and disable editing
    setAutofill(["name"]);
    document.getElementById('name').disabled = true;
    // Manage view for all day types
    manageAllDayView();
    break;
    
  case "detail":
    break;

  case "list":
    button = {
      id: "bt_mass_update_dates_listview",
      title: SUGAR.language.get("stic_Work_Calendar", "LBL_MASS_UPDATE_DATES_BUTTON_TITTLE"),
      text: SUGAR.language.get("stic_Work_Calendar", "LBL_MASS_UPDATE_DATES_BUTTON_TITTLE"),
      onclick: "onClickMassUpdateDatesButton()",
    };
    createListViewButton(button);
    break;

  default:
    break;
}

/**
 * Used as a callback for mass update dates button
 */
function onClickMassUpdateDatesButton() {
  sugarListView.get_checks();
  if(sugarListView.get_checks_count() < 1) {
      alert(SUGAR.language.get('app_strings', 'LBL_LISTVIEW_NO_SELECTED'));
      return false;
  }
  document.MassUpdate.action.value='showMassUpdateDatesForm';
  document.MassUpdate.module.value='stic_Work_Calendar';
  document.MassUpdate.submit();
}


/**
 * Check that the difference between the end date and the start date is less than 24 hours.
 * It is necessary to load at the beginning of the page moment.js by "loadScript("include/javascript/moment.min.js");"
 * It is assumed that if start_date and end_date include hours and minutes, they will be in H:i (php) or HH:MM (momentjs) format
 *
 * @param {String} startDate name of the field whose date must be previous
 * @param {String} endDate name of the field whose date must be prior
 * @returns {Boolean} true if the difference between the end date and the start date is less than 24 hours, and false if not. 
 */
function checkStartAndEndDatesExcceds24Hours(startDate, endDate) 
{
  var userDateFormat = STIC.userDateFormat.toUpperCase();  
  var startDate = moment(getFieldValue(startDate), userDateFormat + "HH:mm");
  var endDate = moment(getFieldValue(endDate), userDateFormat + "HH:mm");

  // Calcular la diferencia entre las dos fechas en horas y verificar que sea menor a 24 horas
  const diferenciaHoras = endDate.diff(startDate, 'minutes');
  // Verificar si la diferencia es mayor a 24 horas
  if (diferenciaHoras > 24*60) {
      return false;
  } 
}


/**
 * Synchronous verification of whether there are Work Calendar records of incompatible type that match the assigned user and time range.
 * @returns {Boolean} false if there are records of incompatible types for the same assigned user and time range, true if there are not.
 */
function checkIfExistsOtherTypesIncompatibleRecords(startDate, endDate, type, assignedUserId) 
{
  //get Id of the record
  const queryString = window.location.search;
  const params = new URLSearchParams(queryString);
  const id = params.get('record') || document.querySelector('input[name="record"]').value;

  var data = {
    id: id,
    startDate: getFieldValue(startDate),
    endDate: getFieldValue(endDate),
    type: getFieldValue(type),
    assignedUserId: getFieldValue(assignedUserId),
  };

  const url = 'index.php?module=stic_Work_Calendar&action=existsOtherTypesIncompatibleRecords';
  var xhr = new XMLHttpRequest();
  xhr.open('POST', url, false); // set asyncto false
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.send(JSON.stringify(data));

  if (xhr.status === 200) {
    if (xhr.responseText == true) {
      return true;
    } else {
      return false;
    }
  } else {
    alert(SUGAR.language.get(module, "LBL_ERROR_REQUEST_INCOMPATIBLE_TYPE") + '\n\n' + SUGAR.language.get(module, "LBL_ERROR_CODE_REQUEST_INCOMPATIBLE_TYPE") + xhr.status);
    return false;
  }
}



/**
 * Check if the record is of type all day or not and show the corresponding view
 */
function manageAllDayView() 
{
  var type = document.getElementById("type");

  // Store default values in previous values
  var previousType = type.value;
  var previousStartDateHours = "09";
  var previousStartDateMinutes = "00";
  var previousEndDateHours = "18";
  var previousEndDateMinutes = "00";

  if (!allDayTypes.includes(type.value)) 
  {
    // Add the validation for end_date field
    addToValidateCallback(getFormName(), "end_date", "datetime", false, SUGAR.language.get(module, "LBL_END_DATE_ERROR"), function () {
      return checkStartAndEndDatesCoherence("start_date", "end_date", true);
    });
    addToValidateCallback(getFormName(), "end_date", "datetime", false, SUGAR.language.get(module, "LBL_END_DATE_EXCEEDS_24_HOURS"), function () {
      return checkStartAndEndDatesExcceds24Hours("start_date", "end_date");
    });
  } 
  else 
  { 
    // Hide the start time and the end_date section
    $("#start_date_time_section").parent().hide();
    document.querySelector('[data-field="end_date"]').style.display='none';
    // Remove the validation for end_date field
    removeFromValidate(getFormName(), "end_date");
  }

  type.addEventListener("change", function() 
  {
    var typeValue = document.getElementById("type").value;
    if (!allDayTypes.includes(typeValue)) 
    {
      if (allDayTypes.includes(previousType)) {      // Set the previous values if the previous type was not type: all day
        
        if (typeValue == 'canceled')
        {
          $("#start_date_hours").val('00');
          $("#start_date_minutes").val('00');
          $("#end_date_hours").val('00');
          $("#end_date_minutes").val('00');
        } else {
          $("#start_date_hours").val(previousStartDateHours);
          $("#start_date_minutes").val(previousStartDateMinutes);
          $("#end_date_hours").val(previousEndDateHours);
          $("#end_date_minutes").val(previousEndDateMinutes);
        }

        $("#start_date_hours").change();
        $("#start_date_minutes").change();
        $("#end_date_hours").change();
        $("#end_date_minutes").change();

        // Show the start time and the end_date section
        $("#start_date_time_section").parent().show();
        document.querySelector('[data-field="end_date"]').style.display='block';

        // Add the validation for end_date field
        addToValidateCallback(getFormName(), "end_date", "datetime", false, SUGAR.language.get(module, "LBL_END_DATE_ERROR"), function () {
          return checkStartAndEndDatesCoherence("start_date", "end_date", true);
        });
        addToValidateCallback(getFormName(), "end_date", "datetime", false, SUGAR.language.get(module, "LBL_END_DATE_EXCEEDS_24_HOURS"), function () {
          return checkStartAndEndDatesExcceds24Hours("start_date", "end_date");
        });
      }
    } 
    else 
    {
      if (!allDayTypes.includes(previousType)) {   
        // Store previous values
        previousStartDateHours = $("#start_date_hours").val();
        previousStartDateMinutes = $("#start_date_minutes").val();
        previousEndDateHours = $("#end_date_hours").val();
        previousEndDateMinutes = $("#end_date_minutes").val();

        // Set all day values
        $("#start_date_hours").val("00");
        $("#start_date_minutes").val("00");
        $("#end_date_hours").val("00");
        $("#end_date_minutes").val("00");
        $("#start_date_hours").change();
        $("#start_date_minutes").change();
        $("#end_date_hours").change();
        $("#end_date_minutes").change();      

        // Hide the start time and the end_date section
        $("#start_date_time_section").parent().hide();
        document.querySelector('[data-field="end_date"]').style.display='none';

        // Remove the validation for end_date field
        removeFromValidate(getFormName(), "end_date");
      }
    }
    previousType = type.value;
  });
}