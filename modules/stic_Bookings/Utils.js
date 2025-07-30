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
var module = "stic_Bookings";
var resourceLineCount = 0;
var resourceMaxCount = 0;
var selectedCenters = [];
var resourceGroups = [];
var currentGroupIndex = 0;
var bookingId = $('[name="record"]').val()
? $('[name="record"]').val()
: $(".listview-checkbox", $(".inlineEditActive").closest("tr")).val();

/* INCLUDES */
// Load moment.js to use in validations
loadScript("include/javascript/moment.min.js");

/* VALIDATION DEPENDENCIES */
var validationDependencies = {
  end_date: "start_date",
  start_date: "end_date",
  planned_end_date: "planned_start_date",
  planned_start_date: "planned_end_date",
};

/* VALIDATION CALLBACKS */
addToValidateCallback(
  getFormName(),
  "end_date",
  "date",
  false,
  SUGAR.language.get(module, "LBL_RESOURCES_END_DATE_ERROR"),
  function () {
    return JSON.parse(
      checkStartAndEndDatesCoherence("start_date", "end_date", false)
    );
  }
);

addToValidateCallback(
  getFormName(),
  "start_date",
  "date",
  false,
  SUGAR.language.get(module, "LBL_RESOURCES_START_DATE_ERROR"),
  function () {
    return JSON.parse(
      checkStartAndEndDatesCoherence("start_date", "end_date", false)
    );
  }
);

addToValidateCallback(
  getFormName(),
  "planned_start_date",
  "date",
  false,
  SUGAR.language.get(module, "LBL_RESOURCES_PLANNED_START_DATE_ERROR"),
  function () {
    return JSON.parse(
      checkStartAndEndDatesCoherence("planned_start_date", "planned_end_date", false)
    );
  }
);

addToValidateCallback(
  getFormName(),
  "planned_end_date",
  "date",
  false,
  SUGAR.language.get(module, "LBL_RESOURCES_PLANNED_END_DATE_ERROR"),
  function () {
    return JSON.parse(
      checkStartAndEndDatesCoherence("planned_start_date", "planned_end_date", false)
    );
  }
);

addToValidateCallback(
  getFormName(),
  "status",
  "enum",
  true,
  SUGAR.language.get(module, "LBL_RESOURCES_STATUS_ERROR"),
  function () {
    return JSON.parse(isResourceAvailable());
  }
);
addToValidateCallback(
  getFormName(),
  "resource_name0",
  "text",
  false,
  SUGAR.language.get(module, "LBL_RESOURCES_EMPTY_RESOURCES_ERROR"),
  function () {
    if (!resourceLineWithData(resourceMaxCount)) {
      return confirm(SUGAR.language.get(module, "LBL_RESOURCES_EMPTY_RESOURCES_ERROR_DIALOG"));
    } else {
      var typesValid = checkBookingResourceTypes();
      if (!typesValid) {
        alert(SUGAR.language.get(module, "LBL_RESOURCES_TYPE_MIX_ERROR"));
        return false;
      }
      return true;
    }
  }
);

/* VIEWS CUSTOM CODE */
switch (viewType()) {
  case "edit":
  case "quickcreate":

    if (typeof resources !== 'undefined' && resources.length > 0) {
      if ($("#place_booking").is(":checked") && $('[name="record"]').val()) {
          loadExistingResourcesData();
      } else {
        resources.forEach((resource) => {
          insertResourceLine();
          populateResourceLine(resource, resourceMaxCount - 1);
        });
      }
    } else {
        insertResourceLine();
    }
    if ($("#place_booking").is(":checked")) {
      $("#place_booking").trigger('change');
    }

    // Set event to add more lines in the resources area when needed
    $("#addResourceLine").click(function () {
      insertResourceLine();
    });
    $("#deleteResourcesButton").click(function () {
      deleteAllResources();
    });
    
    $("#resetResourcesButton").click(function () {
      deleteLastResourceGroup();
    });
    manualStartDateHours = "10";
    manualStartDateMinutes = "00";
    manualEndDateHours = "10";
    manualEndDateMinutes = "30";
    manualPlannedStartHours = "10";
    manualPlannedStartMinutes = "00";
    manualPlannedEndHours = "10";
    manualPlannedEndMinutes = "30";

    // With all_day loadCenterResourcesButtoned the DateTime fields shouldn't display the time section
    // and the end_date should display one day less
    if ($("#all_day", "form").is(":checked")) {
      $("#start_date_hours").val("00");
      $("#start_date_minutes").val("00");
      $("#end_date_hours").val("00");
      $("#end_date_minutes").val("00");

      // Update planned date fields as well
      $("#planned_start_date_hours").val("00");
      $("#planned_start_date_minutes").val("00");
      $("#planned_end_date_hours").val("00");
      $("#planned_end_date_minutes").val("00");

      $("#start_date_hours").change();
      $("#start_date_minutes").change();
      $("#end_date_hours").change();
      $("#end_date_minutes").change();

      // Trigger change on planned date fields
      $("#planned_start_date_hours").change();
      $("#planned_start_date_minutes").change();
      $("#planned_end_date_hours").change();
      $("#planned_end_date_minutes").change();

      $("#start_date_time_section").parent().hide();
      $("#end_date_time_section").parent().hide();

      // Hide planned date time sections
      $("#planned_start_date_time_section").parent().hide();
      $("#planned_end_date_time_section").parent().hide();
      if ($("#end_date_date").val()) {
        var formatString = cal_date_format
            .replace(/%/g, "")
            .toLowerCase()
            .replace(/y/g, "yy")
            .replace(/m/g, "mm")
            .replace(/d/g, "dd");
        endDate = $.datepicker.parseDate(formatString, $("#end_date_date").val());
        endDate.setDate(endDate.getDate() - 1);
        endDateValue = $.datepicker.formatDate(formatString, endDate);
        $("#end_date_date").val(endDateValue);
        $("#end_date_date").change();

        if ($("#planned_end_date_date").val()) {
          plannedEndDate = $.datepicker.parseDate(
            formatString,
            $("#planned_end_date_date").val()
          );
          plannedEndDate.setDate(plannedEndDate.getDate() - 1);
          plannedEndDateValue = $.datepicker.formatDate(
            formatString,
            plannedEndDate
          );
          $("#planned_end_date_date").val(plannedEndDateValue);
          $("#planned_end_date_date").change();
        }
      }
    }
    $("#all_day", "form").on("change", function () {
      if ($("#all_day", "form").is(":checked")) {
        manualStartDateHours = $("#start_date_hours").val();
        manualStartDateMinutes = $("#start_date_minutes").val();
        manualEndDateHours = $("#end_date_hours").val();
        manualEndDateMinutes = $("#end_date_minutes").val();

        manualPlannedStartHours = $("#planned_start_date_hours").val();
        manualPlannedStartMinutes = $("#planned_start_date_minutes").val();
        manualPlannedEndHours = $("#planned_end_date_hours").val();
        manualPlannedEndMinutes = $("#planned_end_date_minutes").val();

        $("#start_date_hours").val("00");
        $("#start_date_minutes").val("00");
        $("#end_date_hours").val("00");
        $("#end_date_minutes").val("00");

        $("#planned_start_date_hours").val("00");
        $("#planned_start_date_minutes").val("00");
        $("#planned_end_date_hours").val("00");
        $("#planned_end_date_minutes").val("00");

        $("#start_date_hours").change();
        $("#start_date_minutes").change();
        $("#end_date_hours").change();
        $("#end_date_minutes").change();

        $("#planned_start_date_hours").change();
        $("#planned_start_date_minutes").change();
        $("#planned_end_date_hours").change();
        $("#planned_end_date_minutes").change();

        $("#start_date_time_section").parent().hide();
        $("#end_date_time_section").parent().hide();

        $("#planned_start_date_time_section").parent().hide();
        $("#planned_end_date_time_section").parent().hide();
      } else {
        $("#start_date_hours").val(manualStartDateHours);
        $("#start_date_minutes").val(manualStartDateMinutes);
        $("#end_date_hours").val(manualEndDateHours);
        $("#end_date_minutes").val(manualEndDateMinutes);

        $("#planned_start_date_hours").val(manualPlannedStartHours);
        $("#planned_start_date_minutes").val(manualPlannedStartMinutes);
        $("#planned_end_date_hours").val(manualPlannedEndHours);
        $("#planned_end_date_minutes").val(manualPlannedEndMinutes);

        $("#start_date_hours").change();
        $("#start_date_minutes").change();
        $("#end_date_hours").change();
        $("#end_date_minutes").change();

        $("#planned_start_date_hours").change();
        $("#planned_start_date_minutes").change();
        $("#planned_end_date_hours").change();
        $("#planned_end_date_minutes").change();

        $("#start_date_time_section").parent().show();
        $("#end_date_time_section").parent().show();

        $("#planned_start_date_time_section").parent().show();
        $("#planned_end_date_time_section").parent().show();
      }
    });

    previousPlannedStartDate = $("#planned_start_date_date").val();
    previousPlannedStartHours = $("#planned_start_date_hours").val();
    previousPlannedStartMinutes = $("#planned_start_date_minutes").val();
    previousPlannedEndDate = $("#planned_end_date_date").val();
    previousPlannedEndHours = $("#planned_end_date_hours").val();
    previousPlannedEndMinutes = $("#planned_end_date_minutes").val();

    setInterval(function () {
      currentPlannedStartDate = $("#planned_start_date_date").val();
      currentPlannedStartHours = $("#planned_start_date_hours").val();
      currentPlannedStartMinutes = $("#planned_start_date_minutes").val();
      currentPlannedEndDate = $("#planned_end_date_date").val();
      currentPlannedEndHours = $("#planned_end_date_hours").val();
      currentPlannedEndMinutes = $("#planned_end_date_minutes").val();

      var startDateInput = $("#start_date_date");
      var startHoursInput = $("#start_date_hours");
      var startMinutesInput = $("#start_date_minutes");

      var endDateInput = $("#end_date_date");
      var endHoursInput = $("#end_date_hours");
      var endMinutesInput = $("#end_date_minutes");

      var now = moment();

      var currentStartDateVal = startDateInput.val();
      var currentStartDateObj = null;

      if (currentStartDateVal) {
          var momentFormatString = cal_date_format
              .replace(/%Y/g, 'YYYY')
              .replace(/%m/g, 'MM')
              .replace(/%d/g, 'DD')
              .replace(/%H/g, 'HH')
              .replace(/%M/g, 'mm');

          if (!$("#all_day", "form").is(":checked")) {
              var startDateHour = $("#start_date_hours").val();
              var startDateMinute = $("#start_date_minutes").val();
              if (startDateHour && startDateMinute) {
                  currentStartDateVal = currentStartDateVal + ' ' + startDateHour + ':' + startDateMinute;
                  momentFormatString = momentFormatString + ' HH:mm';
              }
          }
          currentStartDateObj = moment(currentStartDateVal, momentFormatString);
      }

      // Check if planned_start_date has changed
      if (currentPlannedStartDate !== previousPlannedStartDate ||
          currentPlannedStartHours !== previousPlannedStartHours ||
          currentPlannedStartMinutes !== previousPlannedStartMinutes) {

        // If start_date is empty OR start_date is a future/current date
        if (!currentStartDateVal || (currentStartDateObj && currentStartDateObj.isSameOrAfter(now, 'day'))) {
          startDateInput.val(currentPlannedStartDate).trigger("change");
          startHoursInput.val(currentPlannedStartHours).trigger("change");
          startMinutesInput.val(currentPlannedStartMinutes).trigger("change");
        }
        previousPlannedStartDate = currentPlannedStartDate;
        previousPlannedStartHours = currentPlannedStartHours;
        previousPlannedStartMinutes = currentPlannedStartMinutes;
      }

      var currentEndDateVal = endDateInput.val();
      var currentEndDateObj = null;

      if (currentEndDateVal) {
          var momentFormatStringEnd = cal_date_format
              .replace(/%Y/g, 'YYYY')
              .replace(/%m/g, 'MM')
              .replace(/%d/g, 'DD')
              .replace(/%H/g, 'HH')
              .replace(/%M/g, 'mm');

          if (!$("#all_day", "form").is(":checked")) {
              var endDateHour = $("#end_date_hours").val();
              var endDateMinute = $("#end_date_minutes").val();
              if (endDateHour && endDateMinute) {
                  currentEndDateVal = currentEndDateVal + ' ' + endDateHour + ':' + endDateMinute;
                  momentFormatStringEnd = momentFormatStringEnd + ' HH:mm';
              }
          }
          currentEndDateObj = moment(currentEndDateVal, momentFormatStringEnd);
      }

      // Check if planned_end_date has changed
      if (currentPlannedEndDate !== previousPlannedEndDate ||
          currentPlannedEndHours !== previousPlannedEndHours ||
          currentPlannedEndMinutes !== previousPlannedEndMinutes) {

        // If end_date is empty OR end_date is a future/current date
        if (!currentEndDateVal || (currentEndDateObj && currentEndDateObj.isSameOrAfter(now, 'day'))) {
          endDateInput.val(currentPlannedEndDate).trigger("change");
          endHoursInput.val(currentPlannedEndHours).trigger("change");
          endMinutesInput.val(currentPlannedEndMinutes).trigger("change");
        }
        previousPlannedEndDate = currentPlannedEndDate;
        previousPlannedEndHours = currentPlannedEndHours;
        previousPlannedEndMinutes = currentPlannedEndMinutes;
      }

    }, 500);

    // Set event listener for center popup
    $("#openCenterPopup").click(function () {
      openCenterPopup();
    });
    if ($("#place_booking").is(":checked")) {
      $("#openCenterPopup").show();
      updateLabelsBasedOnBookingType();
    } else {
      $("#openCenterPopup").hide();
    }

    // Set autofill mark beside field label
    setAutofill(["name"]);
    setAutofill(["total_amount"]);
    setAutofill(["copayment_amount"]);
    document
      .getElementById("place_booking")
      .addEventListener("change", function () {
        var currentCheckbox = this;
        var isChecked = currentCheckbox.checked;
        var hasResources = getTotalResourceCount() > 0;

        if (isChecked && hasResources) {
          if (!confirm(SUGAR.language.get(module, "LBL_CONFIRM_CHANGE_BOOKING_TYPE"))) {
            currentCheckbox.checked = false; 
            return;
          }
        }

        if (isChecked) {
          updateResourceFields();
          $("#openCenterPopup").show();
        } else {
          $("#openCenterPopup").hide();
          updateResourceFields();
        }
      });

    break;

  case "list":
  case "detail":
    const copaymentFieldListDetail = document.querySelector('[data-field="copayment_amount"]');
    if (copaymentFieldListDetail) {
      if ($("#place_booking").is(":checked")) {
        copaymentFieldListDetail.style.display = ""; 
      } else {
        copaymentFieldListDetail.style.display = "none";
      }
    }    
    break;
  default:
    break;
}

/* AUX FUNCTIONS */

// This function adds rows to the resources table at the bottom of the Editview
function insertResourceLine() {
  var isPlaceBooking = $("#place_booking").is(":checked");
  var fields = isPlaceBooking ? config_place_fields : config_resource_fields;

  if (!fields || !Array.isArray(fields)) {
    return;
  }

  ln = 0;
  // If there is any empty line, it won't add more lines
  for (var i = 0; i <= resourceMaxCount; i++) {
    if ($("#resource_id" + i).length && !$("#resource_id" + i).val()) {
      return ln;
    } else if (!$("#resource_id" + i).length) {
      ln = i;
      break;
    }
  }
  if ($("#resourceLine thead").length === 0) {
    var header = "<thead><tr>";
    fields.forEach(function (field) {
      header +=
        "<th class='resource_column " +
        field +
        "'>" +
        SUGAR.language.get(
          "stic_Bookings",
          "LBL_RESOURCES_" + field.toUpperCase()
        ) +
        "</th>";
    });
    header += "<th class='resource_column'></th></tr></thead>";
    $("#resourceLine").prepend(header);
  }

  var x = document.getElementById("resourceLine").insertRow(-1);
  x.id = "resourceLine" + ln;

  fields.forEach(function (field) {
    var cell = x.insertCell(-1);
    cell.className = "dataField " + field;

    if (field === "name") {
      cell.innerHTML =
        "<div class='resouce_data_group'> <input type='text' class='sqsEnabled yui-ac-input resouce_data_name' name='resource_name" +
        ln +
        "' id='resource_name" +
        ln +
        "' autocomplete='new-password' value='' title='' tabindex='3' >" +
        "<input type='hidden' name='resource_id[]' id='resource_id" +
        ln +
        "' value=''>" +
        "<span class='id-ff multiple'>" +
        "<button title='" +
        SUGAR.language.get("app_strings", "LBL_SELECT_BUTTON_TITLE") +
        "' type='button' class='button' name='btn_1' onclick='openResourceSelectPopup(" +
        ln +
        ")'>" +
        "<span class='suitepicon suitepicon-action-select'/></span></button>" +
        "<button type='button' name='btn_1' class='button lastChild' onclick='clearRow(this.form," +
        ln +
        ");'>" +
        "<span class='suitepicon suitepicon-action-clear'></span>" +
        "</span></div>";
    } else {
      var input = document.createElement("input");
      input.type = "text";
      input.className = "resource_data " + field;
      input.name = "resource_" + field + ln;
      input.id = "resource_" + field + ln;
      input.value = "";
      input.title = "";
      input.tabIndex = "3";
      input.readOnly = true;
      cell.appendChild(input);
    }
  });

  // Add remove button
  var removeCell = x.insertCell(-1);
  removeCell.innerHTML =
    "<input type='button' class='button' value='" +
    SUGAR.language.get("app_strings", "LBL_REMOVE") +
    "' tabindex='3' onclick='markResourceLineDeleted(" +
    ln +
    ")'>";

  // This is used to add the autofill functionality in the field. It searches records while writing the record name
  sqs_objects[getFormName() + "_resource_name" + ln] = {
    id: ln,
    form: getFormName(),
    method: "query",
    modules: ["stic_Resources"],
    group: "or",
    field_list: ["name", "id"].concat(
      fields.filter((field) => field !== "name")
    ),
    populate_list: ["resource_name" + ln, "resource_id" + ln].concat(
      fields
        .filter((field) => field !== "name")
        .map((field) => "resource_" + field + ln)
    ),
    conditions: [
      {
        name: "name",
        op: "like_custom",
        begin: "%",
        end: "%",
        value: "",
      },
    ],
    order: "name",
    limit: "30",
    post_onblur_function: "callbackResourceSelectQS(" + ln + ")",
    no_match_text: "No Match",
  };

  QSProcessedFieldsArray[getFormName() + "_resource_name" + ln] = false;
  enableQS(false);
  addToValidateCallback(
    getFormName(),
    "resource_name" + ln,
    "text",
    false,
    SUGAR.language.get(module, "LBL_RESOURCES_ERROR"),
    function (formName, resourceElement) {
      return isResourceAvailable(resourceElement.replace("name", "id"));
    }
  );
  resourceMaxCount++;
}
function resourceLineWithData(resourcesCount) {
  for (var i = 0; i <= resourceMaxCount; i++) {
    if ($("#resource_id" + i).length && $("#resource_id" + i).val()) {
      return true;
    }
  }
  return false;
}
function updateResourceFields() {
  var isPlaceBooking = $("#place_booking").is(":checked");
  var fields = isPlaceBooking ? config_place_fields : config_resource_fields;

  var header = "<tr>";
  fields.forEach(function (field) {
    var labelKey = isPlaceBooking ? 
      "LBL_PLACES_" + field.toUpperCase() : 
      "LBL_RESOURCES_" + field.toUpperCase();
    
    header +=
      "<th class='resource_column " +
      field +
      "'>" +
      SUGAR.language.get(
        "stic_Bookings",
        "LBL_RESOURCES_" + field.toUpperCase()
      ) +
      "</th>";
  });
  header += "<th class='resource_column'></th></tr>";
  $("#resourceLine thead").html(header);

  $("#resourceLine tbody").empty();
  resourceMaxCount = 0;

  insertResourceLine();
  if (!isPlaceBooking) {
    $(".filter-box").hide();
    $("#resourceSearchFields").hide();
    $("#resourcePlaceUserType").val("");
    $("#resourcePlaceType").val("");
    $("#resourceGender").val("");
    $("#resourceName").val("");
    $("#numberOfPlaces").val("");
  }
  updateLabelsBasedOnBookingType();
}

function updateLabelsBasedOnBookingType() {
  var isPlaceBooking = $("#place_booking").is(":checked");
  
  if (isPlaceBooking) {
    // Change to PLACES labels
    $("#resourcesTitle").html(SUGAR.language.get('stic_Bookings', 'LBL_PLACES') + '  <button id="openCenterPopup" type="button" class="button">' + SUGAR.language.get('stic_Bookings', 'LBL_CENTERS_BUTTON') + '</button>');
    $("#resourceNameLabel").text(SUGAR.language.get('stic_Bookings', 'LBL_PLACES_NAME'));
    $("#addResourceLine").val(SUGAR.language.get('stic_Bookings', 'LBL_RESOURCES_PLACES_ADD'));
    $("#openCenterPopup").show(); 
  } else {
    // Change to RESOURCES labels
    $("#resourcesTitle").html(SUGAR.language.get('stic_Bookings', 'LBL_RESOURCES') + '  <button id="openCenterPopup" type="button" class="button">' + SUGAR.language.get('stic_Bookings', 'LBL_CENTERS_BUTTON') + '</button>');
    $("#resourceNameLabel").text(SUGAR.language.get('stic_Bookings', 'LBL_RESOURCES_NAME'));
    $("#addResourceLine").val(SUGAR.language.get('stic_Bookings', 'LBL_RESOURCES_ADD'));
    $("#openCenterPopup").hide(); 
  }
  
  $("#openCenterPopup").off('click').on('click', function() {
    openCenterPopup();
  });
}

// Delete a resource row
function markResourceLineDeleted(ln) {
  var isPlaceBooking = $("#place_booking").is(":checked");
  var fields = isPlaceBooking ? config_place_fields : config_resource_fields;
  
  $("#resource_id" + ln).val("");
  $("#resource_name" + ln).val("");
  fields.forEach(function (field) {
    $("#resource_" + field + ln).val("");
    if (field === "color") {
      $("#resource_" + field + ln).css("background-color", "");
    }
  });
  
  $("#resourceLine" + ln).remove();
  
  resourceGroups.forEach(function (group) {
    var indexInGroup = group.resourceLines.indexOf(ln);
    if (indexInGroup > -1) {
      group.resourceLines.splice(indexInGroup, 1);
    }
  });
  
  resourceGroups = resourceGroups.filter(function (group) {
    return group.resourceLines.length > 0;
  });
  
  reorganizeResourceLines();
  
  $("#resourceCount").text(
    SUGAR.language.get("stic_Bookings", "LBL_CENTERS_MESSAGE") + getTotalResourceCount()
  );
  
  if (!resourceLineWithData(resourceMaxCount)) {
    insertResourceLine();
  }
}

function openResourceSelectPopup(ln) {
  var isPlaceBooking = $("#place_booking").is(":checked");
  var fields = isPlaceBooking ? config_place_fields : config_resource_fields;

  var field_to_name_array = {
    id: "resource_id",
  };

  fields.forEach(function (field) {
    field_to_name_array[field] = "resource_" + field;
  });

  var popupRequestData = {
    call_back_function: "callbackResourceSelectPopup",
    passthru_data: { ln: ln },
    form_name: "EditView",
    field_to_name_array: field_to_name_array,
  };

  var resourceTypes =
    SUGAR.language.languages["app_list_strings"]["stic_resources_types_list"];
  var filteredTypes = Object.keys(resourceTypes).filter(function (type) {
    return isPlaceBooking
      ? type === "place"
      : type !== "place";
  });

  var typeQuery = filteredTypes
    .map(function (type) {
      return "&type_advanced[]=" + encodeURIComponent(type);
    })
    .join("");

  open_popup(
    "stic_Resources",
    600,
    400,
    typeQuery,
    true,
    false,
    popupRequestData
  );
}

function callbackResourceSelectQS(ln) {
  var isPlaceBooking = $("#place_booking").is(":checked");
  var fields = isPlaceBooking ? config_place_fields : config_resource_fields;

  if ($("#resource_id" + ln).val()) {
    fields.forEach(function (field) {
      if (field === "hourly_rate" || field === "daily_rate") {
        $("#resource_" + field + ln).val(
          myFormatNumber($("#resource_" + field + ln).val())
        );
      } else if (field === "color") {
        $("#resource_" + field + ln).colorPicker({ opacity: false });
      }
    });
  }
}

var fromPopupReturn = false;
// Callback function used after the Popup that select resources
function callbackResourceSelectPopup(popupReplyData) {
  fromPopupReturn = true;
  var nameToValueArray = popupReplyData.name_to_value_array;
  populateResourceLine(nameToValueArray, popupReplyData.passthru_data.ln);
}

// Fill a resource row with its data
function populateResourceLine(resource, ln) {
  Object.keys(resource).forEach(function (key, index) {
    $("#" + key + ln).val(resource[key]);
  }, resource);
  $("#resource_color" + ln).colorPicker();
}

// Function that asks the server if a resource is available within certain dates
function isResourceAvailable(resourceElement = null) {
  bookingId = $('[name="record"]').val()
    ? $('[name="record"]').val()
    : $(".listview-checkbox", $(".inlineEditActive").closest("tr")).val();
  if (
    $("#all_day", "form").is(":checked") ||
    getFieldValue("end_date").indexOf(" ") == -1
  ) {
    if (getFieldValue("end_date")) {
      var formatString = cal_date_format
        .replace(/%/g, "")
        .toLowerCase()
        .replace(/y/g, "yy")
        .replace(/m/g, "mm")
        .replace(/d/g, "dd");
      endDate = $.datepicker.parseDate(formatString, getFieldValue("end_date"));
      endDate.setDate(endDate.getDate() + 1);
      endDateValue = $.datepicker.formatDate(formatString, endDate);
    }
  } else {
    endDateValue = getFieldValue("end_date");
  }
  if (
    getFieldValue("start_date") &&
    typeof endDateValue !== "undefined" &&
    endDateValue != "" &&
    getFieldValue("status") != "cancelled"
  ) {
    $.ajax({
      url: "index.php?module=stic_Bookings&action=isResourceAvailable&sugar_body_only=true",
      dataType: "json",
      async: false,
      data: {
        startDate: dateToYMDHM(getDateObject(getFieldValue("start_date"))),
        endDate: dateToYMDHM(getDateObject(endDateValue)),
        resourceId: resourceElement ? $("#" + resourceElement).val() : null,
        bookingId: bookingId,
      },
      success: function (res) {
        if (res.success) {
          resourcesAllowed = res.resources_allowed;
        } else {
        }
      },
      error: function () {
        alert("Error send Request");
      },
    });
    if (resourcesAllowed) {
      return true;
    } else {
      return false;
    }
  }
  return true;
}

// Clean a resource row
function clearRow(form, ln) {
  SUGAR.clearRelateField(form, `resource_name${ln}`, `resource_id${ln}`);
  var isPlaceBooking = $("#place_booking").is(":checked");
  var fields = isPlaceBooking ? config_place_fields : config_resource_fields;

  fields.forEach(function (field) {
    $(`#resource_${field}${ln}`).val("");
    if (field === "color") {
      $(`#resource_${field}${ln}`).css("background-color", "");
    }
  });
}

function openCenterPopup() {
  var popupRequestData = {
    call_back_function: "callbackCenterSelectPopup",
    form_name: "EditView",
    field_to_name_array: {
      id: "center_id",
      name: "center_name",
    },
  };

  open_popup("stic_Centers", 600, 400, "", true, false, popupRequestData);
}

function callbackCenterSelectPopup(popupReplyData) {
  var centerId = popupReplyData.name_to_value_array.center_id;
  var centerName = popupReplyData.name_to_value_array.center_name;
  
  var centerAlreadySelected = selectedCenters.some(function(center) {
    return center.centerId === centerId;
  });
  
  if (centerAlreadySelected) {
    alert(SUGAR.language.get(module, "LBL_CENTER_ALREADY_SELECTED")+ ": " + centerName);
    return; 
  }
  
  selectedCenters.push({ centerId: centerId, centerName: centerName });
  updateSelectedCentersList();
  $("#selectedCenterName").text(centerName);
  $(".filter-box").show();
  $("#resourceSearchFields").show();
  if (selectedCenters.length === 1) {
    loadResourceTypes(centerId);
  }
  $("#loadCenterResourcesButton").off("click").on("click", loadResources);
}
$("#loadCenterResourcesButton").on("click", function () {
  loadResources();
});

function loadResources() {
  var startDate = getFieldValue("start_date");
  var endDate = getFieldValue("end_date");
  
  var resourcePlaceUserType = $("#resourcePlaceUserType")[0].selectize ? 
    $("#resourcePlaceUserType")[0].selectize.getValue() : 
    $("#resourcePlaceUserType").val();
  
  var resourcePlaceType = $("#resourcePlaceType")[0].selectize ? 
    $("#resourcePlaceType")[0].selectize.getValue() : 
    $("#resourcePlaceType").val();
  
  var resourceGender = $("#resourceGender")[0].selectize ? 
    $("#resourceGender")[0].selectize.getValue() : 
    $("#resourceGender").val();
  
  var resourceName = $("#resourceName").val();
  var numberOfPlaces = $("#numberOfPlaces").val();

  if (startDate === "" || endDate === "") {
    add_error_style(
      "EditView",
      "start_date",
      SUGAR.language.get("app_strings", "ERR_MISSING_REQUIRED_FIELDS")
    );
    add_error_style(
      "EditView",
      "end_date",
      SUGAR.language.get("app_strings", "ERR_MISSING_REQUIRED_FIELDS")
    );
    return;
  }

  loadCenterResources(
    resourcePlaceUserType,
    resourcePlaceType,
    resourceGender,
    resourceName,
    numberOfPlaces
  );
}

$(document).ready(function() {
  $("#resourceSearchFields").hide();
  $(".filter-box").hide();
  $("#resourcePlaceUserType, #resourcePlaceType, #resourceGender").selectize({
      plugins: ['remove_button'],
      create: false,
      allowEmptyOption: true,
      multiple: true  
  });
});
function updateSelectedCentersList() {
  var list = $("#selectedCentersList");
  list.empty();
  selectedCenters.forEach(function (center, index) {
    list.append(
      "<div>" +
        center.centerName +
        " <input type='button' class='removeCenterButton' data-index='" +
        index +
        "' value='" +
        SUGAR.language.get("app_strings", "LBL_REMOVE") +
        "'>" +
        "</div>"
    );
  });

  $(".removeCenterButton")
  .off("click")
  .on("click", function () {
    var index = $(this).data("index");
    var centerToRemove = selectedCenters[index];
    selectedCenters.splice(index, 1);
    updateSelectedCentersList();
      
      if (selectedCenters.length === 0) {
        $(".filter-box").hide();
        $("#resourceSearchFields").hide();
      }
  });
}
function loadResourceTypes(centerId) {
  $.ajax({
    url: "index.php?module=stic_Bookings&action=getResourceTypes&sugar_body_only=true",
    dataType: "json",
    data: { centerId: centerId },
    success: function (res) {
      if (res.success) {
        $(".filter-box").show();
        $("#resourceSearchFields").show();

        var userTypeSelectize = $("#resourcePlaceUserType")[0].selectize;
        var placeTypeSelectize = $("#resourcePlaceType")[0].selectize;
        var genderSelectize = $("#resourceGender")[0].selectize;
        
        if (userTypeSelectize) {
          userTypeSelectize.clearOptions();
          res.stic_resources_places_users_list.forEach(function(option) {
            userTypeSelectize.addOption({value: option.value, text: option.label});
          });
        }
        
        if (placeTypeSelectize) {
          placeTypeSelectize.clearOptions();
          res.stic_resources_places_type_list.forEach(function(option) {
            placeTypeSelectize.addOption({value: option.value, text: option.label});
          });
        }
        
        if (genderSelectize) {
          genderSelectize.clearOptions();
          res.stic_resources_places_gender_list.forEach(function(option) {
            genderSelectize.addOption({value: option.value, text: option.label});
          });
        }
      } else {
        $(".filter-box").hide();
        $("#resourceSearchFields").hide();
        alert(
          SUGAR.language.get(module, "LBL_RESOURCES_EMPTY_RESOURCES_ERROR")
        );
      }
    },
    error: function () {
      $(".filter-box").hide();
      $("#resourceSearchFields").hide();
      alert(SUGAR.language.get(module, "LBL_RESOURCES_EMPTY_RESOURCES_ERROR"));
    },
  });
}


function loadCenterResources(
  resourcePlaceUserType = "",
  resourcePlaceType = "",
  resourceGender = "",
  resourceName = "",
  numberOfPlaces = ""
) {
  var centerIds = selectedCenters.map((center) => center.centerId).join(",");
  var startDate = getDateObject(getFieldValue("start_date"));
  var endDate = getDateObject(getFieldValue("end_date"));
  
  var existingResourceIds = getCurrentResourceIds();

  $.ajax({
    url: "index.php?module=stic_Bookings&action=loadCenterResources&sugar_body_only=true",
    dataType: "json",
    data: {
      startDate: dateToYMDHM(startDate),
      endDate: dateToYMDHM(endDate),
      centerIds: centerIds,
      resourcePlaceUserType: resourcePlaceUserType,
      resourcePlaceType: resourcePlaceType,
      resourceGender: resourceGender,
      resourceName: resourceName,
      numberOfPlaces: numberOfPlaces,
      bookingId: bookingId,
      existingResourceIds: existingResourceIds.join(',')
    },
    success: function (res) {
      if (res.success) {
        if (res.resources.length > 0) {
          addNewResourceGroup(res.resources);
          $("#resourceCount").text(
            SUGAR.language.get("stic_Bookings", "LBL_CENTERS_MESSAGE") + getTotalResourceCount()
          );
        } else {
          alert(SUGAR.language.get("stic_Bookings", "LBL_NO_NEW_RESOURCES"));
        }
      } else {
        alert(
          SUGAR.language.get("stic_Bookings", "LBL_CENTER_RESOURCE_ERROR") + res.message
        );
      }
    },
    error: function (xhr, textStatus, errorThrown) {
      alert(SUGAR.language.get("stic_Bookings", "LBL_CENTER_RESOURCE_ERROR") + " " + textStatus);
    },
  });
}

function updateResourceLines(resources) {
  for (var i = 0; i < resourceMaxCount; i++) {
    $("#resourceLine" + i).remove();
  }
  resourceMaxCount = 0;
  resourceGroups = [];
  currentGroupIndex = 0;

  resources.forEach(function (resource) {
    insertResourceLine();
    populateResourceLine(resource, resourceMaxCount - 1);
  });
}
function loadExistingResourcesData() {
  var bookingId = $('[name="record"]').val();
  
  if (!bookingId) {
      return;
  }
  
  $.ajax({
      url: "index.php?module=stic_Bookings&action=loadExistingResources&sugar_body_only=true",
      dataType: "json",
      data: {
          bookingId: bookingId
      },
      success: function (res) {
          if (res.success && res.resources.length > 0) {
              for (var i = 0; i < resourceMaxCount; i++) {
                  $("#resourceLine" + i).remove();
              }
              resourceMaxCount = 0;
              
              res.resources.forEach(function (resource) {
                  insertResourceLine();
                  populateResourceLine(resource, resourceMaxCount - 1);
              });
          }
      },
      error: function(xhr, status, error) {
        console.error("Request error:", status, error);
      }
  });
}
function closeResource(resourceId, bookingId) {
  $.ajax({
    url: "index.php?module=stic_Bookings&action=validateResourceDates&sugar_body_only=true",
    dataType: "json",
    async: false,
    data: {
      record_id: bookingId,
    },
    success: function (response) {
      if (!response.valid) {
        alert(
          SUGAR.language.get("stic_Bookings", "LBL_CLOSE_RESOURCE_BEFORE_START_ERROR")
        );
        return;
      }

      if (
        confirm(
          SUGAR.language.get("stic_Bookings", "LBL_CLOSE_RESOURCE_CONFIRM")
        )
      ) {
        $.ajax({
          url: "index.php?module=stic_Bookings&action=closeResource&sugar_body_only=true",
          dataType: "json",
          data: {
            record_id: bookingId,
            resource_id: resourceId,
          },
          success: function (res) {
            if (res.success) {
              window.location.reload();
            } else {
              alert(res.message);
            }
          },
        });
      }
    },
  });
}
function addNewResourceGroup(resources) {
  var newGroup = {
    groupId: currentGroupIndex++,
    resourceLines: []
  };

  resources.forEach(function (resource) {
    insertResourceLine();
    var lineIndex = resourceMaxCount - 1;
    populateResourceLine(resource, lineIndex);
    
    $("#resourceLine" + lineIndex).css('background-color', '#E6E6E6');

    newGroup.resourceLines.push(lineIndex);
  });

  resourceGroups.push(newGroup);
  
  if (resourceGroups.length > 1) {
    var previousGroup = resourceGroups[resourceGroups.length - 2];
    previousGroup.resourceLines.forEach(function(lineIndex) {
      $("#resourceLine" + lineIndex).css('background-color', '');
    });
  }
}

function getTotalResourceCount() {
  var count = 0;
  for (var i = 0; i < resourceMaxCount; i++) {
    if ($("#resource_id" + i).length && $("#resource_id" + i).val()) {
      count++;
    }
  }
  return count;
}

function deleteAllResources() {
  if (confirm(SUGAR.language.get("stic_Bookings", "LBL_CONFIRM_DELETE_ALL_RESOURCES"))) {
    for (var i = 0; i < resourceMaxCount; i++) {
      $("#resourceLine" + i).remove();
    }
    resourceMaxCount = 0;
    resourceGroups = [];
    currentGroupIndex = 0;
    insertResourceLine();
    $("#resourceCount").text("");
  }
}

function deleteLastResourceGroup() {
  if (resourceGroups.length === 0) {
    alert(SUGAR.language.get("stic_Bookings", "LBL_NO_GROUPS_TO_DELETE"));
    return;
  }
  
  if (confirm(SUGAR.language.get("stic_Bookings", "LBL_CONFIRM_DELETE_LAST_GROUP"))) {
    var lastGroup = resourceGroups.pop();
    
    lastGroup.resourceLines.forEach(function(lineIndex) {
      $("#resourceLine" + lineIndex).remove();
    });
    
    resourceMaxCount = 0;
    $("tr[id^='resourceLine']").each(function() {
      var id = $(this).attr('id');
      var index = parseInt(id.replace('resourceLine', ''));
      if (index >= resourceMaxCount) {
        resourceMaxCount = index + 1;
      }
    });
    
    if (getTotalResourceCount() === 0) {
      insertResourceLine();
    } else {
      if (resourceGroups.length > 0) {
        var newLastGroup = resourceGroups[resourceGroups.length - 1];
        newLastGroup.resourceLines.forEach(function(lineIndex) {
          $("#resourceLine" + lineIndex).css('background-color', '#E6E6E6');
        });
      }
    }
  }
}

function reorganizeResourceLines() {
  var existingLines = [];
  var existingResources = [];

  for (var i = 0; i < resourceMaxCount; i++) {
    if ($("#resourceLine" + i).length && $("#resource_id" + i).val()) {
      var resourceData = {};
      var isPlaceBooking = $("#place_booking").is(":checked");
      var fields = isPlaceBooking
        ? config_place_fields
        : config_resource_fields;

      resourceData.resource_id = $("#resource_id" + i).val();
      fields.forEach(function (field) {
        resourceData["resource_" + field] = $("#resource_" + field + i).val();
      });

      existingResources.push(resourceData);
      existingLines.push(i);
    }
  }

  existingLines.forEach(function (lineIndex) {
    $("#resourceLine" + lineIndex).remove();
  });

  resourceMaxCount = 0;
  resourceGroups = [];
  currentGroupIndex = 0;

  existingResources.forEach(function (resource) {
    insertResourceLine();
    populateResourceLine(resource, resourceMaxCount - 1);
  });

  if (existingResources.length === 0) {
    insertResourceLine();
  }
}
function getCurrentResourceIds() {
  var existingResourceIds = [];

  $("tr[id^='resourceLine']").each(function () {
    var rowId = $(this).attr("id");
    var index = rowId.replace("resourceLine", "");
    var resourceId = $("#resource_id" + index).val();

    if (resourceId && resourceId.trim() !== "") {
      existingResourceIds.push(resourceId);
    }
  });

  return existingResourceIds;
}
function getResourceTypeAjax(resourceId) {
  var resourceType = null;
  $.ajax({
    url: "index.php?module=stic_Bookings&action=getResourceType&sugar_body_only=true",
    dataType: "json",
    async: false, 
    data: {
      resourceId: resourceId
    },
    success: function (res) {
      if (res.success) {
        resourceType = res.type;
      } else {
        console.error(res.message);
      }
    },
    error: function () {
      console.error(res.message);
    },
  });
  return resourceType;
}

function checkBookingResourceTypes() {
  var isPlaceBookingChecked = $("#place_booking").is(":checked");
  var resourceIds = [];
  
  for (var i = 0; i < resourceMaxCount; i++) {
    if ($("#resource_id" + i).length && $("#resource_id" + i).val()) {
      resourceIds.push($("#resource_id" + i).val());
    }
  }

  if (resourceIds.length === 0) {
    return true;
  }

  var containsPlace = false;
  var containsOther = false;

  for (var i = 0; i < resourceIds.length; i++) {
    var resourceType = getResourceTypeAjax(resourceIds[i]);
    if (resourceType === 'place') {
      containsPlace = true;
    } else {
      containsOther = true;
    }
  }

  if (isPlaceBookingChecked && containsOther) {
    return false
  }

  if (!isPlaceBookingChecked && containsPlace) {
    return false
  }

  if (containsPlace && containsOther) {
    return false;
  }
  
  return true;
}
