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
var module = "stic_Bookings_Places_Calendar";
var updatedView = false;
loadScript("include/javascript/moment.min.js");

function initializeCalendar() {
  var calendarEl = document.getElementById("calendar");
  if (!calendarEl) {
    console.error("Calendar element not found");
    return;
  }

  var calendarLocale = lang || 'es';
  
  var calendar = new FullCalendar.Calendar(calendarEl, {
    locale: calendarLocale,
    views: {
      threeDays: {
        type: "timeGridWeek",
        duration: { days: 3 },
        buttonText: SUGAR.language.get(
          "stic_Bookings_Places_Calendar",
          "LBL_MOBILE_BUTTON"
        ),
      },
    },

    // Choose init view
    initialView:
      window.innerWidth >= 767
        ? calendarView
          ? calendarView
          : "dayGridMonth"
        : "threeDays",
        buttonText: {
          today: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_TODAY"),
          month: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_MONTH"),
          week: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_WEEK"),
          day: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_DAY"),
          list: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_LIST"),
          mobile: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_MOBILE_BUTTON"),
        },
        
    events: function (fetchInfo, successCallback, failureCallback) {
      var start = moment(fetchInfo.start).format("YYYY-MM-DD");
      var end = moment(fetchInfo.end).format("YYYY-MM-DD");

      $.ajax({
        url: "index.php?module=stic_Bookings_Places_Calendar&action=get_places_availability_data&sugar_body_only=true",
        method: "POST",
        data: {
          start: start,
          end: end,
        },
        success: function (response) {
          try {
            var data = JSON.parse(response);
            var events = [];
            for (var date in data) {
              let occupiedCount = 0;

              for (var booking in data[date].occupied) {
                for (var prop in data[date].occupied[booking]) {
                  if (Array.isArray(data[date].occupied[booking][prop])) {
                    occupiedCount += data[date].occupied[booking][prop].length;
                  }
                }
              }
              events.push({
                start: date,
                allDay: true,
                extendedProps: {
                  available: data[date].available,
                  occupied: occupiedCount,
                  occupiedInfo: data[date].occupied,
                },
              });
            }
            successCallback(events);
          } catch (e) {
            console.error("Error parsing availability data:", e);
            failureCallback(e);
          }
        },
        error: function (xhr, status, error) {
          console.error("Error fetching availability data:", error);
          failureCallback(error);
        },
      });
    },
    eventContent: function (arg) {
      let eventEl = document.createElement("div");
      eventEl.classList.add("custom-event-content");
      eventEl.innerHTML = `
                <div class="availability-info">
                    <span class="available">${arg.event.extendedProps.available}</span>
                    <img src="themes/SuiteP/images/icon_home.png" alt="Home" class="home-icon">
                    <span class="occupied">${arg.event.extendedProps.occupied}</span>
                </div>
            `;
      return { domNodes: [eventEl] };
    },

    eventDidMount: function (info) {
      var eventData = info.event.title;

      var title =
        '<div class="qtip-title-text">' +
        SUGAR.language.translate("app_strings", "LBL_ADDITIONAL_DETAILS") +
        "</div>" +
        '<div class="qtip-title-buttons"></div>';
      var occupiedInfo = info.event.extendedProps.occupiedInfo;

      var body = "";

      for (var booking in occupiedInfo) {
        if (occupiedInfo.hasOwnProperty(booking)) {
          if (occupiedInfo[booking].id && occupiedInfo[booking].name) {
            body += `<div><a href="index.php?action=DetailView&module=stic_Bookings&record=${occupiedInfo[booking].id}" style="color: #444444;">${booking}</a></div>`;
          }
          var centers = occupiedInfo[booking];
          for (var center in centers) {
            if (
              center !== "id" &&
              center !== "name" &&
              centers.hasOwnProperty(center)
            ) {
              body += `<div><strong>- ${SUGAR.language.get(
                "stic_Bookings_Places_Calendar",
                "LBL_STIC_CENTERS"
              )}: </strong>${center}</div>`;

              var resources = centers[center];
              body += `<div>${SUGAR.language.get(
                "stic_Bookings_Places_Calendar",
                "LBL_STIC_RESOURCES"
              )}</div><ul>`;
              if (Array.isArray(resources)) {
                resources.forEach(function (resource) {
                  body += `<li>•${resource}</li>`;
                });
              } else {
                console.warn("Resources is not an array:", resources);
                if (resources) {
                  body += `<li>•${resources}</li>`;
                }
              }
              body += "</ul>";
            }
          }
        }
      }
      $(info.el).qtip({
        content: {
          title: title,
          text: body,
        },
        position: {
          my: "bottom left",
          at: "top left",
          target: "mouse",
          adjust: {
            mouse: false,
          },
        },
        show: {
          solo: true,
        },
        hide: {
          event: "mouseleave",
          fixed: true,
          delay: 200,
        },
        style: {
          width: 224,
          padding: 5,
          color: "black",
          textAlign: "left",
          border: {
            width: 1,
            radius: 3,
          },
          tip: "bottomLeft",
          classes: {
            tooltip: "ui-widget",
            tip: "ui-widget",
            title: "ui-widget-header",
            content: "ui-widget-content",
          },
        },
      });
    },

    selectable: true,
    selectMirror: true,
    select: function (arg) {
      window.location.assign(
        "index.php?module=stic_Bookings&action=EditView&return_action=index&return_module=stic_Bookings_Calendar&start=" +
          arg.startStr +
          "&end=" +
          arg.endStr +
          "&allDay=" +
          arg.allDay
      );
    },
    headerToolbar: {
      left: 'prev,next today,filterButton',
      center: 'title',
      right: "dayGridMonth,timeGridWeek,timeGridDay,listWeek,threeDays",
    },
    customButtons: {
      filterButton: {
        text: SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_FILTERS"),
        click: function() {
          $('#filtersModal').modal('toggle');
        }
      },
    },    
    datesSet: function() {
      setTimeout(function () {
        $('.fc-filterButton-button')
          .html('<span class="glyphicon glyphicon-filter"></span> ');
        updateCrossVisibility();
      }, 0);
    },
    firstDay: start_weekday,
    // Define business hours. This will set a different color for non business hours
    businessHours: {
      daysOfWeek: [1, 2, 3, 4, 5],
      startTime: "07:00",
      endTime: "21:00",
    },
    // ToDo
    // slotMinTime: "06:00:00",
    // slotMaxTime: "24:00:00",
  });
  if (!$('#filtersModal').length) {
    $('body').append(`
      <div id="filtersModal" class="modal fade" role="dialog">
        <div class="modal-dialog">
          <div class="modal-content">
            <div class="modal-header">
              <button type="button" class="close" data-dismiss="modal">&times;</button>
              <h4 class="modal-title">${SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_FILTERS")}</h4>
            </div>
            <div class="modal-body" id="filtersContainer">
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-primary" id="applyFilters">${SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_APPLY_BUTTON")}</button>
              <button type="button" class="btn btn-default" data-dismiss="modal">${SUGAR.language.get("stic_Bookings_Places_Calendar", "LBL_CANCEL_BUTTON")}</button>
            </div>
          </div>
        </div>
      </div>
    `);
    
    // Configurar los manejadores de eventos
    $('#applyFilters').click(function() {
      saveFilters();
      $('#filtersModal').modal('hide');
    });
  }
  
  $('#form_filters').appendTo('#filtersContainer').show();


  // Add custom CSS to the page
  var style = document.createElement("style");
  style.textContent = `
        .fc-daygrid-day-events {
            margin-top: 0 !important;
        }
        .custom-event-content {
            width: 100%;
            height: 100%;
            display: flex;
            align-items: center;
            justify-content: center;
            padding: 2px;
            background: transparent !important;
        }
        .availability-info {
            font-size: 1.5em;
            line-height: 1.2;
            text-align: center;
        }
        .home-icon {
            width: 16px;
            height: 16px;
            margin: 0 3px;
            vertical-align: middle;
        }
        .available {
            display: block;
            color: green;
        }
        .occupied {
            display: block;
            color: red;
        }
        .fc-event {
            background: transparent !important;
            border: none !important;
        }
        .cross {
          position: absolute;
          right: -8px;
          top: -8px;
          background-color: #b5bc31;
          color: white;
          font-size: 10px;
          padding: 2px 4px;
        }
            
        .fc-filterButton-button {
            position: relative;
        }
    `;
  document.head.appendChild(style);

  calendar.render();
  return calendar;
}

$("#openCenterPopup").click(function () {
  openCenterPopup();
});
var globalCalendar;

function runCheckInterval() {
  var checkIfSearchPaneIsLoaded = setInterval(function () {
    if (SUGAR_callsInProgress === 0) {
      globalCalendar = initializeCalendar();
      clearInterval(checkIfSearchPaneIsLoaded);
      loadSavedFilters();
    }
  }, 200);
}

// Check device screen width
function mobileCheck() {
  if (window.innerWidth >= 768) {
    return false;
  } else {
    return true;
  }
}

// callback function used in the Popup that select events
function openSelectPopup(module, field) {
  var popupRequestData = {
    call_back_function: "callbackSelectPopup",
    form_name: "form_filters",
    field_to_name_array: {
      id: field + "_id",
      name: field + "_name",
    },
  };
  open_popup(module, 600, 400, "", true, false, popupRequestData);
}

var fromPopupReturn = false;
function callbackSelectPopup(popupReplyData) {
  fromPopupReturn = true;
  var nameToValueArray = popupReplyData.name_to_value_array;
  Object.keys(nameToValueArray).forEach(function (key, index) {
    $("#" + key).val(nameToValueArray[key]);
  }, nameToValueArray);
}

function clearRow(form, field) {
  SUGAR.clearRelateField(form, field + "_name", field + "_id");
}
$(document).ready(function () {
  globalCalendar = initializeCalendar();
  loadSavedFilters();
  setTimeout(updateCrossVisibility, 500);

  $("#btn-save-filters").click(function (e) {
    e.preventDefault();
    saveFilters();
  });

  $(document).on('click', '#cross_filters', function(e) {
    e.preventDefault();
    e.stopPropagation();
    handleCrossRemoveFilters();
  });
});

function saveFilters() {
  var filters = {
    stic_center_id: $("#stic_center_id").val() || "",
    stic_center_name: $("#stic_center_name").val() || "",
    stic_resources_places_users_list:
      $("#stic_resources_places_users_list").val() || [],
    stic_resources_places_type_list:
      $("#stic_resources_places_type_list").val() || [],
    stic_resources_places_gender_list:
      $("#stic_resources_places_gender_list").val() || [],
  };

  $.ajax({
    url: "index.php?module=stic_Bookings_Places_Calendar&action=SaveFilters&sugar_body_only=true",
    method: "POST",
    data: filters,
    success: function (response) {
      if (globalCalendar) {
        globalCalendar.refetchEvents();
      }
      updateCrossVisibility();
    },
    error: function (xhr, status, error) {
      console.error("Error saving filters:", error);
    },
  });
}

function loadSavedFilters() {
  $.ajax({
    url: "index.php?module=stic_Bookings_Places_Calendar&action=get_places_availability_data&sugar_body_only=true",
    method: "POST",
    data: {
      start: moment().startOf("month").format("YYYY-MM-DD"),
      end: moment().endOf("month").format("YYYY-MM-DD"),
    },
    success: function (response) {
      var data = JSON.parse(response);
      if (data.savedFilters) {
        $("#stic_center_name").val(data.savedFilters.stic_center_name || "");
        $("#stic_center_id").val(data.savedFilters.stic_center_id || "");
        $("#stic_resources_places_users_list").val(
          data.savedFilters.stic_resources_places_users_list || []
        );
        $("#stic_resources_places_type_list").val(
          data.savedFilters.stic_resources_places_type_list || []
        );
        $("#stic_resources_places_gender_list").val(
          data.savedFilters.stic_resources_places_gender_list || []
        );
      }
      if (globalCalendar) {
        globalCalendar.refetchEvents();
      } else {
        console.warn("Calendar not initialized yet");
      }
      updateCrossVisibility();
    },
    error: function (xhr, status, error) {
      console.error("Error loading saved filters:", error);
    },
  });
}

function hasAppliedFilters() {
  var centerValue = $("#stic_center_id").val();
  var usersList = $("#stic_resources_places_users_list").val();
  var typesList = $("#stic_resources_places_type_list").val();
  var gendersList = $("#stic_resources_places_gender_list").val();
  
  return (
    (centerValue !== "" && centerValue !== null) ||
    (Array.isArray(usersList) && usersList.length > 0) ||
    (Array.isArray(typesList) && typesList.length > 0) ||
    (Array.isArray(gendersList) && gendersList.length > 0)
  );
}

function handleCrossRemoveFilters() {
  $("#stic_center_name").val("");
  $("#stic_center_id").val("");
  $("#stic_resources_places_users_list").val([]);
  $("#stic_resources_places_type_list").val([]);
  $("#stic_resources_places_gender_list").val([]);

  if ($("#stic_resources_places_users_list")[0] && $("#stic_resources_places_users_list")[0].selectize) {
    $("#stic_resources_places_users_list")[0].selectize.clear();
  }
  if ($("#stic_resources_places_type_list")[0] && $("#stic_resources_places_type_list")[0].selectize) {
    $("#stic_resources_places_type_list")[0].selectize.clear();
  }
  if ($("#stic_resources_places_gender_list")[0] && $("#stic_resources_places_gender_list")[0].selectize) {
    $("#stic_resources_places_gender_list")[0].selectize.clear();
  }

  saveFilters();

  updateCrossVisibility();

  if (globalCalendar) {
    globalCalendar.refetchEvents();
  }
}
function updateCrossVisibility() {
  setTimeout(function() {
    var filterButton = $('.fc-filterButton-button');

    if (filterButton.length > 0) {
      $('#cross_filters').remove();

      if (hasAppliedFilters()) {
        var crossHtml = '<span id="cross_filters" class="cross glyphicon glyphicon-remove"></span>';
        filterButton.css('position', 'relative').append(crossHtml);

        $('#cross_filters').on('click', function(e) {
          e.preventDefault();
          e.stopPropagation();
          handleCrossRemoveFilters();
        });

        $('#cross_filters').show();
      }
    } else {
      setTimeout(updateCrossVisibility, 100);
    }
  }, 50);
}
