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
var module = "stic_Bookings_Calendar";
var updatedView = false;

// Load the calendar once everything else is loaded
calendar = runCheckInterval();

// If the selectize stylesheet is not loaded, load it
if ($("#selectize-css").length == 0) {
  $("<link>", {
    rel: "stylesheet",
    href: "SticInclude/vendor/selectize/css/selectize.bootstrap3.css",
    id: "selectize-css",
  }).appendTo("head");
}

// Build the CSS styles dynamically, depending on the color set in each resource
newCSS = "";
resourcesGroupArray.map(function (resource) {
  newCSS += "#id-" + resource.id + ", .id-" + resource.id + " { ";
  newCSS += "background-color: " + resource.color + " !important;";
  newCSS += "border-color: " + resource.color + " !important;";
  newCSS += "} ";
  newCSS += "#id-" + resource.id + ", .id-" + resource.id + " :first-child{ ";
  newCSS += "color: " + resource.fontColor + ";";
  newCSS += "} ";
});
$("<style>" + newCSS + "</style>").appendTo("body");

// This function is used by the Show Bookings/Availability button.
// It saves the status in the user preferences and changes the style of the button
function handleAvailabilityModeButtonClick(e, a) {
  availabilityMode = !availabilityMode;
  $.ajax({
    url: "index.php?module=stic_Bookings_Calendar&action=saveUserPreferences",
    type: "post",
    dataType: "json",
    data: {
      user_preference: "stic_bookings_calendar_availability_mode",
      preference_value: availabilityMode,
    },
    success: function (data) {
      if (data) {
        if (availabilityMode == "true") {
          $(".fc-availabilityMode-button").empty();
          $(".fc-availabilityMode-button").text(
            SUGAR.language.get(
              "stic_Bookings_Calendar",
              "LBL_AVAILABILITY_MODE_BUTTON_DISABLED"
            )
          );
          $(".fc-availabilityMode-button").addClass("fc-button-active");
        } else {
          $(".fc-availabilityMode-button").empty();
          $(".fc-availabilityMode-button").text(
            SUGAR.language.get(
              "stic_Bookings_Calendar",
              "LBL_AVAILABILITY_MODE_BUTTON_ENABLED"
            )
          );
          $(".fc-availabilityMode-button").removeClass("fc-button-active");
        }
        calendar.refetchEvents();
      }
    },
  });
}

// Load FullCalendar
function loadCalendar() {
  var calendarEl = document.getElementById("calendar");
  var calendar = new FullCalendar.Calendar(calendarEl, {
    views: {
      threeDays: {
        type: "timeGridWeek",
        duration: { days: 3 },
        buttonText: SUGAR.language.get(
          "stic_Bookings_Calendar",
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
        today: SUGAR.language.get("stic_Bookings_Calendar", "LBL_TODAY"),
        month: SUGAR.language.get("stic_Bookings_Calendar", "LBL_MONTH"),
        week: SUGAR.language.get("stic_Bookings_Calendar", "LBL_WEEK"),
        day: SUGAR.language.get("stic_Bookings_Calendar", "LBL_DAY"),
        list: SUGAR.language.get("stic_Bookings_Calendar", "LBL_LIST"),
        mobile: SUGAR.language.get("stic_Bookings_Calendar", "LBL_MOBILE_BUTTON"),
      },
  
    viewDidMount: function ({ view }) {
      if (updatedView) {
        $.ajax({
          url: "index.php?module=stic_Bookings_Calendar&action=saveUserPreferences",
          type: "post",
          dataType: "json",
          data: {
            user_preference: "stic_bookings_calendar_view",
            preference_value: view.type,
          },
          success: function (success) {
            if (success) console.log("Preferences saved");
            else console.log("Can't save user preference");
          },
        });
      } else {
        updatedView = true;
      }
    },
    // Choose init view
    initialView:
      window.innerWidth >= 767
        ? calendarView
          ? calendarView
          : "dayGridMonth"
        : "threeDays",
    // Set the proper view according to window size
    windowResize: function (view) {
      if (window.innerWidth >= 767) {
        calendar.changeView("dayGridMonth");
      } else {
        calendar.changeView("threeDays");
      }
    },
    // editable: true,

    headerToolbar: {
      left: "prev,next today availabilityMode",
      center: "title",
      right: "dayGridMonth,timeGridWeek,timeGridDay,listWeek,threeDays",
    },
    eventTimeFormat: {
      // like '14:30:00'
      hour: "2-digit",
      minute: "2-digit",
      meridiem: false,
    },
    customButtons: {
      availabilityMode: {
        text: "",
        click: (e, a) => handleAvailabilityModeButtonClick(e, a),
      },
    },
    initialDate: initialCalendarDate
      ? new Date(initialCalendarDate)
      : new Date(),
    navLinks: true, // can click day/week names to navigate views
    dayMaxEvents: true, // allow "more" link when too many events,
    // viewWillUnmount: () => code,
    selectable: true,
    selectMirror: true,
    // This is used for the click in an empty space of the calendar
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
    // This loads the popup of each event/booking
    eventDidMount: function (info) {
      if (info.event.extendedProps.recordId) {
        var url =
          "index.php?to_pdf=1&module=Home&action=AdditionalDetailsRetrieve&bean=" +
          info.event.extendedProps.module +
          "&id=" +
          info.event.extendedProps.recordId +
          "&resource_name=" +
          encodeURI(info.event.extendedProps.resourceName) +
          "&resource_id=" +
          info.event.extendedProps.resourceId;
        var title =
          '<div class="qtip-title-text">' +
          info.event.title +
          "</div>" +
          '<div class="qtip-title-buttons">' +
          "</div>";
        var body = SUGAR.language.translate("app_strings", "LBL_LOADING_PAGE");

        if (
          $("#cal_module").val() != "Home" &&
          typeof info.event.id !== "undefined"
        ) {
          $(info.el).qtip({
            content: { title: { text: title, button: true }, text: body },
            events: {
              render: function (event, api) {
                $.ajax(url)
                  .done(function (data) {
                    SUGAR.util.globalEval(data);
                    var divCaption = "#qtip-" + api.id + "-title";
                    if (data.caption != "") {
                      $(divCaption).html(result.caption);
                    }
                    api.set("content.text", result.body);
                  })
                  .fail(function () {
                    var divBody = "#qtip-" + api.id + "-content";
                    $(divBody).html(
                      SUGAR.language.translate(
                        "app_strings",
                        "LBL_EMAIL_ERROR_GENERAL_TITLE"
                      )
                    );
                  })
                  .always(function () {});
              },
            },
            position: {
              my: "bottom left",
              at: "top left",
              target: "mouse",
              adjust: { mouse: false },
            },
            show: { solo: true },
            hide: { event: "mouseleave", fixed: true, delay: 200 },
            style: {
              width: 224,
              padding: 5,
              color: "black",
              textAlign: "left",
              border: { width: 1, radius: 3 },
              tip: "bottomLeft",
              classes: {
                tooltip: "ui-widget",
                tip: "ui-widget",
                title: "ui-widget-header",
                content: "ui-widget-content",
              },
            },
          });
        }
      }
    },
    eventClick: function (arg) {
      window.location.assign(
        "index.php?module=" +
          arg.event.extendedProps.module +
          "&action=DetailView&record=" +
          arg.event.extendedProps.recordId
      );
    },
    // Load the bookings from the server
    eventSources: {
      url: "index.php?module=stic_Bookings_Calendar&action=getResources",
      // method: 'POST',
      extraParams: function () {
        // a function that returns an object
        return {
          availabilityMode,
          filteredResources: $("#filter-resources").val(),
        };
      },
      failure: function (err) {
        console.log("error fetching events", err);
      },
    },
    eventColor: defaultCalendarObjectColor,
    // Function that runs when is loading and when it finishes
    loading: function (isLoading) {
      if (!$("#dlg_c").length) SUGAR.ajaxUI.showLoadingPanel();
      if (!isLoading) {
        if (!$("#dlg_c").length) SUGAR.ajaxUI.hideLoadingPanel();
        if (availabilityMode) {
          $(".fc-availabilityMode-button").empty();
          $(".fc-availabilityMode-button").text(
            SUGAR.language.get(
              "stic_Bookings_Calendar",
              "LBL_AVAILABILITY_MODE_BUTTON_DISABLED"
            )
          );
          $(".fc-availabilityMode-button").addClass("fc-button-active");
          if (!$("#filter-resources").val().length) {
            $("#info-availability").qtip({
              content: {
                text: SUGAR.language.get(
                  "stic_Bookings_Calendar",
                  "LBL_AVAILABILITY_MODE_BUTTON_HELP"
                ),
                title: {
                  text: SUGAR.language.languages.app_strings.LBL_ALT_INFO,
                },
                style: {
                  classes: "qtip-inline-help",
                },
              },
            });
            $("#filter-resources-selectized").attr(
              "placeholder",
              SUGAR.language.get(
                "stic_Bookings_Calendar",
                "LBL_AVAILABILITY_FILTER_RESOURCES_PLACEHOLDER"
              )
            );
            $("#filter-resources-selectized").attr("style", "width: 100%;");
          } else {
            $("#info-availability").remove();
            $("#filter-resources-selectized").attr("placeholder", "");
          }
        } else {
          $(".fc-availabilityMode-button").empty();
          $(".fc-availabilityMode-button").text(
            SUGAR.language.get(
              "stic_Bookings_Calendar",
              "LBL_AVAILABILITY_MODE_BUTTON_ENABLED"
            )
          );
          $(".fc-availabilityMode-button").removeClass("fc-button-active");
          $("#info-availability").remove();
          if (!$("#filter-resources").val().length) {
            $("#filter-resources-selectized").attr(
              "placeholder",
              SUGAR.language.get(
                "stic_Bookings_Calendar",
                "LBL_FILTER_RESOURCES_PLACEHOLDER"
              )
            );
          } else {
            $("#filter-resources-selectized").attr("placeholder", "");
          }
        }
      }
    },
    firstDay: start_weekday,
    // Define business hours. This will set a different color for non business hours
    businessHours: {
      daysOfWeek: [1, 2, 3, 4, 5],
      startTime: "07:00",
      endTime: "21:00",
    },
    slotDuration: "00:15:00",
    // ToDo
    // slotMinTime: "06:00:00",
    // slotMaxTime: "24:00:00",
    locale: lang,
  });

  calendar.render();

  return calendar;
}

function loadSelectize() {
  // Set multienum filter field as selectize
  $("#filter-resources").selectize({
    plugins: ["remove_button"],
    placeholder: SUGAR.language.get(
      "stic_Bookings_Calendar",
      "LBL_FILTER_RESOURCES_PLACEHOLDER"
    ),
    copyClassesToDropdown: true,
    // Every change in the filter is sent to the server, so it can be saved in user's configuration.
    onChange: function (value) {
      $.ajax({
        url: "index.php?module=stic_Bookings_Calendar&action=saveUserPreferences",
        type: "post",
        dataType: "json",
        data: {
          user_preference: "stic_bookings_calendar_filtered_resources",
          preference_value: value,
        },
        success: function (success) {
          if (success) console.log("Preferences saved");
          else console.log("Can't save user preference");
          calendar.refetchEvents();
        },
        failure: function (err) {
          console.log("Can't save user preference");
        },
      });
    },
    // Set a CSS class to each option, optiongroup and selected item
    render: {
      option: function (data, escape) {
        return (
          '<div class="option id-' +
          data.value +
          '" id="id-' +
          data.value +
          '">' +
          escape(data.text) +
          '<span class="id-' +
          data.value +
          '"> </span></div>'
        );
      },
      optgroup_header: function (data, escape) {
        return (
          '<div class="option option-optgroup" data-selectable onclick="selectOptgroupItems(this)" id=' +
          data.value +
          ">" +
          '<span class="glyphicon glyphicon-th"></span>  <span class="filter-resources-optgroup" >' +
          (data.value == "no_type_assigned"
            ? SUGAR.language.get(
                "stic_Bookings_Calendar",
                "LBL_OPTIONGROUP_EMPTY"
              )
            : SUGAR.language.get(
                "app_list_strings",
                "stic_resources_types_list"
              )[data.value]) +
          "</span></div>"
        );
      },
      optgroup: function (data, escape) {
        return (
          '<div class="optgroup filter-optgroup" data-group=' +
          data.value +
          ">" +
          data.html +
          "</div>"
        );
      },
      item: function (data, escape) {
        return (
          '<div class="item id-' +
          data.value +
          '" id="id-' +
          data.value +
          '">' +
          escape(data.text) +
          "</div>"
        );
      },
    },
  });
  $(".filter-container").css("display", "flex");
}
// Button used to empty the resources filter
function loadButtons() {
  $("#button_clear, #span_clear").on("click", function () {
    var select = $("#filter-resources");
    var control = select[0].selectize;
    control.clear();
  });
}

// Load the calendar only when SuiteCRM has finished to Load
function runCheckInterval() {
  var checkIfSearchPaneIsLoaded = setInterval(function () {
    if (SUGAR_callsInProgress === 0) {
      calendar = loadCalendar();
      loadSelectize();
      loadButtons();
      clearInterval(checkIfSearchPaneIsLoaded);
      return calendar;
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

// Add all items of a resource type in the filter resources
function selectOptgroupItems(optgroup) {
  $("#" + optgroup.id)
    .siblings()
    .each(function (key, elem) {
      $("#filter-resources")[0].selectize.addItem($(elem).attr("data-value"));
    });
}
