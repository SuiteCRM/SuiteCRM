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
var module = "stic_Resources";

// Variable to store the original type value
var originalType = null;

/* VALIDATION CALLBACKS */

addToValidateCallback(getFormName(), "stic_resources_stic_centers_name", "relate", false, SUGAR.language.get(module, "LBL_CENTER_REQUIRED_FOR_PLACES"), function() {
  return checkCenterForPlaces();
});

/* VIEWS CUSTOM CODE */
switch (viewType()) {
    case "edit":
    case "quickcreate":
    case "popup":
  
      $(document).ready(function () {
        // Store the original type value
        originalType = $("#type").val();
        showTabsEdit();
        updateCenterRequirement();
      });
      break;
  
    case "detail":
      $(document).ready(function () {
        var typeSelected = $("#type").val();
        showTabs(typeSelected);
      });
      break;
  
    case "list":
      break;
  
    default:
      break;
  }

/* AUX FUNCTIONS */

// Function to check if center is required based on type
function checkCenterForPlaces() {
  var typeValue = getFieldValue("type");
  var centerValue = getFieldValue("stic_resources_stic_centers_name");

  if (typeValue === "place" && centerValue === "") {
    return false;
  }
  
  return true;
}

// Function to check if resource has bookings
function checkResourceBookings(resourceId, callback) {
  console.log("Se esta checkeando checkResourceBookings")
  if (!resourceId) {
    callback(false);
    return;
  }
  $.ajax({
    url: 'index.php?module=stic_Resources&action=checkBookings',
    type: 'POST',
    data: {
      resource_id: resourceId
    },
    dataType: 'json',
    success: function(response) {
      console.log("Respuesta del servidor (success):", response);
      callback(response.hasBookings || false);
    },
    error: function(jqXHR, textStatus, errorThrown) { 
      callback(false);
    }
  });
}

// Function to validate type change
function validateTypeChange(newType, callback) {
  var resourceId = $('[name="record"]').val();

  var currentType = originalType;

  if (!currentType || currentType === newType) {
    callback(true);
    return;
  }
  
  // Check if we're changing between place and non-place types
  var isChangingToPlace = (currentType !== "place" && newType === "place");
  var isChangingFromPlace = (currentType === "place" && newType !== "place");

  if (isChangingToPlace || isChangingFromPlace) {

    checkResourceBookings(resourceId, function(hasBookings) {

      if (hasBookings) {    
        alert(SUGAR.language.get(module, "LBL_RESOURCE_TYPE_CHANGE_ERROR"));
        callback(false);
      } else {
        callback(true);
      }
    });
  } else {
    callback(true);
  }
}

// Function to update UI indicating center field is required when type is places
function updateCenterRequirement() {
  var typeValue = $("#type").val();
  var centerField = $("#stic_resources_stic_centers_name");
  var centerLabelDiv = $('[data-label="LBL_STIC_RESOURCES_STIC_CENTERS_FROM_STIC_CENTERS_TITLE"]');
  
  if (typeValue === "place") {
    if (centerLabelDiv.length && !centerLabelDiv.find("span.required").length) {
      centerLabelDiv.append('<span class="required">*</span>');
    }

    centerField.addClass("conditional-required");
  } else {
    centerLabelDiv.find("span.required").remove();
    centerField.removeClass("conditional-required");
  }
}

// Function to show the tabs depending of the type
function showTabs(typeSelected) {

    var panelPlaces = $("div.panel-content");
    var hourlyRateField = $("#hourly_rate");
    var dailyRateField = $("#daily_rate");
    var hourlyRateLabel = $('[data-label="LBL_HOURLY_RATE"]');
    var dailyRateLabel = $('[data-label="LBL_DAILY_RATE"]');

    panelPlace(panelPlaces, "hide");

    if (typeSelected === "place") {
        panelPlace(panelPlaces, "show");          
        updateCenterRequirement();
        hourlyRateField.hide();
        dailyRateField.hide();
        hourlyRateLabel.hide();
        dailyRateLabel.hide();
    } else {
        panelPlace(panelPlaces, "hide");
        updateCenterRequirement();
        hourlyRateField.show();
        dailyRateField.show();
        hourlyRateLabel.show();
        dailyRateLabel.show();
    }
}

// Function to show the tabs when the type is changing
function showTabsEdit() {
    var typeSelected = $("#type").val();
  
    showTabs(typeSelected);
  
    // Get the subpanels of the quickcreate
    if (viewType() == "quickcreate") {
      typeContact = document.querySelector(
        "#whole_subpanel_stic_resources_stic_bookings"
      );
      if (typeContact != null) {
        typeSelected = $("#whole_subpanel_stic_resources_stic_bookings #type");
        typeSelected.on("change", function () {
          var newType = $(this).val();
          
          validateTypeChange(newType, function(isValid) {
            if (isValid) {
              showTabs(newType);
              updateCenterRequirement();
              originalType = newType; 
            } else {
              $(this).val(originalType);
            }
          });
        });
      }
    } else {
      $("#type").on("change", function () {
        var newType = $(this).val();
        var $this = $(this);
        
        validateTypeChange(newType, function(isValid) {
          if (isValid) {
            showTabs(newType);
            updateCenterRequirement();
            originalType = newType; 
          } else {
            // Revert to original type
            $this.val(originalType);
          }
        });
      });
    }
}
  
function panelPlace(panelPlaces, view) {
    // Showing the tab Task and put the fields required if is in the EditView
    if (view === "show") {
        panelPlaces.show();
     
      // Hiding the tab Task and put the fields unrequired if is in the EditView
    } else if (view === "hide") {
        panelPlaces.hide();
    }
}
