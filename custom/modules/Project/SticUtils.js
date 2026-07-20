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
var module = "Project";

/* INCLUDES */
// Load moment.js to use in validations
loadScript("include/javascript/moment.min.js");

/* VALIDATION DEPENDENCIES */
var validationDependencies = {
  estimated_start_date: "estimated_end_date",
  estimated_end_date: "estimated_start_date",
};

/* DIRECT VALIDATION CALLBACKS */
addToValidateCallback(getFormName(), "estimated_start_date", "date", false, SUGAR.language.get(module, "LBL_ESTIMATED_START_DATE_ERROR"), function () {
  return checkStartAndEndDatesCoherence("estimated_start_date", "estimated_end_date");
});

addToValidateCallback(getFormName(), "estimated_end_date", "date", false, SUGAR.language.get(module, "LBL_ESTIMATED_END_DATE_ERROR"), function () {
  return checkStartAndEndDatesCoherence("estimated_start_date", "estimated_end_date");
});

/* VIEWS CUSTOM CODE */
switch (viewType()) {
  case "edit":
  case "quickcreate":
    break;

  case "detail":
    break;

  case "list":
    break;

  default:
    break;
}

/* AUX FUNCTIONS */

/* FIX MODULE AND RECORD - Handle duplicate and edit scenarios */
(function() {
  function isDuplicateMode() {
    if (!document.EditView) { return false; }

    function isTruthyDuplicateValue(field) {
      if (!field || typeof field.value === 'undefined' || field.value === null) { return false; }
      var value = String(field.value).trim().toLowerCase();
      return value === 'true' || value === '1' || value === 'yes';
    }

    // Check form flags first
    if (isTruthyDuplicateValue(document.EditView.duplicateSave)) { return true; }
    if (isTruthyDuplicateValue(document.EditView.isDuplicate)) { return true; }
    if (document.EditView.duplicateId && document.EditView.duplicateId.value) { return true; }

    // Fallback to URL params
    var urlParams = new URLSearchParams(window.location.search);
    var urlIsDuplicate = (urlParams.get('isDuplicate') || '').toLowerCase();
    var urlDuplicateSave = (urlParams.get('duplicateSave') || '').toLowerCase();
    var urlDuplicateId = urlParams.get('duplicateId');
    return urlIsDuplicate === 'true' || urlIsDuplicate === '1' || urlDuplicateSave === 'true' || urlDuplicateSave === '1' || !!urlDuplicateId;
  }

  function getRecordIdFromContext() {
    if (document.EditView && document.EditView.record && document.EditView.record.value) {
      return document.EditView.record.value;
    }

    // Prefer return_id; fallback to URL record
    if (document.EditView && document.EditView.return_id && document.EditView.return_id.value) {
      return document.EditView.return_id.value;
    }

    var urlParams = new URLSearchParams(window.location.search);
    return urlParams.get('record') || urlParams.get('return_id') || urlParams.get('duplicateId');
  }

  function fixModule() {
    if (!document.EditView) { return; }
    
    // Always force the correct module
    if (document.EditView.module) { 
      document.EditView.module.value = "Project"; 
    }
    
    // Check if we are in duplicate mode
    var isDuplicate = isDuplicateMode();
    
    // Keep record empty when duplicating; restore it for edits
    if (document.EditView.record) {
      if (isDuplicate) {
        // In duplicate mode, clear record to create a new one
        document.EditView.record.value = '';
      } else {
        // Edit mode: use return_id or URL to get record ID
        if (!document.EditView.record.value) {
          var recordFromContext = getRecordIdFromContext();
          if (recordFromContext) {
            document.EditView.record.value = recordFromContext;
          }
        }
      }
    }
  }

  // Run on page load
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() {
      fixModule();
      setupEventHandlers();
    });
  } else {
    fixModule();
    setupEventHandlers();
  }

  function setupEventHandlers() {
    if (!document.EditView) { return; }

    // Also run on page show
    window.addEventListener("pageshow", fixModule);

    // Handle form submit
    document.EditView.addEventListener('submit', function(e) {
      fixModule();
    });

    // Override formSubmitCheck if exists
    if (typeof window.formSubmitCheck === "function") {
      var originalFormSubmitCheck = window.formSubmitCheck;
      window.formSubmitCheck = function() {
        fixModule();
        return originalFormSubmitCheck.apply(this, arguments);
      };
    }
  }
})();