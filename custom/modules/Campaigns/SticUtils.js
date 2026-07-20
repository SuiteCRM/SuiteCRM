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
var module = "Campaigns";
var activeSection = null;

/* INCLUDES */

if ($('[data-field="end_date"]').length > 0 && $('[data-field="start_date"]').length > 0) {
  /* VALIDATION DEPENDENCIES */
  var validationDependencies = {
    start_date: "end_date",
    end_date: "start_date"
  };

  /* VALIDATION CALLBACKS */
  addToValidateCallback(
    getFormName(),
    "end_date",
    "date",
    false,
    SUGAR.language.get(module, "LBL_END_DATE_ERROR"),
    function() {
      return checkStartAndEndDatesCoherence("start_date", "end_date");
    }
  );
  addToValidateCallback(
    getFormName(),
    "start_date",
    "date",
    false,
    SUGAR.language.get(module, "LBL_START_DATE_ERROR"),
    function() {
      return checkStartAndEndDatesCoherence("start_date", "end_date");
    }
  );
}

/* VIEWS CUSTOM CODE */
switch (viewType()) {
  case "quickcreate":
  case "popup":
    $(document).ready(function() {
      initializeQuickCreate();
    });
    break;

  case "edit":
    $(document).ready(function() {
      initilizeEditView();
    });
    break;

  case "detail":
    $recordId = $("#formDetailView input[type=hidden][name=record]").val();
    $(document).ready(function() {
      initilizeDetailView();
    });
    break;

  case "list":
    break;

  default:
    break;
}

$(document).ready(function() {
  if (viewType() != "list") {
    $("#notification_prospect_list_ids").selectize({ plugins: ["remove_button"] });
    $("#msg_notification_prospect_list_ids").selectize({ plugins: ["remove_button"] });
    if ($("#LBL_NOTIFICATION_NEW_INFO").length == 0) {
      $(
        "<div id='LBL_NOTIFICATION_NEW_INFO' class='msg-warning' style='text-align: center; margin: 1em auto;'>" +
          SUGAR.language.get("Campaigns", "LBL_NOTIFICATION_NEW_INFO") +
          "</div>"
      ).prependTo("[data-id='LBL_NOTIFICATION_INFORMATION_PANEL'] .tab-content .row");

      $("#notification_outbound_email_id").on("change paste keyup", mail_change);
      $("#notification_template_id").on("change paste keyup", template_change);
    }
    if ($("#LBL_MSG_NOTIFICATION_NEW_INFO").length == 0) {
      $(
        "<div id='LBL_MSG_NOTIFICATION_NEW_INFO' class='msg-warning' style='text-align: center; margin: 1em auto;'>" +
          SUGAR.language.get("Campaigns", "LBL_MSG_NOTIFICATION_NEW_INFO") +
          "</div>"
      ).prependTo("[data-id='LBL_MSG_NOTIFICATION_INFORMATION_PANEL'] .tab-content .row");

      $("#msg_notification_template_id").on("change paste keyup", template_change);
    }

    // Check Notification panel exists
    const targetElement = $(".panel-body[data-id='LBL_NOTIFICATION_INFORMATION_PANEL']").parent()[0];
    if (targetElement) {
      var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.attributeName === 'style') {
            type_change();
          }
        });
      });
  
      observer.observe(targetElement, { attributes: true, attributeFilter: ['style'] });
    } else {
        console.log("Notification panel does not exists in DOM.");
    }

    // As no 2 flex relate fields can coexist, we wil use only one, moving it from section to section on demand
    // --- Get references to all the elements we need to control ---
    const campaignTypeDropdown = $('#campaign_type');

    // Panels (SuiteCRM creates DIVs with IDs based on the panel's label)
    const emailPanel = $('#LBL_NOTIFICATION_INFORMATION_PANEL');
    const messagePanel = $('#LBL_MSG_NOTIFICATION_INFORMATION_PANEL');

    // The actual field elements to move. SuiteCRM wraps the input/select/button in a SPAN.
    // We select the SPAN to move everything at once.
    const parentFieldElements = $('#parent_name').parent();

    // The original and target containers for the field
    const originalContainer = parentFieldElements.parent(); // The original <td>
    const targetContainer = $('#parent_field_placeholder').parent().parent();

    $("[field='parent_field_placeholder']").remove();

    // --- This is the main function that controls the display ---
    function toggleCampaignView() {
      // This is the specific text input field for the typeahead
      // const parentNameInput = 'parent_name';
      
      const currentType = campaignTypeDropdown.val();
      // IMPORTANT: Replace 'Email' and 'Message' with your actual values
      if (currentType === 'Notification') {
          // Show the email panel and hide the message panel
          emailPanel.show();
          messagePanel.hide();
          // Move the field elements back to their original location
          parentFieldElements.appendTo(originalContainer);
          // ** Re-initialize the typeahead on the input field **
          // SUGAR.AutoComplete.enable(parentNameInput);
      } else if (currentType === 'NotifMsg') {
          // Show the message panel and hide the email panel
          emailPanel.hide();
          messagePanel.show();
          // Move the field elements to our placeholder target
          parentFieldElements.appendTo(targetContainer);
          // ** Re-initialize the typeahead on the input field **
          // SUGAR.AutoComplete.enable(parentNameInput);
      } else {
          // Optional: Hide both if no relevant type is selected
          emailPanel.hide();
          messagePanel.hide();
      }
    }
    // --- Attach the function to the dropdown's 'change' event ---
    campaignTypeDropdown.on('change', toggleCampaignView);

    // --- Run the function once on page load to set the initial state ---
    toggleCampaignView();

    type_change();
    template_change();
  }
});




function getCampaingType() {
  var typeValue = $('[name="campaign_type"]').val();
  if (typeValue === undefined) {
    typeValue = $('[field="campaign_type"] input').val();
  }
  return typeValue;
}

function type_change() {
  var typeValue = getCampaingType();
  updateViewNewsLetterType(typeValue == "NewsLetter");
  updateViewAnyNotificationType(typeValue);
  updateViewNotificationType(typeValue == "Notification");
  updateViewNotificationMsgType(typeValue == "NotifMsg");
  updateViewSurveyType(typeValue == "Survey");
  mail_change();
}

function mail_change() {
  if (STIC && STIC.campaignEmails && STIC.campaignEmails.outbound) {
    var outbound = STIC.campaignEmails.outbound.find(item => item.id === $("#notification_outbound_email_id").val());
    $("#notification_from_name").val(outbound ? outbound.name : "");
    $("#notification_from_addr").val(outbound ? outbound.addr : "");
    $("#notification_reply_to_name").val(outbound ? outbound.reply_name : "");
    $("#notification_reply_to_addr").val(outbound ? outbound.reply_addr : "");

  }
}

function template_change() {
  if ($("#notification_template_id").val() == "") {
    $("#notification_template_id_edit_link").hide();
  } else {
    $("#notification_template_id_edit_link").show();
  }
  if ($("#msg_notification_template_id").val() == "") {
    $("#msg_notification_template_id_edit_link").hide();
  } else {
    $("#msg_notification_template_id_edit_link").show();
  }
}

function ConvertItems(id) {
  var items = new Array();

  //get the items that are to be converted
  expected_revenue = document.getElementById("expected_revenue");
  budget = document.getElementById("budget");
  actual_cost = document.getElementById("actual_cost");
  expected_cost = document.getElementById("expected_cost");

  //unformat the values of the items to be converted
  expected_revenue.value = unformatNumber(expected_revenue.value, num_grp_sep, dec_sep);
  expected_cost.value = unformatNumber(expected_cost.value, num_grp_sep, dec_sep);
  budget.value = unformatNumber(budget.value, num_grp_sep, dec_sep);
  actual_cost.value = unformatNumber(actual_cost.value, num_grp_sep, dec_sep);

  //add the items to an array
  items[items.length] = expected_revenue;
  items[items.length] = budget;
  items[items.length] = expected_cost;
  items[items.length] = actual_cost;

  //call function that will convert currency
  ConvertRate(id, items);

  //Add formatting back to items
  expected_revenue.value = formatNumber(expected_revenue.value, num_grp_sep, dec_sep);
  expected_cost.value = formatNumber(expected_cost.value, num_grp_sep, dec_sep);
  budget.value = formatNumber(budget.value, num_grp_sep, dec_sep);
  actual_cost.value = formatNumber(actual_cost.value, num_grp_sep, dec_sep);
}

function updateViewNewsLetterType(isNewsLetter) {
  if (isNewsLetter) {
    $('[data-field="frequency"]').show();
    $('#freq_label').show();
    $('#freq_field').show();
  } else {
    $('[data-field="frequency"]').hide();
    $('#freq_label').hide();
    $('#freq_field').hide();
  }
}

function updateViewSurveyType(isSurvey) {
  if (isSurvey) {
    $('[data-field="survey_name"]').show();
  } else {
    $('[data-field="survey_name"]').hide();
  }
}

function setRequired(require, field) {
  var $form = $("form#" + getFormName());

  var labelText = $("[data-field='" + field + "'] [data-label]").contents().filter(function() {
    return this.nodeType === Node.TEXT_NODE;
  }).text().trim().slice(0, -1);
  var type = $("[field='" + field + "']", $form).attr("type");
  $("[data-field='" + field + "'] div.label span.required", $form).remove();

  if (require) {
    addToValidate(getFormName(), field, type, true, labelText);
    addRequiredMark(field);
  } else {
    removeFromValidate(getFormName(), field);
    removeRequiredMark(field);
  }
}

function setAutofillMark(autofill, field) {
  if (autofill) {
    setAutofill([field]);
  } else {
    $row = $("form #" + field).closest(".edit-view-row-item");
    $row.find(".label").removeClass("autofill");
    $row.removeAttr("title");
  }
}

function updateViewAnyNotificationType(type) {
  var isAnyNotification = (type == "Notification" || type == 'NotifMsg');

  setRequired(!isAnyNotification, "name");
  setAutofillMark(isAnyNotification, "name");

  setRequired(isAnyNotification, "start_date");
  setRequired(isAnyNotification, "parent_name");

  var $form = $("form#" + getFormName());

  if (isAnyNotification) {
    $form.find("#status").val("Active");
    $form.find('[data-field="status"]').hide();
    $form.find('[data-field="end_date"]').hide();
    $form.find('[data-field="parent_name"]').show();

    $form.find("[data-label='LBL_NAVIGATION_MENU_GEN2']").hide();
  }
  else {
      $form.find("#parent_type").val("");
      $form.find("#parent_name").val("");
      $form.find("#parent_id").val("");

    $form.find('[data-field="status"]').show();
    $form.find('[data-field="end_date"]').show();
    $form.find('[data-field="parent_name"]').hide();
    $form.find("[data-label='LBL_NAVIGATION_MENU_GEN2']").show();
  }
}

function updateViewNotificationType(isNotification) {
  // setRequired(!isNotification, "name");
  // setAutofillMark(isNotification, "name");

  // setRequired(isNotification, "start_date");
  // setRequired(isNotification, "parent_name");
  setRequired(isNotification, "notification_outbound_email_id");
  setRequired(isNotification, "notification_inbound_email_id");
  setRequired(isNotification, "notification_prospect_list_ids");
  setRequired(isNotification, "notification_template_id");
  setRequired(isNotification, "notification_from_name");
  addRequiredMark("notification_from_addr");
  

  var $form = $("form#" + getFormName());

  if (isNotification) {
    $form.find(".panel-body[data-id='LBL_NOTIFICATION_INFORMATION_PANEL']").parent().show();
    if ($form.find("#start_date").val() == "") {
      var formatDate = $form.find("#start_date").parent().children(".dateFormat").text().toUpperCase();
      if (formatDate == "") {
        formatDate = STIC.userDateFormat.toUpperCase();
      }
      if (formatDate != "") {
        $form.find("#start_date").val(moment().format(formatDate));
      }
    }
    if ($('#notification_from_addr').length > 0 && $('#notification_reply_to_addr').length > 0) {
      /* VALIDATION CALLBACKS */
      addToValidate(
        getFormName(), 
        'notification_from_addr',
        'email', 
        true,
        SUGAR.language.get('app_strings', 'ERR_INVALID_EMAIL_ADDRESS')
      );
      addToValidate(
        getFormName(),
        'notification_reply_to_addr',
        'email',
        false,
        SUGAR.language.get('app_strings', 'ERR_INVALID_EMAIL_ADDRESS')
      );
    }
  } else {
    $form.find(".panel-body[data-id='LBL_NOTIFICATION_INFORMATION_PANEL']").parent().hide();
    removeFromValidate(getFormName(), 'notification_from_addr');
    removeFromValidate(getFormName(), 'notification_reply_to_addr');
  }
}
function updateViewNotificationMsgType(isNotification) {
  setRequired(isNotification, "msg_parent_name");
  setRequired(isNotification, "msg_notification_prospect_list_ids");
  setRequired(isNotification, "msg_notification_template_id");
  setRequired(isNotification, "sender");
  addRequiredMark("sender");
  

  var $form = $("form#" + getFormName());

  if (isNotification) {
    $form.find("#status").val("Active");
    $form.find('[data-field="msg_parent_name"]').show();
    $form.find(".panel-body[data-id='LBL_MSG_NOTIFICATION_INFORMATION_PANEL']").parent().show();
    if ($form.find("#start_date").val() == "") {
      var formatDate = $form.find("#start_date").parent().children(".dateFormat").text().toUpperCase();
      if (formatDate == "") {
        formatDate = STIC.userDateFormat.toUpperCase();
      }
      if (formatDate != "") {
        $form.find("#start_date").val(moment().format(formatDate));
      }
    }

    // Prevent selecting whatsapp_web in this panel when campaign is NotifMsg
    (function removeWhatsAppInMsgPanel() {
      var $typeSelect = $form.find('[name="notification_message_type"], #notification_message_type');

      if ($typeSelect.length) {
        $typeSelect.find('option[value="whatsapp_web"]').remove().end().change();
      }
    })();

  } else {
    $form.find(".panel-body[data-id='LBL_MSG_NOTIFICATION_INFORMATION_PANEL']").parent().hide();
  }
}

function initializeQuickCreate() {
  var formName = getFormName();
  if (formName == "form_SubpanelQuickCreate_Campaigns") {
    // Is a New notification from Subpanel

    var $form = $("form#" + formName);
    $form.find("[data-field='campaign_type']").hide();

    $form.find("#status").val("Active");
    addEditCreateTemplateLinks();
  }
}

function initilizeEditView() {
  var record = $("[name='record']").val();
  var isEdition = record !== undefined && record != "";

  if (isEdition && getCampaingType() == "Notification") {
    // Disable editions
    $("#LBL_NOTIFICATION_NEW_INFO").hide();
    $("#campaign_type").prop("disabled", true);
    $("#start_date").parent().children().prop("disabled", true);
    $("#parent_id").parent().children().prop("disabled", true);
    $("#parent_id").parent().find("span").hide();
    $("#notification_outbound_email_id").prop("disabled", true);
    $("#notification_inbound_email_id").prop("disabled", true);
    $("#notification_template_id").prop("disabled", true);
    $("#notification_from_name").parent().children().prop("disabled", true);
    $("#notification_from_addr").parent().children().prop("disabled", true);
    $("#notification_reply_to_name").parent().children().prop("disabled", true);
    $("#notification_reply_to_addr").parent().children().prop("disabled", true);
    $("#notification_prospect_list_ids")[0].selectize.disable();
  } else if (isEdition && getCampaingType() == "NotifMsg") {
    // Disable editions
    // TODO: Haurem d etenir un missatge d'avís semblant per notificacionsper missatge
    $("#LBL_NOTIFICATION_NEW_INFO").hide();
    $("#campaign_type").prop("disabled", true);
    $("#start_date").parent().children().prop("disabled", true);
    $("#parent_id").parent().children().prop("disabled", true);
    $("#parent_id").parent().find("span").hide();
    $("#msg_notification_template_id").prop("disabled", true);
    $("#notification_message_type").prop("disabled", true);
    $("#sender").parent().children().prop("disabled", true);
    // TODOEPS: Hauré de fer una llista diferent
    $("#msg_notification_prospect_list_ids")[0].selectize.disable();
  } else {
    addEditCreateTemplateLinks();
  }
}

function initilizeDetailView() {
  var typeValue = getCampaingType();

  if (typeValue == "Notification" || typeValue == "NotifMsg") {
    // Disable all editable actions

    // Action menu buttons
    $("#launch_wizard_button").hide();

    // All Subpanels buttons
    $(".clickMenu").hide();
    $("#viewRoiButtonId").hide();
    $("#mark_as_sent_button").parent().hide();
  }

  // Manipulate action links
  if (typeValue == "Message" || typeValue == "NotifMsg") {
    // Remove "Send Email" action
    $("#send_emails_button").parent().hide();
    $("#send_test_button").parent().hide();
    recordId = $("#formDetailView input[type=hidden][name=record]").val();
    var buttons = {
      testMessagesSendButton: {
        id: "bt_testMessagesSendButton_detailview",
        title: SUGAR.language.get("Campaigns", "LBL_SEND_MESSAGES_TEST"),
        onclick: "window.location='index.php?module=stic_Message_Marketing&action=selectMessageMarketing&record=" + recordId + "&return_module="+ module +"&return_action=TrackDetailView&test=true'"
      },
      queueMessagesSendButton: {
        id: "bt_queueMessagesSendButton_detailview",
        title: SUGAR.language.get("Campaigns", "LBL_SEND_MESSAGES"),
        onclick: "window.location='index.php?module=stic_Message_Marketing&action=selectMessageMarketing&record=" + recordId + "&return_module="+ module +"&return_action=TrackDetailView'"
      }
    };
    createDetailViewButton(buttons.testMessagesSendButton);
    createDetailViewButton(buttons.queueMessagesSendButton);
  }

}

function addEditCreateTemplateLinks() {
  if ($("#notification_template_id_edit_link").length == 0) {
    var $select = $("#notification_template_id");
    var $div = $select.parent();

    $select.css("width","50%");

    var editText = SUGAR.language.translate("app_strings", "LNK_EDIT");
    var $editLink = $('<a href="#" id="notification_template_id_edit_link" style="margin-left:10px;">'+editText+'</a>').on("click", function(e) {
      e.preventDefault();
      edit_email_template_form('notification');
    });
    $div.append($editLink);

    var createText = SUGAR.language.translate("app_strings", "LNK_CREATE");
    var $createLink = $('<a href="#" id="notification_template_id_create_link" style="margin-left:10px;">'+createText+'</a>').on("click", function(e) {
      e.preventDefault();
      activeSection = 'notification';
      open_email_template_form('notification');
    });
    $div.append($createLink);
  }
  if ($("#msg_notification_template_id_edit_link").length == 0) {
    var $select = $("#msg_notification_template_id");
    var $div = $select.parent();

    $select.css("width","50%");

    var editText = SUGAR.language.translate("app_strings", "LNK_EDIT");
    var $editLink = $('<a href="#" id="msg_notification_template_id_edit_link" style="margin-left:10px;">'+editText+'</a>').on("click", function(e) {
      e.preventDefault();
      edit_email_template_form('sms');
    });
    $div.append($editLink);

    var createText = SUGAR.language.translate("app_strings", "LNK_CREATE");
    var $createLink = $('<a href="#" id="msg_notification_template_id_create_link" style="margin-left:10px;">'+createText+'</a>').on("click", function(e) {
      e.preventDefault();
      activeSection = 'sms';
      open_email_template_form('sms');
    });
    $div.append($createLink);
  }
}

function open_email_template_form(type) {
  var inboundId = $("#notification_outbound_email_id").val();
  var parent_type = "";
  if ($("#parent_type").length>0) {
    parent_type = $("#parent_type").val();
  } else if(typeof currentModule !== 'undefined') {
    parent_type = currentModule;
  } 
  URL = "index.php?module=EmailTemplates&action=EditView&type="+type+"&inboundEmail=" + inboundId + "&parent_type=" + parent_type;
  URL += "&show_js=1";

  windowName = 'email_template';
  windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

  win = window.open(URL, windowName, windowFeatures);
  if (window.focus) {
      // put the focus on the popup if the browser supports the focus() method
      win.focus();
  }
}

function edit_email_template_form(type) {
  var inboundId = $("#notification_outbound_email_id").val();
  var parent_type = "";
  if ($("#parent_type").length>0) {
    parent_type = $("#parent_type").val();
  } else if(typeof currentModule !== 'undefined') {
    parent_type = currentModule;
  } 
  URL = "index.php?module=EmailTemplates&action=EditView&type="+type+"&inboundEmail=" + inboundId + "&parent_type=" + parent_type;

  if (type == 'sms') {
    var field = document.getElementById('msg_notification_template_id');
  }
  else {
    var field = document.getElementById('notification_template_id');
  }
  if (field.options[field.selectedIndex].value != 'undefined') {
      URL += "&record=" + field.options[field.selectedIndex].value;
  }
  URL += "&show_js=1";

  windowName = 'email_template';
  windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

  win = window.open(URL, windowName, windowFeatures);
  if (window.focus) {
      // put the focus on the popup if the browser supports the focus() method
      win.focus();
  }
}

function refresh_email_template_list(template_id, template_name) {
  if (activeSection != 'sms') {
    var field = document.getElementById('notification_template_id');
  } else {
    var field = document.getElementById('msg_notification_template_id');
  }  
  var bfound = 0;
  for (var i = 0; i < field.options.length; i++) {
      if (field.options[i].value == template_id) {
          if (field.options[i].selected == false) {
              field.options[i].selected = true;
          }
          field.options[i].text = template_name;
          bfound = 1;
      }
  }
  //add item to selection list.
  if (bfound == 0) {
      var newElement = document.createElement('option');
      newElement.text = template_name;
      newElement.value = template_id;
      field.options.add(newElement);
      newElement.selected = true;
  }
  template_change();
}

/**
 * Custom JS for auto-prospect list creation from category
 */
$('#notification_auto_prospect_list_name').on('change', function () {
  var $select = $('#notification_prospect_list_ids')[0].selectize;
  $select.clear();

  var selectedCategory = $(this).val();
  var selectedLabel = $(this).find('option:selected').text();
  if (selectedCategory.length > 0) {
    // $select.disable();
    createAutoProspectListFromCategory(selectedCategory, selectedLabel);
  } else {
    $select.enable();
  }
});



/* Function to create an auto-prospect list from a selected category.
 * The function sends an AJAX request to the server to create the list and handles the response.
 * On success, it adds the new prospect list to the selectize field and alerts the user.
 * On failure, it logs the error and alerts the user.
 * The expected response from the server is a JSON object with 'status', 'lpoId', and 'lpoName' fields.
 * example: { "status": "success", "lpoId": "12345", "lpoName": "My Prospect List" }
 *
 * @param {string} type - The type of category selected (e.g., 'stic_Signatures_all_signers').
 * @param {string} label - The label of the selected category for naming the prospect list.
 */

function createAutoProspectListFromCategory(filterName, label) {
  console.log("Creating auto-prospect list for category: " + label + " (" + type + ")");

  const url = "index.php";

  const data = {
    module: 'ProspectLists',
    action: 'createAutoLpo',
    filterModule: new URLSearchParams(window.location.search).get('module'),
    filterName: filterName,
    label: label,
    id: $('[name="record"]').val()
  }
  console.log("Request Data: ", data);
  console.log("Request URL: " + url);

  // Send AJAX request to create the auto-prospect list
  fetch(url, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
    },
    body: new URLSearchParams(data).toString()
  }).then(response => {

    // Manage the response
    if (!response.ok) {
      throw new Error('Error de red o del servidor');
    }
    return response.json();

  }
  ).then(data => {

    console.log("Response data:", data);
    if (data.status == 'success') {
      console.log("Auto-prospect list created successfully: " + data.prospectListId);
      // Add the new prospect list to the selectize field
      var $select = $('#notification_prospect_list_ids')[0].selectize;
      $select.addOption({ value: data.lpoId, text: data.lpoName });
      $select.addItem(data.lpoId);


      alert(SUGAR.language.get('Campaigns', 'LBL_NOTIFICATION_AUTO_PROSPECT_LIST_SUCCESS') + '\n' + data.lpoName );
    }
  }
  ).catch(error => {
    console.error('Error al crear la auto-prospect list:', error);
    var $select = $('#notification_prospect_list_ids')[0].selectize;
    $select.enable();
    // Clear the auto-prospect list selector
    $('#notification_auto_prospect_list_name').val('');
    alert(SUGAR.language.get('Campaigns', 'LBL_NOTIFICATION_AUTO_PROSPECT_LIST_ERROR'));
  }
  );

}

function populateLPOFilters() {

  // Remove existing options from other modules and set disabled state remaining options
  $('#notification_auto_prospect_list_name option').each(function () {
    if ($(this).val().split('__')[0] != new URLSearchParams(window.location.search).get('module') && $(this).val() != '') {
      $(this).remove();
    } else{
      $(this).prop('disabled', true).attr('title', 'Filter unconfigured');
    }
  });

  // Fetch the filter options from the server
  fetch("index.php?module=ProspectLists&action=populateLPOFilters", { method: 'GET' })
    .then(response => {
      if (!response.ok) {
        throw new Error('Network response was not ok');
      }
      return response.json();
    })
    .then(data => {
      console.log("LPO Filters data:", data);
      var $select = $('#notification_auto_prospect_list_name');
      
      $.each(data, function (key, value) {
        // debugger;
        if (key.split('__')[0] == new URLSearchParams(window.location.search).get('module')) {
          $select.append($('<option>', { value: key, text: value }));
        }
      });

    })
    .catch(error => {
      console.error('Error fetching LPO filters:', error);
    });

}

populateLPOFilters();

