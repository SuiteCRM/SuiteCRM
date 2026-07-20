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
var module = "stic_Message_Marketing";
var inputsWithErrors2 = new Array();

addToValidateCallback(
    getFormName(),
    "prospect_lists",
    "enum",
    false,
    SUGAR.language.get(module, "LBL_MUST_SELECT_ALL_OR_CHOSEN_LISTS"),
    function() {
      console.log('popoopopopo');
        return JSON.parse(checkAllOrSelectedLists());
    }
);


/* VIEWS CUSTOM CODE */
switch (viewType()) {
  case "edit":
  case "quickcreate":
  case "popup":
    function toggle_message_for() {
        const isChecked = $("#select_all").is(":checked");
        let multiSelect = $("#prospect_lists");
        if (isChecked) {
          multiSelect.prop("disabled", "disabled");
          multiSelect.css("background-color", "#bcbcbc");
        } else {
          multiSelect.prop("disabled", "");
          multiSelect.css("background-color", "");
        }
    }
    $(document).ready(function() {
      addEditCreateTemplateLinks();
      $("#select_all").on("click", toggle_message_for);
      toggle_message_for();

      (function removeWhatsappOption() {
        var $type = $("#type");
        if ($type.length) {
          $type.find('option[value="whatsapp_web"]').remove();
        }
      })();
    });
    if (viewType() == "quickcreate") {
      // Disabling the "Campaign" field relationship
      $("#campaigns_stic_message_marketing_name").prop("disabled", "disabled").css("background-color", "#bcbcbc").prop("readonly", "readonly");
      $("#btn_campaigns_stic_message_marketing_name").prop("disabled", "disabled");
      $("#btn_clr_campaigns_stic_message_marketing_name").prop("disabled", "disabled");
    }


    break;

  case "detail":
    break;

  case "list":
    break;

  default:
    break;
}

/**
 * Check if there is a person or a account or both
 */
function checkAllOrSelectedLists() {
    var checkFlag = $("#select_all").is(':checked');
    var numListsSelected = $("#prospect_lists").find(':selected').length;

    if (checkFlag || numListsSelected > 0) {
        return true;
    }
    return false;
}

function addEditCreateTemplateLinks() {
  if ($("#template_id_edit_link").length == 0) {
    var $select = $("#template_id");
    var $div = $select.parent();

    $select.css("width","50%");

    var editText = SUGAR.language.translate("app_strings", "LNK_EDIT");
    var $editLink = $('<a href="#" id="template_id_edit_link" style="margin-left:10px;">'+editText+'</a>').on("click", function(e) {
      e.preventDefault();
      edit_mm_email_template_form();
    });
    $div.append($editLink);

    var createText = SUGAR.language.translate("app_strings", "LNK_CREATE");
    var $createLink = $('<a href="#" id="template_id_create_link" style="margin-left:10px;">'+createText+'</a>').on("click", function(e) {
      e.preventDefault();
      open_mm_email_template_form();
    });
    $div.append($createLink);
  }
}

function open_mm_email_template_form() {
  var URL = "index.php?module=EmailTemplates&action=EditView&type=sms";
  URL += "&inboundEmail=false&show_js=1";

  var windowName = 'email_template';
  var windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

  var win = window.open(URL, windowName, windowFeatures);
  if (window.focus) {
      // put the focus on the popup if the browser supports the focus() method
      win.focus();
  }
}

function edit_mm_email_template_form() {
  var URL = "index.php?module=EmailTemplates&action=EditView&type=sms";

  var field = document.getElementById('template_id');
  if (field.options[field.selectedIndex].value != 'undefined') {
      URL += "&record=" + field.options[field.selectedIndex].value;
  }
  URL += "&inboundEmail=null&show_js=1";

  var windowName = 'email_template';
  var windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

  var win = window.open(URL, windowName, windowFeatures);
  if (window.focus) {
      // put the focus on the popup if the browser supports the focus() method
      win.focus();
  }
}

function refresh_email_template_list(template_id, template_name) {
  var field = document.getElementById('template_id');
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

function template_change() {
  console.log('template_chage #' + $("#template_id").val() +'#');
  if ($("#template_id").val() == "") {
    $("#template_id_edit_link").hide();
  } else {
    $("#template_id_edit_link").show();
  }
}
