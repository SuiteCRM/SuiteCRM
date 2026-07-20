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
var module = "stic_Messages";

// Helper to get UI config for a message type
function sticGetHelperConfig(type) {
  if (typeof STIC_HELPERS_CONFIG !== 'undefined' && STIC_HELPERS_CONFIG[type]) {
    return STIC_HELPERS_CONFIG[type];
  }
  return null;
}

function sticIsConversationType(type) {
  var config = sticGetHelperConfig(type);
  return config && config.fixedStatus === 'sent' && config.lockSender && config.hideAttachment;
}

/* VIEWS CUSTOM CODE */

var sticViewType = originView = viewType();
if ((sticViewType == 'detail' || sticViewType == 'list') && $("select[name='parent_type']").length > 0) {
  sticViewType = 'edit';
}

switch (sticViewType) {
  case "edit":
  case "quickcreate":
  case "popup":
    $(document).ready(function() {
      setAutofill(["name"]);
      state = $('#status').val();
      var messageType = $('#type').val();
      
      addEditCreateTemplateLinks();

      // Function to lock sender field and sync with assigned user
      function lockSenderField() {
        $('#sender').prop('disabled', true);
        $('#sender').attr('readonly', true);
        $('#sender').css('background', '#F8F8F8');
        $('#sender').css('border-color', '#E2E7EB');

        syncSenderWithAssignedUser();
      }

      // Function to lock sender and status fields when type is WhatsAppWeb or private_area
      function lockSenderAndStatus(statusValue) {
        $('#status').val(statusValue);
        $('#status').prop('disabled', true);
        $('#status').attr('readonly', true);
        $('#status').css('background', '#F8F8F8');
        $('#status').css('border-color', '#E2E7EB');

        lockSenderField();
      }

      // Keep sender synchronized with assigned user when config says lockSender
      function syncSenderWithAssignedUser() {
        var type = $('#type').val();
        var config = sticGetHelperConfig(type);
        if (!config || !config.lockSender) {
          return;
        }

        var assignedUserName = ($('#assigned_user_name').val() || '').trim();
        if (assignedUserName) {
          $('#sender').val(assignedUserName);
        }
      }

      // Function to unlock sender and status fields for new messages that are not of type WhatsAppWeb or conversation
      function unlockSenderAndStatusForNewMessage() {
        $('#status').prop('disabled', false);
        $('#status').attr('readonly', false);
        $('#status').css('background', '');
        $('#status').css('border-color', '');

        $('#sender').prop('disabled', false);
        $('#sender').attr('readonly', false);
        $('#sender').css('background', '');
        $('#sender').css('border-color', '');
      }

      // Function to unlock status field only (for types with lockSender but no fixedStatus)
      function unlockStatusField() {
        $('#status').prop('disabled', false);
        $('#status').attr('readonly', false);
        $('#status').css('background', '');
        $('#status').css('border-color', '');
      }
      
      // Messages with fixedStatus are always sent and cannot be edited
      var currentConfig = sticGetHelperConfig(messageType);
      if (currentConfig && currentConfig.fixedStatus && $('#EditView input[name="record"]').val()) {
        // Disable all fields except parent relationship
        $('#type').prop('disabled', true);
        $('#type').attr('readonly', true);
        $('#type').css('background', '#F8F8F8');
        $('#type').css('border-color', '#E2E7EB');

        $('#sender').prop('disabled', true);
        $('#sender').attr('readonly', true);
        $('#sender').css('background', '#F8F8F8');
        $('#sender').css('border-color', '#E2E7EB');
        
        $('#phone').prop('disabled', true);
        $('#phone').attr('readonly', true);
        $('#phone').css('background', '#F8F8F8');
        $('#phone').css('border-color', '#E2E7EB');

        $('#message').prop('disabled', true);
        $('#message').attr('readonly', true);
        $('#message').css('background', '#F8F8F8');
        $('#message').css('border-color', '#E2E7EB');

        $('#template_id').prop('disabled', true);
        $('#template_id').attr('readonly', true);
        $('#template_id').css('background', '#F8F8F8');
        $('#template_id').css('border-color', '#E2E7EB');

        $('#status').prop('disabled', true);
        $('#status').attr('readonly', true);
        $('#status').css('background', '#F8F8F8');
        $('#status').css('border-color', '#E2E7EB');

        $("#template_id_edit_link").addClass("ui-state-disabled");
        $("#template_id_create_link").addClass("ui-state-disabled");

      }
      
      if ($('#EditView input[name="record"]').val()) {
        // Status can only be changed through actions
        $('#status').prop('disabled', true);
        $('#status').attr('readonly', true);
        $('#status').css('background', '#F8F8F8');
        $('#status').css('border-color', '#E2E7EB');
      }
      if (state !== 'draft' && $('#EditView input[name="record"]').val()) {
        // Some fields can only be edited when message is in draft
        $('#type').prop('disabled', true);
        $('#type').attr('readonly', true);
        $('#type').css('background', '#F8F8F8');
        $('#type').css('border-color', '#E2E7EB');

        $('#sender').prop('disabled', true);
        $('#sender').attr('readonly', true);
        $('#sender').css('background', '#F8F8F8');
        $('#sender').css('border-color', '#E2E7EB');
        
        $('#phone').prop('disabled', true);
        $('#phone').attr('readonly', true);
        $('#phone').css('background', '#F8F8F8');
        $('#phone').css('border-color', '#E2E7EB');

        $('#message').prop('disabled', true);
        $('#message').attr('readonly', true);
        $('#message').css('background', '#F8F8F8');
        $('#message').css('border-color', '#E2E7EB');

        $('#template_id').prop('disabled', true);
        $('#template_id').attr('readonly', true);
        $('#template_id').css('background', '#F8F8F8');
        $('#template_id').css('border-color', '#E2E7EB');

        $("#template_id_edit_link").addClass("ui-state-disabled");
        $("#template_id_create_link").addClass("ui-state-disabled");
      }

      var originalStatusOptions = [];

      function filterStatusOptionsByType() {
        var type = $('#type').val();
        var config = sticGetHelperConfig(type);
        var $statusSelect = $('#status');
        if (!$statusSelect.length) {
          return;
        }

        if (originalStatusOptions.length === 0) {
          $statusSelect.find('option').each(function () {
            originalStatusOptions.push({ value: $(this).val(), text: $(this).text() });
          });
        }

        var currentValue = $statusSelect.val();
        var allowedStatuses = config && config.allowedStatus;

        $statusSelect.empty();

        originalStatusOptions.forEach(function (opt) {
          if (!allowedStatuses || allowedStatuses.indexOf(opt.value) !== -1) {
            $statusSelect.append($('<option>').val(opt.value).text(opt.text));
          }
        });

        if ($statusSelect.find('option[value="' + currentValue + '"]').length) {
          $statusSelect.val(currentValue);
        }
      }

      // When type changes, lock/unlock sender and status according to config
      $('#type').on('change', function() {
        var type = $(this).val();
        var config = sticGetHelperConfig(type);
        if (config) {
          if (config.fixedStatus) {
            lockSenderAndStatus(config.fixedStatus);
          } else if (config.lockSender) {
            unlockStatusField();
            lockSenderField();
          } else if (!$('#EditView input[name="record"]').val()) {
            unlockSenderAndStatusForNewMessage();
          }
        } else if (!$('#EditView input[name="record"]').val()) {
          unlockSenderAndStatusForNewMessage();
        }
        // Refresh templates list when type changes
        try { refreshTemplateOptions(); } catch (e) { console.log('refreshTemplateOptions error', e); }
        toggleParentTypeForConversation();
        filterStatusOptionsByType();
      });

      // On page load, apply config-based behavior
      var loadConfig = sticGetHelperConfig($('#type').val());
      if (loadConfig && loadConfig.fixedStatus) {
        lockSenderAndStatus(loadConfig.fixedStatus);
      } else if (loadConfig && loadConfig.lockSender) {
        lockSenderField();
      }
      filterStatusOptionsByType();

  // On load, refresh templates select according to current type
  try { refreshTemplateOptions(); } catch (e) { console.log('refreshTemplateOptions error', e); }
      // Keep sender synced with assigned user for types that lock sender
      function bindAssignedUserChanges() {
        var $assignedUserName = $('#assigned_user_name');
        var $assignedUserId = $('#assigned_user_id');
        var lastAssignedUserName = $assignedUserName.val() || '';

        $assignedUserName.add($assignedUserId).on('change keyup blur', syncSenderWithAssignedUser);

        $('#btn_assigned_user_name, #btn_clr_assigned_user_name').on('click', function() {
          setTimeout(syncSenderWithAssignedUser, 300);
        });

        setInterval(function() {
          var currentName = $assignedUserName.val() || '';
          if (currentName !== lastAssignedUserName) {
            lastAssignedUserName = currentName;
            syncSenderWithAssignedUser();
          }
        }, 500);
      }

      bindAssignedUserChanges();

      if($("#mass_ids").val()) {
        $('#parent_name').prop('disabled', true);
        $('#parent_name').attr('readonly', true);
        $('#parent_name').css('background', '#F8F8F8');
        $('#parent_name').css('border-color', '#E2E7EB');

        $('#parent_type').prop('disabled', true);
        $('#parent_type').attr('readonly', true);
        $('#parent_type').css('background', '#F8F8F8');
        $('#parent_type').css('border-color', '#E2E7EB');

        $('#btn_parent_name').prop('disabled', true);
        $('#btn_parent_name').css('background-color', '#F8F8F8 !important');
        $('#btn_clr_parent_name').prop('disabled', true);
        $('#btn_clr_parent_name').css('background-color', '#F8F8F8 !important');
      }

      $("#template_id").on("change paste keyup", template_change);
      if ($("#template_id").val() == "") {
        $("#template_id_edit_link").hide();
      } else {
        $("#template_id_edit_link").show();
      }

      // Find element with data-label = LBL_INFO
      var infoElement = $('div[data-label="LBL_INFO"]');
      // Clear div content from infoElement
      infoElement.html('');
      // Supress bottom margin
      infoElement.parent().css('margin-bottom', '0px');

      // Toggle conversation fields visibility and validation based on type and new conversation checkbox
      toggleConversationFieldsByType();
      toggleParentTypeForConversation();
      $('#type').on('change', toggleConversationFieldsByType);
      $('#new_conversation[type="checkbox"]').on('change', toggleConversationFieldsByType);

      // Conversations messages are always sent and cannot be edited
      if(messageType === 'private_area'  && $('#EditView input[name="record"]').val()) {
          var $newConversationCheckbox = $('#new_conversation[type="checkbox"]');
          var $conversationName = $('#stic_conversations_stic_messages_name');

          $newConversationCheckbox.prop('disabled', true);

          $conversationName.prop('disabled', true);
          $conversationName.attr('readonly', true);
          $conversationName.css('background', '#F8F8F8');
          $conversationName.css('border-color', '#E2E7EB');

          $('#btn_stic_conversations_stic_messages_name').prop('disabled', true);
          $('#btn_clr_stic_conversations_stic_messages_name').prop('disabled', true);
          $('#btn_stic_conversations_stic_messages_name').hide();
          $('#btn_clr_stic_conversations_stic_messages_name').hide();
      }

      // If we are in the conversations subpanel quick create, initialize conversation logic
      var $wrapper = $('#whole_subpanel_stic_conversations_stic_messages');
      if ($wrapper.length > 0) {
        var $form = $wrapper.find('form').first();
        if ($form.length > 0) {
          initSubpanelConversationLogic($form);
        }
      }

    });

    break;

  case "detail":
    // Get record Id 
    recordId = $("#formDetailView input[type=hidden][name=record]").val();
    var messageType = $("#type").val();

    // If message is not of conversation type, hide conversation field and new conversation checkbox
    if (!sticIsConversationType(messageType)) {
      $('div[data-field="new_conversation"]').hide();
      $('div[data-field="stic_conversations_stic_messages_name"]').hide();
    }
    
    // Define button content - Don't show retry button if config says no retry
    var detailConfig = sticGetHelperConfig(messageType);
    var canRetry = detailConfig ? detailConfig.canRetry : true;
    if ($("#status").val() != 'sent' && canRetry) {
      var buttons = {
        retry: {
          id: "bt_retry_detailview",
          title: SUGAR.language.get("stic_Messages", "LBL_MASS_RETRY_MESSAGE_BUTTON_TITTLE"),
          // onclick: "window.location='index.php?module=stic_Incorpora&action=fromDetailView&record=" + recordId + "&return_module="+ module +"'"
          onclick: "onClickRetryMessagesButton(recordId)"
        }
      };
      createDetailViewButton(buttons.retry);
    }
    // Field response is only showed when an error is present
    if ($("#status").val() != 'error') {
      $('div[data-field="response"]').hide();
    }
    break;

  case "list":
    var buttons = {
      retry: {
        id: "bt_retryMessage_listview",
        title: SUGAR.language.get("stic_Messages", "LBL_MASS_RETRY_MESSAGE_BUTTON_TITTLE"),
        text: SUGAR.language.get("stic_Messages", "LBL_MASS_RETRY_MESSAGE_BUTTON_TITTLE"),
        onclick: "onClickMassRetryMessagesButton()"
    }
  };

  createListViewButton(buttons.retry);
    break;

  default:
    break;
}

function refreshTemplateOptions() {
  var messageType = ($('#type').val() || '').toLowerCase();
  var templateType = 'sms';
  if (messageType.indexOf('whatsapp') !== -1) {
    templateType = 'whatsapp';
  } else if (messageType.indexOf('sms') !== -1) {
    templateType = 'sms';
  }

  $.ajax({
    url: 'index.php?module=stic_Messages&action=FillDynamicListMessageTemplate',
    method: 'POST',
    data: { type: templateType },
    dataType: 'json'
  }).done(function(res) {
    if (!res || !res.success) {
      console.log('No templates response or error', res);
      return;
    }
    var $select = $('#template_id');
    if ($select.length === 0) return;
    var current = $select.val();
    $select.empty();
    $select.append($('<option>').val('').text(SUGAR.language.get('app_strings', 'LBL_NONE')));
    if (Array.isArray(res.data)) {
      res.data.forEach(function(t) {
        $select.append($('<option>').val(t.id).text(t.name));
      });
    } else {
      for (var id in res.data) {
        if (!res.data.hasOwnProperty(id)) continue;
        $select.append($('<option>').val(id).text(res.data[id]));
      }
    }
    if (current) {
      $select.val(current);
    }
    template_change();
  }).fail(function(xhr) {
    console.log('Error fetching templates', xhr);
  });
}

function decodeHtmlEntities(text) {
    var txt = document.createElement('textarea');
    txt.innerHTML = text;
    return txt.value;
}

function onTemplateSelect(args) {
    var confirmed = function (args) {
      var args = JSON.parse(args);
      $.post('index.php?entryPoint=emailTemplateData', {
        // emailTemplateId: args.name_to_value_array.template_id_c
        // emailTemplateId: args.name_to_value_array.template_id
        emailTemplateId: $("#template_id").val()
      }, function (jsonResponse) {
        var response = JSON.parse(jsonResponse);
        $("#message").val(decodeHtmlEntities(response.data.body));
        var tplType = $('#type').val();
        var tplConfig = sticGetHelperConfig(tplType);
        if (tplConfig && tplConfig.lockMessageOnTemplate) {
          $('#message').prop('disabled', true);
          $('#message').attr('readonly', true);
          $('#message').css('background', '#F8F8F8');
          $('#message').css('border-color', '#E2E7EB');
        }
      });
      set_return(args);
    };

    var mb = messageBox();
    mb.setTitle(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE'));
    mb.setBody(SUGAR.language.translate('stic_Messages', 'LBL_CONFIRM_APPLY_MESSAGES_TEMPLATE_BODY'));
    mb.css('z-index', 26000);
    mb.show();

    var args = JSON.stringify(args);

    mb.on('ok', function () {
      "use strict";
      confirmed(args);
      mb.remove();
    });

    mb.on('cancel', function () {
      "use strict";
      mb.remove();
    });
  };

function showMessageBox(title, detail, onOk = null, onCancel = null) {
  var mb = messageBox({backdrop:'static'});
  mb.setTitle(title);
  mb.setBody(detail);
  if (!onCancel){
    mb.hideCancel();
  }
  mb.css('z-index', 26000);
  mb.show();
  mb.on('ok', function () {
    "use strict";
    mb.remove();
    if(onOk){
      onOk();
    }
  });
}



function onClickRetryMessagesButton(recordId) {
  var status = $("#status").val();
  var messageType = $("#type").val();
  var retryConfig = sticGetHelperConfig(messageType);
  
  if(retryConfig && !retryConfig.canRetry) {
    showMessageBox(SUGAR.language.get('stic_Messages', 'LBL_ERROR'), SUGAR.language.get('stic_Messages', 'LBL_WHATSAPP_WEB_RETRY'));
  }
  else if(status === 'sent') {
    showMessageBox(SUGAR.language.get('stic_Messages', 'LBL_ERROR'), SUGAR.language.get('stic_Messages', 'LBL_ALREADY_SENT'));
  }
  else {
    $.ajax({
      url: "index.php?module=stic_Messages&action=retryOne",
      type:"post",
      dataType: "json",
      async: false,
      data: {
          'recordId':recordId
      },
      success: function(res) {
          if (res.success) {
            showMessageBox(res.title, res.detail,function() {window.location.reload();});
          } else {
              showMessageBox(res.title, res.detail);
          }
      },
      error: function() {
          showMessageBox(SUGAR.language.get('stic_Messages', 'LBL_ERROR'), SUGAR.language.get('stic_Messages', 'LBL_MESSAGE_NOT_SENT'));
      }
    });
  }
}



function onClickMassRetryMessagesButton() {
  // confirmation panel
  var confirmed = function () {
    sugarListView.get_checks();
    if(sugarListView.get_checks_count() < 1) {
        alert(SUGAR.language.get('app_strings', 'LBL_LISTVIEW_NO_SELECTED'));
        return false;
    }
    document.MassUpdate.action.value='Retry';
    document.MassUpdate.module.value='stic_Messages';
    document.MassUpdate.submit();
  };

  var mb = messageBox();
  mb.setTitle(SUGAR.language.translate('stic_Messages', 'LBL_CONFIRM_SEND_BULK_MESSAGES_TITLE'));
  mb.setBody(SUGAR.language.translate('stic_Messages', 'LBL_CONFIRM_APPLY_SEND_BULK_MESSAGES_BODY'));
  mb.css('z-index', 26000);
  mb.show();

  mb.on('ok', function () {
    "use strict";
    confirmed();
    mb.remove();
  });
  mb.on('cancel', function () {
    "use strict";
    mb.remove();
  });

}

function addEditCreateTemplateLinks() {
  if ($("#template_id_edit_link").length == 0) {
    var $select = $("#template_id");
    var $div = $select.parent();

    $select.css("width","50%");

    var editText = SUGAR.language.translate("app_strings", "LNK_EDIT");
    var $editLink = $('<a href="#" id="template_id_edit_link" style="margin-left:10px;">'+editText+'</a>').on("click", function(e) {
      e.preventDefault();
      edit_email_template_form();
    });
    $div.append($editLink);

    var createText = SUGAR.language.translate("app_strings", "LNK_CREATE");
    var $createLink = $('<a href="#" id="template_id_create_link" style="margin-left:10px;">'+createText+'</a>').on("click", function(e) {
      e.preventDefault();
      open_email_template_form();
    });
    $div.append($createLink);
  }
}

function open_email_template_form() {
  var typeVal = $("#type").val().toLowerCase();
  var templateType = typeVal.indexOf('whatsapp') !== -1 ? 'whatsapp' : 'sms';

  URL = "index.php?module=EmailTemplates&action=EditView&type=" + templateType;
  URL += "&inboundEmail=false&show_js=1";

  windowName = 'email_template';
  windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

  win = window.open(URL, windowName, windowFeatures);
  if (window.focus) {
      // put the focus on the popup if the browser supports the focus() method
      win.focus();
  }
}

function edit_email_template_form() {
  var typeVal = $("#type").val().toLowerCase();
  var templateType = typeVal.indexOf('whatsapp') !== -1 ? 'whatsapp' : 'sms';

  URL = "index.php?module=EmailTemplates&action=EditView&type=" + templateType;

  var field = document.getElementById('template_id');
  if (field.options[field.selectedIndex].value != 'undefined') {
      URL += "&record=" + field.options[field.selectedIndex].value;
  }
  URL += "&inboundEmail=null&show_js=1";

  windowName = 'email_template';
  windowFeatures = 'width=800' + ',height=600' + ',resizable=1,scrollbars=1';

  win = window.open(URL, windowName, windowFeatures);
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
    var tplType = $('#type').val();
    var tplConfig = sticGetHelperConfig(tplType);
    if (tplConfig && tplConfig.lockMessageOnTemplate) {
      $('#message').prop('disabled', false);
      $('#message').attr('readonly', false);
      $('#message').css('background', '');
      $('#message').css('border-color', '');
    }
  } else {
    $("#template_id_edit_link").show();
  }
  if ($("#template_id").val()) {
    updateMessageBox();
  }
}

function updateMessageBox (args) {
    var confirmed = function (args) {
      var args = JSON.parse(args);

      $.post('index.php?entryPoint=emailTemplateData', {
        emailTemplateId: $('#template_id').val()
      }, function (jsonResponse) {
        var response = JSON.parse(jsonResponse);
        $("#message").val(decodeHtmlEntities(response.data.body));
        var msgType = $('#type').val();
        var msgConfig = sticGetHelperConfig(msgType);
        if (msgConfig && msgConfig.lockMessageOnTemplate) {
          $('#message').prop('disabled', true);
          $('#message').attr('readonly', true);
          $('#message').css('background', '#F8F8F8');
          $('#message').css('border-color', '#E2E7EB');
        }
      });
      set_return(args);
    };

    var mb = messageBox();
    mb.setTitle(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE'));
    mb.setBody(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_MESSAGES_TEMPLATE_BODY'));
    mb.css('z-index', 26000);
    mb.show();

    mb.on('ok', function () {
      "use strict";
      var id=$('#emails_email_templates_idb').val();
      var name=$('#emails_email_templates_name').val();
      args = JSON.stringify({"form_name":"ComposeView","name_to_value_array":{"emails_email_templates_idb": id,"emails_email_templates_name": name}})
      confirmed(args);
      mb.remove();
    });

    mb.on('cancel', function () {
      "use strict";
      mb.remove();
    });
  }

/**
 * Opens a WhatsApp conversation window for a given record.
 * Can be called from any module included in stic_Messages messageable modules.
 *
 * @param {String} recordId   The bean id
 * @param {String} parentType The module name (Contacts, Leads, Accounts, Employees...)
 */
function openWhatsAppConversation(recordId, parentType) {
    var parentName = $("#formDetailView input[type=hidden][name=full_name]").val()
                  || $("h1.module-title-text").text().trim()
                  || '';
    var url = 'index.php?module=stic_Messages&action=conversation'
            + '&parent_id='   + encodeURIComponent(recordId)
            + '&parent_type=' + encodeURIComponent(parentType)
            + '&parent_name=' + encodeURIComponent(parentName);
    window.open(
        url,
        'whatsapp_conv_' + recordId,
        'width=480,height=700,resizable=yes,scrollbars=yes'
    );
}
var phoneInitiallyRequired = null;

// Function to clear conversation selection and related fields
function clearConversationSelection() {
  $('#stic_conversations_stic_messages_name').val('');
  $('#stic_conversations_ida').val('');
}

// Function to clear conversation subject field
function clearConversationSubject() {
  $('#stic_conversation_subject').val('');
}

// Function to toggle visibility and validation of conversation fields based on type and new conversation checkbox
function toggleConversationFieldsByType() {
  var formName = getFormName();
  var type = $('#type').val();
  var isConversationType = sticIsConversationType(type);
  var $newConversationCheckbox = $('#new_conversation[type="checkbox"]');
  var isNewConversationChecked = $newConversationCheckbox.is(':checked');
  var conversationLabel = SUGAR.language.get(module, 'LBL_STIC_CONVERSATIONS_STIC_MESSAGES');
  var conversationSubjectLabel = SUGAR.language.get(module, 'LBL_LIST_SUBJECT');
  var phoneLabel = SUGAR.language.get(module, 'LBL_PHONE');

  var $conversationSubjectRow = $('#stic_conversation_subject').closest('.edit-view-row-item');
  if (!$conversationSubjectRow.length) {
    $conversationSubjectRow = $('div[data-field="stic_conversation_subject"]');
  }

  // Hidden by default; only shown for conversation + new conversation.
  $conversationSubjectRow.hide();

  if (!$newConversationCheckbox.length || !$('#stic_conversations_stic_messages_name').length || !$('#stic_conversation_subject').length || !$('#phone').length) {
    return;
  }

  if (phoneInitiallyRequired === null && typeof getRequiredStatus === 'function') {
    phoneInitiallyRequired = getRequiredStatus('phone');
  }

  var $newConversationRow = $newConversationCheckbox.closest('.edit-view-row-item');
  var $conversationRow = $('#stic_conversations_stic_messages_name').closest('.edit-view-row-item');
  var $phoneRequiredMark = $('#phone').closest('.edit-view-row-item').find('.label .required');

  if (isConversationType) {
    $newConversationRow.show();
    addRequiredMark('new_conversation');

    removeFromValidate(formName, 'phone');
    removeRequiredMark('phone');
    $phoneRequiredMark.hide();

    if (isNewConversationChecked) {
      $conversationRow.hide();
      $conversationSubjectRow.show();

      removeFromValidate(formName, 'stic_conversations_stic_messages_name');
      removeRequiredMark('stic_conversations_stic_messages_name');
      removeFromValidate(formName, 'stic_conversation_subject');
      addToValidate(formName, 'stic_conversation_subject', 'varchar', true, conversationSubjectLabel);
      addRequiredMark('stic_conversation_subject');

      $('#stic_conversations_ida').val('');
      clearConversationSelection();
      clearConversationSubject();

      $('#stic_conversations_stic_messages_name').prop('readonly', false).attr('disabled', false);
      $('#stic_conversation_subject').prop('readonly', false).attr('disabled', false);
      $('#btn_stic_conversations_stic_messages_name, #btn_clr_stic_conversations_stic_messages_name').hide();
    } else {
      $conversationRow.show();
      $conversationSubjectRow.hide();
      removeFromValidate(formName, 'stic_conversations_stic_messages_name');
      addToValidate(formName, 'stic_conversations_stic_messages_name', 'relate', true, conversationLabel);
      addRequiredMark('stic_conversations_stic_messages_name');

      removeFromValidate(formName, 'stic_conversation_subject');
      removeRequiredMark('stic_conversation_subject');
      clearConversationSubject();

      $('#stic_conversations_stic_messages_name').prop('readonly', false).attr('disabled', false);
      $('#btn_stic_conversations_stic_messages_name, #btn_clr_stic_conversations_stic_messages_name').show();
    }
  } else {
    $newConversationRow.hide();
    $conversationRow.hide();
    $conversationSubjectRow.hide();

    $newConversationCheckbox.prop('checked', false);
    clearConversationSelection();
    clearConversationSubject();

    removeFromValidate(formName, 'stic_conversations_stic_messages_name');
    removeRequiredMark('stic_conversations_stic_messages_name');
    removeFromValidate(formName, 'stic_conversation_subject');
    removeRequiredMark('stic_conversation_subject');
    removeFromValidate(formName, 'new_conversation');
    removeRequiredMark('new_conversation');

    if (phoneInitiallyRequired === true) {
      $phoneRequiredMark.show();
      removeRequiredMark('phone');
      removeFromValidate(formName, 'phone');
      if (typeof getRequiredStatus === 'function' && !getRequiredStatus('phone')) {
        addToValidate(formName, 'phone', 'phone', true, phoneLabel);
      }
    }
  }
}
// Function to toggle parent type field based on conversation type and mass update
function toggleParentTypeForConversation() {
  var type = $('#type').val();
  var isConversationType = sticIsConversationType(type);
  var $parentType = $('#parent_type');

  if (!$parentType.length) {
    return;
  }

  if (isConversationType) {
    if ($parentType.val() !== 'Contacts') {
      $parentType.val('Contacts').trigger('change');
    }

    $parentType.prop('disabled', true);
    $parentType.attr('readonly', true);
    $parentType.css('background', '#F8F8F8');
    $parentType.css('border-color', '#E2E7EB');
  } else if (!$('#mass_ids').val()) {
    $parentType.prop('disabled', false);
    $parentType.attr('readonly', false);
    $parentType.css('background', '');
    $parentType.css('border-color', '');
  }
}

// Function to initialize conversation logic in the conversations subpanel quick create form
function initSubpanelConversationLogic($form) {
  if (!$form || !$form.length || $form.hasClass('stic-custom-init')) {
    return;
  }
  $form.addClass('stic-custom-init');

    var formName = $form.attr('id') || $form.attr('name');
    var messageLabel = SUGAR.language.get('stic_Messages', 'LBL_MESSAGE') || 'LBL_MESSAGE';
    var senderLabel = SUGAR.language.get('stic_Messages', 'LBL_SENDER') || 'LBL_SENDER';

    // If the validate array is not defined for the form, initialize it to avoid errors when adding/removing validation rules
    if (typeof validate !== 'undefined' && formName) {
      if (typeof validate[formName] === 'undefined') {
        validate[formName] = [];
      }
    }

    $form.find('#type').val('private_area').prop('disabled', true).css({'background': '#F8F8F8', 'border-color': '#E2E7EB'});
    var convConfig = sticGetHelperConfig('private_area');
    $form.find('#status').val(convConfig ? convConfig.fixedStatus : 'sent').prop('disabled', true).attr('readonly', true).css({'background': '#F8F8F8', 'border-color': '#E2E7EB'});
    var assignedUserName = $form.find('#assigned_user_name').val();
    if (assignedUserName) {
      $form.find('#sender').val(assignedUserName);
    }
    $form.find('#sender').prop('disabled', true).attr('readonly', true).css({'background': '#F8F8F8', 'border-color': '#E2E7EB'});
    
    if ($form.find('#hidden_type_fixed').length === 0) {
        $form.append('<input type="hidden" id="hidden_type_fixed" name="type" value="conversation">');
        $form.append('<input type="hidden" id="hidden_parent_fixed" name="parent_type" value="Contacts">');
    }

    $form.find('#parent_type').val('Contacts').prop('disabled', true).css({'background': '#F8F8F8', 'border-color': '#E2E7EB'});
    $form.find('#new_conversation[type="checkbox"]').prop('checked', false).prop('disabled', true).closest('.edit-view-row-item').hide();

    // Remove required validation phone
    if (typeof validate !== 'undefined' && formName && typeof validate[formName] !== 'undefined') {
      removeFromValidate(formName, 'phone');
    }
    $form.find('#phone').closest('.edit-view-row-item').find('.label').removeClass('required').removeClass('conditional-required');
    $form.find('#phone').closest('.edit-view-row-item').find('.label .required').hide();
    
    $form.find('#stic_conversations_stic_messages_name').attr('readonly', true).css({'background': '#F8F8F8'}).closest('.edit-view-row-item').show();
    $form.find('#stic_conversations_stic_messages_name').attr('disabled', true);
    $form.find('#stic_conversation_subject').val('').closest('.edit-view-row-item').hide();
    $form.find('#btn_stic_conversations_stic_messages_name, #btn_clr_stic_conversations_stic_messages_name').hide();
    $form.find('#parent_name').attr('readonly', true).css({'background': '#F8F8F8'});
    $form.find('#parent_name').attr('disabled', true);
    $form.find('#btn_parent_name, #btn_clr_parent_name').hide();

    // Validate message and sender fields on save
    $form.find('input[name="button"], input[type="submit"]').on('mousedown', function(e) {
      // Check if the clicked button is a cancel action by looking at its id, title and value attributes
      var $clickedButton = $(this);
      var buttonId = ($clickedButton.attr('id') || '').toUpperCase();
      var buttonTitle = ($clickedButton.attr('title') || '').toLowerCase();
      var buttonValue = ($clickedButton.val() || '').toLowerCase();

      // Do not validate on Cancel actions
      if (buttonId === 'CANCEL' || buttonTitle.indexOf('cancel') !== -1 || buttonValue.indexOf('cancel') !== -1) {
        return true;
      }

        var msgVal = $form.find('#message').val() || '';
        var sndVal = $form.find('#sender').val() || '';
        var errorFound = false;

        if (typeof clear_all_errors === 'function') clear_all_errors();

        var missingMsg = SUGAR.language.get('app_strings', 'ERR_MISSING_REQUIRED_FIELDS');

        if ($.trim(msgVal) === '') {
            add_error_style(formName, 'message', missingMsg + " " + messageLabel, true);
            errorFound = true;
        }
        if ($.trim(sndVal) === '') {
            add_error_style(formName, 'sender', missingMsg + " " + senderLabel, true);
            errorFound = true;
        }

        if (errorFound) {
            e.preventDefault();
            e.stopImmediatePropagation();
            return false;
        }
    });

    // Get record Id from parent form and fetch conversation data to prefill fields
    var recordId = $('#formDetailView input[name="record"]').val() || new URLSearchParams(window.location.search).get('record');
    if (recordId) {
        $.ajax({
            url: 'index.php?module=stic_Messages&action=getConversationData',
            type: 'POST',
            dataType: 'json',
            data: { conversationId: recordId },
            success: function(res) {
                if (res && res.code === 'OK' && res.data) {
                    $form.find('#stic_conversations_ida').val(res.data.conversation_id || '');
                    $form.find('#stic_conversations_stic_messages_name').val(res.data.conversation_name || '');
                    $form.find('#parent_id').val(res.data.parent_id || '');
                    $form.find('#parent_name').val(res.data.parent_name || '');
                }
            }
        });
  }
}
