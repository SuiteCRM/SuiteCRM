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

var module = 'stic_Messages';
if (!($("#mass_ids").length > 0) || $("#mass_ids").val() == ''){
  addToValidateCallback(
    getFormName(),
    "parent_id",
    "related",
    true,
    SUGAR.language.get(module, "LBL_LIST_RELATED_TO"),
    function() {
        return true;
    }
  );
  addRequiredMark('parent_id', 'conditional-required');
}



(function ($) {
  /**
   *
   * @param options
   * @returns {jQuery|HTMLElement}
   * @constructor
   */
  $.fn.stic_MessagesComposeView = function (options) {
    "use strict";
    var self = $(this);

    /**
     * @return string UUID
     */
    self.generateID = function () {
      "use strict";
      var characters = ['a', 'b', 'c', 'd', 'e', 'f', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      var format = '0000000-0000-0000-0000-00000000000';
      return Array.prototype.map.call(format, function ($obj) {
        var min = 0;
        var max = characters.length - 1;

        if ($obj === '0') {
          var index = Math.round(Math.random() * (max - min) + min);
          $obj = characters[index];
        }

        return $obj;
      }).toString().replace(/(,)/g, '');
    };


    /**
     * Constructor
     */
    self.construct = function () {
      "use strict";

      if (self.length === 0) {
        console.error('stic_MessagesComposeView - Invalid Selector');
        return;
      }

      if (self.attr('id').length === 0) {
        console.warn('stic_MessagesComposeView - expects element to have an id. stic_MessagesComposeView has generated one.');
        self.attr('id', self.generateID());
      }


      $(self).trigger("constructstic_MessagesComposeView", [self]);
    };

    /**
     * @destructor
     */
    self.destruct = function () {
      
      return true;
    };

    self.construct();

    return $(self);
  };


  $.fn.stic_MessagesComposeView.showAjaxErrorMessage = function (response) {
    var message = '';
    $.each(response.errors, function (i, v) {
      message = message + v.title;
    });
    var mb = messageBox();
    mb.setBody(message);
    mb.show();

    mb.on('ok', function () {
      "use strict";
      mb.remove();
    });

    mb.on('cancel', function () {
      "use strict";
      mb.remove();
    });
  };


  $.fn.stic_MessagesComposeView.onParentSelect = function (args) {
    set_return(args);
    if ($("#status").val() == 'draft') {
      if (args.name_to_value_array.phone_mobile && args.name_to_value_array.phone_mobile !== 'undefined' && args.name_to_value_array.phone_mobile !== '') {
        $("#phone").val(args.name_to_value_array.phone_mobile);  
      }
      if (args.name_to_value_array.phone_office && args.name_to_value_array.phone_office !== 'undefined' && args.name_to_value_array.phone_office !== '') {
        $("#phone").val(args.name_to_value_array.phone_office);  
      }
    }
  };
  $.fn.stic_MessagesComposeView.onParentChange = function (args) {
    if ($("#status").val() == 'draft') {
      if (args.name_to_value_array.phone_mobile && args.name_to_value_array.phone_mobile !== 'undefined' && args.name_to_value_array.phone_mobile !== '') {
        $("#phone").val(args.name_to_value_array.phone_mobile);  
      }
      if (args.name_to_value_array.phone_office && args.name_to_value_array.phone_office !== 'undefined' && args.name_to_value_array.phone_office !== '') {
        $("#phone").val(args.name_to_value_array.phone_office);  
      }
    }
  };

function checkStatus() {
  if($('#status').val() === 'sent' && !$('#EditView input[name="record"]').val()) {
    $('input.button.primary').val(SUGAR.language.get('app_strings', 'LBL_EMAIL_SEND'));
  } 
  else {
    $('input.button.primary').val(SUGAR.language.get('app_strings', 'LBL_SAVE_BUTTON_LABEL'));
  }
}
  
  $('#status').on('change', checkStatus);
  checkStatus();

}(jQuery));

YAHOO.util.Event.addListener('parent_id','change',parentIdChanged);

// Function to check if an existing conversation is selected
function isExistingConversationSelected() {
  return (
    $('#type').val() === 'private_area'
    && !$('#new_conversation').is(':checked')
    && $('#stic_conversations_ida').val() !== ''
  );
}

// Function to apply conversation data to the form fields
function applyConversationData(conversationData) {
  if (!conversationData) {
    return;
  }

  if (conversationData.assigned_user_id) {
    $('#assigned_user_id').val(conversationData.assigned_user_id).trigger('change');
  }

  if (typeof conversationData.assigned_user_name !== 'undefined' && conversationData.assigned_user_name !== null) {
    $('#assigned_user_name').val(conversationData.assigned_user_name).trigger('change');
  }

  if (conversationData.assigned_user_name) {
    $('#sender').val(conversationData.assigned_user_name);
  } else if (conversationData.sender) {
    $('#sender').val(conversationData.sender);
  }

  if (conversationData.parent_type) {
    $('#parent_type').val(conversationData.parent_type);
  }

  if (conversationData.parent_id) {
    $('#stic_conversations_ida').data('parent-id', conversationData.parent_id);
    $('#parent_id').val(conversationData.parent_id).trigger('change');
  } else {
    $('#stic_conversations_ida').data('parent-id', '');
    $('#parent_id').val('').trigger('change');
  }

  if (typeof conversationData.parent_name !== 'undefined' && conversationData.parent_name !== null) {
    $('#parent_name').val(conversationData.parent_name);
  } else if (!conversationData.parent_id) {
    $('#parent_name').val('');
  }
}

// Function to fetch conversation data asynchronously
function getConversationDataAsync(conversationId, callbackFunction) {
  $.ajax({
    url: 'index.php?module=stic_Messages&action=getConversationData',
    type: 'post',
    dataType: 'json',
    data: {
      conversationId: conversationId,
    },
    success: function(resultado) {
      if (resultado.code === 'OK') {
        callbackFunction(resultado.data);
      }
    },
  });
}

// Function to load selected conversation data into the form
function loadSelectedConversationData() {
  if (!isExistingConversationSelected()) {
    return;
  }

  var conversationId = $('#stic_conversations_ida').val();
  getConversationDataAsync(conversationId, applyConversationData);
}

// Function to check if the popup selection is for a conversation
function isConversationPopupSelection(popupReplyData) {
  if (!popupReplyData || !popupReplyData.name_to_value_array) {
    return false;
  }

  return typeof popupReplyData.name_to_value_array.stic_conversations_ida !== 'undefined';
}

// Function to check if the popup selection is for a parent (contact)
function isParentPopupSelection(popupReplyData) {
  if (!popupReplyData || !popupReplyData.name_to_value_array) {
    return false;
  }

  return typeof popupReplyData.name_to_value_array.parent_id !== 'undefined';
}

function clearConversationSelectionGlobal(triggerChange) {
  $('#stic_conversations_stic_messages_name').val('');
  $('#stic_conversations_ida').val('');
  $('#stic_conversations_ida').data('parent-id', '');
  if (triggerChange) {
    $('#stic_conversations_ida').trigger('change');
  }
}

var sticMessagesSkipParentClear = false;

if (typeof window.set_return === 'function') {
  var sticMessagesOriginalSetReturn = window.set_return;
  window.set_return = function(popupReplyData) {
    sticMessagesOriginalSetReturn(popupReplyData);

    if (isConversationPopupSelection(popupReplyData)) {
      loadSelectedConversationData();
    } else if (isParentPopupSelection(popupReplyData)) {
      var isConversationType = $('#type').val() === 'private_area';
      var isNewConversation = $('#new_conversation').is(':checked');
      var conversationId = $('#stic_conversations_ida').val() || '';
      var conversationParentId = $('#stic_conversations_ida').data('parent-id') || '';
      var newParentId = popupReplyData.name_to_value_array.parent_id || '';

      if (isConversationType && !isNewConversation && conversationId && (!conversationParentId || newParentId !== conversationParentId)) {
        sticMessagesSkipParentClear = true;
        clearConversationSelectionGlobal(true);
      }

      $('#parent_id').trigger('change');
    }
  };
}

function parentIdChanged() {
  let parentId = $('#parent_id').val();
  let parentType = $('#parent_type').val();

  if ((parentId !== null && parentId !== '')) {
    getParentAsync(parentId, parentType, applyParent);
  }

}

function applyParent(parentData) {
  if(parentData!= null && ($("#status").val() === 'draft' || !$('#EditView input[name="record"]').val())) {
    $('#phone').val(parentData['phone']);
  }
}

function getParentAsync(parentId, parentType, callbackFunction) {
  $.ajax({
    url: "index.php?module=stic_Messages&action=getParentPhone",
    type: "post",
    dataType: "json",
    data: {
      "parentId": parentId,
      "parentType": parentType
    },
    success: function(resultado) {
      if (resultado.code == 'OK') {
        callbackFunction(resultado.data);
      }
      else if (resultado.code == 'No data') {
        console.log('No data');
        return null;
      }
      else {
        console.log('Error:', resultado.code);
      }
    }
  });
}

$(function () {
  // Function to clear conversation selection fields
  function clearConversationSelectionFields(triggerChange) {
    $('#stic_conversations_stic_messages_name').val('');
    $('#stic_conversations_ida').val('');
    if (triggerChange) {
      $('#stic_conversations_ida').trigger('change');
    }
  }

  // Function to update quicksearch filter for conversations based on selected contact
  function updateConversationQuickSearchFilter() {
    if (typeof sqs_objects === 'undefined' || !sqs_objects) {
      return;
    }

    var parentId = $('#parent_id').val() || '';
    var parentType = $('#parent_type').val() || '';
    var isConversationType = $('#type').val() === 'private_area';
    var mustFilterByContact = isConversationType && parentType === 'Contacts' && parentId !== '';

    for (var key in sqs_objects) {
      if (!sqs_objects.hasOwnProperty(key)) {
        continue;
      }

      var obj = sqs_objects[key];
      if (!obj || !obj.modules) {
        continue;
      }

      var isConversationsModule = obj.modules.indexOf('stic_Conversations') !== -1;
      if (!isConversationsModule) {
        continue;
      }

      var targetsConversationField = obj.populate_list &&
        (obj.populate_list.indexOf('stic_conversations_stic_messages_name') !== -1 ||
         obj.populate_list.indexOf('stic_conversations_ida') !== -1);

      if (!targetsConversationField) {
        continue;
      }

      if (mustFilterByContact) {
        obj.stic_contact_filter_id = parentId;
      } else if (typeof obj.stic_contact_filter_id !== 'undefined') {
        delete obj.stic_contact_filter_id;
      }
    }
  }

  // Function to bind synchronization between conversation parent and conversation selection
  function bindConversationParentSync() {
    updateConversationQuickSearchFilter();

    $('#parent_id').on('change', function () {
      if ($('#type').val() !== 'private_area' || $('#new_conversation').is(':checked')) {
        return;
      }

      updateConversationQuickSearchFilter();

      var parentId = $('#parent_id').val() || '';
      var conversationId = $('#stic_conversations_ida').val() || '';
      var conversationParentId = $('#stic_conversations_ida').data('parent-id') || '';

      if (!conversationId) {
        return;
      }

      if (!parentId || parentId !== conversationParentId) {
        $('#stic_conversations_ida').data('parent-id', '');
        clearConversationSelectionFields(true);
      }
    });

    $('#stic_conversations_ida').on('change', function () {
      if ($('#type').val() !== 'private_area' || $('#new_conversation').is(':checked')) {
        return;
      }

      if (!$('#stic_conversations_ida').val()) {
        $('#stic_conversations_ida').data('parent-id', '');
        sticMessagesSkipParentClear = false;
      }
    });

    $('#btn_clr_parent_name').on('click', function () {
      if ($('#type').val() !== 'private_area' || $('#new_conversation').is(':checked')) {
        return;
      }

      $('#stic_conversations_ida').data('parent-id', '');
      clearConversationSelectionFields(true);
    });

    $('#btn_clr_stic_conversations_stic_messages_name').on('click', function () {
      if ($('#type').val() !== 'private_area' || $('#new_conversation').is(':checked')) {
        return;
      }

      $('#stic_conversations_ida').data('parent-id', '');
    });

    $('#btn_clr_parent_name').on('click', function() {
      if ($('#type').val() !== 'private_area' || $('#new_conversation').is(':checked')) {
        return;
      }
      setTimeout(function() {
        updateConversationQuickSearchFilter();
      }, 100);
    });
  }

  // Function to build the initial filter for the conversation popup based on the selected parent
  function buildConversationPopupFilter() {
    var parentType = $('#parent_type').val();
    var parentId = $('#parent_id').val();

    if (parentType !== 'Contacts' || !parentId) {
      return '';
    }

    var encodedId = encodeURIComponent(parentId);

    return '&CONTACTS_IDA=' + encodedId;
  }

  // Function to open the conversation selection popup with the appropriate filter
  function openConversationPopupWithFilter(event) {
    if ($('#type').val() !== 'private_area' || $('#new_conversation').is(':checked')) {
      return true;
    }

    if (event && typeof event.preventDefault === 'function') {
      event.preventDefault();
    }

    var formName = (typeof getFormName === 'function' && getFormName()) || 'EditView';
    var popupRequestData = {
      call_back_function: 'set_return',
      form_name: formName,
      field_to_name_array: {
        id: 'stic_conversations_ida',
        name: 'stic_conversations_stic_messages_name',
      },
    };

    var initialFilter = buildConversationPopupFilter();
    open_popup('stic_Conversations', 600, 400, initialFilter, true, false, popupRequestData, 'single', false);
    return false;
  }

  var lastConversationId = '';

  function handleConversationSelectionFromQuickSearch() {
    var conversationId = $('#stic_conversations_ida').val() || '';
    if (conversationId && conversationId !== lastConversationId) {
      lastConversationId = conversationId;
      loadSelectedConversationData();
    }
  }

  $('#stic_conversations_ida').on('change', handleConversationSelectionFromQuickSearch);

  $('#stic_conversations_stic_messages_name').on('blur', function() {
    var originalValue = $(this).data('original-value') || '';
    var currentValue = $(this).val() || '';
    if (currentValue !== originalValue && currentValue !== '') {
      var conversationId = $('#stic_conversations_ida').val();
      if (conversationId) {
        handleConversationSelectionFromQuickSearch();
      }
    }
    $(this).data('original-value', currentValue);
  });

  $('#stic_conversations_stic_messages_name').on('focus', function() {
    $(this).data('original-value', $(this).val());
    updateConversationQuickSearchFilter();
  });

  $('#btn_clr_stic_conversations_stic_messages_name').on('click', function() {
    setTimeout(function() {
      lastConversationId = '';
      updateConversationQuickSearchFilter();
    }, 100);
  });

  setInterval(function() {
    var conversationId = $('#stic_conversations_ida').val() || '';
    if (conversationId && conversationId !== lastConversationId) {
      handleConversationSelectionFromQuickSearch();
    }
  }, 500);

  $('#new_conversation, #type').on('change', loadSelectedConversationData);
  $('#btn_stic_conversations_stic_messages_name')
    .removeAttr('onclick')
    .off('click')
    .on('click', openConversationPopupWithFilter);

  bindConversationParentSync();

  const myButtons = $('[id="SAVE"]');
  saveMessage = function (event) {
    event.preventDefault();
    if (check_form("EditView")) {
      const formDataArray = $("#EditView").serializeArray();
      const formObject = {};

      $.each(formDataArray, function (i, field) {
        if (formObject[field.name]) {
          if (!Array.isArray(formObject[field.name])) {
            formObject[field.name] = [formObject[field.name]];
          }
          formObject[field.name].push(field.value);
        } else {
          formObject[field.name] = field.value;
        }
      });

      function getFormDataAsObject($form) {
        var unindexed_array = $form.serializeArray();
        var indexed_array = {};

        $.map(unindexed_array, function (n, i) {
          indexed_array[n["name"]] = n["value"];
        });

        return indexed_array;
      }
      var formData = getFormDataAsObject($("#EditView"));

      if ($('#type').val() === 'private_area') {
        formData.parent_type = 'Contacts';
        formData.parent_id = $('#parent_id').val() || '';
        formData.parent_name = $('#parent_name').val() || '';
      }

      formData.action='SavePopUp';

      $.ajax({
        url: "index.php?module=stic_Messages&action=savePopUp",
        type: "post",
        dataType: "json",
        async: false,
        data: formData,
        success: function (res) {
            var decodeHTML = function (html) {
                var txt = document.createElement("textarea");
                txt.innerHTML = html;
                return txt.value;
            };

            if (res && res.success === false) {
              showMessageBox(
                res.title || SUGAR.language.get('stic_Messages', 'LBL_ERROR'),
                res.detail || SUGAR.language.get('stic_Messages', 'LBL_MESSAGE_NOT_SENT')
              );
              return;
            }
            
            // Check if this is a WhatsAppWeb message using explicit type field
            if (res.type === 'WhatsAppWeb') {
              // Single message
              if (res.phone && res.text) {
                var cleanText = decodeHTML(res.text);
                var waUrl = 'https://wa.me/' + res.phone + '?text=' + encodeURIComponent(cleanText);
                console.log('Opening WhatsApp URL:', waUrl);
                window.open(waUrl, '_blank');
              }

              // Multiple messages (mass send)
              if (res.open_data && Array.isArray(res.open_data)) {
                  res.open_data.forEach(function(item) {
                      var cleanItemText = decodeHTML(item.text);
                      var waUrl = 'https://wa.me/' + item.phone + '?text=' + encodeURIComponent(cleanItemText);
                      window.open(waUrl, '_blank');
                  });
              }
            }
            
            var baseUrl = window.location.href.split("?")[0];
            var returnModule = $('#EditView [name="return_module"]').val();
            var returnAction = $('#EditView [name="return_action"]').val();
            var returnId = $('#EditView [name="return_id"]').val();
            if (!returnId && res.id) {
              returnId = res.id;
            }
            var newUrl =
                baseUrl +
                "?module=" +
                encodeURIComponent(returnModule) +
                "&action=" +
                encodeURIComponent(returnAction) +
                (returnId ? "&record=" + encodeURIComponent(returnId) : "");
            
            if($("#status").val() == 'draft') {
              window.location.href = newUrl;
            }
            else {
              var isWhatsAppWeb = res.type === 'WhatsAppWeb';
              var title = isWhatsAppWeb ? 
                (res.title || SUGAR.language.get('app_strings', 'LBL_EMAIL_SUCCESS')) : 
                res.title;
              var detail = isWhatsAppWeb ? 
                SUGAR.language.get('stic_Messages', 'LBL_WHATSAPP_WEB_SENT') : 
                res.detail;
              
              showMessageBox(title, detail, function () {
                window.location.href = newUrl;
              });
            }
          },
        error: function () {
          showMessageBox(
            SUGAR.language.get('stic_Messages', 'LBL_ERROR'),
            SUGAR.language.get('stic_Messages', 'LBL_MESSAGE_NOT_SENT'),
            function () {
              var baseUrl = window.location.href.split("?")[0];
              var returnModule = $('#EditView [name="return_module"]').val();
              var returnAction = $('#EditView [name="return_action"]').val();
              var returnId = $('#EditView [name="return_id"]').val();
              if (returnId) {
                var newUrl =
                  baseUrl +
                  "?module=" +
                  encodeURIComponent(returnModule) +
                  "&action=" +
                  encodeURIComponent(returnAction) +
                  (returnId ? "&record=" + encodeURIComponent(returnId) : "");
                window.location.href = newUrl;
              }
            }
          );
        },
      });
    }
    return false;
  };
  // var _form = document.getElementById('EditView'); _form.action.value='Save'; if(check_form('EditView'))SUGAR.ajaxUI.submitForm(_form);return false;
  myButtons.removeAttr("onclick");
  myButtons.on("click", saveMessage);
});