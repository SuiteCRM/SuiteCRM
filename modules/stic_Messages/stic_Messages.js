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

/**
 * 
 * Used as a callback for sending various messages from list view
 */
function onClickMassSendMessagesButton() {
  let obj = { return_action: "ListView" };
  let jsonString = JSON.stringify(obj);

  openMessagesModal(this, jsonString);
}

function openMessagesModal(source, paramsJson = '{"return_action":"DetailView"}') {
    if (source && typeof source.blur === 'function') {
      source.blur();
    }

    let numRecordsSelected = $(".selectedRecords.value").eq(0).text();

    if (numRecordsSelected > getMessagesLimit()) {
      alert(SUGAR.language.get('app_strings', 'LBL_TOO_MANY_RECORDS_SELECTED'));
      return;
    }

    // return_action = 'DetailView';
    let params = JSON.parse(paramsJson);
    return_action = params['return_action'];
    if (return_action == '') {
      return_action = 'DetailView';
    }
    
    var relatedId = $('[name="record"]').val();
    var ids = '';
    if (typeof $(source).attr('data-record-id') !== 'undefined' && $(source).attr('data-record-id') !== '') {
      // One record
      ids = $(source).attr('data-record-id');
      relatedId = $(source).attr('data-record-id');
    }
    else{
      // Multiple record (bulk actions)
      $("div[data-field='parent_name']").hide();
      var inputs = document.MassUpdate.elements;
      for (var i = 0; i < inputs.length; i++) {
        if (inputs[i].name === 'mass[]' && inputs[i].checked) {
          ids = ids + inputs[i].value + ',';
        }
      }
    }

    var targetModule = currentModule;
    if (typeof $(source).attr('data-module') !== 'undefined' && $(source).attr('data-module') !== '') {
        targetModule = $(source).attr('data-module');
    }

    SUGAR.ajaxUI.showLoadingPanel();

    var baseURL = 'index.php';

    // Create an object with all the parameters
    var paramsPost = {
        module: 'stic_Messages',
        return_module: currentModule,
        return_action: return_action,
        return_id: relatedId,
        action: 'ComposeView',
        in_popup: '1',
        targetModule: targetModule,
        relatedModule: currentModule,
        relatedId: relatedId,
        ids: ids,
        current_query_by_page: $("[name='current_query_by_page']").val(),
        select_entire_list: $("[name='select_entire_list']").val()
    };
    
    // Make the POST request
    $.ajax({
        url: baseURL,
        type: 'POST',
        data: paramsPost,
        success: function(data) {
        var panelBody = $('<div class="content">').append(data).find('#EditView').parent();
        // Check if we are in the context of the Conversations subpanel to apply some defaults to the compose view
        var sourceModule = $(source).attr('data-module') || '';
        var isConversationsMessagesSubpanel =
          paramsPost.relatedModule === 'stic_Conversations'
          && (
            sourceModule === 'stic_Conversations'
            || $(source).closest('#whole_subpanel_stic_conversations_stic_messages').length > 0
          );

        function applyConversationSubpanelModalDefaults($panel) {
          if (!isConversationsMessagesSubpanel) {
            return;
          }

          var $form = $panel.find('#EditView');
          if (!$form.length) {
            return;
          }

          var conversationId = $(source).attr('data-record-id') || paramsPost.relatedId || '';
          var conversationName = $(source).attr('data-name') || '';

          if (conversationId) {
            $form.find('#stic_conversations_ida').val(conversationId);
          }
          if (conversationName) {
            $form.find('#stic_conversations_stic_messages_name').val(conversationName);
          }

          if (typeof initSubpanelConversationLogic === 'function') {
            initSubpanelConversationLogic($form);
          }
        }

        // Hide conversation type when context module is not Contacts
        function applyConversationTypeVisibilityByModule($panel, moduleName) {
          if (moduleName === 'Contacts' || moduleName === 'stic_Conversations') {
            return;
          }

          var $typeSelect = $panel.find('select[name="type"]');
          if (!$typeSelect.length) {
            return;
          }

          var $conversationOption = $typeSelect.find('option[value="conversation"]');
          if (!$conversationOption.length) {
            return;
          }

          if ($typeSelect.val() === 'private_area') {
            var fallbackValue = '';
            $typeSelect.find('option').each(function () {
              var value = $(this).val();
              if (value !== 'private_area') {
                fallbackValue = value;
                return false;
              }
            });

            if (fallbackValue) {
              $typeSelect.val(fallbackValue).trigger('change');
            }
          }

          $conversationOption.remove();
        }

        applyConversationTypeVisibilityByModule(panelBody, paramsPost.relatedModule);

        var dataPhone = $(source).attr('data-phone');
        var dataName = $(source).attr('data-name');
        // If the attribute data-record-id is present, then we come from subpanel, else we come from mass send or Edit View.
          var dataRecordId = $(source).attr('data-record-id');
        if (typeof dataRecordId !== 'undefined' && dataRecordId !== '' && !isConversationsMessagesSubpanel) {
          panelBody.find('#phone').val(dataPhone);
          panelBody.find('#parent_name').val(dataName);
        }
        else if (typeof dataRecordId === 'undefined' || dataRecordId === '') {
          // Mass send messages
          phoneList = '';
          namesList = '';
          idsList = '';
          targetCount = 0;
          totalRecords = 0;
          singleRecordName = '';
          singleRecordId = '';
          panelBody.find('.phone-compose-view-to-list').each(function () {
            dataPhone = $(this).attr('data-record-phone');
            dataId = $(this).attr('data-record-id');
            dataName = $(this).attr('data-record-name');
            totalRecords++;
            if (totalRecords === 1) {
              singleRecordName = dataName;
              singleRecordId = dataId;
            }
            if (dataPhone !== '') {
              if (targetCount > 0 ){
                phoneList += ',';
                namesList += ', ';
                idsList += ';';
              }
              phoneList += dataPhone;
              namesList += '<' + dataName + '> ' + dataPhone;
              idsList += dataId;
              targetCount++;
            }
          });
          panelBody.find('#phone').val(phoneList);
          if (totalRecords === 1) {
            setTimeout(function() {
              panelBody.find('#parent_name').val(singleRecordName);
              panelBody.find('#parent_id').val(singleRecordId);
            }, 200);
          }
          function replacePhoneField(panelBody) {
            var originalPhone = panelBody.find('#phone');
            if (!originalPhone) return; // Exit if the original phone input doesn't exist
        
            originalPhone.parent().append('<input type="text" id="namesList" size="30" disabled>');
            originalPhone[0].style='display:none'
          }
          replacePhoneField(panelBody);
          phoneElement = panelBody.find('#phone')
          phoneElement.attr('readonly', true);
          phoneElement.css('background', '#F8F8F8');
          phoneElement.css('border-color', '#E2E7EB');

          // panelBody.find('#phone').attr('disabled', true);
          // $("#EditView .dcQuickEdit td.buttons").append("<input type='hidden' name='mass_ids' id='mass_ids' value='{$idsList}'>");
          panelBody.find('.dcQuickEdit td.buttons').append("<input type='hidden' name='mass_ids' id='mass_ids' value='"+idsList+"'>");
          // panelBody.find('#mass_ids').val(idsList);
          // If this is a mass send with more than one target, disable WhatsAppWeb option and force SMS
            if (targetCount > 1) {
              var $typeSelect = panelBody.find('select[name="type"]');
              if ($typeSelect.length) {
                // Disable WhatsApp option and change to SMS if WhatsAppWeb is selected
                $typeSelect.find('option[value="WhatsAppWeb"]').prop('disabled', true);
                if ($typeSelect.val() === 'WhatsAppWeb') {
                  const smsOption = $typeSelect.find('option[value="SevenSmsHelper"]');
                  $typeSelect.val(smsOption.length ? 'SevenSmsHelper' : $typeSelect.find('option:not(:disabled)').first().val());
                }
              }
            }
        }
          SUGAR.ajaxUI.hideLoadingPanel();

          var $dialogWrapper = $('<div>').append(panelBody).dialog({
              modal: true,
              // title: SUGAR.language.get(buttonModule, 'LBL_NEW_FORM_TITLE'),
              title: '',
              width: '80%',
              open: function() {
                // Focus message field when modal is opened
                var focusMessageField = function() {
                  var $messageField = $dialogWrapper.find('#message');
                  if ($messageField.length) {
                    $messageField.trigger('focus');
                  }
                };

                setTimeout(focusMessageField, 0);
                $(window).off('focus.sticMessagesModal').on('focus.sticMessagesModal', function() {
                  setTimeout(focusMessageField, 0);
                });
              },
              close: function() {
                $(window).off('focus.sticMessagesModal');
              }
          });
          // If the modal is opened from the Conversations subpanel, we need to apply some defaults and hide some fields
          if (isConversationsMessagesSubpanel) {
            setTimeout(function() {
              applyConversationSubpanelModalDefaults($dialogWrapper);
            }, 0);
          }
          if (typeof namesList !== 'undefined') {
            $('#namesList').val(namesList);
          }
          // Hiding close button from dialog. We want the user to use Save or Cancel.
          $("#EditView").parent().parent().parent().children().first().find('button').hide()

        },
        error: function(xhr, status, error) {
            // Your error handler here
        }
    });

}

$(function() {
  $.fn.qtip.zindex = 26000;

  // Disable editing for WhatsAppWeb messages
  if (typeof viewType !== 'undefined' && (viewType() === 'detail' || viewType() === 'edit')) {
    var messageType = $('select[name="type"]').val() || $('input[name="type"]').val();
    var messageId = $('input[name="record"]').val();
    
    // If editing an existing WhatsAppWeb message, disable all fields
    if (messageId && messageType === 'WhatsAppWeb') {
      // Disable all form fields except parent relationship
      $('#EditView input:not([name="parent_name"]):not([name="parent_id"]):not([name="parent_type"])').prop('disabled', true);
      $('#EditView select:not([name="parent_type"])').prop('disabled', true);
      $('#EditView textarea').prop('disabled', true);
      
      // Hide save button, only allow cancel/back
      $('#EditView input[type="submit"][name="button"]').hide();
      
      // Add a notice
      if ($('#EditView .whatsapp-readonly-notice').length === 0) {
        $('#EditView').prepend('<div class="whatsapp-readonly-notice" style="background: #fff3cd; border: 1px solid #ffc107; padding: 10px; margin-bottom: 10px; border-radius: 4px;"><strong>Aviso:</strong> Los mensajes de WhatsApp Web no pueden ser editados una vez enviados.</div>');
      }
    }
  }

  // Force status to 'sent' when type is WhatsAppWeb
  $('select[name="type"]').on('change', function() {
    if ($(this).val() === 'WhatsAppWeb') {
      $('select[name="status"]').val('sent').prop('disabled', true);
    } else {
      $('select[name="status"]').prop('disabled', false);
    }
  });

  // On page load, check if type is WhatsAppWeb
  if ($('select[name="type"]').val() === 'WhatsAppWeb') {
    $('select[name="status"]').val('sent').prop('disabled', true);
  }

  if (typeof viewType !== 'undefined' && viewType() === 'detail') {

    const attr = 'sms-button'

    const recordId = $("input[name=record]")[0].value;
    const toName = trim($(".module-title-text").text());

    if (getMessagesActive()) {
      for (const phone of [...document.querySelectorAll('[type=phone]')]) {
          if (phone.getAttribute(attr) !== null) continue
      
          const to = phone.textContent.trim()
          if (to === '') continue
      
          phone.insertAdjacentHTML('beforeend',
              `<span class="suitepicon suitepicon-module-stic-messages suitepiconInView" data-record-id= '${recordId}' data-record-module= '${module}' data-name='${toName}' data-phone='${to}' onclick='openMessagesModal(this)'></span>`)
          phone.setAttribute(attr, 'true')
      }
    }
  }

  if (typeof viewType !== 'undefined' && viewType() === 'list') {
    buttons = {
      sentMessage: {
        id: "bt_sentMessage_listview",
        title: SUGAR.language.get("app_strings", "LBL_MASS_SENT_MESSAGE_BUTTON_TITTLE"),
        text: SUGAR.language.get("app_strings", "LBL_MASS_SENT_MESSAGE_BUTTON_TITTLE"),
        onclick: "onClickMassSendMessagesButton()"
      }
    };
    if (getMessagesActive()){
      createListViewButton(buttons.sentMessage);
    }

  }


});
