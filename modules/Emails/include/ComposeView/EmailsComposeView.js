/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

(function ($) {
  /**
   *
   * @param options
   * @returns {jQuery|HTMLElement}
   * @constructor
   */
  $.fn.EmailsComposeView = function (options) {
    "use strict";
    var self = $(this);
    var opts = $.extend({}, $.fn.EmailsComposeView.defaults, options);
    var jQueryFormComposeView = $('form[name="ComposeView"]')[0];

    self.attachFile = undefined;
    self.attachNote = undefined;
    self.attachDocument = undefined;
    /**
     * Determines if the signature comes before the reply to message
     * @type {boolean}
     */
    self.prependSignature = false;

    /**
     * Defines the buttons that are displayed when the user focuses in on a to, cc and bcc field.
     *
     * @param data-open-popup-module - The module to popup
     * @param data-open-popup-email-address-field - the field name that holds a single email address (assumes email1)
     *
     * To add a button (using popup behavior)
     * $('.compose-view#id).EmailsComposeView().qtipBar +=
     * '<button class="btn btn-default btn-sm btn-qtip-bar" '+
     * 'data-open-popup-module="Contacts" data-open-popup-email-address-field="email1">'+'</button>';
     *
     * To add a button (using your own behavior)
     * $('.compose-view#id).EmailsComposeView().qtipBar += '<button class="btn btn-default btn-sm"></button>';
     *
     * @type {string}
     */
    self.qtipBar =
      '<button class="btn btn-default btn-sm btn-qtip-bar" data-open-popup-module="Contacts" data-open-popup-email-address-field="email1" title="' + SUGAR.language.translate('Emails', 'LBL_INSERT_CONTACT_EMAIL') + '"><span class="glyphicon"><img src="themes/' + SUGAR.themes.theme_name + '/images/sidebar/modules/Contacts.svg"></span></button>' +
      '<button class="btn btn-default btn-sm btn-qtip-bar" data-open-popup-module="Accounts" title="' + SUGAR.language.translate('Emails', 'LBL_INSERT_ACCOUNT_EMAIL') + '"><span class="glyphicon"><img src="themes/' + SUGAR.themes.theme_name + '/images/sidebar/modules/Accounts.svg"></span></button>' +
      '<button class="btn btn-default btn-sm btn-qtip-bar" data-open-popup-module="Prospects" title="' + SUGAR.language.translate('Emails', 'LBL_INSERT_TARGET_EMAIL') + '"><span class="glyphicon"><img src="themes/' + SUGAR.themes.theme_name + '/images/sidebar/modules/Prospects.svg"></span></button>' +
      '<button class="btn btn-default btn-sm btn-qtip-bar" data-open-popup-module="Users" title="' + SUGAR.language.translate('Emails', 'LBL_INSERT_USER_EMAIL') + '"><span class="glyphicon"><img src="themes/' + SUGAR.themes.theme_name + '/images/sidebar/modules/Users.svg"></span></button>' +
      '<button class="btn btn-default btn-sm btn-qtip-bar" data-open-popup-module="Leads" title="' + SUGAR.language.translate('Emails', 'LBL_INSERT_LEAD_EMAIL') + '"><span class="glyphicon"><img src="themes/' + SUGAR.themes.theme_name + '/images/sidebar/modules/Leads.svg"></span></button>';

    /**
     * opens a popup when a btn-qtip-bar is clicked
     */
    self.handleQTipBarClick = function () {
      var module = $('#qtip_bar_module');
      module.val($(this).attr('data-open-popup-module'));

      var fields = {
        'id': 'qtip_bar_id',
        'name': 'qtip_bar_name'
      }

      if (typeof $(this).attr('data-open-popup-email-address-field') === "undefined") {
        fields['email1'] = 'qtip_bar_email_address';
      } else {
        fields[$(this).attr('data-open-popup-email-address-field')] = 'qtip_bar_email_address';
      }

      $.fn.EmailsComposeView.setEmailAddressFieldFromPopup = function(resultData) {
        var contact_name = resultData.name_to_value_array.qtip_bar_name;
        var contact_email_address = resultData.name_to_value_array.qtip_bar_email_address;

        if (trim(contact_email_address) !== '') {
          var formatted_email_address = '';
          if (trim(contact_name) !== '') {
            // use name <email address> format
            formatted_email_address = contact_name + ' <' + contact_email_address + '>';
          } else {
            // use email address
            formatted_email_address = contact_email_address;
          }

          if (trim($(self.active_elementQTipBar).val()) === '') {
            $(self.active_elementQTipBar).val(formatted_email_address);
          } else {
            $(self.active_elementQTipBar).val(
              $(self.active_elementQTipBar).val() + ', ' +
              formatted_email_address
            );
          }
        }
      };

      var popupWindow = open_popup(
        $(this).attr('data-open-popup-module'),
        600,
        400,
        "",
        true,
        false,
        {
          "call_back_function": '$.fn.EmailsComposeView.setEmailAddressFieldFromPopup',
          "form_name": "ComposeView",
          "field_to_name_array": fields
        },
        "single",
        false
      );
    };

    /**
     * Shows the qtip bar when the user focuses in on a to, cc and bcc field.
     *
     * To reuse this behaviour for an other field simply bind the focus event to this method call
     */
    self.showQTipBar = function () {
      self.active_elementQTipBar = this;
      $(this).qtip({
        content: {
          text: self.qtipBar
        },
        position: {
          my: 'bottom left',
          at: 'top left'
        },
        show: {solo: true, ready: true, event: false},
        hide: {event: false},
        style: {classes: 'emails-qtip'}
      });
      $(this).qtip("show");
      $(this).unbind('unfocus').blur(function (e) {
        var isButton = $(e.relatedTarget).hasClass('btn-qtip-bar');
        var isQtipContent = $(e.relatedTarget).hasClass('qtip-content');
        var isQtip = $(e.relatedTarget).hasClass('qtip-tip');

        if (isButton || isQtipContent || isQtip) {
          return false;
        }

        $(this).qtip("hide");
      });
      $('.btn-qtip-bar').unbind('click').click(self.handleQTipBarClick);
    };

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
     * confirms if form is valid
     * @returns {boolean}
     */
    self.isValid = function () {
      "use strict";
      return self.isToValid() &&
        self.isCcValid() &&
        self.isBccValid() &&
        self.isSubjectValid() &&
        self.isBodyValid();
    };

    /**
     * validates form and displays error
     * @returns {boolean}
     */
    self.validate = function () {
      var valid = self.isValid();
      if (valid === false) {
        if (typeof messageBox !== "undefined") {
          var mb = messageBox({size: 'lg', backdrop: 'static'});
          mb.setTitle(SUGAR.language.translate('', 'ERR_INVALID_REQUIRED_FIELDS'));
          mb.setBody(self.translatedErrorMessage);

          mb.on('ok', function () {
            mb.remove();
          });

          mb.on('cancel', function () {
            mb.remove();
          });

          mb.show();
          mb.hideCancel();
        } else {
          alert(self.translatedErrorMessage);
        }
      }
      return valid;
    };

    /**
     * Is the To field valid
     * @returns {boolean}
     */
    self.isToValid = function () {
      "use strict";
      var emailAddresses = $(self).find('[name=to_addrs_names]').val().split('/[,;]/');

      if (self.isValidEmailAddresses(emailAddresses)) {
        return true;
      }

      self.setValidationMessage('to_addrs_names', 'LBL_HAS_INVALID_EMAIL_TO');
      return false;
    };

    /**
     * Is the CC field valid
     * @returns {boolean}
     */
    self.isCcValid = function () {
      "use strict";

      var cc = $(self).find('[name=cc_addrs_names]').val();
      var emailAddresses = cc.split('/[,;]/');

      if (self.isValidEmailAddresses(emailAddresses) || cc === '') {
        return true;
      }

      self.setValidationMessage('cc_addrs_names', 'LBL_HAS_INVALID_EMAIL_CC');
      return false;
    };

    /**
     * Is the BCC field valid
     * @returns {boolean}
     */
    self.isBccValid = function () {
      "use strict";
      var bcc = $(self).find('[name=bcc_addrs_names]').val();
      var emailAddresses = bcc.split('/[,;]/');

      if (self.isValidEmailAddresses(emailAddresses) || bcc === '') {
        return true;
      }

      self.setValidationMessage('bcc_addrs_names', 'LBL_HAS_INVALID_EMAIL_BCC');
      return false;
    };

    /**
     * Is the Subject field valid
     * @returns {boolean}
     */
    self.isSubjectValid = function () {
      "use strict";

      if ($(self).find('[name=name]').val() !== '') {
        return true;
      }

      self.setValidationMessage('name', 'LBL_HAS_EMPTY_EMAIL_SUBJECT');
      return false;
    };

    /**
     * Is the Body field valid
     * @returns {boolean}
     */
    self.isBodyValid = function () {
      "use strict";

      if ($(self).find('#description').val() !== '') {
        return true;
      }

      self.setValidationMessage('description_html', 'LBL_HAS_EMPTY_EMAIL_BODY');
      return false;
    };

    /**
     *
     * @param field string name
     * @param label string eg LBL_OK
     */
    self.setValidationMessage = function (field, label) {
      "use strict";
      self.translatedErrorMessage = SUGAR.language.translate('Emails', label);
    };

    /**
     * Determines if a set of email addresses are valid
     * @param emailAddresses array|object eg ['a@example.com', 'b@example.com']
     * @returns {boolean}
     */
    self.isValidEmailAddresses = function (emailAddresses) {
      "use strict";
      if (typeof emailAddresses === 'object') {
        for (var i = 0; i < emailAddresses.length; i++) {
          emailAddresses[i] = (emailAddresses[i] !== '') && isValidEmail(emailAddresses[i]);
        }
        if (emailAddresses.indexOf(false) === -1) {
          return true;
        }
      }

      return false;
    };


    $.fn.EmailsComposeView.updateSignature = self.updateSignature = function ($selected) {
      if(!$selected) {
        $selected = $('#from_addr_name').find('option:selected');
      }

      var signatureId = $selected.attr('data-email-signature-id');
      if (signatureId === undefined) {
        console.warn('Unable to retrieve signature id');
        return false;
      }

      var body = tinymce.activeEditor.getContent();
      if (body !== '' && $(body).hasClass('email-signature-element')) {
        var $body = $(body);
        var $existingSignature = $body.find('.email-signature-element');
        $existingSignature.remove();
        tinymce.activeEditor.setContent($body.html(), {format: 'html'});
      }

      var signatureElement = $('<div></div>')
        .addClass('email-signature-element');
      var signatures = $(self).find('.email-signature');
      var htmlSignature = null;
      var plainTextSignature = null;

      // Find signature
      $.each(signatures, function (index, value) {
        if ($(value).attr('data-email-signature-id') === signatureId) {

          if ($(value).hasClass('html')) {
            htmlSignature = $(value).val();
          } else if ($(value).hasClass('plain')) {
            plainTextSignature = $(value).val();
          }
        }
      });

      if (
        htmlSignature === null &&
        plainTextSignature === null
      ) {
        console.warn('Unable to retrieve signature from document.');
        return false;
      }

      if (htmlSignature === null) {
        // use plain signature instead
        $(plainTextSignature).appendTo(signatureElement);
      } else if (plainTextSignature === null) {
        // use html signature
        $(htmlSignature).appendTo(signatureElement);
      } else {
        $(htmlSignature).appendTo(signatureElement);
      }

      if (tinymce.editors.length < 1) {
        console.warn('unable to find tinymce editor');
        return false;
      }

      body = tinymce.activeEditor.getContent();
      if (body === '') {
        tinymce.activeEditor.setContent('<p></p><p></p>' + signatureElement[0].outerHTML, {format: 'html'});
      } else if ($(body).hasClass('email-signature-element')) {
        var newBody = $('<div></div>');
        $(body).appendTo(newBody);
        $(newBody).find('.email-signature-element').replaceWith(signatureElement[0].outerHTML);
        tinymce.activeEditor.setContent(newBody.html(), {format: 'html'});
      } else {
        // reply to / forward
        if (self.prependSignature === true) {
          tinymce.activeEditor.setContent('<p></p><p></p>' + signatureElement[0].outerHTML + body, {format: 'html'});
        } else {
          tinymce.activeEditor.setContent(body + signatureElement[0].outerHTML, {format: 'html'});
        }
      }
    };

    self.updateFromInfos = function ($selected) {
      if (!$selected) {
        $selected = $('#from_addr_name').find('option:selected');
      }
      var infos = $selected.attr('infos');
      if(infos === undefined) {
        console.warn('Unable to retrieve selected infos in the "From" field.');
        return false;
      }

      if(!$('#from_addr_name_infos').length) {
          $('#from_addr_name').parent().append('<span id="from_addr_name_infos"></span>');
      }

      $('#from_addr_name_infos').html(infos);

    };

    self.updateFromAddressName = function (selectFrom) {
      var newlySelected = selectFrom.find('option:selected');
      var newlySelectedFromName = newlySelected.attr('data-from-name');
      $('#from_addr_name_hidden').val(newlySelectedFromName);
    };

    self.updateOutboundEmailId = function (selectFrom, newlySelected) {
      if(!newlySelected) {
        newlySelected = selectFrom.find('option:selected');
      }

      var entryType = newlySelected.attr('data-type') || '';
      $('#outbound_email_id').val('');

      if (entryType === 'OutboundEmailAccount' || entryType === 'system') {
        var outboundId = newlySelected.attr('data-outbound-email-id');
        $('#outbound_email_id').val(outboundId);
      }
    };

    self.onFromSelect = function (selectFrom, ui) {
      var newlySelected = ui.item.element;
      var entryType = newlySelected.attr('data-type') || '';

      $('#outbound_email_id').val('');
      if (entryType === 'OutboundEmailAccount' || entryType === 'system') {
        self.updateOutboundEmailId(selectFrom, newlySelected);
      } else {
        $(self).find('[name=inbound_email_id]').val(newlySelected.attr('inboundId'));
      }

      self.updateSignature(newlySelected);
    };

    self.getFromEntryLabel = function (entry) {
      if (!entry) {
        return '';
      }

      var entryValue = '';
      if(entry.attributes.from && (typeof entry.attributes.from === 'string' || entry.attributes.from instanceof String)) {
        entryValue = entry.attributes.from;
      }

      var entryLabel = '';
      if(entry.attributes.name && (typeof entry.attributes.name === 'string' || entry.attributes.name instanceof String)) {
        entryLabel = entry.attributes.name;
        if (entryValue && entryValue !== entryLabel) {
          entryLabel = entryLabel + ' (' + entryValue + ')';
        }
      } else if (entryValue) {
        entryLabel = entryValue;
      }



      if (entryLabel.length > 40) {
        entryLabel = entryLabel.substring(0, 40) + '...';
      }

      return entryLabel;
    };

    /**
     *
     * @param editor
     */
    self.tinyMceSetup = function (editor) {

      editor.on('init', function () {
        this.getDoc().body.style.fontName = 'tahoma';
        this.getDoc().body.style.fontSize = '13px';
      });

      editor.on('change', function () {
        // copy html to plain
        $(self).find('.html_preview').html(editor.getContent());
        $(self).find('input#description_html').val(editor.getContent());
        $(self).find('textarea#description').val($(self).find('.html_preview').text());
      });

      editor.on('SetContent', function () {
        // copy html to plain
        $(self).find('.html_preview').html(editor.getContent());
        $(self).find('input#description_html').val(editor.getContent());
        $(self).find('textarea#description').val($(self).find('.html_preview').text());
      });
    };

    /**
     *
     * @event sendEmail
     * @event sentEmailError
     * @event sentEmailAlways
     * @event sentEmail
     * @returns {boolean}
     */
    self.onSendEmail = function () {
      $(self).trigger("sendEmail", [self]);

      // Tell the user we are sending an email
      var mb = messageBox({backdrop:'static'});
      mb.hideHeader();
      mb.hideFooter();
      document.activeElement.blur();
      mb.setBody('<div class="email-in-progress"><img src="themes/' + SUGAR.themes.theme_name + '/images/loading.gif"></div>');
      mb.show();

      mb.on('ok', function () {
        "use strict";
        mb.remove();
      });

      mb.on('cancel', function () {
        "use strict";
        mb.remove();
      });

      var fileCount = 0;
      // Use FormData v2 to send form data via ajax
      var formData = new FormData(jQueryFormComposeView);

      $(this).find('input').each(function (inputIndex, inputValue) {
        if ($(inputValue).attr('type').toLowerCase() !== 'file') {
          if ($(inputValue).attr('name') === 'action') {
            formData.append('refer_' + $(inputValue).attr('name'), $(inputValue).val());
            formData.append($(inputValue).attr('name'), 'send');
          } else if ($(inputValue).attr('name') === 'send') {
            formData.append($(inputValue).attr('name'), 1);
          } else {
            formData.append($(inputValue).attr('name'), $(inputValue).val());
          }
        }
      });

      $(this).find('select').each(function (i, v) {
        if (typeof $(v).attr('is_file') === 'undefined') {
          formData.append($(v).attr('name'), $(v).val());
        }
      });

      $(this).find('textarea').each(function (i, v) {
        formData.append($(v).attr('name'), $(v).val());
      });

      $(this).find('button').each(function (i, v) {
        formData.append($(v).attr('name'), $(v).val());
      });


      $(this).find('input[type=checkbox]').each(function (i, v) {
        formData.append($(v).attr('name'), $(v).prop('checked'));
      })

      $.ajax({
        type: "POST",
        data: formData,
        cache: false,
        processData: false,  // tell jQuery not to process the data
        contentType: false,   // tell jQuery not to set contentType
        url: $(this).attr('action')
      }).done(function (response) {
        "use strict";
        response = JSON.parse(response);
        if (typeof response.errors !== "undefined") {
          mb.showHeader();
          mb.setBody(response.errors.title);
          mb.showFooter();
          mb.hideCancel();
          $(self).trigger("sentEmailError", [self, response]);
        } else {
          mb.showHeader();
          mb.setBody(response.data.title);
          mb.showFooter();
          mb.hideCancel();

          // If the user is viewing the form in the standard view
          if ($(self).find('input[type="hidden"][name="return_module"]').val() !== '') {
            mb.on('ok', function () {
              var url = 'index.php?';

              var module = $('#' + self.attr('id') + ' input[type="hidden"][name="return_module"]').val();
              if (module !== undefined) {
                url = url + 'module=' + module;
              }

              var action = $('#' + self.attr('id') + ' input[type="hidden"][name="return_action"]').val();
              if (action !== undefined) {
                url = url + '&action=' + action;
              }

              var record = $('#' + self.attr('id') + ' input[type="hidden"][name="return_id"]').val();
              if (record !== undefined) {
                url = url + '&record=' + record;
              }

              location.href = url;
            });
          } else {
            mb.on('ok', function () {
              // The user is viewing in the modal view
              $(self).trigger("sentEmail", [self, response]);
            });

          }
        }
      }).fail(function (response) {
        "use strict";
        mb.showHeader();
        mb.setBody(response.errors.title);
        $(self).trigger("sentEmailError", [self, response]);
      }).always(function (data) {
        $(self).trigger("sentEmailAlways", [self, data]);
      });


      return false;
    };


    /**
     * @event sendEmail
     * @returns {boolean}
     */
    self.sendEmail = function (e) {
      "use strict";
      e.preventDefault();
      $(this).find('[name=action]').val('send');
      if (self.validate()) {
        $(this).submit();
      }
      return false;
    };


    /**
     * @event attachFile
     * @returns {boolean}
     */
    self.attachFile = function (event) {
      "use strict";
      event.preventDefault();
      $(self).trigger("attachFile", [self]);

      // Add the file input onto the page
      var id = self.generateID();

      var fileGroupContainer = $('<div></div>')
        .addClass('attachment-group-container')
        .appendTo(self.find('.file-attachments'));

      var fileInput = $('<input>')
        .attr('type', 'file')
        .attr('id', 'file_' + id)
        .attr('name', 'email_attachment[]')
        .attr('multiple', 'true')
        .appendTo(fileGroupContainer);


      var fileLabel = $('<label></label>')
        .attr('for', 'file_' + id)
        .addClass('attachment-blank')
        .html('<span class="glyphicon glyphicon-paperclip"></span>')
        .appendTo(fileGroupContainer);

      // use the label to open file dialog
      fileLabel.click();

      // handle when the a file is selected
      fileInput.change(function (event) {

        if (event.target.files.length === 0) {
          fileGroupContainer.remove();
          return false;
        }
        if (event.target.files.length > 1) {
          $(fileLabel.addClass('label-with-multiple-files'));
        } else {
          $(fileLabel.removeClass('label-with-multiple-files'));
        }

        fileLabel.html('');
        fileLabel.empty();

        if (fileGroupContainer.find('.attachment-remove').length === 0) {
          var removeAttachment = $('<a class="attachment-remove"><span class="glyphicon glyphicon-remove"></span></a>');
          fileGroupContainer.append(removeAttachment);
          // handle when user removes attachment
          removeAttachment.click(function () {
            fileGroupContainer.remove();
          });
        }

        for (var i = 0; i < event.target.files.length; i++) {
          var file = event.target.files[i];
          var name = file.name;
          var size = file.size;
          var type = file.type;

          var fileContainer = $('<div class="attachment-file-container"></div>');
          fileContainer.appendTo(fileLabel);
          // Create icons based on file type
          if (type.indexOf('image') !== -1) {
            fileContainer.addClass('file-image');
            fileContainer.append('<span class="attachment-type glyphicon glyphicon-picture"></span>');
          } else if (type.indexOf('audio') !== -1) {
            fileContainer.addClass('file-audio');
            fileContainer.append('<span class="attachment-type glyphicon glyphicon-music"></span>');
          } else if (type.indexOf('video') !== -1) {
            fileContainer.addClass('file-video');
            fileContainer.append('<span class="attachment-type glyphicon glyphicon-film"></span>');
          } else if (type.indexOf('zip') !== -1) {
            fileContainer.addClass('file-video');
            fileContainer.append('<span class="attachment-type glyphicon glyphicon-compressed"></span>');
          } else {
            fileContainer.addClass('file-other');
            fileContainer.append('<span class="attachment-type glyphicon glyphicon-file"></span>');
          }
          fileContainer.append('<span class="attachment-name"> ' + name + ' </span>');
          fileContainer.append('<span class="attachment-size"> ' + self.humanReadableFileSize(size, true) + ' </span>');

          fileLabel.removeClass('attachment-blank');

        }

      });

      return false;
    };

    /**
     * @event attachDocument
     * @returns {boolean}
     */
    self.attachDocument = function (event) {
      "use strict";
      event.preventDefault();
      $(self).trigger("attachDocument", [self]);

      // Add the file input onto the page
      var id = self.generateID();

      var fileGroupContainer = $('<div></div>')
        .addClass('attachment-group-container')
        .appendTo(self.find('.document-attachments'));

      var fileInput = $('<input>')
        .attr('type', 'hidden')
        .attr('id', 'file_' + id)
        .attr('name', 'documentId')
        .attr('data-file-input', 'documentId')
        .appendTo(fileGroupContainer);


      //language=JQuery-CSS
      var document_attachment_id = $('[name=document_attachment_id]');
      var fileInputID = undefined;
      if (document_attachment_id.length === 0) {
        fileInputID = $('<input>')
          .attr('type', 'hidden')
          .attr('name', 'document_attachment_id')
          .appendTo(fileGroupContainer.closest('.attachments'));
      } else {
        fileInputID = document_attachment_id;
      }

      $.fn.EmailsComposeView.selectDocumentFromPopup = function(resultData) {
        set_return(resultData);
        if (fileInputID.val().length === 0) {
          // id is empty
          fileGroupContainer.remove();
          self.updateDocumentIDs();
        } else {
          // id is full
          if (fileGroupContainer.find('.attachment-remove').length === 0) {
            var removeAttachment = $('<a class="attachment-remove"><span class="glyphicon glyphicon-remove"></span></a>');
            fileGroupContainer.append(removeAttachment);
            // handle when user removes attachment
            removeAttachment.click(function () {
              fileGroupContainer.remove();
              self.updateDocumentIDs();
            });
          }

          fileInput.val(fileInputID.val());
          fileLabel.empty();

          var fileContainer = $('<div class="attachment-file-container"></div>');
          fileContainer.appendTo(fileLabel);
          fileContainer.append('<span class="attachment-name"> ' + fileInputName.val() + ' </span>');

          fileLabel.removeClass('attachment-blank');

          self.updateDocumentIDs();
        }
      };

      //language=JQuery-CSS
      var document_attachment_name = $('[name=document_attachment_name]');
      var fileInputName = undefined;
      if (document_attachment_name.length === 0) {
        fileInputName = $('<input>')
          .attr('type', 'hidden')
          .attr('name', 'document_attachment_name')
          .appendTo(fileGroupContainer.closest('.attachments'));
      } else {
        fileInputName = document_attachment_name;
      }
      fileInputName.val('');

      var fileLabel = $('<label></label>')
        .attr('for', 'file_' + id)
        .addClass('attachment-blank')
        .html('<img src="themes/' + SUGAR.themes.theme_name + '/images/sidebar/modules/Documents.svg">')
        .appendTo(fileGroupContainer);

      var showSelectDocumentDialog = function () {
        fileInputID.val('');
        fileInputName.val('');
        var popupWindow = open_popup(
          "Documents",
          600,
          400,
          "",
          true,
          false,
          {
            "call_back_function": '$.fn.EmailsComposeView.selectDocumentFromPopup',
            "form_name": "ComposeView",
            "field_to_name_array": {
              "id": "document_attachment_id",
              "name": "document_attachment_name"
            }
          },
          "single",
          false
        );

      };

      // Mimic the file attachment behaviour
      fileLabel.click(showSelectDocumentDialog);
      // Call the select document dialog
      fileLabel.click();

      return false;
    };

    self.updateDocumentIDs = function () {
      self.find('.document-attachments')
        .find('.attachment-group-container')
        .each(function (index, value) {
          $(value).find('[data-file-input]').attr('name', 'documentId' + index);
        });
    };

    /**
     * @event saveDraft
     * @returns {boolean}
     */
    self.saveDraft = function (e) {
      "use strict";
      e.preventDefault();
      $(this).closest('[name=action]').val('SaveDraft');

      if (self.validate()) {
        self.onSavingDraft();
      }
      return false;
    };

    self.onSavingDraft = function () {
      "use strict";
      $(self).trigger("saveDraft", [self]);
      // Tell the user we are sending an email
      var mb = messageBox({backdrop:'static'});
      mb.hideHeader();
      mb.hideFooter();
      mb.setBody('<div class="email-in-progress"><img src="themes/' + SUGAR.themes.theme_name + '/images/loading.gif"></div>');
      mb.show();

      mb.on('ok', function () {
        "use strict";
        mb.remove();
      });

      mb.on('cancel', function () {
        "use strict";
        mb.remove();
      });

      var fileCount = 0;
      // Use FormData v2 to send form data via ajax
      var formData = new FormData(jQueryFormComposeView);

      $(this).find('input').each(function (i, v) {
        if ($(v).attr('type').toLowerCase() !== 'file') {
          var name = $(v).attr('name');
          if (name === 'action') {
            formData.append(name, 'SaveDraft');
          } else if (name === 'send') {
            formData.append(name, 0);
          } else {
            formData.append(name, $(v).val());
          }
        }
      });

      $(this).find('select').each(function (i, v) {
        if (typeof $(v).attr('is_file') === 'undefined') {
          formData.append($(v).attr('name'), $(v).val());
        }
      });

      $(this).find('textarea').each(function (i, v) {
        formData.append($(v).attr('name'), $(v).val());
      });

      $(this).find('button').each(function (i, v) {
        formData.append($(v).attr('name'), $(v).val());
      });

      $.ajax({
        type: "POST",
        data: formData,
        cache: false,
        processData: false,  // tell jQuery not to process the data
        contentType: false,   // tell jQuery not to set contentType
        url: 'index.php?module=Emails'
      }).done(function (response) {
        "use strict";
        response = JSON.parse(response);
        if (typeof response.errors !== "undefined") {
          mb.showHeader();
          mb.setBody(response.errors.title);
          mb.showFooter();
          mb.hideCancel();
          $(self).trigger("saveEmailError", [self, response]);
        } else {
          mb.showHeader();
          mb.setBody(response.data.title);
          mb.showFooter();
          mb.hideCancel();
          $(self).trigger("saveEmailSuccess", [self, response]);

          var id = undefined;
          if ($(self).find('[name=id]').length === 0) {
            id = $('<input>').attr('type', 'hidden').attr('name', 'id').val(response.data.id);
            id.appendTo($(self).closest('[name=ComposeView]'));
          } else {
            id = $(self).find('[name=id]');
            $(id).val(response.data.id);
          }
          $(self).find('input[name=record]').val(response.data.id);
          $.fn.EmailsComposeView.checkForDraftAttachments(response.data.id);
        }
      }).fail(function (response) {
        "use strict";
        response = JSON.parse(response);
        mb.setBody(response.errors.title);
        $(self).trigger("saveEmailError", [self, response]);
      }).always(function (response) {
        response = JSON.parse(response);
        $(self).trigger("saveEmailAlways", [self, response]);
      });

      return false;
    };
    /**
     *
     * @event disregardDraft
     * @returns {boolean}
     */
    self.disregardDraft = function () {
      "use strict";

      var mb = messageBox();
      mb.setTitle(SUGAR.language.translate('Emails', 'LBL_CONFIRM_DISREGARD_DRAFT_TITLE'));
      mb.setBody(SUGAR.language.translate('Emails', 'LBL_CONFIRM_DISREGARD_DRAFT_BODY'));
      mb.show();

      mb.on('ok', function () {
        "use strict";

        mb.setBody('<div class="email-in-progress"><img src="themes/' + SUGAR.themes.theme_name + '/images/loading.gif"></div>');

        $(jQueryFormComposeView).find('input[name=action]').val('DeleteDraft');
        // Use FormData v2 to send form data via ajax
        var formData = new FormData(jQueryFormComposeView);

        $(this).find('input').each(function (i, v) {
          if ($(v).attr('type').toLowerCase() !== 'file') {
            var name = $(v).attr('name');
            if (name === 'action') {
              formData.append(name, 'Delete');
            } else if (name === 'send') {
              formData.append(name, 0);
            } else {
              formData.append(name, $(v).val());
            }
          }
        });

        $(this).find('select').each(function (i, v) {
          if (typeof $(v).attr('is_file') === 'undefined') {
            formData.append($(v).attr('name'), $(v).val());
          }
        });

        $(this).find('textarea').each(function (i, v) {
          formData.append($(v).attr('name'), $(v).val());
        });

        $(this).find('button').each(function (i, v) {
          formData.append($(v).attr('name'), $(v).val());
        });

        $.ajax({
          type: "POST",
          data: formData,
          cache: false,
          processData: false,  // tell jQuery not to process the data
          contentType: false,   // tell jQuery not to set contentType
          url: 'index.php?module=Emails'
        }).done(function (response) {
          $(self).trigger("discardDraftDone", [self, response]);
        }).error(function (response) {
          mb.setBody(SUGAR.language.translate('', 'LBL_ERROR_SAVING_DRAFT'));
          $(self).trigger("discardDraftBody", [self, response]);
        }).always(function (response) {
          $(self).trigger("discardDraftAlways", [self, response]);
          mb.remove();
          if ($(self).find('input[type="hidden"][name="return_module"]').val() !== '') {
            location.href = 'index.php?module=' + $('#' + self.attr('id') + ' input[type="hidden"][name="return_module"]').val() +
              '&action=' +
              $(self).find('input[type="hidden"][name="return_action"]').val();
          } else {
            // The user is viewing in the modal view
            location.reload();
          }
        });
      });
      mb.on('cancel', function () {
        "use strict";
        // do something
        mb.remove();
      });

      return false;
    };

    self.humanReadableFileSize = function (bytes, si) {
      var thresh = si ? 1000 : 1024;
      if (Math.abs(bytes) < thresh) {
        return bytes + ' B';
      }
      var units = si
        ? ['kB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB']
        : ['KiB', 'MiB', 'GiB', 'TiB', 'PiB', 'EiB', 'ZiB', 'YiB'];
      var u = -1;
      do {
        bytes /= thresh;
        ++u;
      } while (Math.abs(bytes) >= thresh && u < units.length - 1);
      return bytes.toFixed(1) + ' ' + units[u];
    };

    /**
     * Constructor
     */
    self.construct = function () {
      "use strict";

      if (self.length === 0) {
        console.error('EmailsComposeView - Invalid Selector');
        return;
      }

      if (self.attr('id').length === 0) {
        console.warn('EmailsComposeView - expects element to have an id. EmailsComposeView has generated one.');
        self.attr('id', self.generateID());
      }

      if (self.find("[name=record]").val().length > 0) {
        $.fn.EmailsComposeView.checkForDraftAttachments(self.find("[name=record]").val());
      }

      if (typeof opts.tinyMceOptions.setup === "undefined") {
        opts.tinyMceOptions.setup = self.tinyMceSetup;
      }

      if (typeof opts.tinyMceOptions.selector === "undefined") {
        opts.tinyMceOptions.selector = 'form[name="ComposeView"] textarea[id=description]';
      }

      if ($(self).find('#from_addr_name').length !== 0) {
        var selectFrom = $('<select></select>')
          .attr('name', 'from_addr')
          .attr('id', 'from_addr_name');
        var from_addr = $(self).find('#from_addr_name');
        from_addr.replaceWith(selectFrom);

        var hiddenOutboundEmailId = $('<input type="hidden" name="outbound_email_id" id="outbound_email_id" value="">');
        selectFrom.parent().append(hiddenOutboundEmailId);



        $.ajax({
          "url": 'index.php?module=Emails&action=getFromFields'
        }).done(function (response) {
          var json = JSON.parse(response);
          if (typeof json.data !== "undefined") {
            var optionGroups = {
              InboundEmail: {
                label: 'LBL_INBOUND_ACCOUNT',
                options: []
              },
              OutboundEmailAccount: {
                label: 'LBL_OUTBOUND_ACCOUNT',
                options: []
              },
              system: {
                label: 'LBL_SYSTEM_ACCOUNT',
                options: []
              },
              personal: {
                label: 'LBL_FROM_SYSTEM',
                options: []
              },
            };

            $(json.data).each(function (i, v) {
              var selectOption = $('<option></option>');
              selectOption.attr('value', v.attributes.from);

              var entryType = v.type || '';
              var entryLabel = self.getFromEntryLabel(v);

              if (entryType === 'OutboundEmailAccount' || entryType === 'system') {
                selectOption.attr('data-outbound-email-id', v.id);
              } else {
                selectOption.attr('inboundId', v.id);
              }

              var fromName = v.attributes.name;
              if (!fromName) {
                fromName = v.attributes.from;
              }

              selectOption.attr('infos', '(<b>Reply-to:</b> ' + v.attributes.reply_to + ' , <b>Reply-to Name:</b> ' + v.attributes.reply_to_name + ' , <b>From:</b> ' + v.attributes.from + ', <b>From Name:</b> ' + v.attributes.name + ')');
              selectOption.attr('data-from-name', fromName);
              selectOption.attr('data-from-address', v.attributes.from);
              selectOption.attr('data-reply-to-name', v.attributes.reply_to_name);
              selectOption.attr('data-reply-to-address', v.attributes.reply_to);
              selectOption.attr('data-type', v.type);
              selectOption.attr('label', entryLabel);
              selectOption.html(entryLabel);
              optionGroups[entryType].options.push(selectOption);

              selectOption.attr('data-email-signature-id', v.id);
              // include signature for account
              $('<textarea></textarea>')
                .val(v.emailSignatures.html)
                .addClass('email-signature')
                .addClass('html')
                .addClass('hidden')
                .attr('data-email-signature-id', v.id)
                .appendTo(self);

              $('<textarea></textarea>')
                .val(v.emailSignatures.plain)
                .addClass('email-signature')
                .addClass('plain')
                .addClass('hidden')
                .attr('data-email-signature-id', v.id)
                .appendTo(self);

              if (typeof v.prepend !== "undefined" && v.prepend === true) {
                self.prependSignature = true;
              }

            });

            Object.keys(optionGroups).forEach(function (type) {
              var optionGroup = optionGroups[type];

              if (!optionGroup || !optionGroup.options || !optionGroup.options.length) {
                return;
              }

              var $optionGroup = $('<optgroup>', {
                label: SUGAR.language.translate('', optionGroup.label)
              });

              optionGroup.options.forEach(function (option) {
                selectFrom.append(option);
              });
            });

            self.updateOutboundEmailId(selectFrom);

            var selectedInboundEmail = $(self).find('[name=inbound_email_id]').val();
            var selectInboundEmailOption = $(selectFrom).find('[inboundid="' + selectedInboundEmail + '"]');
            if (selectInboundEmailOption.val()) {
              $(selectFrom).val(selectInboundEmailOption.val());
            }


            $.widget( "custom.emailselectmenu", $.ui.selectmenu, {
              _renderItem: function( ul, item ) {

                var $option = item.element;
                var fromName = $option.attr('data-from-name');
                var fromAddress = $option.attr('data-from-address');

                var $info = $('<div>', {
                  class: 'compose-from-email-account'
                });
                $info.append("<div class='compose-from-email-address-name'><span>" + fromName + "</span></div>");
                $info.append("<div class='compose-from-email-address-from'><span class='small'>&lt;" + fromAddress + "&gt;</span></div>");

                return $( "<li>" )
                  .append($info)
                  .appendTo( ul );
              }
            });

            $( selectFrom ).emailselectmenu({
              change: function( event, ui ) {
                self.onFromSelect(selectFrom, ui);
              },
              select: function( event, ui ) {
                self.onFromSelect(selectFrom, ui);
              },
              appendTo: selectFrom.parent(),
              'max-width': '200px'
            });

            $(selectFrom).change(function (e) {

              var newlySelected = selectFrom.find('option:selected');
              var entryType = newlySelected.attr('data-type') || '';

              $('#outbound_email_id').val('');
              if (entryType === 'OutboundEmailAccount' || entryType === 'system') {
                self.updateOutboundEmailId(selectFrom);
              } else {
                $(self).find('[name=inbound_email_id]').val($(this).find('option:selected').attr('inboundId'));
              }

              self.updateSignature();
            });

            $(self).trigger('emailComposeViewGetFromFields');


            if (tinymce.initialized === true) {
              self.updateSignature();
            } else if(tinymce.EditorManager && tinymce.EditorManager.activeEditor) {
              tinymce.EditorManager.activeEditor.on('init', function(e) {
                self.updateSignature();
              });
            }
          }

          if ($(self).find('#is_only_plain_text').length === 1) {
            $(self).find('#is_only_plain_text').click(function () {
              let tinymceContainer = $(tinymce.EditorManager.activeEditor.getContainer());
              let description = $(self).find('#description');
              if ($('#is_only_plain_text').prop('checked')) {
                tinymceContainer.hide();
                description.show();
              } else {
                description.hide();
                tinymceContainer.show();
              }
            });
          }

          if (typeof json.errors !== "undefined") {
            $.fn.EmailsComposeView.showAjaxErrorMessage(json);
          }
        }).error(function (response) {
          console.error(response);
        });
      }

      /**
       * Used to preview email. It also doubles as a means to get the plain text version
       * using $('#'+self.attr('id') + ' .html_preview').text();s
       */
      $('<div></div>').addClass('hidden').addClass('html_preview').appendTo($(self));

      $('<input>')
        .attr('name', 'description_html')
        .attr('type', 'hidden')
        .attr('id', 'description_html')
        .appendTo($(self));

      if (typeof tinymce === "undefined") {
        console.error('EmailsComposeView - Missing Dependency: Cannot find tinyMCE.');

        // copy plain to html
        $(self).find('#description_html').closest('.edit-view-row-item').addClass('hidden');
        $(self).find('textarea#description_html').on("keyup", function () {
          $(self).find('input#description_html').val($(self).find('textarea#description').val().replace('\n', '<br>'));
        });
      } else {
        $(self).find('[data-label="description_html"]').closest('.edit-view-row-item').addClass('hidden');

        var intervalCheckTinymce = window.setInterval(function () {
          var isFromPopulated = $('#from_addr_name').prop("tagName").toLowerCase() === 'select';
          if (tinymce.editors.length > 0 && isFromPopulated === true) {
            self.updateSignature();
            clearInterval(intervalCheckTinymce);
          }
        }, 300);

        tinymce.init(opts.tinyMceOptions);

      }

      // Handle sent email submission
      self.submit(self.onSendEmail);

      // Handle toolbar (default) button events
      $(self).find('.btn-send-email').click(self.sendEmail);
      $(self).find('.btn-attach-file').click(self.attachFile);
      $(self).find('.btn-attach-notes').click(self.attachNote);
      $(self).find('.btn-attach-document').click(self.attachDocument);
      $(self).find('.btn-save-draft').click(self.saveDraft);
      $(self).find('.btn-disregard-draft').click(self.disregardDraft);

      var file = $('<input />')
        .attr('name', file);

      $(self).on('remove', self.destruct);

      // detect empty rows
      $(self).find('.edit-view-row-item').each(function () {
        if (trim($(this).html()).length === 0) {
          $(this).addClass('empty');
        }
      });

      // qtipBar
      var hidden = $('<input type="hidden" id="qtip_bar_module">' +
        '<input type="hidden" id="qtip_bar_id">' +
        '<input type="hidden" id="qtip_bar_name">' +
        '<input type="hidden" id="qtip_bar_email_address">').appendTo(self);
      $(self).find('#to_addrs_names').focus(self.showQTipBar);
      $(self).find('#cc_addrs_names').focus(self.showQTipBar);
      $(self).find('#bcc_addrs_names').focus(self.showQTipBar);
      $(self).on('sendEmail', function () {
        $('.emails-qtip').remove();
      });


      $(self).trigger("constructEmailsComposeView", [self]);
    };

    /**
     * @destructor
     */
    self.destruct = function () {
      // TODO: Find a better way only display one tiny mce
      // Remove the hanging tinyMCE div
      $('.mce-panel').remove();
      var length = tinyMCE.editors.length;
      for (var i = length; i > 0; i--) {
        tinyMCE.editors[i - 1].remove();
      }
      $('.emails-qtip').remove();
      return true;
    };

    self.construct();

    return $(self);
  };

  $.fn.EmailsComposeView.checkForDraftAttachments = function (id) {
    // Check if this is a draft email with attachments
    $.ajax({
      "url": 'index.php?module=Emails&action=GetDraftAttachmentData&id=' + id
    }).done(function (jsonResponse) {
      var response = JSON.parse(jsonResponse);
      if (typeof response.data !== "undefined") {
        $('.file-attachments').empty();
        $.fn.EmailsComposeView.loadAttachmentDataFromAjaxResponse(response);
      }
      if (typeof response.errors !== "undefined") {
        $.fn.EmailsComposeView.showAjaxErrorMessage(response);
      }
    }).error(function (response) {
      console.error(response);
    });
  };

  $.fn.EmailsComposeView.showAjaxErrorMessage = function (response) {
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

  $.fn.EmailsComposeView.loadAttachmentDataFromAjaxResponse = function (response) {
    var isDraft = (typeof response.data.draft !== undefined && response.data.draft ? true : false);
    var inputName = 'template_attachment[]';
    var removeName = 'temp_remove_attachment[]';
    if (isDraft) {
      var inputName = 'dummy_attachment[]';
      var removeName = 'remove_attachment[]';
    }
    if (typeof response.data.attachments !== 'undefined' && response.data.attachments.length > 0) {
      var removeDraftAttachmentInput = $('<input>')
        .attr('type', 'hidden')
        .attr('name', 'removeAttachment')
        .appendTo($('.file-attachments'));
      for (i = 0; i < response.data.attachments.length; i++) {
        var id = response.data.attachments[i]['id'];
        var fileGroupContainer = $('<div></div>')
          .addClass('attachment-group-container')
          .appendTo($('.file-attachments'));

        var fileInput = $('<select></select>')
          .attr('style', 'display:none')
          .attr('id', id)
          .attr('is_file', true)
          .attr('name', inputName)
          .attr('multiple', 'multiple');

        var fileOptions = $('<option></option>')
          .attr('selected', 'selected')
          .attr('value', id)
          .appendTo(fileInput);

        fileInput.appendTo(fileGroupContainer);
        var fileLabel = $('<label></label>')
          .attr('for', 'file_' + id)
          .html('<span class="glyphicon glyphicon-paperclip"></span>')
          .appendTo(fileGroupContainer);

        var fileContainer = $('<div class="attachment-file-container"></div>');
        fileContainer.appendTo(fileLabel);
        fileContainer.append('<span class="attachment-name"> ' + response.data.attachments[i]['name'] + ' </span>');

        var removeAttachment = $('<a class="attachment-remove"><span class="glyphicon glyphicon-remove"></span></a>');
        removeAttachment.click(function () {
          $(this).parent().hide();
          $(this).parent().find('[name="' + inputName + '"]').attr('name', removeName);
          if (isDraft) {
            removeDraftAttachmentInput.val(removeDraftAttachmentInput.val() + '::' + id);
          }
        });
        fileGroupContainer.append(removeAttachment);
      }
    }
  };

  $.fn.EmailsComposeView.onTemplateSelect = function (args) {

    var confirmed = function (args) {
      var args = JSON.parse(args);
      var form = $('[name="' + args.form_name + '"]');
      $.post('index.php?entryPoint=emailTemplateData', {
        emailTemplateId: args.name_to_value_array.emails_email_templates_idb
      }, function (jsonResponse) {
        var response = JSON.parse(jsonResponse);
        $.fn.EmailsComposeView.loadAttachmentDataFromAjaxResponse(response);
        $(form).find('[name="name"]').val(response.data.subject);
        tinymce.activeEditor.setContent(response.data.body_from_html, {format: 'html'});
        $.fn.EmailsComposeView.updateSignature();
      });
      set_return(args);
    };

    var mb = messageBox();
    mb.setTitle(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE'));
    mb.setBody(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_BODY'));
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

  $.fn.EmailsComposeView.onTemplateChange = function (args) {
    var confirmed = function (args) {
      var args = JSON.parse(args);
      var form = $('[name="' + args.form_name + '"]');
      $.post('index.php?entryPoint=emailTemplateData', {
        emailTemplateId: args.name_to_value_array.emails_email_templates_idb
      }, function (jsonResponse) {
        var response = JSON.parse(jsonResponse);
        $.fn.EmailsComposeView.loadAttachmentDataFromAjaxResponse(response);
        $(form).find('[name="name"]').val(response.data.subject);
        tinymce.activeEditor.setContent(response.data.body_from_html, {format: 'html'});
        $.fn.EmailsComposeView.updateSignature();
      });
      set_return(args);
    };
    var mb = messageBox();
    mb.setTitle(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_TITLE'));
    mb.setBody(SUGAR.language.translate('Emails', 'LBL_CONFIRM_APPLY_EMAIL_TEMPLATE_BODY'));
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

  $.fn.EmailsComposeView.onParentSelect = function (args) {
    set_return(args);
    if (isValidEmail(args.name_to_value_array.email1)) {
      var emailAddress = args.name_to_value_array.email1;
      var self = $('[name="' + args.form_name + '"]');
      var toField = $(self).find('[name=to_addrs_names]');
      if (toField.val().indexOf(emailAddress) === -1) {
        var toFieldVal = toField.val();
        if (toFieldVal === '') {
          toField.val(emailAddress);
        } else {
          toField.val(toFieldVal + ', ' + emailAddress);
        }

      }
    }
  };

  $.fn.EmailsComposeView.defaults = {
    "tinyMceOptions": {
      menubar: false,
      plugins: ['link'],
      toolbar: ['fontselect | fontsizeselect | bold italic underline | forecolor backcolor | styleselect | outdent indent | link'],
      formats: {
        bold: {inline: 'b'},
        italic: {inline: 'i'},
        underline: {inline: 'u'}
      },
      convert_urls: false,
      relative_urls: false,
      remove_script_host: false,
    }
  };
}(jQuery));
