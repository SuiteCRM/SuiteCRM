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
(function( $ ) {
  /**
   *
   * @param options
   * @returns {jQuery|HTMLElement}
   * @constructor
   */
  $.fn.messageBox = function (options) {
    "use strict";
    var self = this;
    self.controls = {};
    self.controls.modal = {};

    if(typeof $.fn.modal === "undefined") {
      console.error('messageBox - Missing Dependency Required: Bootstrap model');
      return self;
    }

    self.modal = $.fn.modal;

    var opts = $.extend( {}, $.fn.messageBox.defaults, options );

    /**
     * Call open when there is no DOM. Show dialog
     */
    self.show = function () {
      "use strict";
      self.controls.modal.container.modal('show');

      return self;
    };

    /**
     * @event ok
     */
    self.onOk = function () {
      "use strict";
      $(self).trigger('ok');
      return false;
    };

    /**
     * @event cancel
     */
    self.onCancel = function () {
      "use strict";
      $(self).trigger('cancel');
      return false;
    };

    // Utilities
    /**
     * @return string UUID
     */
    self.generateID = function () {
      "use strict";
      var characters = ['a', 'b', 'c', 'd', 'e', 'f', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
      var format = '0000000-0000-0000-0000-00000000000';
      var z = Array.prototype.map.call(format, function ($obj) {
        var min = 0;
        var max = characters.length - 1;

        if ($obj == '0') {
          var index = Math.round(Math.random() * (max - min) + min);
          $obj = characters[index];
        }

        return $obj;
      }).toString().replace(/(,)/g, '');
      return z;
    };

    /**
     *
     * @param content htmlString|function
     */
    self.setTitle = function(content) {
      "use strict";
      self.controls.modal.container.find('.modal-title').html(content);
    };

    /**
     * @return {jQuery}
     */
    self.getTitle = function() {
      "use strict";
      return self.controls.modal.container.find('.modal-title');
    };

    /**
     *
     * @param content htmlString|function
     */
    self.setBody = function(content) {
      "use strict";
      self.controls.modal.container.find('.modal-body').html(content);
    };

    /**
     * @return {jQuery}
     */
    self.getBody = function() {
      "use strict";
      return self.controls.modal.container.find('.modal-body');
    };

    /**
     *
     */
    self.showHeader = function() {
      "use strict";
      return self.controls.modal.container.find('.modal-header').show();
    };

    /**
     *
     */
    self.hideHeader = function() {
      "use strict";
      return self.controls.modal.container.find('.modal-header').hide();
    };

    /**
     *
     */
    self.showFooter = function() {
      "use strict";
      return self.controls.modal.container.find('.modal-footer').show();
    };

    /**
     *
     */
    self.hideCancel = function() {
      "use strict";
      return self.controls.modal.container.find('.btn-cancel').hide();
    };

    /**
     *
     */
    self.hideOk = function() {
      "use strict";
      return self.controls.modal.container.find('.btn-ok').hide();
    };

    /**
     *
     */
    self.hideFooter = function() {
      "use strict";
      return self.controls.modal.container.find('.modal-footer').hide();
    };

    self.headerContent = '<button type="button" class="close btn-cancel" aria-label="Close"><span aria-hidden="true">×</span></button><h4 class="modal-title"></h4>';
    self.footerContent = '<button class="button btn-ok" type="button">'+SUGAR.language.translate('','LBL_OK')+'</button> <button class="button btn-cancel" type="button">'+SUGAR.language.translate('','LBL_CANCEL_BUTTON_LABEL')+'</button> ';
    
    self.construct = function (constructOptions) {
      "use strict";
      if(typeof self.controls.modal.container === "undefined") {
        if(typeof opts.onOK === "undefined") {
          opts.onOK = self.onOk;
        }

        if(typeof opts.onCancel === "undefined") {
          opts.onCancel = self.onCancel;
        }


        if(typeof opts.headerContent === "undefined") {
          opts.headerContent = self.headerContent;
        }

        if(typeof opts.footerContent === "undefined") {
          opts.footerContent = self.footerContent;
        }


        self.controls.modal.container =
          self
            .addClass('modal fade message-box')
            .attr('id', self.generateID())
            .attr('role', 'dialog');

        self.controls.modal.dialog =
          $('<div></div>')
            .addClass('modal-dialog modal-' + opts.size)
            .attr('role', 'document')
            .appendTo(self.controls.modal.container);

        self.controls.modal.content =
          $('<div></div>')
            .addClass('modal-content')
            .appendTo(self.controls.modal.dialog);

        self.controls.modal.header =
          $('<div></div>')
            .addClass('modal-header')
            .appendTo(self.controls.modal.content);

        self.controls.modal.header.html(opts.headerContent);

        self.controls.modal.body =
          $('<div></div>')
            .addClass('modal-body')
            .appendTo(self.controls.modal.content);

        self.controls.modal.footer =
          $('<div></div>')
            .addClass('modal-footer')
            .html(opts.footerContent)
            .appendTo(self.controls.modal.content);

        self.controls.modal.footer.html(opts.footerContent);

        // OK / Cancel
        self.controls.modal.buttons = {};
        self.controls.modal.buttons.ok  = $(self.controls.modal.container).find('.btn-ok');
        self.controls.modal.buttons.cancel  = $(self.controls.modal.container).find('.btn-cancel');
        self.controls.modal.container.find('.btn-ok').click(opts.onOK);
        self.controls.modal.container.find('.btn-cancel').click(opts.onCancel);

        if(opts.showHeader === false) {
          self.controls.modal.header.hide();
        };

        if(opts.showFooter === false) {
          self.controls.modal.footer.hide();
        };

        self.modal(opts);
      }

      $(self).on('remove', self.destruct);

      return self;
    }

    /**
     * @destructor
     */
    self.destruct = function () {
      "use strict";
      self.attr('aria-hidden', "true");
      $('.modal-backdrop').last().remove();
      if($('.message-box').length <= 1) {
        $('.modal-open').removeClass('modal-open');
      }


      return true;
    }

    self.construct(opts);
    // Return this
    return self;
  };


  $.fn.messageBox.defaults = {
    "showHeader": true,
    "headerContent": self.headerContent,
    "footerContent": self.footerContent,
    "showFooter" : true,
    // Message Box Specific events
    "onOK": self.onOK,
    "onCancel": self.onCancel,
    // Style
    "size": 'sm',
    // Bootstrap
    "show": false,
    "backdrop": true,
    "keyboard": true
  }
}( jQuery ));


/**
 * Alias for $.fn.messageBox(options)
 * @param options
 */
messageBox = function (options) {
  "use strict";
  return $('<div></div>').appendTo('body').messageBox(options);
};