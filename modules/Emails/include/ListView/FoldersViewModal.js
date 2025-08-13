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
   * @return {*|HTMLElement}
   */
  $.fn.FoldersViewModal =  function(options) {
    "use strict";
    var self = this;
    var opts = $.extend({}, $.fn.FoldersViewModal.defaults, options);

    self.handleClick = function () {
      "use strict";
      self.emailFoldersView = null;
      var foldersBox = $('<div></div>').appendTo(opts.contentSelector);
      foldersBox.messageBox({
        "showHeader": false,
        "showFooter": false,
        "size": 'lg'
      });
      foldersBox.setBody('<div class="in-progress"><img src="themes/'+SUGAR.themes.theme_name+'/images/loading.gif"></div>');
      foldersBox.show();

      $.ajax({
        type: "GET",
        cache: false,
        url: 'index.php?module=Emails&action=GetFolders&to_pdf=1'
      }).done(function (data) {
        var response = JSON.parse(data);

        if(typeof response.errors !== "undefined") {
          $(response.errors).each(function(i,v) {
            foldersBox.setBody('<div class="error">' + v + '</div>');
          });
          return false;
        }

        response = response.response;

        self.tree = $('<div></div>');
        self.tree.jstree({
          'core' : {
            'data' : response
          }
        }).on('select_node.jstree', function(e, data) {
          "use strict";
          if(typeof data.selected[0] !== "undefined") {
            var mbox = data.selected[0];
            // reload with different inbox
            $('[name=folders_id]').val(mbox);
            top.location = 'index.php?module=Emails&action=index&folders_id=' + mbox;
          }
        });

        foldersBox.setBody(self.tree);
      });

    };

    /**
     * @constructor
     */
    self.construct = function () {
      "use strict";
      $(opts.buttonSelector).click(self.handleClick);
    };

    /**
    * @destructor
    */
    self.destruct = function() {

    };

    self.construct();

    return $(self);
  };

  $.fn.FoldersViewModal.defaults = {
    'buttonSelector': '[data-action=emails-show-folders-modal]',
    'contentSelector': '#content',
    'defaultFolder': 'INBOX'
  }
}(jQuery));

$(document).ready(function() {
  $(document).FoldersViewModal();
});
