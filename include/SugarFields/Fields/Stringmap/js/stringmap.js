/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

var StringMap = (function () {
  return {
    $container:null,
    $list:null,
    idName: null,
    keyPlaceholder: null,
    valuePlaceholder: null,
    events: {
      'click .add-btn': 'addRow',
      'click .remove-btn': 'removeRow'
    },
    bindEvents: function(){
      var self = this;
      $.each(this.events, function(key, callback){
        var parts = key.split(' ');
        var eventType = parts[0];
        var selector = parts[1];

        self.$container.on(eventType, selector, function(event){
          self[callback](event);
        });
      });
    },
    generateInputCol: function(colClass, inputName, placeholder, inputValue){
      var $col = $('<div>', {
        class:(colClass || '')
      });


      var options = {
        name: inputName || '',
        value: inputValue || '',
        type: 'text'
      };

      if (placeholder) {
        options.placeholder = placeholder
      }

      var $input = $('<input>', options);

      $col.append($input);

      return $col;
    },
    generateButtonCol: function(){
      var $buttonCol = $('<div class="string-map-button-col"></div>');
      var $button = $('<button type="button" class="btn btn-sm btn-primary remove-btn" onClick=""> - </button>');
      $buttonCol.append($button);

      return $buttonCol;
    },
    generateRow: function(){

      var $row = $('<div>', {
        class: "string-map-entry-row"
      });

      if(this.showKeys) {
        var keyInputName = (this.idName || '') + '-key[]';
        var $keyCol =  this.generateInputCol('string-map-key-col', keyInputName, this.keyPlaceholder);
        $row.append($keyCol);
      }

      var valueInputName = (this.idName || '') + '-value[]';
      var placeholder = this.valuePlaceholder;
      if (!this.showKeys) {
        placeholder = '';
      }
      var $valueCol =  this.generateInputCol('string-map-value-col', valueInputName, placeholder);
      $row.append($valueCol);

      var $buttonCol = this.generateButtonCol();
      $row.append($buttonCol);

      return $row;
    },
    addRow: function(event){
      var self = this;
      var $newRow = self.generateRow();

      self.$list.append($newRow);
      event.stopPropagation();
    },
    removeRow: function(event){
      var self = this;
      var $removeButton = $(event.currentTarget);
      var $row = $removeButton.closest('.string-map-entry-row');
      $row.remove();
      event.stopPropagation();
    },
    initEdit: function (options) {
      var self = this;
      this.bindEvents();
    },
  };
})();
