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
var module = "stic_Conversations";

/* VIEWS CUSTOM CODE */
switch (viewType()) {
  case "edit":
  case "quickcreate":
  case "popup":
    $(document).ready(function() {
      setAutofill(["name"]);

      // Readonly contact_name field when editing an existing record
      var recordId = $('#EditView input[name="record"]').val();
      if (recordId) {
        var $contactName = $('#contact_name');
        var $contactSearchBtn = $('#btn_contact_name');
        var $contactClearBtn = $('#btn_clr_contact_name');

        $contactName.attr('readonly', true);
        $contactName.prop('readonly', true);
        $contactName.css('background', '#F8F8F8');
        $contactName.css('border-color', '#E2E7EB');

        $contactSearchBtn.prop('disabled', true);
        $contactClearBtn.prop('disabled', true);
        $contactSearchBtn.hide();
        $contactClearBtn.hide();
      }
    });
    break;

  case "detail":
    break;

  case "list":
    break;

  default:
    break;
}

/* VALIDATION CALLBACKS */
