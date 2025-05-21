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

function hideElem(id) {
  if (document.getElementById(id)) {
    document.getElementById(id).style.display = "none";
  }
}

function showElem(id) {
  if (document.getElementById(id)) {
    document.getElementById(id).style.display = "";
  }
}

function assign_field_change(field) {
  hideElem(field + '[1]');
  hideElem(field + '[2]');

  if (document.getElementById(field + '[0]').value == 'role') {
    showElem(field + '[2]');
  } else if (document.getElementById(field + '[0]').value == 'security_group') {
    showElem(field + '[1]');
    showElem(field + '[2]');
  }
}

function displayDistributionOptions(display) {
  if (display) {
    $('[data-field="distribution_options"]').show();
    $('#distribution_options\\[0\\]').show();
    $('#distribution_options\\[1\\]').show();
    $('#distribution_options\\[2\\]').show();
    assign_field_change('distribution_options');
  } else {
    $('[data-field="distribution_options"]').hide();
    $('#distribution_options\\[0\\]').hide();
    $('#distribution_options\\[1\\]').hide();
    $('#distribution_options\\[2\\]').hide();
  }
}

function displayDistributionUser(display) {
  if (display) {
    $('[data-field="distribution_user_name"]').show();
  } else {
    $('[data-field="distribution_user_name"]').hide();
    $('#distribution_user_name').val('');
    $('#distribution_user_name').change();
    $('#distribution_user_id').val('');
    $('#distribution_user_id').change();
  }
}

$(document).ready(function () {
  displayDistributionOptions(false);
  $('#distrib_method').change(function () {
    var val = $('#distrib_method').val();
    switch (val) {
      case 'roundRobin':
      case 'leastBusy':
      case 'random':
        displayDistributionOptions(true);
        displayDistributionUser(false);
        break;
      case 'singleUser':
        displayDistributionOptions(false);
        displayDistributionUser(true);
        break;
      case 'AOPDefault':
      default:
        displayDistributionOptions(false);
        displayDistributionUser(false);
        break;
    }
  });
  $('#distrib_method').change();
});
