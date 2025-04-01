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

var inboundEmailFields = function () {

  var requiredLabelTemplate = '<span class="required">*</span>';
  var validationMessageTemplate = '<div class="required validation-message">Missing required field: $FIELD_NAME</div>';

  var getValidationDefinition = function (formName, field) {
    if (validate[formName]) {
      for (i = 0; i < validate[formName].length; i++) {
        if (validate[formName][i][nameIndex] == field) {
          return validate[formName][i];
        }
      }
    }
    return null;
  };

  var configureValidation = function (formName, field, required) {

    var definition = getValidationDefinition(formName, field);

    if(!definition){
      return;
    }
    var isRequired = true;

    if(!required){
      isRequired = false;
    }

    definition[requiredIndex] = isRequired;
  };

  var addRequiredIndicator = function ($label) {
    var $indicator = $label.find('.required');

    if ($indicator.length < 1) {
      $label.append($(requiredLabelTemplate));
    }
  };

  var removeRequiredIndicator = function ($label) {
    var $indicator = $label.find('.required');

    if ($indicator.length > 0) {
      $indicator.remove();
    }
  };

  var getDefaultFieldGetter = function () {
    return function (field$) {
      return (field$ && field$.val()) || '';
    };
  };

  var getDefaultFieldSetter = function () {
    return function (field$, value) {
      if (!field$) {
        return;
      }

      field$.val(value);
      field$.change();
    };
  };

  return {
    fields: {
      'record': {
        type: 'varchar',
        getField$: function (field) {
          return $('input[name=' + field + ']') || null;
        }
      },
      'origin_id': {
        type: 'varchar'
      },
      'server_url': {
        type: 'varchar'
      },
      'protocol': {
        type: 'varchar'
      },
      'port': {
        type: 'varchar'
      },
      'email_user': {
        type: 'varchar'
      },
      'email_password': {
        type: 'varchar'
      },
      'sentFolder': {
        type: 'varchar'
      },
      'trashFolder': {
        type: 'varchar'
      },
      'mailbox': {
        type: 'varchar'
      },
      'is_ssl': {
        type: 'checkbox'
      },
      'type': {
        type: 'varchar'
      },
      'is_create_case': {
        type: 'checkbox'
      },
      'searchField': {
        type: 'varchar'
      },
      'auth_type': {
        type: 'varchar'
      },
      'external_oauth_connection_name': {
        type: 'varchar',
        getField$: function (field) {
          return $('input[name=' + field + ']') || null;
        }
      },
      'external_oauth_connection_id': {
        type: 'varchar',
        getField$: function (field) {
          return $('input[name=' + field + ']') || null;
        }
      },
    },

    getters: {
      default: getDefaultFieldGetter(),
      varchar: getDefaultFieldGetter(),
      checkbox: function (field$) {
        return (field$ && field$.prop('checked')) || false;
      }
    },

    setters: {
      default: getDefaultFieldSetter(),
      varchar: getDefaultFieldSetter(),
      checkbox: function (field$, value) {
        if (!field$) {
          return;
        }

        field$.prop('checked', !!value);
      }
    },

    setValue: function (field, value) {
      var field$ = this.getField$(field);
      if (!field$) {
        return null;
      }

      var setter = this.getValueSetter(field);
      if (!setter) {
        return null;
      }

      return setter(field$, value);
    },

    getValue: function (field) {
      var field$ = this.getField$(field);
      if (!field$) {
        return null;
      }

      var getter = this.getValueGetter(field);
      if (!getter) {
        return null;
      }

      return getter(field$);
    },

    getData: function (field, dataKey) {
      var field$ = this.getField$(field);
      if (!field$) {
        return null;
      }

      return field$.data(dataKey);
    },

    hide: function (field) {
      var field$ = this.getFieldCell$(field);

      if (!field$ || !field$.length) {
        return;
      }

      field$.hide();
    },

    show: function (field) {
      var field$ = this.getFieldCell$(field);

      if (!field$ || !field$.length) {
        return;
      }

      field$.show();
    },


    getField$: function (field) {
      var handler = (this.fields[field] && this.fields[field].getField$) || null;

      if (handler) {
        return handler(field);
      }

      return $('#' + field) || null;
    },

    getFieldCell$: function (field) {
      return $('[data-field="' + field + '"]') || null;
    },

    getFieldType: function (field) {
      return (this.fields[field] && this.fields[field].type) || 'varchar';
    },

    getValueGetter: function (field) {
      var handler = (this.fields[field] && this.fields[field].getter) || null;

      if (handler) {
        return handler;
      }

      var type = this.getFieldType(field);
      return this.getters[type] || this.getters['default'];
    },

    getValueSetter: function (field) {
      var handler = (this.fields[field] && this.fields[field].setter) || null;

      if (handler) {
        return handler;
      }

      var type = this.getFieldType(field);
      return this.setters[type] || this.setters['default'];
    },

    setRequired: function (field, fieldType, formName, required) {
      configureValidation(this.formName, this.name, required);

      this.setRequiredIndicator(field, required);

      if (required) {
        addToValidate(formName, field, fieldType, true, SUGAR.language.get('InboundEmail',"LBL_" + field.toUpperCase()));
      } else {
        removeFromValidate(formName, field);
      }

    },

    setRequiredIndicator: function (field, required) {
      var $label = this.getFieldCell$(field).find('.label');
      if (required) {
        addRequiredIndicator($label);
      } else {
        removeRequiredIndicator($label);
      }
    },

  };
}();


