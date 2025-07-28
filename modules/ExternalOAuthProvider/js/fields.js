/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

var externalOAuthProviderFields = function () {

    var getDefaultFieldGetter = function () {
        return function (field$) {
            return (field$ && field$.val()) || '';
        };
    };

    var getFunctionFieldGetter = function () {
        return function (field$) {
            field$ = field$[0];
            if (!field$) {
                return '';
            }

            return field$.innerText || field$.textContent || '';
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

    var getFunctionFieldSetter = function () {
        return function (field$, value) {
            field$ = field$[0];
            if (!field$) {
                return;
            }

            field$.innerText = value || '';
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
            'name': {
                type: 'varchar'
            },
            'client_id': {
                type: 'varchar'
            },
            'client_secret': {
                type: 'varchar'
            },
            'redirect_uri_span': {
                type: 'function'
            },
            'redirect_uri_type': {
                type: 'enum'
            },
        },

        getters: {
            default: getDefaultFieldGetter(),
            varchar: getDefaultFieldGetter(),
            function: getFunctionFieldGetter(),
            checkbox: function (field$) {
                return (field$ && field$.prop('checked')) || false;
            }
        },

        setters: {
            default: getDefaultFieldSetter(),
            varchar: getDefaultFieldSetter(),
            function: getFunctionFieldSetter(),
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
            var type = this.getFieldType(field);
            return this.getters[type] || this.getters['default'];
        },

        getValueSetter: function (field) {
            var type = this.getFieldType(field);
            return this.setters[type] || this.setters['default'];
        }

    };
}();
