/*
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

/* global $, console, window, SUGAR */
// Depends on calendar_account_fields.js

/**
 * @namespace CalendarAuthVisibility
 * @description Calendar authentication field management with safe multiple script loading
 */
window.CalendarAuthVisibility = window.CalendarAuthVisibility || (function () {
    'use strict';

    var fieldMappings = {
        'oauth2': {fields: ['oauth_connection_name']},
        'basic': {fields: ['username', 'password', 'server_url']},
        'api_key': {fields: ['api_key', 'api_endpoint']}
    };

    var allAuthFields = [
        'oauth_connection_name', 'username', 'password', 'server_url',
        'api_key', 'api_endpoint'
    ];

    /**
     * @private
     * @param {string} sourceValue
     * @param {Function} callback
     */
    function fetchAuthMethodAndExecute(sourceValue, callback) {
        $.ajax({
            url: 'index.php?module=CalendarAccount&action=getProviderAuthMethod',
            data: { source: sourceValue },
            dataType: 'json',
            success: function (response) {
                if (response.status === 'success') {
                    callback(response.data.auth_method);
                } else {
                    console.error('Error getting auth method:', response.message);
                }
            },
            error: function () {
                console.error('AJAX error getting auth method for source:', sourceValue);
            }
        });
    }

    /**
     * @private
     * @param {string} authMethod
     */
    function showFieldsForAuthMethod(authMethod) {
        var mapping = fieldMappings[authMethod];
        if (!mapping) {
            console.warn('No field mapping found for auth method:', authMethod);
            return;
        }

        if (mapping.fields) {
            window.CalendarAccountFields.toggleFieldRows(isEditMode, mapping.fields, true);
        }

        $('#test-connection-btn').show();
        updateRequiredFields(authMethod);
    }

    /**
     * @private
     * @param {string} authMethod
     */
    function updateRequiredFields(authMethod) {
        $('.auth-field input, .auth-field select').removeAttr('required');

        var mapping = fieldMappings[authMethod];
        if (mapping && mapping.fields) {
            window.CalendarAccountFields.setFieldsRequired(mapping.fields, true);
        }
    }

    var isEditMode = false;

    /**
     * @private
     * @returns {boolean} True if current user is admin
     */
    function isAdmin() {
        return typeof SUGAR !== 'undefined' && SUGAR.userIsAdmin === true;
    }

    return {
        /**
         * @public
         * @param {boolean} editMode - true for edit, false for detail
         */
        setMode: function (editMode) {
            isEditMode = editMode;
        },

        /**
         * @public
         * @returns {boolean}
         */
        getMode: function () {
            return isEditMode;
        },

        /**
         * @public
         * @param {string} sourceValue
         * @param {boolean} isUserChange - true if triggered by user interaction, false on the initial load
         */
        onSourceChange: function (sourceValue, isUserChange) {
            if (isUserChange) {
                this.clearAllAuthFieldValues();
            }
            this.hideAllAuthFields();
            if (sourceValue) {
                fetchAuthMethodAndExecute(sourceValue, showFieldsForAuthMethod);
            }
        },

        /**
         * @public
         * @description Clear all auth field values to prevent stale data on provider switch
         */
        clearAllAuthFieldValues: function () {
            allAuthFields.forEach(function (fieldName) {
                window.CalendarAccountFields.setFieldValue(fieldName, '');

                var field$ = window.CalendarAccountFields.getField$(fieldName);
                if (field$ && field$.attr('data-is-value-set')) {
                    field$.attr('data-is-value-set', 'false');
                    field$.attr('placeholder', '');
                }

                var idField = fieldName.replace('_name', '_id');
                if (idField !== fieldName) {
                    window.CalendarAccountFields.setFieldValue(idField, '');
                }
            });
        },

        /**
         * @public
         */
        hideAllAuthFields: function () {
            window.CalendarAccountFields.toggleFieldRows(this.getMode(), allAuthFields, false);
            $('.auth-button').hide();
        },

        /**
         * @public
         * @param {string} fieldName
         * @returns {string}
         */
        getFieldValue: function (fieldName) {
            return window.CalendarAccountFields.getFieldValue(fieldName);
        },

        /**
         * @public
         * @description Handle calendar user field permissions based on admin status
         */
        handleCalendarUserField: function () {
            var adminStatus = isAdmin();
            var field = document.getElementById('calendar_user_name');
            var buttons = [document.getElementById('btn_calendar_user_name'), document.getElementById('btn_clr_calendar_user_name')];

            if (field) {
                if (adminStatus) {
                    field.removeAttribute('readonly');
                    field.removeAttribute('title');
                    field.classList.remove('readonly-field');
                } else {
                    field.setAttribute('readonly', 'readonly');
                    field.setAttribute('title', 'Only administrators can change the Calendar User');
                    field.classList.add('readonly-field');
                }
            }

            buttons.forEach(function(btn) {
                if (!btn) return;
                btn.toggleAttribute('disabled', !adminStatus);
                btn.style.display = adminStatus ? '' : 'none';
            });
        }
    };
})();