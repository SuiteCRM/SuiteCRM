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

/* jshint esversion: 6 */
/* global console, $, convertDbTimestampToUserTZ */
// Depends on calendar_account_fields.js

/**
 * @namespace CalendarSync
 * @description Calendar synchronization functionality with safe multiple script loading
 */
window.CalendarSync = window.CalendarSync || (function () {
    'use strict';

    if (typeof window.CalendarAccountFields === 'undefined') {
        console.error('CalendarSync requires CalendarAccountFields to be loaded first');
        return {};
    }

    var activeSync = null;
    var buttonId = 'sync_now_button';

    /**
     * @private
     * @param {string} key - Language key
     * @returns {string} Localized label or key as fallback
     */
    function getLabel(key) {
        return window.CalendarAccountFields.getLabel(key);
    }

    /**
     * @private
     * @param {string} message - Message to display in popup
     * @param {string} [type='alert'] - Message type: 'alert', 'success', 'error', 'warning'
     * @param {string} [title] - Optional custom title
     */
    function showPopupMessage(message, type, title) {
        window.CalendarAccountFields.showPopupMessage(message, type, title);
    }

    /**
     * @private
     * @param {string} title - Dialog title
     * @param {string} message - Confirmation message
     * @param {function} onConfirm - Callback function when user confirms
     * @param {function} [onCancel] - Optional callback function when user cancels
     */
    function showConfirmDialog(title, message, onConfirm, onCancel) {
        window.CalendarAccountFields.showConfirmDialog(title, message, onConfirm, onCancel);
    }

    /**
     * @private
     * @param {string} [title] - Modal title
     * @param {string} [message] - Loading message
     */
    function showLoadingModal(title, message) {
        window.CalendarAccountFields.showLoadingModal(title, message);
    }

    /**
     * @private
     */
    function hideLoadingModal() {
        window.CalendarAccountFields.hideLoadingModal();
    }

    /**
     * @private
     * @param {boolean} isLoading - Whether sync is in progress
     */
    function setSyncButtonState(isLoading) {
        var button = document.getElementById(buttonId);
        if (!button) {
            return;
        }

        button.disabled = isLoading;
        button.value = isLoading ? getLabel('LBL_SYNCING') : getLabel('LBL_SYNC_NOW');
    }

    /**
     * @private
     * @param {XMLHttpRequest} xhr - The XMLHttpRequest object
     */
    function handleSyncResponse(xhr) {
        try {
            var response = JSON.parse(xhr.responseText);

            var isSuccess = xhr.status === 200 && response.status === 'success';
            showResponseMessage(isSuccess ? response.data : response.message, isSuccess);

            if (isSuccess) {
                setTimeout(function() {
                    window.location.reload();
                }, 1500);
                return;
            }
        } catch (e) {
            showPopupMessage(getLabel('LBL_SYNC_RESPONSE_ERROR'));
            console.error('CalendarAccount sync response parse error:', e, xhr.responseText);
        }

        setSyncButtonState(false);
        activeSync = null;
    }

    /**
     * @private
     * @param {Object} data - Response data object
     * @param {boolean} isSuccess - Whether this is a success or error message
     */
    function showResponseMessage(data, isSuccess) {
        if (isSuccess) {
            showPopupMessage(getLabel('LBL_SYNC_SUCCESS'));
        } else {
            var errorMessage = data || getLabel('LBL_SYNC_FAILED_DEFAULT');
            showPopupMessage(getLabel('LBL_SYNC_FAILED') + ': ' + errorMessage);
        }
    }

    /**
     * @private
     * @param {string} recordId - Calendar account record ID
     */
    function performSync(recordId) {
        if (activeSync) {
            showPopupMessage(getLabel('LBL_SYNC_IN_PROGRESS'));
            return;
        }

        var xhr = new XMLHttpRequest();
        xhr.open('POST', 'index.php', true);
        xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');

        activeSync = xhr;
        showLoadingModal(getLabel('LBL_SYNC_NOW'), getLabel('LBL_SYNC_IN_PROGRESS_MESSAGE'));

        xhr.onreadystatechange = function () {
            if (xhr.readyState === 4) {
                hideLoadingModal();
                handleSyncResponse(xhr);
            }
        };

        xhr.onerror = function () {
            hideLoadingModal();
            showPopupMessage(getLabel('LBL_NETWORK_ERROR'));
            setSyncButtonState(false);
            activeSync = null;
        };

        xhr.send('module=CalendarAccount&action=syncCalendarAccount&record=' + encodeURIComponent(recordId));
    }

    /**
     * @private
     * @param {string} authMethod
     * @param {string} source
     */
    function testConnectionByAuthMethod(authMethod, source) {
        var authMethodConfigs = {
            oauth2: {
                requiredFields: ['oauth_connection_id'],
                errorMessage: getLabel('LBL_OAUTH_CONNECTION_REQUIRED'),
                collectData: function () {
                    return {
                        oauth_connection_id: getFieldValue('oauth_connection_id')
                    };
                }
            },
            basic: {
                requiredFields: ['username', 'server_url'],
                errorMessage: getLabel('LBL_BASIC_AUTH_FIELDS_REQUIRED'),
                collectData: function () {
                    return {
                        username: getFieldValue('username'),
                        password: getFieldValue('password'),
                        server_url: getFieldValue('server_url')
                    };
                }
            },
            api_key: {
                requiredFields: ['api_key'],
                errorMessage: getLabel('LBL_API_KEY_REQUIRED'),
                validateFields: function () {
                    var calendarAccountId = getCurrentAccountId();
                    var apiKeyField = getFieldValue('api_key');
                    var hasApiKeyStored = $('#api_key').attr('data-is-value-set') === 'true';

                    if (calendarAccountId && hasApiKeyStored && (!apiKeyField || apiKeyField.trim() === '')) {
                        return {isValid: true, message: getLabel('LBL_USING_SAVED_API_KEY')};
                    }

                    if (!apiKeyField || apiKeyField.trim() === '') {
                        return {isValid: false, message: this.errorMessage};
                    }

                    return {isValid: true};
                },
                collectData: function () {
                    return {
                        api_key: getFieldValue('api_key'),
                        api_endpoint: getFieldValue('api_endpoint')
                    };
                }
            }
        };

        var config = authMethodConfigs[authMethod];
        if (!config) {
            showPopupMessage(getLabel('LBL_UNKNOWN_AUTH_METHOD') + ': ' + authMethod);
            return;
        }

        if (typeof config.validateFields === 'function') {
            var validationResult = config.validateFields();
            if (!validationResult.isValid) {
                showPopupMessage(validationResult.message);
                return;
            }
        } else {
            var missingFields = config.requiredFields.filter(function (fieldName) {
                var value = getFieldValue(fieldName);
                return !value || value.trim() === '';
            });

            if (missingFields.length > 0) {
                showPopupMessage(config.errorMessage);
                return;
            }
        }

        var authData = config.collectData();
        var calendarAccountId = getCurrentAccountId();

        var data = 'module=CalendarAccount&action=testConnection&source=' + encodeURIComponent(source);

        Object.keys(authData).forEach(function (key) {
            data += '&' + encodeURIComponent(key) + '=' + encodeURIComponent(authData[key]);
        });

        if (calendarAccountId) {
            data += '&calendar_account_id=' + encodeURIComponent(calendarAccountId);
        }

        performConnectionTest(data);
    }

    /**
     * @private
     * @param {string} fieldName
     * @returns {string}
     */
    function getFieldValue(fieldName) {
        return window.CalendarAccountFields.getFieldValue(fieldName);
    }

    /**
     * @private
     * @returns {string|null}
     */
    function getCurrentAccountId() {
        var recordFromUrl = new URLSearchParams(window.location.search).get('record');
        if (recordFromUrl) {
            return recordFromUrl;
        }

        var recordFromField = getFieldValue('record');
        if (recordFromField) {
            return recordFromField;
        }

        return null;
    }


    /**
     * @private
     * @param {string} message
     * @param {string} source
     * @param {boolean} isSuccess
     */
    function showConnectionMessage(message, source, isSuccess) {
        var title = getLabel('LBL_CONNECTION_TEST') + ' ' + (isSuccess ? getLabel('LBL_SUCCESSFUL') : getLabel('LBL_FAILED'));
        var type = isSuccess ? 'alert' : 'alert';

        showPopupMessage(message, type, title);
    }

    /**
     * @private
     * @param {number} status
     * @param {string|null} timestamp - Server-formatted timestamp or null to clear timestamp
     * @param {string|null} externalCalendarId - External calendar ID from provider or null to clear
     */
    function updateConnectionStatusFields(status, timestamp, externalCalendarId) {
        var statusField = $('#last_connection_status');
        if (statusField.length && statusField.is('span')) {
            statusField.text(status === 1 ? '1' : '0');
        }

        var statusInput = $('input[name="last_connection_status"]');
        if (statusInput.length) {
            statusInput.val(status);
        } else {
            $('<input>').attr({
                type: 'hidden',
                name: 'last_connection_status',
                value: status
            }).appendTo('form[name="EditView"]');
        }

        var testField = $('#last_connection_test');
        if (testField.length && testField.is('span')) {
            testField.text(convertDbTimestampToUserTZ(timestamp || ''));
        }

        var testInput = $('input[name="last_connection_test"]');
        if (testInput.length) {
            testInput.val(timestamp || '');
        } else {
            $('<input>').attr({
                type: 'hidden',
                name: 'last_connection_test',
                value: timestamp || ''
            }).appendTo('form[name="EditView"]');
        }

        var externalIdInput = $('input[name="external_calendar_id"]');
        if (externalIdInput.length) {
            externalIdInput.val(externalCalendarId || '');
        } else {
            $('<input>').attr({
                type: 'hidden',
                name: 'external_calendar_id',
                value: externalCalendarId || ''
            }).appendTo('form[name="EditView"]');
        }

        if (typeof translateEnumFields === 'function') {
            translateEnumFields();
        }
    }

    /**
     * @private
     * @param {string} data
     */
    function performConnectionTest(data) {
        var xhr = new XMLHttpRequest();
        xhr.open('POST', 'index.php', true);
        xhr.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');

        xhr.onreadystatechange = function () {
            if (xhr.readyState !== 4) {
                return;
            }
            var message = null;
            if (xhr.status !== 200) {
                message = getLabel('LBL_CONNECTION_TEST_HTTP_ERROR') + ': ' + xhr.status;
                try {
                    var errorResponse = JSON.parse(xhr.responseText);
                    if (errorResponse.message) {
                        message = errorResponse.message;
                    }
                } catch (e) {
                }
                showConnectionMessage(message, getLabel('LBL_CALENDAR'), false);
                updateConnectionStatusFields(0, null, null);
                return;
            }
            try {
                var response = JSON.parse(xhr.responseText);
                var isSuccess = response.status === 'success';
                var timestamp = response.data && response.data.test_timestamp ? response.data.test_timestamp : null;
                var externalCalendarId = response.data && response.data.external_calendar_id ? response.data.external_calendar_id : null;
                var duplicateAccount = response.data && response.data.duplicate_account ? response.data.duplicate_account : null;

                if (duplicateAccount) {
                    message = getLabel('LBL_DUPLICATE_CALENDAR_ERROR') + ': "' + duplicateAccount.account_name + '"';
                    showConnectionMessage(message, response.data ? response.data.source : 'Calendar', false);
                    updateConnectionStatusFields(0, null, null);
                    return;
                }

                message = isSuccess ?
                    getLabel('LBL_CONNECTION_TEST_SUCCESS_MESSAGE') :
                    response.message;
                showConnectionMessage(message, response.data ? response.data.source : 'Calendar', isSuccess);
                updateConnectionStatusFields(isSuccess ? 1 : 0, timestamp, externalCalendarId);
            } catch (e) {
                showPopupMessage(getLabel('LBL_CONNECTION_TEST_RESPONSE_ERROR'));
                updateConnectionStatusFields(0, null, null);
            }
        };

        xhr.onerror = function () {
            showConnectionMessage(getLabel('LBL_CONNECTION_NETWORK_ERROR'), getLabel('LBL_CALENDAR'), false);
            updateConnectionStatusFields(0, null, null);
        };

        xhr.send(data);
    }

    return {
        /**
         * @public
         * @param {string} recordId - Calendar account record ID
         * @param {string} connectionStatus - Connection test status ("1" for successful, "0" for failed)
         * @returns {boolean} True if sync started, false otherwise
         */
        sync: function (recordId, connectionStatus) {
            if (!recordId) {
                showPopupMessage(getLabel('LBL_NO_ACCOUNT_ID'));
                return false;
            }

            if (activeSync) {
                showPopupMessage(getLabel('LBL_SYNC_IN_PROGRESS'));
                return false;
            }

            showConfirmDialog(
                getLabel('LBL_SYNC_NOW'),
                getLabel('LBL_SYNC_NOW_CONFIRM'),
                function () {
                    setSyncButtonState(true);
                    performSync(recordId);
                }
            );
            return true;
        },

        /**
         * @public
         * @description Tests the connection for the selected calendar source
         */
        testConnection: function () {
            var source = getFieldValue('source');

            if (!source) {
                showPopupMessage(getLabel('LBL_SELECT_CALENDAR_SOURCE_FIRST'));
                return;
            }

            var xhr = new XMLHttpRequest();
            xhr.open('GET', 'index.php?module=CalendarAccount&action=getProviderAuthMethod&source=' + encodeURIComponent(source), true);

            xhr.onreadystatechange = function () {
                if (xhr.readyState !== 4) {
                    return;
                }

                if (xhr.status !== 200) {
                    showPopupMessage(getLabel('LBL_AUTH_METHOD_GET_ERROR') + ': ' + source);
                    return;
                }

                try {
                    var response = JSON.parse(xhr.responseText);
                    if (response.status === 'success') {
                        testConnectionByAuthMethod(response.data.auth_method, source);
                    } else {
                        showPopupMessage(getLabel('LBL_AUTH_METHOD_ERROR') + ': ' + response.message);
                    }
                } catch (e) {
                    showPopupMessage(getLabel('LBL_AUTH_METHOD_PARSE_ERROR'));
                }
            };

            xhr.send();
        }
    };

})();
