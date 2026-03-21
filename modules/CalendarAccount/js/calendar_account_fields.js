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

/* global SUGAR, alert, confirm, console, $ */

window.CalendarAccountFields = window.CalendarAccountFields || (function () {
    'use strict';

    /**
     * Get localized label
     * @private
     * @param {string} key - Language key
     * @returns {string} Localized label or key as fallback
     */
    function getLabel(key) {
        if (typeof SUGAR !== 'undefined' && SUGAR.language && SUGAR.language.get) {
            var translated = SUGAR.language.get('CalendarAccount', key);
            if (translated !== undefined && translated !== null) {
                return translated;
            }
        }

        return key;
    }

    var activeLoadingDialog = null;

    /**
     * Show YUI modal dialog for calendar account operations
     * @private
     * @param {string} title - Modal title
     * @param {string} content - HTML content to display in modal
     * @param {Object} [options] - Optional configuration
     * @param {boolean} [options.isLoading] - Show as loading modal (no close button, with spinner)
     * @returns {Object|boolean} - Dialog instance for loading modals, true/false otherwise
     */
    function showYUIModal(title, content, options) {
        options = options || {};
        var isLoading = options.isLoading || false;

        try {
            var dialogId = 'calendarModal_' + Date.now();

            var dialogDiv = document.createElement('div');
            dialogDiv.id = dialogId;
            document.body.appendChild(dialogDiv);

            var dialog = new window.YAHOO.widget.Dialog(dialogId, {
                width: '500px',
                fixedcenter: false,
                visible: false,
                modal: true,
                close: !isLoading,
                shadow: true
            });

            if (isLoading) {
                var keyframes = document.getElementById('calendarSyncSpinKeyframes');
                if (!keyframes) {
                    var style = document.createElement('style');
                    style.id = 'calendarSyncSpinKeyframes';
                    style.textContent = '@keyframes calendarSyncSpin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }';
                    document.head.appendChild(style);
                }

                var spinnerStyle = 'width:24px;height:24px;border:3px solid #f3f3f3;' +
                    'border-top:3px solid #c44;border-radius:50%;animation:calendarSyncSpin 1s linear infinite;' +
                    'flex-shrink:0;';

                content = '<div style="display:flex;align-items:center;gap:12px;">' +
                    '<div style="' + spinnerStyle + '"></div>' +
                    '<div style="font-size:14px;">' + (content || 'Loading...') + '</div>' +
                    '</div>';
            }

            dialog.setHeader(title || 'Notification');
            dialog.setBody('<div style="padding: 16px; font-size: 14px;">' + (content || '') + '</div>');

            dialog.render(document.body);

            if (dialog.innerElement && dialog.innerElement.style.width) {
                dialog.element.style.width = dialog.innerElement.style.width;
            }

            dialog.center();
            dialog.show();

            if (isLoading) {
                return dialog;
            }

            return true;
        } catch (e) {
            console.error('YUI Dialog error:', e);
            return isLoading ? null : false;
        }
    }

    /**
     * Show YUI confirmation dialog
     * @private
     * @param {string} title - Modal title
     * @param {string} message - Confirmation message
     * @param {function} onConfirm - Callback function when user confirms
     * @param {function} onCancel - Optional callback function when user cancels
     * @returns {boolean} - Success/failure of dialog creation
     */
    function showYUIConfirm(title, message, onConfirm, onCancel) {
        try {
            var dialogId = 'confirmModal_' + Date.now();

            var dialogDiv = document.createElement('div');
            dialogDiv.id = dialogId;
            document.body.appendChild(dialogDiv);

            var dialog = new window.YAHOO.widget.Dialog(dialogId, {
                width: '400px',
                fixedcenter: false,
                visible: false,
                modal: true,
                close: true,
                shadow: true,
                buttons: [
                    {
                        text: "OK",
                        handler: function () {
                            this.hide();
                            this.destroy();
                            onConfirm();
                        },
                        isDefault: true
                    },
                    {
                        text: "Cancel",
                        handler: function () {
                            this.hide();
                            this.destroy();
                            onCancel();
                        }
                    }
                ]
            });

            dialog.setHeader(title || 'Confirm');
            dialog.setBody('<div style="padding: 16px; font-size: 14px;">' + (message || 'Are you sure?') + '</div>');

            dialog.render(document.body);

            if (dialog.innerElement && dialog.innerElement.style.width) {
                dialog.element.style.width = dialog.innerElement.style.width;
            }

            dialog.center();
            dialog.show();

            return true;
        } catch (e) {
            console.error('YUI Confirm Dialog error:', e);
            var result = confirm(message);
            if (result) {
                onConfirm();
            } else {
                onCancel();
            }
            return false;
        }
    }

    return {
        getField$: function (field) {
            if (field === 'record') {
                return $('input[name=' + field + ']') || null;
            }

            var field$ = $('#' + field);
            if (field$.length > 0) {
                return field$;
            }

            field$ = $('#' + field + '_display');
            if (field$.length > 0) {
                return field$;
            }

            field$ = $('[name="' + field + '"]');
            if (field$.length > 0) {
                return field$;
            }

            return null;
        },

        getFieldValue: function (field) {
            var field$ = this.getField$(field);
            if (!field$) {
                return '';
            }

            var value = field$.val();
            if (value !== undefined && value !== null) {
                return value;
            }

            return field$.text().trim() || '';
        },

        setFieldValue: function (field, value) {
            var field$ = this.getField$(field);
            if (field$) {
                field$.val(value).change();
            }
        },

        toggleFieldRows: function (isEditMode, fields, show) {
            var self = this;
            fields.forEach(function (fieldName) {
                if (!isEditMode){
                    $('#' + fieldName + ', #' + fieldName + '_display, [name="' + fieldName + '"], [data-field="' + fieldName + '"], .detail-view-row-item[data-field="' + fieldName + '"]')[show ? 'show' : 'hide']();
                    return;
                }
                var field$ = self.getField$(fieldName);
                if (field$) {
                    var rowItem = field$.closest('.edit-view-row-item');
                    if (rowItem.length === 0) {
                        rowItem = field$.closest('td').parent('tr');
                        if (rowItem.length === 0) {
                            rowItem = field$.closest('.slot, .field');
                        }
                        if (rowItem.length === 0) {
                            rowItem = field$.parents('tr').first();
                        }
                    }

                    if (rowItem.length > 0) {
                        rowItem[show ? 'show' : 'hide']();
                    } else {
                        field$[show ? 'show' : 'hide']();
                    }
                }
            });
        },

        setFieldsRequired: function (fields, required) {
            var self = this;
            fields.forEach(function (fieldName) {
                var field$ = self.getField$(fieldName);
                if (field$ && required) {
                    field$.attr('required', 'required');
                } else if (field$) {
                    field$.removeAttr('required');
                }
            });
        },

        /**
         * Show popup message using YUI modal or alert fallback
         * @param {string} message - Message to display in popup
         * @param {string} [type='alert'] - Message type: 'alert', 'success', 'error', 'warning'
         * @param {string} [title] - Optional custom title
         */
        showPopupMessage: function (message, type, title) {
            title = title || getLabel('LBL_NOTIFICATION');

            if (typeof window.YAHOO === 'undefined' || !window.YAHOO || !window.YAHOO.widget || !window.YAHOO.widget.Dialog) {
                alert(title + '\n\n' + message);

                return;
            }

            showYUIModal(title, message);
        },

        /**
         * Show confirmation dialog using YUI modal or confirm fallback
         * @param {string} title - Dialog title
         * @param {string} message - Confirmation message
         * @param {function} onConfirm - Callback function when user confirms
         * @param {function} [onCancel] - Optional callback function when user cancels
         */
        showConfirmDialog: function (title, message, onConfirm, onCancel) {
            title = title || getLabel('LBL_CONFIRM');
            message = message || getLabel('LBL_ARE_YOU_SURE');
            onConfirm = onConfirm || function () {
            };
            onCancel = onCancel || function () {
            };

            if (typeof window.YAHOO === 'undefined' || !window.YAHOO || !window.YAHOO.widget || !window.YAHOO.widget.Dialog) {
                var result = confirm(message);
                if (result) {
                    onConfirm();
                } else {
                    onCancel();
                }

                return;
            }

            showYUIConfirm(title, message, onConfirm, onCancel);
        },

        /**
         * Get localized label
         * @param {string} key - Language key
         * @returns {string} Localized label or key as fallback
         */
        getLabel: getLabel,

        /**
         * Show loading modal with spinner
         * @param {string} [title] - Modal title
         * @param {string} [message] - Loading message
         */
        showLoadingModal: function (title, message) {
            if (activeLoadingDialog) {
                return;
            }

            title = title || getLabel('LBL_PLEASE_WAIT');
            message = message || getLabel('LBL_SYNCING');

            if (typeof window.YAHOO === 'undefined' || !window.YAHOO || !window.YAHOO.widget || !window.YAHOO.widget.Dialog) {
                return;
            }

            activeLoadingDialog = showYUIModal(title, message, { isLoading: true });
        },

        /**
         * Hide active loading modal
         */
        hideLoadingModal: function () {
            if (activeLoadingDialog) {
                try {
                    activeLoadingDialog.hide();
                    activeLoadingDialog.destroy();
                } catch (e) {
                    console.error('Error hiding loading modal:', e);
                }
                activeLoadingDialog = null;
            }
        }
    };
})();