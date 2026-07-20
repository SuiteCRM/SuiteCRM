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

var WHATSAPP_HELPER = 'whatsapp';
var sticMessagesWindowChecked = false;
var sticMessagesWindowOpen = false;

function sticMessagesGetTemplateRequiredMsg() {
    return SUGAR.language.get('stic_Messages', 'LBL_WHATSAPP_TEMPLATE_REQUIRED') || SUGAR.language.get('stic_Messages', 'LBL_WHATSAPP_TEMPLATE_REQUIRED');
}

function sticMessagesGetWindowClosedAlertMsg() {
    return SUGAR.language.get('stic_Messages', 'LBL_WHATSAPP_WINDOW_CLOSED_ALERT') || SUGAR.language.get('stic_Messages', 'LBL_WHATSAPP_WINDOW_CLOSED_ALERT');
}

function sticMessagesCheckWhatsAppWindow(parentId, parentType) {
    var typeEl = document.getElementById('type');
    var msgEl = document.getElementById('message');

    if (!typeEl) return;

    var typeVal = typeEl.value || '';

    if (typeVal !== WHATSAPP_HELPER) {
        sticMessagesWindowChecked = true;
        sticMessagesWindowOpen = true;
        sticMessagesRemoveWhatsAppWarning();
        if (msgEl) msgEl.disabled = false;
        return;
    }

    // In ComposeView (mass send), there is no single parent - WhatsApp always requires template
    if (!parentId || !parentType) {
        sticMessagesWindowChecked = true;
        sticMessagesWindowOpen = false;
        sticMessagesDisableMessageField();
        sticMessagesShowWhatsAppWarning(sticMessagesGetTemplateRequiredMsg());
        return;
    }

    fetch('index.php?module=stic_Messages&action=checkWhatsAppWindow&parent_id=' + encodeURIComponent(parentId) + '&parent_type=' + encodeURIComponent(parentType))
        .then(function(r) { return r.json(); })
        .then(function(data) {
            sticMessagesWindowChecked = true;
            var msgEl = document.getElementById('message');
            if (data.success && data.windowOpen) {
                sticMessagesWindowOpen = true;
                if (msgEl) msgEl.disabled = false;
                sticMessagesRemoveWhatsAppWarning();
            } else {
                sticMessagesWindowOpen = false;
                sticMessagesDisableMessageField();
                sticMessagesShowWhatsAppWarning(sticMessagesGetTemplateRequiredMsg());
            }
        })
        .catch(function() {
            sticMessagesWindowChecked = true;
            sticMessagesWindowOpen = false;
            sticMessagesDisableMessageField();
            sticMessagesShowWhatsAppWarning(sticMessagesGetTemplateRequiredMsg());
        });
}

function sticMessagesDisableMessageField() {
    var msgEl = document.getElementById('message');
    if (msgEl) {
        msgEl.disabled = true;
        msgEl.value = '';
    }
}

function sticMessagesCheckTemplateSelection() {
    var typeEl = document.getElementById('type');
    var templateEl = document.getElementById('template_id');
    var msgEl = document.getElementById('message');

    if (!typeEl || typeEl.value !== WHATSAPP_HELPER) {
        return;
    }

    var hasTemplate = templateEl && templateEl.value && templateEl.value !== '';

    if (hasTemplate) {
        // Template selected - remove warning and enable message field
        sticMessagesRemoveWhatsAppWarning();
        if (msgEl) msgEl.disabled = false;
    } else if (!sticMessagesWindowOpen) {
        // No template and window closed - show warning and disable field
        sticMessagesDisableMessageField();
        sticMessagesShowWhatsAppWarning(sticMessagesGetTemplateRequiredMsg());
    }
}

function sticMessagesRemoveWhatsAppWarning() {
    var existing = document.querySelector('.whatsapp-window-warning');
    if (existing) existing.remove();
    // Also remove SuiteCRM validation style error
    var existingError = document.querySelector('#message_error');
    if (existingError) existingError.remove();
}

function sticMessagesShowWhatsAppWarning(msg) {
    sticMessagesRemoveWhatsAppWarning();

    // Find the message field and add error after it (SuiteCRM validation style)
    var msgEl = document.getElementById('message');
    if (msgEl) {
        // Remove any existing error for message field
        var existingFieldError = document.querySelector('#message_error');
        if (existingFieldError) existingFieldError.remove();

        // Create error div in SuiteCRM validation style (text only, like "Required field" errors)
        var errorDiv = document.createElement('div');
        errorDiv.id = 'message_error';
        errorDiv.className = 'whatsapp-window-warning';
        errorDiv.setAttribute('aria-live', 'polite');
        errorDiv.style.cssText = 'color:#FF0000;font-size:13px;margin-top:5px;';
        errorDiv.textContent = msg;

        // Insert after the message field's container (usually in a td)
        var cell = msgEl.closest('td');
        if (cell) {
            cell.appendChild(errorDiv);
        } else {
            msgEl.parentNode.insertBefore(errorDiv, msgEl.nextSibling);
        }
    }
}

function sticMessagesInitWhatsAppWindowCheck(parentId, parentType) {
    var typeEl = document.getElementById('type');
    var templateEl = document.getElementById('template_id');

    if (typeEl) {
        typeEl.removeEventListener('change', sticMessagesCheckWhatsAppWindowBound);
        sticMessagesCheckWhatsAppWindowBound = sticMessagesCheckWhatsAppWindow.bind(null, parentId, parentType);
        typeEl.addEventListener('change', sticMessagesCheckWhatsAppWindowBound);
    }

    // Also listen for template changes
    if (templateEl) {
        templateEl.addEventListener('change', sticMessagesCheckTemplateSelection);
    }

    sticMessagesCheckWhatsAppWindow(parentId, parentType);
}

var sticMessagesCheckWhatsAppWindowBound = null;

function sticMessagesInitOnLoad() {
    if (typeof sticMessagesParentId !== 'undefined' && typeof sticMessagesParentType !== 'undefined') {
        var typeEl = document.getElementById('type');
        if (typeEl) {
            sticMessagesInitWhatsAppWindowCheck(sticMessagesParentId, sticMessagesParentType);
            return true;
        }
    }
    return false;
}

// Try immediately in case DOM is already ready (popup case)
if (!sticMessagesInitOnLoad()) {
    // Retry with interval for popup cases where elements load later
    var attempts = 0;
    var interval = setInterval(function() {
        if (sticMessagesInitOnLoad() || attempts >= 20) {
            clearInterval(interval);
        }
        attempts++;
    }, 100);
}

// Validation function for WhatsApp messages
function sticMessagesValidateWhatsApp() {
    var typeEl = document.getElementById('type');
    var templateEl = document.getElementById('template_id');

    if (!typeEl || typeEl.value !== WHATSAPP_HELPER) {
        return true;
    }

    if (!sticMessagesWindowOpen) {
        if (!templateEl || !templateEl.value || templateEl.value === '') {
            alert(sticMessagesGetWindowClosedAlertMsg());
            return false;
        }
    }

    return true;
}

// Hook into EditView form - wait for saveMessage to be defined (it may be in a jQuery ready handler)
function sticMessagesHookSaveMessage() {
    if (typeof saveMessage !== 'undefined') {
        var originalSaveMessage = saveMessage;
        saveMessage = function(event) {
            if (!sticMessagesValidateWhatsApp()) {
                event.preventDefault();
                return false;
            }
            return originalSaveMessage.call(this, event);
        };
        return true;
    }
    return false;
}

// Try immediately and also after DOM ready
if (!sticMessagesHookSaveMessage()) {
    document.addEventListener('DOMContentLoaded', function() {
        if (!sticMessagesHookSaveMessage()) {
            // Retry a few times with interval for jQuery ready handlers
            var attempts = 0;
            var interval = setInterval(function() {
                if (sticMessagesHookSaveMessage() || attempts >= 10) {
                    clearInterval(interval);
                }
                attempts++;
            }, 100);
        }
    });
}