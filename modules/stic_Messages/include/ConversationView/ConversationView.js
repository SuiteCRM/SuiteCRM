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

/**
 * Opens a WhatsApp conversation window for a given record.
 * Can be called from any module included in stic_Messages messageable modules.
 *
 * @param {String} recordId   The bean id
 * @param {String} parentType The module name (Contacts, Leads, Accounts, Employees...)
 */
function openWhatsAppConversation(recordId, parentType) {
    var parentName = $("#formDetailView input[type=hidden][name=full_name]").val()
                  || $("h1.module-title-text").text().trim()
                  || '';
    var url = 'index.php?module=stic_Messages&action=conversation'
            + '&parent_id='   + encodeURIComponent(recordId)
            + '&parent_type=' + encodeURIComponent(parentType)
            + '&parent_name=' + encodeURIComponent(parentName);
    window.open(
        url,
        'whatsapp_conv_' + recordId,
        'width=480,height=700,resizable=yes,scrollbars=yes'
    );
}

/* ── Conversation View (popup) ── */

(function() {
    var waBody = document.getElementById('waBody');
    if (waBody) {
        waBody.scrollTop = 9999999;
    }
    var clearNew = function() {
        var bubbles = document.querySelectorAll('.bubble-new');
        for (var i = 0; i < bubbles.length; i++) {
            bubbles[i].classList.remove('bubble-new');
        }
    };
    document.addEventListener('click', function(e) {
        if (e.target.closest('.wa-body') || e.target.closest('.wa-footer')) {
            clearNew();
        }
    });
})();

var _pendingMediaNoteId = null;
var _pendingMediaName = null;
var _pendingMediaMime = null;

function handleFileSelected(input) {
    if (!input.files || !input.files[0]) return;
    var file = input.files[0];

    document.getElementById('uploadingIndicator').style.display = 'block';
    document.getElementById('attachmentPreview').style.display = 'none';

    var formData = new FormData();
    formData.append('module', 'stic_Messages');
    formData.append('action', 'uploadConversationMedia');
    formData.append('media',  file);

    fetch('index.php', {
        method: 'POST',
        body: formData,
        credentials: 'same-origin'
    })
    .then(function(r) { return r.json(); })
    .then(function(data) {
        document.getElementById('uploadingIndicator').style.display = 'none';
        if (!data.success) {
            alert(SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_UPLOAD') + ': ' + (data.error || SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_UNKNOWN')));
            input.value = '';
            return;
        }
        _pendingMediaNoteId = data.media_note_id;
        _pendingMediaName = data.name;
        _pendingMediaMime = data.mime;
        showAttachmentPreview(file, data.name);
    })
    .catch(function(err) {
        document.getElementById('uploadingIndicator').style.display = 'none';
        alert(SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_UPLOAD') + ': ' + err);
        input.value = '';
    });
}

function showAttachmentPreview(file, name) {
    var preview = document.getElementById('attachmentPreview');
    var img = document.getElementById('previewImg');
    var icon = document.getElementById('previewIcon');

    document.getElementById('previewName').textContent = name;

    if (file.type.startsWith('image/')) {
        var reader = new FileReader();
        reader.onload = function(e) {
            img.src = e.target.result;
            img.style.display = 'block';
            icon.style.display = 'none';
        };
        reader.readAsDataURL(file);
    } else {
        img.style.display = 'none';
        icon.style.display = 'block';
    }

    preview.style.display = 'flex';
}

function removeAttachment() {
    _pendingMediaNoteId = null;
    _pendingMediaName = null;
    _pendingMediaMime = null;
    document.getElementById('mediaFile').value = '';
    document.getElementById('attachmentPreview').style.display = 'none';
    document.getElementById('previewImg').src = '';
}

function sendMessage() {
    var text = document.getElementById('msgText').value.trim();
    var sendBtn = document.getElementById('sendBtn');

    if (!text && !_pendingMediaNoteId) {
        return;
    }

    sendBtn.disabled = true;

    var formData = new FormData();
    formData.append('module',      'stic_Messages');
    formData.append('action',      'Save');
    formData.append('type',        'whatsapp');
    formData.append('status',      'sent');
    formData.append('message',     text);
    formData.append('parent_type', CONVERSATION.parentType);
    formData.append('parent_id',   CONVERSATION.parentId);

    if (_pendingMediaNoteId) {
        formData.append('media_note_id',   _pendingMediaNoteId);
        formData.append('media_note_name',  _pendingMediaName);
        formData.append('media_note_mime',  _pendingMediaMime);
    }

    fetch('index.php', {
        method: 'POST',
        body: formData,
        credentials: 'same-origin'
    })
    .then(function(r) { return r.text(); })
    .then(function() { location.reload(); })
    .catch(function(err) {
        alert(SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_SEND') + ': ' + err);
        sendBtn.disabled = false;
    });
}

/* ── Live polling ── */

var _pollLastDate = CONVERSATION.lastDate || '';
var _pollInterval = null;
var _pollDelay = CONVERSATION.pollDelay || 5000; // 5 seconds default
var _pollEmptyCount = 0;
var _pollMaxDelay = 30000; // 30 seconds max with backoff

function pollNewMessages() {
    // Skip if tab is not visible (background tab or minimized)
    if (document.hidden) return;
    var waBody = document.getElementById('waBody');
    if (!waBody || waBody.offsetParent === null || waBody.getBoundingClientRect().height === 0) return;

    var url = 'index.php?entryPoint=sticConversationMessages'
            + '&parent_id='   + encodeURIComponent(CONVERSATION.parentId)
            + '&parent_type=' + encodeURIComponent(CONVERSATION.parentType)
            + '&last_date='   + encodeURIComponent(_pollLastDate);

    fetch(url, { credentials: 'same-origin' })
    .then(function(r) { return r.json(); })
    .then(function(data) {
        if (!data.success || !data.messages || data.messages.length === 0) {
            _pollEmptyCount++;
            applyBackoff();
            return;
        }
        if (!waBody) return;
        _pollEmptyCount = 0;
        resetPollDelay();

        var wasAtBottom = waBody.scrollTop + waBody.clientHeight >= waBody.scrollHeight - 50;

        data.messages.forEach(function(msg) {
            appendMessage(msg, true);
            _pollLastDate = msg.date_entered;
        });

        if (wasAtBottom) {
            waBody.scrollTop = waBody.scrollHeight;
        }
    })
    .catch(function(err) {
        console.error('[ConversationView] Poll error:', err);
    });
}

function applyBackoff() {
    if (_pollEmptyCount > 3) {
        stopPolling();
        var backoffDelay = Math.min(_pollDelay * 2, _pollMaxDelay);
        _pollInterval = setInterval(pollNewMessages, backoffDelay);
    }
}

function resetPollDelay() {
    stopPolling();
    _pollInterval = setInterval(pollNewMessages, _pollDelay);
}

function startPolling() {
    if (_pollInterval) return;
    console.log('[ConversationView] Poll started');
    // Stagger initial poll to avoid all conversations firing at once
    var stagger = Math.random() * _pollDelay;
    setTimeout(function() {
        _pollInterval = setInterval(pollNewMessages, _pollDelay);
    }, stagger);
}

function appendMessage(msg, isNew) {
    var waBody = document.getElementById('waBody');
    var direction = (msg.direction || 'outbound').toLowerCase();
    var status = (msg.status || 'sent').toLowerCase();
    var isOut = direction === 'outbound' || direction === 'out';
    var isError = status === 'error';

    var bubble = document.createElement('div');
    bubble.className = 'bubble ' + (isError ? 'error' : (isOut ? 'out' : 'in'));
    if (isNew) {
        bubble.className += ' bubble-new';
    } else {
        bubble.style.animation = 'fadeIn 0.3s ease-in';
    }

    var textDiv = document.createElement('div');
    textDiv.className = 'text';
    textDiv.innerHTML = (msg.message || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/\n/g, '<br>');
    bubble.appendChild(textDiv);

    // Attachments
    if (msg.notes && msg.notes.length > 0) {
        msg.notes.forEach(function(note) {
            var attachDiv = document.createElement('div');
            attachDiv.className = 'bubble-attachment';

            var isImage = note.file_mime_type && note.file_mime_type.indexOf('image/') === 0;
            if (isImage) {
                var img = document.createElement('img');
                img.className = 'attachment-bubble-img';
                img.src = 'upload/' + note.id;
                img.onclick = function() { window.open('index.php?module=Notes&action=DetailView&record=' + note.id); };
                attachDiv.appendChild(img);
            } else {
                var a = document.createElement('a');
                a.className = 'attachment-bubble-file';
                a.href = 'index.php?module=Notes&action=DetailView&record=' + note.id;
                a.target = '_blank';
                a.textContent = '\uD83D\uDCC4 ' + (note.filename || note.name);
                attachDiv.appendChild(a);
            }

            bubble.appendChild(attachDiv);
        });
    }

    // Meta (time + tick)
    var meta = document.createElement('div');
    meta.className = 'meta';

    var timeSpan = document.createElement('span');
    var msgDate = new Date(msg.date_entered.replace(' ', 'T') + 'Z');
    var hours = msgDate.getHours().toString().padStart(2, '0');
    var minutes = msgDate.getMinutes().toString().padStart(2, '0');
    timeSpan.textContent = hours + ':' + minutes;
    meta.appendChild(timeSpan);

    if (isOut) {
        var tick = document.createElement('span');
        var tickClass = 'sent';
        var tickSymbol = '\u2713';
        if (status === 'delivered') { tickClass = 'delivered'; tickSymbol = '\u2713\u2713'; }
        else if (status === 'read') { tickClass = 'read'; tickSymbol = '\u2713\u2713'; }
        else if (status === 'error') { tickClass = 'error'; tickSymbol = '\u2717'; }
        tick.className = 'tick ' + tickClass;
        tick.textContent = tickSymbol;
        meta.appendChild(tick);
    }

    bubble.appendChild(meta);
    waBody.appendChild(bubble);
}

function stopPolling() {
    if (_pollInterval) {
        clearInterval(_pollInterval);
        _pollInterval = null;
    }
}

// Always start polling on load
startPolling();

// Stop interval when tab is hidden, restart when visible
document.addEventListener('visibilitychange', function() {
    if (document.visibilityState === 'hidden') {
        stopPolling();
    } else if (document.visibilityState === 'visible') {
        startPolling();
    }
});
