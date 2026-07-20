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

(function () {
    var WHATSAPP_TYPES = ['whatsapp'];

    function sticToggleAttachment() {
        var typeEl = document.getElementById('type');
        if (!typeEl) return;
        var val = typeEl.value || '';
        var row = document.querySelector('div[data-field="attachment_widget"]');
        var widget = document.getElementById('stic_attachment_widget');
        if (!row || !widget) return;
        if (WHATSAPP_TYPES.indexOf(val) !== -1) {
            row.style.display = '';
            widget.style.display = 'block';
        } else {
            row.style.display = 'none';
            widget.style.display = 'none';
            sticRemoveAttachment();
        }
    }

    function sticInitAttachmentToggle() {
        var typeEl = document.getElementById('type');
        if (typeEl) {
            typeEl.removeEventListener('change', sticToggleAttachment);
            typeEl.addEventListener('change', sticToggleAttachment);
        }
        sticToggleAttachment();
    }

    function sticInitAttachmentFile() {
        var fileInput = document.getElementById('stic_media_file');
        if (!fileInput) return;
        fileInput.addEventListener('change', function () {
            if (!this.files || !this.files[0]) return;
            var file = this.files[0];

            document.getElementById('stic_attach_uploading').style.display = 'inline';
            document.getElementById('stic_attach_btn').disabled = true;

            var formData = new FormData();
            formData.append('module', 'stic_Messages');
            formData.append('action', 'uploadConversationMedia');
            formData.append('media', file);

            fetch('index.php', {
                method: 'POST',
                body: formData,
                credentials: 'same-origin'
            })
            .then(function (r) {
                return r.json();
            })
            .then(function (data) {
                document.getElementById('stic_attach_uploading').style.display = 'none';
                document.getElementById('stic_attach_btn').disabled = false;

                if (!data.success) {
                    alert(SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_UPLOAD') + ': ' + (data.error || SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_UNKNOWN')));
                    fileInput.value = '';
                    return;
                }

                document.getElementById('media_note_id').value = data.media_note_id;
                sticShowAttachPreview(file, data.name);
            })
            .catch(function (err) {
                document.getElementById('stic_attach_uploading').style.display = 'none';
                document.getElementById('stic_attach_btn').disabled = false;
                alert(SUGAR.language.get('stic_Messages', 'LBL_CONVERSATION_ERROR_UPLOAD') + ': ' + err);
                fileInput.value = '';
            });
        });
    }

    document.addEventListener('DOMContentLoaded', function () {
        sticInitAttachmentToggle();
        sticInitAttachmentFile();
    });

    // Also initialize immediately in case the script is loaded after DOMContentLoaded
    // (e.g., when the form is rendered directly, not via AJAX)
    if (document.readyState === 'complete' || document.readyState === 'interactive') {
        setTimeout(function() {
            sticInitAttachmentToggle();
            sticInitAttachmentFile();
        }, 100);
    }

    if (typeof jQuery !== 'undefined') {
        jQuery(document).ajaxComplete(function (event, xhr, settings) {
            if (settings && settings.url && settings.url.indexOf('ComposeView') !== -1) {
                sticInitAttachmentToggle();
            }
        });
    }

    window.sticShowAttachPreview = function (file, name) {
        document.getElementById('stic_attach_name').textContent = name;
        var img = document.getElementById('stic_attach_img');
        var icon = document.getElementById('stic_attach_icon');
        if (file.type.startsWith('image/')) {
            var reader = new FileReader();
            reader.onload = function (e) {
                img.src = e.target.result;
                img.style.display = 'inline-block';
                icon.style.display = 'none';
            };
            reader.readAsDataURL(file);
        } else {
            img.style.display = 'none';
            icon.style.display = 'inline-block';
        }
        var preview = document.getElementById('stic_attach_preview');
        preview.style.display = 'flex';
    };

    window.sticRemoveAttachment = function () {
        document.getElementById('media_note_id').value = '';
        var fi = document.getElementById('stic_media_file');
        if (fi) fi.value = '';
        var preview = document.getElementById('stic_attach_preview');
        if (preview) preview.style.display = 'none';
        var img = document.getElementById('stic_attach_img');
        if (img) {
            img.src = '';
            img.style.display = 'none';
        }
    };
})();