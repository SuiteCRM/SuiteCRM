<?php
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

$lblAttach = htmlspecialchars($mod_strings['LBL_ATTACHMENT'] ?? 'LBL_ATTACHMENT');
$lblRemove = htmlspecialchars($mod_strings['LBL_ATTACHMENT_REMOVE'] ?? 'LBL_ATTACHMENT_REMOVE');
$lblUploading = htmlspecialchars($mod_strings['LBL_CONVERSATION_UPLOADING'] ?? 'LBL_CONVERSATION_UPLOADING');
?>
<div id="stic_attachment_widget" style="display:none;">
    <input type="hidden" id="media_note_id" name="media_note_id" value="">

    <input type="file" id="stic_media_file" style="display:none;"
        accept="image/jpeg,image/png,image/gif,image/webp,
                video/mp4,video/3gpp,
                audio/ogg,audio/mpeg,audio/mp4,audio/amr,
                application/pdf,
                application/msword,
                application/vnd.openxmlformats-officedocument.wordprocessingml.document,
                application/vnd.ms-excel,
                application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,
                text/csv,.csv">

    <div id="stic_attach_btn_row" style="display:flex;align-items:center;gap:10px;">
        <button type="button" class="button" id="stic_attach_btn"
                onclick="document.getElementById('stic_media_file').click();">
            📎 <?= $lblAttach ?>
        </button>
        <span id="stic_attach_uploading" style="display:none;font-size:11px;color:#888;">
            ⏳ <?= $lblUploading ?>
        </span>
    </div>

    <div id="stic_attach_preview" style="display:none;margin-top:6px;
         padding:6px 10px;background:#f5f5f5;border-radius:6px;
         font-size:12px;display:none;align-items:center;gap:8px;">
        <span id="stic_attach_icon" style="font-size:20px;">📄</span>
        <img  id="stic_attach_img" style="max-height:48px;max-width:48px;
              border-radius:4px;object-fit:cover;display:none;">
        <span id="stic_attach_name" style="flex:1;"></span>
        <a href="#" id="stic_attach_remove"
           style="color:#e53935;font-size:13px;text-decoration:none;"
           onclick="sticRemoveAttachment();return false;">✕ <?= $lblRemove ?></a>
    </div>
</div>