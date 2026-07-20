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

use SuiteCRM\Utility\SuiteValidator;

$timedate = $GLOBALS['timedate'];

$lbl = function (string $key) use ($mod_strings): string {
    return htmlspecialchars($mod_strings[$key] ?? $key);
};

$messageIds = array_column($messages, 'id');
$notesByMessage = [];
if (!empty($messageIds)) {
    $db = DBManagerFactory::getInstance();
    $idList = implode("','", array_map([$db, 'quote'], $messageIds));
    $notesSql = "SELECT id, parent_id, name, filename, file_mime_type FROM notes WHERE parent_id IN ('{$idList}') AND deleted = 0";
    $notesResult = $db->query($notesSql);
    while ($note = $db->fetchByAssoc($notesResult)) {
        $notesByMessage[$note['parent_id']][] = $note;
    }
}

?><!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title><?= $lbl('LBL_CONVERSATION_TITLE') ?> — <?= htmlspecialchars($parentName) ?></title>
    <style>
        *{box-sizing:border-box;margin:0;padding:0}
        body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif;background:#e5ddd5;display:flex;flex-direction:column;height:100vh;overflow:hidden}
        .wa-header{background:#075e54;color:#fff;padding:14px 16px;display:flex;align-items:center;gap:12px;flex-shrink:0;box-shadow:0 2px 4px rgba(0,0,0,.3)}
        .wa-header .avatar{width:40px;height:40px;background:#25d366;border-radius:50%;display:flex;align-items:center;justify-content:center;font-size:18px;font-weight:bold;color:#fff;flex-shrink:0}
        .wa-header .info .name{font-size:16px;font-weight:600}
        .wa-header .info .phone{font-size:12px;opacity:.75;margin-top:2px}
        .wa-body{flex:1;overflow-y:auto;padding:12px 16px;display:flex;flex-direction:column;gap:6px}
        .date-separator{text-align:center;margin:10px 0}
        .date-separator span{background:#fff8c5;border-radius:8px;padding:3px 10px;font-size:11px;color:#555;box-shadow:0 1px 2px rgba(0,0,0,.15)}
        .bubble{max-width:75%;padding:8px 10px 6px;border-radius:8px;font-size:13.5px;line-height:1.45;position:relative;word-break:break-word;box-shadow:0 1px 2px rgba(0,0,0,.18)}
        .bubble.out{align-self:flex-end;background:#dcf8c6;border-bottom-right-radius:2px}
        .bubble.in{align-self:flex-start;background:#fff;border-bottom-left-radius:2px}
        .bubble.error{align-self:flex-end;background:#ffe0e0;border-bottom-right-radius:2px}
        .bubble .text{margin-bottom:4px}
        .bubble .meta{display:flex;align-items:center;justify-content:flex-end;gap:4px;font-size:10px;color:#888;white-space:nowrap}
        .tick{font-size:12px}
        .tick.sent{color:#aaa}
        .tick.delivered{color:#aaa}
        .tick.read{color:#34b7f1}
        .tick.error{color:#e53935}
        .wa-footer{background:#f0f0f0;border-top:1px solid #ddd;padding:8px 12px;flex-shrink:0}
        .footer-inner{display:flex;flex-direction:column;gap:6px}
        .window-status{font-size:11px;display:flex;align-items:center;gap:5px;padding:3px 6px;border-radius:6px}
        .window-status.open{color:#1a7a3a;background:#e6f9ed}
        .window-status.closed{color:#a00;background:#fdecea}
        .window-icon{font-size:10px}
        .window-closed-hint{font-size:11px;color:#888;text-align:center;padding:4px 0 2px}
        .input-row{display:flex;gap:8px;align-items:flex-end}
        .input-row textarea{flex:1;border:none;border-radius:20px;padding:9px 14px;font-size:14px;resize:none;outline:none;max-height:100px;overflow-y:auto;line-height:1.4}
        .input-row button{background:#25d366;border:none;border-radius:50%;width:44px;height:44px;color:#fff;font-size:20px;cursor:pointer;display:flex;align-items:center;justify-content:center;flex-shrink:0;transition:background .2s}
        .input-row button:hover{background:#1da851}
        .input-row button:disabled{background:#aaa;cursor:not-allowed}
        .empty-state{text-align:center;color:#888;margin:auto;font-size:14px}
        .attach-btn{background:none;border:none;color:#555;font-size:22px;cursor:pointer;padding:0 4px;line-height:44px;flex-shrink:0}
        .attach-btn:hover{color:#075e54}
        .attachment-preview{display:flex;align-items:center;gap:8px;background:#fff;border-radius:10px;padding:6px 10px;font-size:12px;color:#333;margin-bottom:4px}
        .attachment-preview .preview-img{max-height:48px;max-width:48px;border-radius:4px;object-fit:cover}
        .attachment-preview .remove-attach{margin-left:auto;cursor:pointer;color:#e53935;font-size:16px;line-height:1}
        .uploading-indicator{font-size:11px;color:#888;padding:2px 4px}
        .btn-new-message{display:inline-flex;align-items:center;gap:6px;margin-top:8px;padding:9px 18px;background:#075e54;color:#fff;border:none;border-radius:20px;font-size:13px;font-weight:600;cursor:pointer;text-decoration:none;transition:background .2s;align-self:center}
        .btn-new-message:hover{background:#054d44}
        .btn-new-message .btn-icon{font-size:16px}
        .bubble-attachment{margin-top:6px}
        .attachment-bubble-img{max-width:220px;max-height:220px;border-radius:6px;display:block;object-fit:cover;cursor:pointer}
        .attachment-bubble-file{display:inline-flex;align-items:center;gap:6px;padding:6px 10px;background:rgba(0,0,0,.06);border-radius:8px;font-size:12px;color:#333;text-decoration:none}
        .attachment-bubble-file:hover{background:rgba(0,0,0,.12)}
        @keyframes fadeIn{from{opacity:0;transform:translateY(8px)}to{opacity:1;transform:translateY(0)}}
        @keyframes newMsgSlide{from{opacity:0;transform:translateX(-12px)}to{opacity:1;transform:translateX(0)}}
        .bubble-new.in{background:#a8e6a8;border-left:3px solid #25d366;animation:newMsgSlide 0.4s ease-out}
        .bubble-new.out{background:#a8e6a8;border-right:3px solid #25d366;animation:newMsgSlide 0.4s ease-out}
        .bubble-new.error{background:#a8e6a8;border-right:3px solid #25d366;animation:newMsgSlide 0.4s ease-out}
    </style>
</head>
<body>

<div class="wa-header">
    <div class="avatar"><?= mb_strtoupper(mb_substr($parentName, 0, 1)) ?></div>
    <div class="info">
        <div class="name"><?= htmlspecialchars($parentName) ?></div>
        <?php if (!empty($messages)): ?>
            <div class="phone"><?= htmlspecialchars($messages[0]['phone'] ?? '') ?></div>
        <?php endif; ?>
    </div>
</div>

<div class="wa-body" id="waBody">
<?php if (empty($messages)): ?>
    <div class="empty-state"><?= $lbl('LBL_CONVERSATION_NO_MESSAGES') ?></div>
<?php else:
    $lastDate = null;
    foreach ($messages as $msg):
        $direction = strtolower($msg['direction'] ?? '');
        $type = strtolower($msg['type'] ?? '');
        $status = strtolower($msg['status'] ?? 'sent');

        if (empty($direction)) {
            $direction = 'outbound';
        }

        $isOut = ($direction === 'outbound' || $direction === 'out');
        $isError = ($status === 'error');
        $msgNotes = $notesByMessage[$msg['id']] ?? [];

        $msgDate = $timedate->to_display_date($msg['date_entered']);
        $msgTime = $timedate->to_display_time($msg['date_entered']);

        if ($msgDate !== $lastDate):
            $lastDate = $msgDate;
?>
    <div class="date-separator"><span><?= htmlspecialchars($msgDate) ?></span></div>
<?php endif; ?>

    <div class="bubble <?= $isError ? 'error' : ($isOut ? 'out' : 'in') ?>">
        <div class="text"><?= nl2br(htmlspecialchars($msg['message'] ?? '')) ?></div>
        <?php if (!empty($msgNotes)): ?>
            <?php foreach ($msgNotes as $note): ?>
                <?php
                $isImage = strpos($note['file_mime_type'], 'image/') === 0;
                $noteUrl = 'index.php?module=Notes&action=DetailView&record=' . $note['id'];
                ?>
                <?php if ($isImage): ?>
                    <div class="bubble-attachment">
                        <img class="attachment-bubble-img" src="upload/<?= $note['id'] ?>" onclick="window.open('<?= $noteUrl ?>')">
                    </div>
                <?php else: ?>
                    <div class="bubble-attachment">
                        <a class="attachment-bubble-file" href="<?= $noteUrl ?>" target="_blank">
                            📄 <?= htmlspecialchars($note['filename'] ?? $note['name']) ?>
                        </a>
                    </div>
                <?php endif; ?>
            <?php endforeach; ?>
        <?php endif; ?>
        <div class="meta">
            <span><?= htmlspecialchars($msgTime) ?></span>
            <?php if ($isOut): ?>
                <?php
                $tickClass = 'sent';
                $tickSymbol = '✓';
                if ($status === 'delivered') { $tickClass = 'delivered'; $tickSymbol = '✓✓'; }
                elseif ($status === 'read') { $tickClass = 'read'; $tickSymbol = '✓✓'; }
                elseif ($status === 'error') { $tickClass = 'error'; $tickSymbol = '✗'; }
                ?>
                <span class="tick <?= $tickClass ?>"><?= $tickSymbol ?></span>
            <?php endif; ?>
        </div>
    </div>

<?php endforeach; endif; ?>
</div>

<div class="wa-footer">
    <?php if ($windowOpen): ?>
        <div class="footer-inner">
            <div class="window-status open">
                <span class="window-icon">🟢</span>
                <?= htmlspecialchars($windowMessage) ?>
            </div>
            <div id="attachmentPreview" style="display:none;" class="attachment-preview">
                <img id="previewImg" class="preview-img" style="display:none;">
                <span id="previewIcon" style="font-size:24px;display:none;">📄</span>
                <span id="previewName"></span>
                <span class="remove-attach" onclick="removeAttachment()" title="<?= $lbl('LBL_CONVERSATION_REMOVE_ATTACHMENT') ?>">✕</span>
            </div>
            <div id="uploadingIndicator" class="uploading-indicator" style="display:none;">⏳ <?= $lbl('LBL_CONVERSATION_UPLOADING') ?></div>
            <div class="input-row">
                <input type="file" id="mediaFile" style="display:none;"
                    accept="image/jpeg,image/png,image/gif,image/webp,video/mp4,video/3gpp,audio/ogg,audio/mpeg,audio/mp4,audio/amr,application/pdf,application/msword,application/vnd.openxmlformats-officedocument.wordprocessingml.document,application/vnd.ms-excel,application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                    onchange="handleFileSelected(this)">
                <button class="attach-btn" onclick="document.getElementById('mediaFile').click()" title="<?= $lbl('LBL_ATTACHMENT') ?>">📎</button>
                <textarea id="msgText"
                    placeholder="<?= $lbl('LBL_CONVERSATION_PLACEHOLDER') ?>"
                    rows="1"
                    onInput="this.style.height='auto';this.style.height=this.scrollHeight+'px';"
                    onKeydown="if(event.key==='Enter'&&!event.shiftKey){event.preventDefault();sendMessage();}"></textarea>
                <button id="sendBtn" onclick="sendMessage()" title="<?= $lbl('LBL_CONVERSATION_SEND') ?>">➤</button>
            </div>
        </div>
    <?php else: ?>
        <div class="footer-inner">
            <div class="window-status closed">
                <span class="window-icon">🔴</span>
                <?= htmlspecialchars($windowMessage) ?>
            </div>
            <div class="window-closed-hint">
                <?= $lbl('LBL_CONVERSATION_WINDOW_CLOSED_HINT') ?>
            </div>
            <?php if (!empty($newMessageUrl)): ?>
            <a href="<?= htmlspecialchars($newMessageUrl) ?>" class="btn-new-message"
               onclick="window.opener.location.href=this.href;window.close();return false;">
                <span class="btn-icon">✉</span>
                <?= $lbl('LBL_CONVERSATION_NEW_MESSAGE_BTN') ?>
            </a>
            <?php endif; ?>
        </div>
    <?php endif; ?>
</div>

<script>
    var CONVERSATION = {
        parentType: '<?= addslashes($parentType) ?>',
        parentId:   '<?= addslashes($parentId) ?>',
        lastDate:   '<?= addslashes(end($messages)['date_entered'] ?? '') ?>',
        pollDelay:  <?= (int)$pollDelay ?>
    };
</script>
<?= getVersionedScript('modules/stic_Messages/include/ConversationView/ConversationView.js') ?>

</body>
</html>
