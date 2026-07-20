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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * Entry point that returns conversation messages as JSON.
 * Used by ConversationView.js for live polling.
 */
class WhatsAppViewEntryPoint
{
    public function run()
    {
        header('Content-Type: application/json; charset=utf-8');

        $parentId = $_REQUEST['parent_id'] ?? '';
        $parentType = $_REQUEST['parent_type'] ?? '';
        $lastDate = $_REQUEST['last_date'] ?? '';

        if (empty($parentId) || empty($parentType)) {
            echo json_encode(['success' => false, 'error' => 'Missing parent_id or parent_type']);
            exit;
        }

        $currentUser = $GLOBALS['current_user'];
        if (empty($currentUser->id)) {
            echo json_encode(['success' => false, 'error' => 'Not authenticated']);
            exit;
        }

        $db = DBManagerFactory::getInstance();
        $parentIdSafe = $db->quote($parentId);
        $parentTypeSafe = $db->quote($parentType);

        $sql = "SELECT id, message, type, status, date_entered, sender, phone, direction, template_id
                FROM stic_messages
                WHERE parent_id = '{$parentIdSafe}'
                AND parent_type = '{$parentTypeSafe}'
                AND deleted = 0";

        if (!empty($lastDate)) {
            $lastDateSafe = $db->quote($lastDate);
            $sql .= " AND date_entered > '{$lastDateSafe}'";
        }

        $sql .= " ORDER BY date_entered ASC";

        $result = $db->query($sql);
        $messages = [];
        while ($row = $db->fetchByAssoc($result)) {
            $messages[] = $row;
        }

        // Fetch notes (attachments) for the new messages
        $notesByMessage = [];
        if (!empty($messages)) {
            $messageIds = array_column($messages, 'id');
            $idList = implode("','", array_map([$db, 'quote'], $messageIds));
            $notesSql = "SELECT id, parent_id, name, filename, file_mime_type
                         FROM notes
                         WHERE parent_id IN ('{$idList}') AND deleted = 0";
            $notesResult = $db->query($notesSql);
            while ($note = $db->fetchByAssoc($notesResult)) {
                $notesByMessage[$note['parent_id']][] = $note;
            }
        }

        // Attach notes to each message
        foreach ($messages as &$msg) {
            $msg['notes'] = $notesByMessage[$msg['id']] ?? [];
        }
        unset($msg);

        echo json_encode([
            'success' => true,
            'messages' => $messages,
        ]);
        exit;
    }
}

$entryPoint = new WhatsAppViewEntryPoint();
$entryPoint->run();
