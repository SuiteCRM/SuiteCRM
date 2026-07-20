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
 * Public entry point that serves WhatsApp message attachments to Twilio.
 *
 * Access is protected by a short-lived HMAC token signed with SuiteCRM's
 * unique_key. The token is generated in stic_Messages::save() just before
 * calling the Twilio API, so it is valid for TOKEN_TTL seconds — long enough
 * for Twilio to fetch the file but useless if intercepted afterwards.
 *
 * URL format:
 *   index.php?entryPoint=sticWhatsappMedia&note_id=UUID&expires=TS&token=HMAC
 */
class WhatsAppMediaEntryPoint
{
    /** Seconds the token stays valid after generation */
    const TOKEN_TTL = 300;

    public function run()
    {
        try {
            $noteId = $_GET['note_id']  ?? '';
            $expires = (int)($_GET['expires'] ?? 0);
            $token = $_GET['token']    ?? '';

            if (!$this->validateToken($noteId, $expires, $token)) {
                $this->sendError(403, 'Forbidden');
                return;
            }

            $this->serveFile($noteId);

        } catch (Exception $e) {
            $GLOBALS['log']->error('WhatsAppMediaEntryPoint: ' . $e->getMessage());
            $this->sendError(500, 'Internal server error');
        }
    }

    /**
     * Validates the HMAC token and expiry.
     */
    private function validateToken(string $noteId, int $expires, string $token): bool
    {
        if (empty($noteId) || empty($expires) || empty($token)) {
            return false;
        }

        // Validate UUID format to avoid path traversal
        if (!preg_match('/^[a-f0-9\-]{36}$/i', $noteId)) {
            return false;
        }

        if (time() > $expires) {
            $GLOBALS['log']->warn('WhatsAppMediaEntryPoint: token expired for note_id=' . $noteId);
            return false;
        }

        $secret = $GLOBALS['sugar_config']['unique_key'] ?? '';
        $expected = hash_hmac('sha256', $noteId . $expires, $secret);

        return hash_equals($expected, $token);
    }

    /**
     * Looks up the Note, finds the physical file and streams it to the client.
     */
    private function serveFile(string $noteId): void
    {
        $note = BeanFactory::getBean('Notes', $noteId);
        if (!$note || empty($note->id)) {
            $GLOBALS['log']->warn('WhatsAppMediaEntryPoint: Note not found for id=' . $noteId);
            $this->sendError(404, 'Not found');
            return;
        }

        $filePath = rtrim(getcwd(), '/') . '/upload/' . $noteId;
        if (!file_exists($filePath)) {
            $GLOBALS['log']->warn('WhatsAppMediaEntryPoint: File not found at ' . $filePath);
            $this->sendError(404, 'File not found');
            return;
        }

        $mime = $note->file_mime_type ?: mime_content_type($filePath);
        $name = $note->filename       ?: basename($filePath);

        header('Content-Type: '        . $mime);
        header('Content-Length: '      . filesize($filePath));
        header('Content-Disposition: inline; filename="' . addslashes($name) . '"');
        header('Cache-Control: no-store');

        readfile($filePath);
        exit;
    }

    private function sendError(int $code, string $message): void
    {
        http_response_code($code);
        header('Content-Type: application/json');
        echo json_encode(['success' => false, 'message' => $message]);
        exit;
    }
}

$entryPoint = new WhatsAppMediaEntryPoint();
$entryPoint->run();