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
 * EntryPoint: stic_AWF_renderForm
 * Entry point responsible for rendering an Advanced Web Form
 */

require_once 'modules/stic_AWF_Forms/core/FormRenderService.php';

if (ob_get_level()) {
    ob_end_clean();
}
header('Access-Control-Allow-Origin: *');
header('X-Frame-Options: ALLOWALL');

$id = $_REQUEST['id'] ?? '';
$mode = $_REQUEST['mode'] ?? 'view'; // 'view' / 'download'

try {
    $service = new FormRenderService();
    $html = $service->render($id, false);

    if ($mode === 'download') {
        header('Content-Description: File Transfer');
        header('Content-Type: application/octet-stream');
        header('Content-Disposition: attachment; filename="form_' . $id . '.html"');
        header('Content-Length: ' . strlen($html));
    } else {
        header('Content-Type: text/html; charset=UTF-8');
    }

    echo $html;

} catch (Exception $e) {
    http_response_code(404);
    echo "<h1>Error</h1><p>" . $e->getMessage() . "</p>";
    $GLOBALS['log']->error("Line ".__LINE__.": ".__METHOD__.": Error rendering form from EntryPoint. Error: " . $e->getMessage());
}

// Terminate the web thread cleanly
exit;