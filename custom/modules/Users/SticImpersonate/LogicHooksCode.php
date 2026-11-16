<?php
/**
 * SinergiaCRM User Impersonation Module
 * 
 * This file is part of SinergiaCRM.
 * 
 * SinergiaCRM is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * SinergiaCRM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with SinergiaCRM. If not, see <http://www.gnu.org/licenses/>.
 * 
 * @package SinergiaCRM
 * @subpackage stic_Impersonate
 * @author SinergiaCRM Development Team
 * @copyright 2025 SinergiaCRM
 * @license http://www.gnu.org/licenses/gpl-3.0.html GPL v3 or later
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class ImpersonateLogicHooks
{
    /**
     * Checks if the current user is impersonating another user and injects message and indicator to all views
     */
    public function after_ui_frame($event, $arguments)
    {
        global $current_user;
        
        // STIC 2026-05-19 - Skip injection during AJAX requests (to_pdf=true)
        // to_pdf=true is used by KReports and other modules for AJAX calls,
        // and injecting HTML script tags would corrupt the JSON response
        if (!empty($_REQUEST['to_pdf']) || !empty($_REQUEST['sugar_body_only'])) {
            return;
        }
        
        // Only inject UI elements if user is logged in
        if (empty($current_user) || empty($current_user->id)) {
            return;
        }

        require_once('custom/modules/Users/SticImpersonate/Impersonate.php');
        $impersonate = new Impersonate();
        
        // Check if currently impersonating
        if ($impersonate->isImpersonating()) {
            $this->injectImpersonationMarks($impersonate);
        }
    }

    /**
     * Injects the impersonation indicator and message into the UI
     */
    private function injectImpersonationMarks($impersonate)
    {
        global $current_language;
        $mod_strings = return_module_language($current_language, 'Users');
        $status = $impersonate->getImpersonationStatus();

        $modImpersonate = array_filter($mod_strings, function($key) {
            return strpos($key, 'LBL_IMPERSONATE') === 0;
        }, ARRAY_FILTER_USE_KEY);
        
        $modImpersonateEncoded = json_encode($modImpersonate);
        $statusEncoded = json_encode($status);
        echo <<<SCRIPT
            <script>
                var modImpersonate = $modImpersonateEncoded;
                var impersonationData = $statusEncoded;
            </script>
        SCRIPT;

        echo getVersionedScript("custom/modules/Users/SticImpersonate/ImpersonateUtils.js");
        echo '<link rel="stylesheet" type="text/css" href="' . getJSPath('custom/modules/Users/SticImpersonate/Impersonate.css') . '" />';
    }
}

