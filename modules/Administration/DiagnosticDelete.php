<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */


if (!is_admin($GLOBALS['current_user'])) {
    sugar_die("Unauthorized access to administration.");
}
if (isset($GLOBALS['sugar_config']['hide_admin_diagnostics']) && $GLOBALS['sugar_config']['hide_admin_diagnostics']) {
    sugar_die("Unauthorized access to diagnostic tool.");
}

echo getClassicModuleTitle(
    "Administration",
    array(
            "<a href='index.php?module=Administration&action=index'>{$mod_strings['LBL_MODULE_NAME']}</a>",
           translate('LBL_DIAGNOSTIC_TITLE')
           ),
    true
        );


if (empty($_REQUEST['file']) || empty($_REQUEST['guid'])) {
    echo $mod_strings['LBL_DIAGNOSTIC_DELETE_ERROR'];
} else {
    // Make sure the guid and file are valid file names for security purposes
    clean_string($_REQUEST['guid'], "ALPHANUM");
    clean_string($_REQUEST['file'], "FILE");

    //Making sure someone doesn't pass a variable name as a false reference
    //  to delete a file
    if (strcmp(substr((string) $_REQUEST['file'], 0, 10), "diagnostic") != 0) {
        die($mod_strings['LBL_DIAGNOSTIC_DELETE_DIE']);
    }

    if (file_exists($cachedfile = sugar_cached("diagnostic/".$_REQUEST['guid']."/".$_REQUEST['file'].".zip"))) {
        unlink($cachedfile);
        rmdir(dirname((string) $cachedfile));
        echo $mod_strings['LBL_DIAGNOSTIC_DELETED']."<br><br>";
    } else {
        echo $mod_strings['LBL_DIAGNOSTIC_FILE'] . $_REQUEST['file'].$mod_strings['LBL_DIAGNOSTIC_ZIP'];
    }
}

print "<a href=\"index.php?module=Administration&action=index\">" . $mod_strings['LBL_DIAGNOSTIC_DELETE_RETURN'] . "</a><br>";
