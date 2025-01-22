<?php
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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
require_once 'include/utils.php';

global $current_user, $timedate;
$db = DBManagerFactory::getInstance();


if (isset($_SESSION['rebuild_relationships'])) {
    displayAdminError(translate('MSG_REBUILD_RELATIONSHIPS', 'Administration'));
}

if (isset($_SESSION['rebuild_extensions'])) {
    displayAdminError(translate('MSG_REBUILD_EXTENSIONS', 'Administration'));
}

if (!isset($_SERVER['SERVER_SOFTWARE'])) {
    LoggerManager::getLogger()->warn('SERVER_SOFTVARE is undefined got Display Warnings');
}

if (isset($_SERVER['SERVER_SOFTWARE']) && (strpos((string) $_SERVER['SERVER_SOFTWARE'], 'Microsoft-IIS') !== false) && (php_sapi_name() == 'cgi-fcgi') && (ini_get('fastcgi.logging') != '0')) {
    displayAdminError(translate('LBL_FASTCGI_LOGGING', 'Administration'));
}
if (is_admin($current_user)) {
    if (!empty($_SESSION['COULD_NOT_CONNECT'])) {
        displayAdminError(translate('LBL_COULD_NOT_CONNECT', 'Administration') . ' ' . $timedate->to_display_date_time($_SESSION['COULD_NOT_CONNECT']));
    }

    //No SMTP server is set up Error.
    $admin = BeanFactory::newBean('Administration');
    $smtp_error = $admin->checkSmtpError();

    if (!isset($sugar_config['installer_locked']) || $sugar_config['installer_locked'] == false) {
        displayAdminError(translate('WARN_INSTALLER_LOCKED', 'Administration'));
    }

    if (empty($sugar_config['admin_access_control'])) {
        if (isset($_SESSION['invalid_versions'])) {
            $invalid_versions = $_SESSION['invalid_versions'];
            foreach ($invalid_versions as $invalid) {
                displayAdminError(translate('WARN_UPGRADE', 'Administration') . $invalid['name'] . translate('WARN_UPGRADE2', 'Administration'));
            }
        }
    }

    if (isset($_SESSION['administrator_error'])) {
        // Only print DB errors once otherwise they will still look broken
        // after they are fixed.
        displayAdminError($_SESSION['administrator_error']);
    }

    unset($_SESSION['administrator_error']);
}
