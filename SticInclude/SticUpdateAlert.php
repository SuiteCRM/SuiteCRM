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

/**
 * Manages data for visibility of update alerts
 */
$sticVersionCookie = $_COOKIE['SticVersion'];
global $sugar_config;

if (isset($sugar_config['sinergiacrm_version']) && !empty($sticVersionCookie) && $sugar_config['sinergiacrm_version'] != $sticVersionCookie) {
    $this->_tpl_vars['lastSticVersion'] = $sugar_config['sinergiacrm_version'];
    $this->_tpl_vars['showUpdateAlert'] = $sugar_config['stic_show_update_alert'];
}