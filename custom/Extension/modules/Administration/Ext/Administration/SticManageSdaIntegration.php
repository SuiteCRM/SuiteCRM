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

$admin_option_defs = array();

// Create SinergiaDA element in SinergiaCRM admin section
require_once 'modules/stic_Settings/Utils.php';
global $sugar_config;
$sdaEnabled = $sugar_config['stic_sinergiada']['enabled'] ?? false;
if (!empty($sdaEnabled) && $sdaEnabled) {
    $admin_option_defs['Administration']['manage-sda-integration-link'] = array(
        'manage-sda-integration-link',
        'LBL_STIC_SINERGIADA_LINK_TITLE',
        'LBL_STIC_SINERGIADA_DESCRIPTION',
        './index.php?module=Administration&action=sticmanagesdaintegration',
        'activity-streams',
    );

// Create SinergiaCRM admin section if not exists.
    // SinergiaCRM admin section is shared by several modules. If it is not initialized, it must be created.
    // Otherwise, old elements should be mixed with the new one.
    if (!isset($admin_group_header['LBL_SINERGIACRM_TAB_TITLE']) || !isset($admin_group_header['LBL_SINERGIACRM_TAB_TITLE'][3])) {
        $admin_group_header['LBL_SINERGIACRM_TAB_TITLE'] = array(
            'LBL_SINERGIACRM_TAB_TITLE',
            '',
            false,
            $admin_option_defs,
            'LBL_SINERGIACRM_TAB_DESCRIPTION',
        );
    } else {
        $admin_group_header['LBL_SINERGIACRM_TAB_TITLE'][3] = array_replace_recursive($admin_option_defs, $admin_group_header['LBL_SINERGIACRM_TAB_TITLE'][3]);
    }

} else {
    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "SinergiaDA setting is not = 1.");
}
