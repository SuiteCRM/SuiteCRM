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
$admin_option_defs['Administration']['trackers'] = [
    'Trackers',
    'LBL_TRACKERS_TITLE',
    'LBL_TRACKERS_DESCRIPTION',
    './index.php?module=Trackers&action=index',
    'module-trackers'
];

// Search inside the arrays of the admin_group_header for the section in which we want to add our new subsection, also getting its key
foreach (array_keys($admin_group_header) as $key) {
    if ($admin_group_header[$key][0] === 'LBL_ADMIN_TOOLS_TITLE' && isset($admin_group_header[$key][3]['Administration'])) {
        // We add the subsection to the end of the defined array
        $admin_group_header[$key][3]['Administration'] = array_merge($admin_group_header[$key][3]['Administration'], $admin_option_defs['Administration']);
        break;
    }
}
