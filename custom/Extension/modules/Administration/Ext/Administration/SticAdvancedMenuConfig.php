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


$admin_option_defs = [];
$admin_option_defs['Administration']['configure_advanced_menu_tabs'] = [
    'ConfigureTabs',
    'LBL_STIC_MAIN_MENU_LINK_TITLE',
    'LBL_STIC_MAIN_MENU_DESCRIPTION',
    './index.php?module=Administration&action=SticManageMainMenu',
    'stic-configure-main-menu'
];
$admin_group_header['sagility'][3] = array_replace_recursive($admin_option_defs, $admin_group_header['sagility'][3]);

