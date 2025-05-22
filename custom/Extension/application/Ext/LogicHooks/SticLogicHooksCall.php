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

if (!isset($hook_array) || !is_array($hook_array)) {
    $hook_array = array();
}
if (!isset($hook_array['after_ui_frame']) || !is_array($hook_array['after_ui_frame'])) {
    $hook_array['after_ui_frame'] = array();
}
$hook_array['after_ui_frame'][] = Array(1, 'stic_Custom_Views processor', 'modules/stic_Custom_Views/processor/LogicHooksCode.php','stic_Custom_Views_ProcessorLogicHooks', 'after_ui_frame'); 

if (!isset($hook_array['server_round_trip']) || !is_array($hook_array['server_round_trip'])) {
    $hook_array['server_round_trip'] = array();
}
$hook_array['server_round_trip'][] = Array(1, 'SticRemoteLog Hook', 'SticInclude/SticRemoteLogLogicHooksCode.php','SticRemoteLogLogicHooks', 'server_round_trip'); 
