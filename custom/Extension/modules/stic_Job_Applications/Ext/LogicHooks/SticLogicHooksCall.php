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

// Offer counts on create and status change
$hook_array['after_save'][] = array(100, 'after_save', 'modules/stic_Job_Applications/LogicHooksCode.php', 'stic_Job_ApplicationsLogicHooks', 'after_save');
// Update offer counts on delete
$hook_array['after_delete'][] = array(100, 'after_delete', 'modules/stic_Job_Applications/LogicHooksCode.php', 'stic_Job_ApplicationsLogicHooks', 'after_delete');
// Update offer counts on relationship changes
$hook_array['after_relationship_add'][] = array(100, 'after_relationship_add', 'modules/stic_Job_Applications/LogicHooksCode.php', 'stic_Job_ApplicationsLogicHooks', 'after_relationship_add');
$hook_array['after_relationship_delete'][] = array(100, 'after_relationship_delete', 'modules/stic_Job_Applications/LogicHooksCode.php', 'stic_Job_ApplicationsLogicHooks', 'after_relationship_delete');

