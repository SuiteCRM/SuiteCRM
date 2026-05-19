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

require_once 'modules/Home/QuickSearch.php';

#[\AllowDynamicProperties]
class quicksearchQueryCustom extends quicksearchQuery
{
    protected function updateQueryArguments($args)
    {
        $args = parent::updateQueryArguments($args);

        // Check if a contact filter is specified and if the Conversations module is included in the search
        $contactFilterId = trim((string)($args['stic_contact_filter_id'] ?? ''));
        $hasConversationsModule = !empty($args['modules'])
            && is_array($args['modules'])
            && in_array('stic_Conversations', $args['modules'], true);

        if ($contactFilterId === '' || !$hasConversationsModule) {
            return $args;
        }

        // Add filter to only show conversations linked to the specified contact
        $db = DBManagerFactory::getInstance();
        $contactIdQuoted = $db->quote($contactFilterId);
        $contactWhere = "stic_conversations.id IN (
            SELECT csc.stic_conversations_idb
            FROM contacts_stic_conversations_c csc
            WHERE csc.deleted = 0
              AND csc.contacts_ida = '{$contactIdQuoted}'
        )";

        // Append the contact filter to the existing extra_where clause
        if (!empty($this->extra_where)) {
            $this->extra_where .= ' AND ' . $contactWhere;
        } else {
            $this->extra_where = $contactWhere;
        }

        return $args;
    }
}