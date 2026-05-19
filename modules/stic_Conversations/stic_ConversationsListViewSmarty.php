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

require_once 'include/ListView/ListViewSmarty.php';

class stic_ConversationsListViewSmarty extends ListViewSmarty
{
    /**
     * Remove "Mass Duplicate & Update" from actions only for Conversations module
     *
     * @param string $id
     * @param string $location
     * @return array
     */
    protected function buildActionsLink($id = 'actions_link', $location = 'top')
    {
        global $app_strings;

        $link = parent::buildActionsLink($id, $location);

        if (empty($link['buttons']) || !is_array($link['buttons'])) {
            return $link;
        }

        $massDuplicateLabel = $app_strings['LBL_MASS_DUPLICATE_UPDATE'] ?? '';

        $link['buttons'] = array_values(array_filter($link['buttons'], function ($button) use ($massDuplicateLabel) {
            if (!is_string($button)) {
                return true;
            }

            if ($massDuplicateLabel !== '' && strpos($button, '>' . $massDuplicateLabel . '</a>') !== false) {
                return false;
            }

            return true;
        }));

        return $link;
    }
}
