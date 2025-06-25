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

require_once 'include/MVC/View/views/view.list.php';
require_once 'SticInclude/Views.php';

class CustomTrackersViewList extends ViewList
{

    public function preDisplay()
    {
        parent::preDisplay();

        $this->lv = new ListViewSmarty();
        // Don't Mass Update the list
        $this->lv->showMassupdateFields = false;
        // Hide Quick Edit Pencil
        $this->lv->quickViewLinks = false;
        // Don't Delete any record from the list
        $this->lv->delete = false;

        // Sort by date_modified in DESC if there are no params
        if (empty($this->params['orderBy'])) {
            $this->params['orderBy'] = 'date_modified';
            $this->params['overrideOrder'] = true;
            if (empty($this->params['sortOrder'])) {
                $this->params['sortOrder'] = 'DESC';
            }
        }

    }
}