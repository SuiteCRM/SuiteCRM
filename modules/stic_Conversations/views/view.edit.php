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

require_once 'include/MVC/View/views/view.edit.php';
require_once 'SticInclude/Views.php';
class stic_ConversationsViewEdit extends ViewEdit{

    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = true;
        $this->useModuleQuickCreateTemplate = true;
    }

    public function preDisplay()
    {
        if (!empty($_REQUEST['isDuplicate']) && in_array((string)$_REQUEST['isDuplicate'], array('1', 'true'), true)) {
            $recordId = $_REQUEST['record'] ?? $this->bean->id ?? '';
            if (!empty($recordId)) {
                SugarApplication::redirect("index.php?module=stic_Conversations&action=DetailView&record={$recordId}");
            }
            SugarApplication::redirect('index.php?module=stic_Conversations&action=index');
        }

        parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here you custom code

    }

    public function display()
    {
        parent::display();

        SticViews::display($this);

        // Write here you custom code
        echo getVersionedScript("modules/stic_Conversations/Utils.js");
    }
}
