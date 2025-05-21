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

class stic_Security_Groups_RulesViewList extends ViewList
{

    public function __construct()
    {
        parent::__construct();

    }

    public function preDisplay()
    {
        global $sugar_config, $current_user;

        // Obtain user preferences
        $userPreferenceOrder = $current_user->getPreference('listviewOrder', 'stic_Advanced_Security_Groups2_STIC_ADVANCED_SECUR');

        if (empty($userPreferenceOrder['orderBy'])) {
            $_REQUEST['orderBy'] = 'name_label';
            $_REQUEST['sortOrder'] = 'ASC';
        }

        parent::preDisplay();

        $sugar_config['list_max_entries_per_page'] = 500;
        SticViews::preDisplay($this);

        /**
         * Inserts a record into the 'stic_security_groups_rules' table for each module
         * that is not already included in the table.
         */
        global $db, $app_list_strings, $sugar_config;
        require_once 'modules/MySettings/TabController.php';
        $systemTabs = (new TabController())->get_system_tabs();
        foreach ($systemTabs as $key) {
            // Check if the module already exists in the table
            $moduleCount = $db->getOne("SELECT count(*) FROM stic_security_groups_rules WHERE deleted=0 AND name='$key'");
            // If the module is not in the table, create a new record
            if ($moduleCount == 0) {
                // Create a new bean for the 'stic_Security_Groups_Rules' module
                $ASGBean = BeanFactory::newBean('stic_Security_Groups_Rules');
                $ASGBean->name = $key;
                $ASGBean->name_label = $app_list_strings['moduleList'][$key];
                $ASGBean->save();
            }
        }

        require_once 'modules/stic_Security_Groups_Rules/Utils.php';

        // We load the list of security groups
        stic_Security_Groups_RulesUtils::setCustomSecurityGroupList();

        // Populate the list of related modules with all available values to ensure the inclusion of all related modules.
        stic_Security_Groups_RulesUtils::setAllRelatedModuleList();

    }

    public function display()
    {

        parent::display();

        SticViews::display($this);

        echo getVersionedScript("modules/stic_Security_Groups_Rules/Utils.js");

    }

}
