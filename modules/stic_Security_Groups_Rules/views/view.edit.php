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

require_once 'include/MVC/View/views/view.edit.php';
require_once 'SticInclude/Views.php';

#[\AllowDynamicProperties]
class stic_Security_Groups_RulesViewEdit extends ViewEdit
{

    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = true;
        $this->useModuleQuickCreateTemplate = true;
    }

    public function preDisplay()
    {
        global $sugar_config;
        
        parent::preDisplay();

        SticViews::preDisplay($this);

        echo "<script>let stic_security_groups_rules_enabled = '{$sugar_config['stic_security_groups_rules_enabled']}' </script>";

        require_once 'modules/stic_Security_Groups_Rules/Utils.php';
        
        // We load the list of security groups
        stic_Security_Groups_RulesUtils::setCustomSecurityGroupList();
        
        // Charge relate modules list
        stic_Security_Groups_RulesUtils::setCustomRelatedModuleList($this->bean->name);


    }

    public function display()
    {
        parent::display();

        SticViews::display($this);

        // Write here you custom code

        echo getVersionedScript("modules/stic_Security_Groups_Rules/Utils.js");
    }
}
