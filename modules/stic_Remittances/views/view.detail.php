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

require_once 'include/MVC/View/views/view.detail.php';
require_once 'SticInclude/Views.php';

class stic_RemittancesViewDetail extends ViewDetail
{

    public function __construct()
    {
        parent::__construct();

    }

    public function preDisplay()
    {

        parent::preDisplay();
        /**
         * This code is used in the SinergiaCRM-SugarCRM hotfix #150 that allows properly viewing HTML fields without setting DeveloperMode to true.
         *
         * This view will load CustomDetailView in preDisplay in order to enable disableCheckTemplate parameter.
         * This will force the cached view to be rebuilt and show the right values in HTML fields.
         *
         * This code needs to be placed right after the preDisplay of the module
         *
         */
        $metadataFile = $this->getMetaDataFile();
        require_once 'custom/include/DetailView/CustomDetailView.php';
        $this->dv = new CustomDetailView();
        $this->dv->ss = &$this->ss; // ss is the SugarSmarty object
        $this->dv->setup($this->module, $this->bean, $metadataFile, get_custom_file_if_exists('include/DetailView/DetailView.tpl'));

        SticViews::preDisplay($this);

        include_once "modules/stic_Remittances/Utils.php";
        stic_RemittancesUtils::fillDynamicListForIssuingOrganizations();
    }

    public function display()
    {
        /**
         * Same code for the hotfix #150 of SinergiaCRM-SugarCRM.
         *
         * This line needs to be placed right before the display of the module
         *
         */
        $this->dv->th->disableCheckTemplate = true;
        parent::display();

        SticViews::display($this);

        echo getVersionedScript("modules/stic_Remittances/Utils.js");

        // Write here you custom code

    }

}
