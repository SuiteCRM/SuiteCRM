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

#[\AllowDynamicProperties]
class CustomCampaignsViewEdit extends ViewEdit
{

    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = false;
        $this->useModuleQuickCreateTemplate = true;
        $this->moduleName = 'Campaigns';
    }

    public function preDisplay()
    {
        parent::preDisplay();

        SticViews::preDisplay($this);

        // Write here you custom code
        include_once "custom/modules/Campaigns/SticUtils.php";
        CampaignsUtils::fillDynamicListsForNotifications();

        // Adding the Notification panel dinamically.
        $this->ev->defs['panels']['templateMeta']['tabdefs']['LBL_NOTIFICATION_INFORMATION_PANEL'] = array(
            'newTab' => false,
            'panelDefault' => 'expanded',
        );

        $this->ev->defs['panels']['LBL_NOTIFICATION_INFORMATION_PANEL'] = array(
            0 => array(
                0 => array(
                    'name' => 'parent_name',
                    'label' => 'LBL_FLEX_RELATE',
                ),
                1 => '',
            ),
            1 => array(
                0 => array(
                    'name' => 'notification_prospect_list_ids',
                    'label' => 'LBL_NOTIFICATION_PROSPECT_LIST_ID',
                ),
                1 => array(
                    'name' => 'notification_template_id',
                    'label' => 'LBL_NOTIFICATION_TEMPLATE_ID',
                ),
            ),
            2 => array(
                0 => array(
                    'name' => 'notification_outbound_email_id',
                    'label' => 'LBL_NOTIFICATION_OUTBOUND_EMAIL_ID',
                ),
                1 => array(
                    'name' => 'notification_inbound_email_id',
                    'label' => 'LBL_NOTIFICATION_INBOUND_EMAIL_ID',
                ),
            ),
            3 => array(
                0 => array(
                    'name' => 'notification_from_name',
                    'label' => 'LBL_NOTIFICATION_FROM_NAME',
                ),
                1 => array(
                    'name' => 'notification_from_addr',
                    'label' => 'LBL_NOTIFICATION_FROM_ADDR',
                ),
            ),
            4 => array(
                0 => array(
                    'name' => 'notification_reply_to_name',
                    'label' => 'LBL_NOTIFICATION_REPLY_TO_NAME',
                ),
                1 => array(
                    'name' => 'notification_reply_to_addr',
                    'label' => 'LBL_NOTIFICATION_REPLY_TO_ADDR',
                ),
            ),
        );
    }

    public function display()
    {
        parent::display();

        SticViews::display($this);

        // Write here you custom code
        echo getVersionedScript("custom/modules/Campaigns/SticUtils.js");

        include_once "custom/modules/Campaigns/SticUtils.php";
        echo CampaignsUtils::getNotificationCampaignEmailDataScript();
    }

}
