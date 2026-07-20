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
require_once 'modules/stic_Messages/Utils.php';
require_once('modules/stic_Settings/Utils.php');
class stic_MessagesViewEdit extends ViewEdit
{

    public function __construct()
    {
        parent::__construct();
        $this->useForSubpanel = true;
        $this->useModuleQuickCreateTemplate = true;

        // $this->type = 'compose';
        if(!empty($_GET['in_popup'])&& $_GET['in_popup'] == '1' ||
           !empty($_POST['in_popup'])&& $_POST['in_popup'] == '1'){
            $this->options['show_title'] = false;
            $this->options['show_header'] = false;
            $this->options['show_footer'] = false;
            $this->options['show_javascript'] = false;
            $this->options['show_subpanels'] = false;
            $this->options['show_search'] = false;
        }
    }


    public function preDisplay()
    {
        global $app_list_strings;
        parent::preDisplay();

        SticViews::preDisplay($this);
        stic_MessagesUtils::fillDynamicListMessageTemplate();

        $this->ev->ss->assign('IS_MODAL', isset($_REQUEST['in_popup']) ? $_REQUEST['in_popup'] : false);

        // Support both URL parameter names: relatedModule/relatedId (from subpanel) and parent_type/parent_id (from conversation view)
        $this->bean->parent_type = !empty($this->bean->parent_type) ? $this->bean->parent_type : ($_REQUEST['parent_type'] ?? $_REQUEST['relatedModule'] ?? '');
        $this->bean->parent_id = !empty($this->bean->parent_id) ? $this->bean->parent_id : ($_REQUEST['parent_id'] ?? $_REQUEST['relatedId'] ?? null);
        $this->bean->fill_in_additional_parent_fields();

        // Pre-fill phone and type from URL parameters (e.g. from conversation view)
        if (empty($this->bean->phone) && !empty($_REQUEST['phone'])) {
            $this->bean->phone = $_REQUEST['phone'];
        }
        if (empty($this->bean->type) && !empty($_REQUEST['type'])) {
            $this->bean->type = $_REQUEST['type'];
        }

        $this->bean->sender = stic_SettingsUtils::getSetting('messages_sender') ?? '';

        if (!$this->bean->fetched_row) {
            unset($app_list_strings['stic_messages_status_list']['error']);
        }

        // Write here you custom code

    }

    public function display()
    {
        global $mod_strings;
        $this->bean->info = "<p class='msg-warning'><span style='font-style: italic;'>⚠️{$mod_strings['LBL_INFO_TXT']}.</span></p>";

        // Inject attachment widget HTML into the attachment_widget field placeholder
        $this->bean->attachment_widget = $this->_buildAttachmentWidget($mod_strings);

        parent::display();

        SticViews::display($this);

        // Inject helpers UI configuration for JavaScript
        $helpersConfig = stic_MessagesUtils::getHelpersUIConfig();
        echo "<script>var STIC_HELPERS_CONFIG = " . json_encode($helpersConfig) . ";</script>";

        echo getVersionedScript("modules/stic_Messages/include/ComposeView/stic_MessagesComposeView.js");
        echo getVersionedScript("modules/stic_Messages/Utils.js");

        // Attachment widget JS
        echo $this->_buildAttachmentScript($mod_strings);

        // WhatsApp window check JS
        $parentId = $this->bean->parent_id ?? '';
        $parentType = $this->bean->parent_type ?? '';
        echo "<script>var sticMessagesParentId = '" . addslashes($parentId) . "'; var sticMessagesParentType = '" . addslashes($parentType) . "';</script>";
        echo getVersionedScript("modules/stic_Messages/include/EditView/stic_MessagesEditView.js");
    }

    /**
     * Builds the HTML for the attachment widget.
     * Now uses external HTML file.
     */
    private function _buildAttachmentWidget(array $mod_strings): string
    {
        ob_start();
        include 'modules/stic_Messages/include/Attachment/stic_MessagesAttachment.tpl';
        return ob_get_clean();
    }

    /**
     * Builds the inline JS for the attachment widget.
     * Now loads external CSS and JS files instead of inline code.
     */
    private function _buildAttachmentScript(array $mod_strings): string
    {
        return getVersionedScript('modules/stic_Messages/include/Attachment/stic_MessagesAttachment.js');
    }
}