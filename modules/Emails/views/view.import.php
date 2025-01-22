<?php

/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}


#[\AllowDynamicProperties]
class EmailsViewImport extends ViewEdit
{

    /**
     * @var Email $bean
     */
    public $bean;

    /**
     * EmailsViewCompose constructor.
     */
    public function __construct()
    {
        $this->options['show_title'] = false;
        $this->options['show_header'] = false;
        $this->options['show_footer'] = false;
        $this->options['show_javascript'] = false;
        $this->options['show_subpanels'] = false;
        $this->options['show_search'] = false;
        $this->type = 'ImportView';
    }

    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user;

        $metadataFile = $this->getMetaDataFile();
        $this->ev = $this->getEditView();
        $this->ev->ss =& $this->ss;

        // Set a distinct view name to avoid cache conflicts with regular edit view
        $this->ev->formName = 'EditNonImported';

        if (!isset($this->bean->mailbox_id) || empty($this->bean->mailbox_id)) {
            $inboundEmailID = $current_user->getPreference('defaultIEAccount', 'Emails');
            $this->ev->ss->assign('INBOUND_ID', $inboundEmailID);
        } else {
            $this->ev->ss->assign('INBOUND_ID', $this->bean->mailbox_id);
        }

        $this->ev->ss->assign('TEMP_ID', create_guid());
        $this->ev->ss->assign('RETURN_MODULE', isset($_REQUEST['return_module']) ? $_REQUEST['return_module'] : '');
        $this->ev->ss->assign('RETURN_ACTION', isset($_REQUEST['return_action']) ? $_REQUEST['return_action'] : '');
        $this->ev->setup(
            $this->module,
            $this->bean,
            'modules/Emails/metadata/importviewdefs.php',
            'modules/Emails/include/ImportView/ImportView.tpl'
        );
    }
}
