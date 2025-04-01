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
class EmailsViewCompose extends ViewEdit
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
        parent::__construct();

        $this->type = 'compose';
        if (empty($_REQUEST['return_module'])) {
            $this->options['show_title'] = false;
            $this->options['show_header'] = false;
            $this->options['show_footer'] = false;
            $this->options['show_javascript'] = false;
            $this->options['show_subpanels'] = false;
            $this->options['show_search'] = false;
        }
    }

    /**
     * @see SugarView::preDisplay()
     */
    public function preDisplay()
    {
        global $current_user, $mod_strings, $log;
        $metadataFile = $this->getMetaDataFile();
        $this->ev = $this->getEditView();
        $this->ev->ss =& $this->ss;

        if (!isset($this->bean->mailbox_id) || empty($this->bean->mailbox_id)) {
            $inboundEmailID = $current_user->getPreference('defaultIEAccount', 'Emails');
            $this->ev->ss->assign('INBOUND_ID', $inboundEmailID);
        } else {
            $this->ev->ss->assign('INBOUND_ID', $this->bean->mailbox_id);
        }

        $this->ev->ss->assign('TEMP_ID', create_guid());
        $record = isset($_REQUEST['record']) ? $_REQUEST['record'] : '';
        if (empty($record) && !empty($this->bean->id)) {
            $record = $this->bean->id;
        }
        $this->ev->ss->assign('RECORD', $record);
        $this->ev->ss->assign('ACTION', isset($_REQUEST['action']) ? $_REQUEST['action'] : 'send');

        $this->ev->ss->assign('RETURN_MODULE', isset($_GET['return_module']) ? $_GET['return_module'] : '');
        $this->ev->ss->assign('RETURN_ACTION', isset($_GET['return_action']) ? $_GET['return_action'] : '');
        $this->ev->ss->assign('RETURN_ID', isset($_GET['return_id']) ? $_GET['return_id'] : '');
        $this->ev->ss->assign('IS_MODAL', isset($_GET['in_popup']) ? $_GET['in_popup'] : false);

        $attachmentName = $mod_strings['LBL_ATTACHMENT'];
        if (isset($_GET['return_module']) && isset($_GET['return_id'])) {
            $attachmentName = $attachmentName . ' (' . $_GET['return_module'] . ')';
            $attachment = BeanFactory::getBean($_GET['return_module'], $_GET['return_id']);
            if (!$attachment) {
                SugarApplication::appendErrorMessage($mod_strings['ERR_NO_RETURN_ID']);
                $log->fatal('Attachment not found. Requested return ID is not related to an existing Bean.');
            } else {
                if (isset($attachment->name) && $attachment->name) {
                    $attachmentName = $attachment->name;
                } elseif (isset($attachment->title) && $attachment->title) {
                    $attachmentName = $attachment->title;
                } elseif (isset($attachment->subject) && $attachment->subject) {
                    $attachmentName = $attachment->subject;
                }
            }
        }
        $this->ev->ss->assign('ATTACHMENT_NAME', $attachmentName);

        $this->ev->setup(
            $this->module,
            $this->bean,
            $metadataFile,
            get_custom_file_if_exists('modules/Emails/include/ComposeView/ComposeView.tpl')
        );
    }

    /**
     * Get EditView object
     * @return EditView
     */
    public function getEditView()
    {
        $a = dirname(__FILE__, 2) . '/include/ComposeView/ComposeView.php';
        require_once 'modules/Emails/include/ComposeView/ComposeView.php';
        return new ComposeView();
    }

    /**
     * Prepends body with $user's default signature
     *
     * @deprecated
     *
     * @param Email $email
     * @param User $user
     * @return bool|Email
     * @throws SugarControllerException
     */
    public function getSignatures(User $user)
    {
        $email = null;
        if (empty($user->id) || $user->new_with_id === true) {
            throw new \SugarControllerException(
                'EmailsController::composeSignature() requires an existing User and not a new User object. '.
                'This is typically the $current_user global'
            );
        }

        $emailSignatures = sugar_unserialize(base64_decode($user->getPreference('account_signatures', 'Emails')));

        if (isset($emailSignatures[$email->mailbox_id])) {
            $emailSignatureId = $emailSignatures[$email->mailbox_id];
        } else {
            $emailSignatureId = $user->getPreference('signature_default');
        }
        if (gettype($emailSignatureId) === 'string') {
            $emailSignatures = $user->getSignature($emailSignatureId);
            $email->description .= $emailSignatures['signature'];
            $email->description_html .= html_entity_decode((string) $emailSignatures['signature_html']);
            return $email;
        }
        $GLOBALS['log']->warn(
            'EmailsController::composeSignature() was unable to get the signature id for user: '.
                $user->name
            );
        return false;
    }
}
