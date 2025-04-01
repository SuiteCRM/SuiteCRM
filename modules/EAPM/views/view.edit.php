<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

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


#[\AllowDynamicProperties]
class EAPMViewEdit extends ViewEdit
{
    private $_returnId;

    public function __construct()
    {
        $this->setReturnId();
        parent::__construct();
    }

    protected function setReturnId()
    {
        $returnId = $GLOBALS['current_user']->id;
        if (!empty($_REQUEST['user_id']) && !empty($_REQUEST['return_module']) && 'Users' == $_REQUEST['return_module']) {
            $returnId = $_REQUEST['user_id'];
        }
        $this->_returnId = $returnId;
    }

    protected function _getModuleTab()
    {
        return 'Users';
    }

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        global $mod_strings;

        $returnAction = 'DetailView';
        $returnModule = 'Users';
        $returnId = $GLOBALS['current_user']->id;
        $returnName = $GLOBALS['current_user']->full_name;
        if (!empty($_REQUEST['return_action']) && !empty($_REQUEST['return_module'])) {
            if ('Users' == $_REQUEST['return_module']) {
                if ('EditView' == $_REQUEST['return_action']) {
                    $returnAction = 'EditView';
                }
                if (!empty($_REQUEST['return_name'])) {
                    $returnName = $_REQUEST['return_name'];
                }
                if (!empty($_REQUEST['user_id'])) {
                    $returnId = $_REQUEST['user_id'];
                }
            }
        }
        $this->_returnId = $returnId;

        $iconPath = $this->getModuleTitleIconPath($this->module);
        $params = array();
        if (!empty($iconPath) && !$browserTitle) {
            $params[] = "<a href='index.php?module=Users&action=index'><!--not_in_theme!--><img src='{$iconPath}' alt='".translate('LBL_MODULE_NAME', 'Users')."' title='".translate('LBL_MODULE_NAME', 'Users')."' align='absmiddle'></a>";
        } else {
            $params[] = translate('LBL_MODULE_NAME', 'Users');
        }
        $params[] = "<a href='index.php?module={$returnModule}&action=EditView&record={$returnId}'>".$returnName."</a>";
        $params[] = $GLOBALS['app_strings']['LBL_EDIT_BUTTON_LABEL'];

        return $params;
    }

    /**
     * @see SugarView::getModuleTitleIconPath()
     */
    protected function getModuleTitleIconPath($module)
    {
        return parent::getModuleTitleIconPath('Users');
    }

    public function display()
    {
        $this->bean->password = empty($this->bean->password) ? '' : EAPM::$passwordPlaceholder;

        $this->ss->assign('return_id', $this->_returnId);

        $cancelUrl = "index.php?action=EditView&module=Users&record={$this->_returnId}#tab5";

        if (isset($_REQUEST['return_module']) && $_REQUEST['return_module'] == 'Import') {
            $cancelUrl = "index.php?module=Import&action=Step1&import_module=". $_REQUEST['return_action'] . "&application=" . $_REQUEST['application'];
        }
        $this->ss->assign('cancelUrl', $cancelUrl);

        if ($GLOBALS['current_user']->is_admin || empty($this->bean) || empty($this->bean->id) || $this->bean->isOwner($GLOBALS['current_user']->id)) {
            if (!empty($this->bean) && empty($this->bean->id) && $this->_returnId != $GLOBALS['current_user']->id) {
                $this->bean->assigned_user_id = $this->_returnId;
            }
            
            parent::display();
        } else {
            ACLController::displayNoAccess();
        }
    }
}
