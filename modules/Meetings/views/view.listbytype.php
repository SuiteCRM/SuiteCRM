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


require_once('modules/EAPM/EAPM.php');
#[\AllowDynamicProperties]
class MeetingsViewListbytype extends ViewList
{
    public $options = array('show_header' => false, 'show_title' => false, 'show_subpanels' => false, 'show_search' => true, 'show_footer' => false, 'show_javascript' => false, 'view_print' => false,);

    public function __construct()
    {
        parent::__construct();
    }




    public function listViewProcess()
    {
        if (!$eapmBean = EAPM::getLoginInfo('IBMSmartCloud', true)) {
            $smarty = new Sugar_Smarty();
            echo $smarty->fetch('include/externalAPI/IBMSmartCloud/IBMSmartCloudSignup.'.$GLOBALS['current_language'].'.tpl');
            return;
        }

        $apiName = 'IBMSmartCloud';
        $api = ExternalAPIFactory::loadAPI($apiName, true);
        $api->loadEAPM($eapmBean);

        $quickCheck = $api->quickCheckLogin();
        if (! $quickCheck['success']) {
            $errorMessage = string_format(translate('LBL_ERR_FAILED_QUICKCHECK', 'EAPM'), array('IBM SmartCloud'));
            $errorMessage .= '<form method="POST" target="_EAPM_CHECK" action="index.php">';
            $errorMessage .= '<input type="hidden" name="module" value="EAPM">';
            $errorMessage .= '<input type="hidden" name="action" value="Save">';
            $errorMessage .= '<input type="hidden" name="record" value="'.$eapmBean->id.'">';
            $errorMessage .= '<input type="hidden" name="active" value="1">';
            $errorMessage .= '<input type="hidden" name="closeWhenDone" value="1">';
            $errorMessage .= '<input type="hidden" name="refreshParentWindow" value="1">';

            $errorMessage .= '<br><input type="submit" value="'.$GLOBALS['app_strings']['LBL_EMAIL_OK'].'">&nbsp;';
            $errorMessage .= '<input type="button" onclick="lastLoadedMenu=undefined;DCMenu.closeOverlay();return false;" value="'.$GLOBALS['app_strings']['LBL_CANCEL_BUTTON_LABEL'].'">';
            $errorMessage .= '</form>';
            echo $errorMessage;
            return;
        }

        $this->processSearchForm();
        $this->params['orderBy'] = 'meetings.date_start';
        $this->params['overrideOrder'] = true;
        $this->lv->searchColumns = $this->searchForm->searchColumns;
        $this->lv->show_action_dropdown = false;
        $this->lv->multiSelect = false;

        unset($this->searchForm->searchdefs['layout']['advanced_search']);

        if (!$this->headers) {
            return;
        }

        if (empty($_REQUEST['search_form_only']) || $_REQUEST['search_form_only'] == false) {
            $this->lv->ss->assign("SEARCH", false);
            if (!isset($_REQUEST['name_basic'])) {
                $_REQUEST['name_basic'] = '';
            }
            $this->lv->ss->assign('DCSEARCH', $_REQUEST['name_basic']);
            $this->lv->setup($this->seed, 'include/ListView/ListViewDCMenu.tpl', $this->where, $this->params);
            $savedSearchName = empty($_REQUEST['saved_search_select_name']) ? '' : (' - ' . $_REQUEST['saved_search_select_name']);
            echo $this->lv->display();
        }
    }

    public function listViewPrepare()
    {
        $oldRequest = $_REQUEST;
        parent::listViewPrepare();
        $_REQUEST = $oldRequest;
    }

    public function processSearchForm()
    {
        // $type = 'LotusLiveDirect';
        $type = 'IBMSmartCloud';
        global $timedate;

        $two_hours_ago = DBManagerFactory::getInstance()->convert(DBManagerFactory::getInstance()->quoted($timedate->asDb($timedate->getNow()->get("-2 hours"))), 'datetime');

        $where =  " meetings.type = '$type' AND meetings.status != 'Held' AND meetings.status != 'Not Held' AND meetings.date_start > {$two_hours_ago} AND ( meetings.assigned_user_id = '".DBManagerFactory::getInstance()->quote($GLOBALS['current_user']->id)."' OR exists ( SELECT id FROM meetings_users WHERE meeting_id = meetings.id AND user_id = '".DBManagerFactory::getInstance()->quote($GLOBALS['current_user']->id)."' AND deleted = 0 ) ) ";

        if (isset($_REQUEST['name_basic'])) {
            $name_search = trim($_REQUEST['name_basic']);
            if (! empty($name_search)) {
                $where .= " AND meetings.name LIKE '".DBManagerFactory::getInstance()->quote($name_search)."%' ";
            }
        }

        $this->where = $where;
    }
}
