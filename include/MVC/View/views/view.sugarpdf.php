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



require_once('include/Sugarpdf/SugarpdfFactory.php');

class ViewSugarpdf extends SugarView
{
    public $type ='sugarpdf';
    /**
     * It is set by the "sugarpdf" request parameter and it is use by SugarpdfFactory to load the good sugarpdf class.
     * @var String
     */
    public $sugarpdf='default';
    /**
     * The sugarpdf object (Include the TCPDF object).
     * The atributs of this object are destroy in the output method.
     * @var Sugarpdf object
     */
    public $sugarpdfBean=null;


    public function __construct()
    {
        parent::__construct();


        if (isset($_REQUEST["sugarpdf"])) {
            $this->sugarpdf = $_REQUEST["sugarpdf"];
        } else {
            if (!isset($_REQUEST['module'])) {
                LoggerManager::getLogger()->warn('Undefined index: module');
            }

            if (!isset($_REQUEST['record'])) {
                LoggerManager::getLogger()->warn('Undefined index: record');
            }

            header('Location:index.php?module='.(isset($_REQUEST['module']) ? $_REQUEST['module'] : null).'&action=DetailView&record='.(isset($_REQUEST['record']) ? $_REQUEST['record'] : null));
        }
    }




    public function preDisplay()
    {
        $this->sugarpdfBean = SugarpdfFactory::loadSugarpdf($this->sugarpdf, $this->module, $this->bean, $this->view_object_map);

        // ACL control
        if (!empty($this->bean) && !$this->bean->ACLAccess($this->sugarpdfBean->aclAction)) {
            ACLController::displayNoAccess(true);
            sugar_cleanup(true);
        }

        if (isset($this->errors)) {
            $this->sugarpdfBean->errors = $this->errors;
        }
    }

    public function display()
    {
        $this->sugarpdfBean->process();
        $this->sugarpdfBean->Output($this->sugarpdfBean->fileName, 'I');
    }
}
