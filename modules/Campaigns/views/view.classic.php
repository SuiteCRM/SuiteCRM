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

require_once('include/MVC/View/SugarView.php');
require_once('include/MVC/Controller/SugarController.php');

#[\AllowDynamicProperties]
class CampaignsViewClassic extends SugarView
{
    public function __construct()
    {
        parent::__construct();
        $this->type = $this->action;
    }




    /**
     * @see SugarView::display()
     */
    public function display()
    {
        // Call SugarController::getActionFilename to handle case sensitive file names
        $file = SugarController::getActionFilename($this->action);
        if (file_exists('custom/modules/' . $this->module . '/'. $file . '.php')) {
            $this->includeClassicFile('custom/modules/'. $this->module . '/'. $file . '.php');
            return true;
        } elseif (file_exists('modules/' . $this->module . '/'. $file . '.php')) {
            $this->includeClassicFile('modules/'. $this->module . '/'. $file . '.php');
            return true;
        }
        return false;
    }

    /**
     * @see SugarView::_getModuleTitleParams()
     */
    protected function _getModuleTitleParams($browserTitle = false)
    {
        $params = array();
        $params[] = $this->_getModuleTitleListParam($browserTitle);
        if (isset($this->action)) {
            switch ($_REQUEST['action']) {
                    case 'WizardHome':
                        if (!empty($this->bean->id)) {
                            $params[] = "<a href='index.php?module={$this->module}&action=DetailView&record={$this->bean->id}'>".$this->bean->name."</a>";
                        }
                        $params[] = $GLOBALS['mod_strings']['LBL_CAMPAIGN_WIZARD'];
                        break;
                    case 'WebToLeadCreation':
                        $params[] = $GLOBALS['mod_strings']['LBL_LEAD_FORM_WIZARD'];
                        break;
                    case 'WizardNewsletter':
                        if (!empty($this->bean->id)) {
                            $params[] = "<a href='index.php?module={$this->module}&action=DetailView&record={$this->bean->id}'>".$GLOBALS['mod_strings']['LBL_NEWSLETTER_TITLE']."</a>";
                        }
                        $params[] = $GLOBALS['mod_strings']['LBL_CREATE_NEWSLETTER'];
                        break;
                    case 'CampaignDiagnostic':
                        $params[] = $GLOBALS['mod_strings']['LBL_CAMPAIGN_DIAGNOSTICS'];
                        break;
                    case 'WizardEmailSetup':
                        $params[] = $GLOBALS['mod_strings']['LBL_EMAIL_SETUP_WIZARD_TITLE'];
                        break;
                    case 'TrackDetailView':
                        if (!empty($this->bean->id)) {
                            $params[] = "<a href='index.php?module={$this->module}&action=DetailView&record={$this->bean->id}'>".$this->bean->name."</a>";
                        }
                        $params[] = $GLOBALS['mod_strings']['LBL_LIST_TO_ACTIVITY'];
                        break;
            }//switch
        }//fi

        return $params;
    }
}
