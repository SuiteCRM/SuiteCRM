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


/**
 * view.showduplicates.php
 *
 * This is the SugarView subclass to handle displaying the list of duplicate Leads found when attempting to create
 * a new Lead.  This class is called from the LeadFormBase class handleSave function.
 */
#[\AllowDynamicProperties]
class ViewShowDuplicates extends SugarView
{
    public function display()
    {
        global $app_strings;
        global $app_list_strings;
        global $theme;
        global $current_language;
        global $mod_strings;

        if (!isset($_SESSION['SHOW_DUPLICATES'])) {
            $GLOBALS['log']->error("Unauthorized access to this area.");
            sugar_die("Unauthorized access to this area.");
        }

        parse_str($_SESSION['SHOW_DUPLICATES'], $_POST);
        $post = array_map("securexss", $_POST);
        foreach ($post as $k => $v) {
            $_POST[$k] = $v;
        }
        unset($_SESSION['SHOW_DUPLICATES']);


        $lead = BeanFactory::newBean('Leads');
        require_once('modules/Leads/LeadFormBase.php');
        $leadForm = new LeadFormBase();
        $GLOBALS['check_notify'] = false;

        $query = 'SELECT id, first_name, last_name, title FROM leads WHERE deleted=0 ';

        $duplicates = $_POST['duplicate'];
        $count = is_countable($duplicates) ? count($duplicates) : 0;
        $db = DBManagerFactory::getInstance();
        if ($count > 0) {
            $query .= "and (";
            $first = true;
            foreach ($duplicates as $duplicate_id) {
                if (!$first) {
                    $query .= ' OR ';
                }
                $first = false;
                $duplicateIdQuoted = $db->quote($duplicate_id);
                $query .= "id='$duplicateIdQuoted' ";
            }
            $query .= ')';
        }

        $duplicateLeads = array();
        $result = $db->query($query);
        $i=0;
        while (($row=$db->fetchByAssoc($result)) != null) {
            $duplicateLeads[$i] = $row;
            $i++;
        }

        $this->ss->assign('FORMBODY', $leadForm->buildTableForm($duplicateLeads));

        $input = '';
        foreach ($lead->column_fields as $field) {
            if (!empty($_POST['Leads'.$field])) {
                $input .= "<input type='hidden' name='$field' value='{$_POST['Leads'.$field]}'>\n";
            }
        }

        foreach ($lead->additional_column_fields as $field) {
            if (!empty($_POST['Leads'.$field])) {
                $input .= "<input type='hidden' name='$field' value='{$_POST['Leads'.$field]}'>\n";
            }
        }

        // Bug 25311 - Add special handling for when the form specifies many-to-many relationships
        if (!empty($_POST['Leadsrelate_to'])) {
            $input .= "<input type='hidden' name='relate_to' value='{$_POST['Leadsrelate_to']}'>\n";
        }

        if (!empty($_POST['Leadsrelate_id'])) {
            $input .= "<input type='hidden' name='relate_id' value='{$_POST['Leadsrelate_id']}'>\n";
        }


        $emailAddress = new SugarEmailAddress();
        $input .= $emailAddress->getEmailAddressWidgetDuplicatesView($lead);

        $get = '';
        if (!empty($_POST['return_module'])) {
            $this->ss->assign('RETURN_MODULE', $_POST['return_module']);
        } else {
            $get .= "Leads";
        }

        $get .= "&return_action=";
        if (!empty($_POST['return_action'])) {
            $this->ss->assign('RETURN_ACTION', $_POST['return_action']);
        } else {
            $get .= "DetailView";
        }

        ///////////////////////////////////////////////////////////////////////////////
        ////	INBOUND EMAIL WORKFLOW
        if (isset($_REQUEST['inbound_email_id'])) {
            $this->ss->assign('INBOUND_EMAIL_ID', $_REQUEST['inbound_email_id']);
            $this->ss->assign('RETURN_MODULE', 'Emails');
            $this->ss->assign('RETURN_ACTION', 'EditView');
            if (isset($_REQUEST['start'])) {
                $this->ss->assign('START', $_REQUEST['start']);
            }
        }
        ////	END INBOUND EMAIL WORKFLOW
        ///////////////////////////////////////////////////////////////////////////////
        if (!empty($_POST['popup'])) {
            $input .= '<input type="hidden" name="popup" value="'.$_POST['popup'].'">';
        } else {
            $input .= '<input type="hidden" name="popup" value="false">';
        }

        if (!empty($_POST['to_pdf'])) {
            $input .= '<input type="hidden" name="to_pdf" value="'.$_POST['to_pdf'].'">';
        } else {
            $input .= '<input type="hidden" name="to_pdf" value="false">';
        }

        if (!empty($_POST['create'])) {
            $input .= '<input type="hidden" name="create" value="'.$_POST['create'].'">';
        } else {
            $input .= '<input type="hidden" name="create" value="false">';
        }

        if (!empty($_POST['return_id'])) {
            $this->ss->assign('RETURN_ID', $_POST['return_id']);
        }

        $this->ss->assign('INPUT_FIELDS', $input);

        //Load the appropriate template
        $template = 'modules/Leads/tpls/ShowDuplicates.tpl';
        if (file_exists('custom/' . $template)) {
            $template = 'custom/' . $template;
        }

        $saveLabel = string_format($app_strings['LBL_SAVE_OBJECT'], array($this->module));
        $this->ss->assign('TITLE', getClassicModuleTitle('Leads', array($this->module, $saveLabel), true));
        $this->ss->display($template);
    }
}
