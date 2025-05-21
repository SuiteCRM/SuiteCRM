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
 * PersonFormBase.php
 *
 * @author Collin Lee
 *
 * This is an abstract class to handle separating some of the common logic used between the form base code.
 * One of the main common function shared was the checking of duplicate records.  This is now handled in the
 * checkForDuplicates method of this function.  When duplicates were found, we created an HTML block using
 * buildTableForm.
 *
 * @see LeadFormBase.php, ContactFormBase.php, ProspectFormBase.php
 */
require_once('include/SugarObjects/forms/FormBase.php');

abstract class PersonFormBase extends FormBase
{
    public $moduleName;
    public $objectName;

    /**
     * buildTableForm
     *
     * This function creates a table with form data.  It is used by the form base code when checking for duplicates
     *
     * @param $rows Array of duplicate row data
     * @return $form The HTML form data
     */
    public function buildTableForm($rows)
    {
        global $action;
        global $mod_strings;
        global $app_strings;

        $newLinkLabel = 'LNK_NEW_' . strtoupper($this->objectName);

        $cols = count($rows[0]) * 2 + 1;

        if ($action != 'ShowDuplicates') {
            $duplicateLabel = string_format($app_strings['MSG_DUPLICATE'], array(strtolower($this->objectName), $this->moduleName));
            $form = '<table width="100%"><tr><td>'.$duplicateLabel.'</td></tr><tr><td height="20"></td></tr></table>';
            $form .= "<form action='index.php' method='post' name='dup{$this->moduleName}'><input type='hidden' name='selected{$this->objectName}' value=''>";
            $form .= getPostToForm('/emailAddress(PrimaryFlag|OptOutFlag|InvalidFlag)?[0-9]*?$/', true);
        } else {
            $duplicateLabel = string_format($app_strings['MSG_SHOW_DUPLICATES'], array(strtolower($this->objectName), $this->moduleName));
            $form = '<table width="100%"><tr><td>'.$duplicateLabel.'</td></tr><tr><td height="20"></td></tr></table>';
        }

        $form .= "<table width='100%' cellpadding='0' cellspacing='0' class='list view' border='0'><tr class='pagination'><td colspan='$cols'><table width='100%' cellspacing='0' cellpadding='0' border='0'><tr><td>";
        if ($action == 'ShowDuplicates') {
            $form .= "<input title='{$app_strings['LBL_SAVE_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_SAVE_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='Save';\" type='submit' name='button' value='  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  '>\n";
            if (!empty($_REQUEST['return_module']) && !empty($_REQUEST['return_action']) && !empty($_REQUEST['return_id'])) {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.module.value='".$_REQUEST['return_module']."';this.form.action.value='".$_REQUEST['return_action']."';this.form.record.value='".$_REQUEST['return_id']."'\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            }

            //Bug 1074, if the action is detailview and the id is empty, then do not show detailview
            else {
                if (!empty($_POST['return_module']) && !empty($_POST['return_action'])    &&(($_POST['return_action'] == "DetailView" && !empty($_REQUEST['return_id'])) || $_POST['return_action'] != "DetailView")) {
                    $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.module.value='".$_POST['return_module']."';this.form.action.value='". $_POST['return_action']."';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
                } else {
                    $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='ListView';\" type='submit' type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
                }
            }
        } else {
            $form .= "<input type='submit' class='button' name='Continue{$this->objectName}' value='{$mod_strings[$newLinkLabel]}'>";
        }
        $form .= "</td></tr></table></td></tr><tr>";

        if ($action != 'ShowDuplicates') {
            $form .= "<td scope='col'>&nbsp;</td>";
        }

        require_once('include/formbase.php');

        if (isset($_POST['return_action']) && $_POST['return_action'] == 'SubPanelViewer') {
            $_POST['return_action'] = 'DetailView';
        }

        if (isset($_POST['return_action']) && $_POST['return_action'] == 'DetailView' && empty($_REQUEST['return_id'])) {
            unset($_POST['return_action']);
        }

        $form .= getPostToForm();

        if (isset($rows[0])) {
            foreach ($rows[0] as $key=>$value) {
                if ($key != 'id') {
                    $newKey = $mod_strings['db_'.$key] ?? '';
                    $string = $mod_strings[$newKey] ?? '';
                    $form .= "<td scope='col' >". $string . "</td>";
                }
            }
            $form .= "</tr>";
        }

        $rowColor = 'oddListRowS1';

        foreach ($rows as $row) {
            $form .= "<tr class='$rowColor'>";
            if ($action != 'ShowDuplicates') {
                $form .= "<td width='1%' nowrap='nowrap'><a href='#' onClick=\"document.forms['dup{$this->moduleName}'].selected{$this->objectName}.value='{$row['id']}';document.forms['dup{$this->moduleName}'].submit() \">[{$app_strings['LBL_SELECT_BUTTON_LABEL']}]</a>&nbsp;&nbsp;</td>\n";
            }
            $wasSet = false;

            foreach ($row as $key=>$value) {
                if ($key != 'id') {
                    if (isset($_POST['popup']) && $_POST['popup']==true) {
                        $form .= "<td scope='row'><a  href='#' onclick=\"window.opener.location='index.php?module={$this->moduleName}&action=DetailView&record={$row['id']}'\">$value</a></td>\n";
                    } else {
                        if (!$wasSet) {
                            $form .= "<td scope='row'><a target='_blank' href='index.php?module={$this->moduleName}&action=DetailView&record={$row['id']}'>$value</a></td>\n";
                            $wasSet = true;
                        } else {
                            $form .= "<td><a target='_blank' href='index.php?module={$this->moduleName}&action=DetailView&record={$row['id']}'>$value</a></td>\n";
                        }
                    }
                }
            }

            if ($rowColor == 'evenListRowS1') {
                $rowColor = 'oddListRowS1';
            } else {
                $rowColor = 'evenListRowS1';
            }
            $form .= "</tr>";
        }
        $form .= "<tr class='pagination'><td colspan='$cols'><table width='100%' cellspacing='0' cellpadding='0' border='0'><tr><td>";
        if ($action == 'ShowDuplicates') {
            $form .= "<input title='{$app_strings['LBL_SAVE_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_SAVE_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='Save';\" type='submit' name='button' value='  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  '>\n";
            if (!empty($_REQUEST['return_module']) && !empty($_REQUEST['return_action']) && !empty($_REQUEST['return_id'])) {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.module.value='".$_REQUEST['return_module']."';this.form.action.value='".$_REQUEST['return_action']."';this.form.record.value='".$_REQUEST['return_id']."';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            } else {
                if (!empty($_POST['return_module']) && !empty($_POST['return_action'])) {
                    $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.module.value='".$_POST['return_module']."';this.form.action.value='". $_POST['return_action']."';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
                } else {
                    $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='ListView';\" type='submit' type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
                }
            }
        } else {
            $form .= "<input type='submit' class='button' name='Continue{$this->objectName}' value='{$mod_strings[$newLinkLabel]}'></form>";
        }
        $form .= "</td></tr></table></td></tr></table>";
        return $form;
    }


    /**
     * checkForDuplicates
     *
     * This function is used to locate any duplicate Leads that may be created when a new Lead is saved.  It is called from the handleSave method
     * of this class.
     *
     * @param $prefix String value of any prefix to the form input names
     * @return $rows Array of matching Leads entries found; null if none found
     */
    public function checkForDuplicates($prefix='')
    {
        require_once('include/formbase.php');
        require_once('include/MVC/SugarModule.php');
        $focus = SugarModule::get($this->moduleName)->loadBean();

        $query = $this->getDuplicateQuery($focus, $prefix);

        if (empty($query)) {
            return null;
        }

        $rows = array();

        $db = DBManagerFactory::getInstance();
        $result = $db->query($query);

        //Loop through the results and store
        while (($row = $db->fetchByAssoc($result)) != null) {
            if (!isset($rows[$row['id']])) {
                $rows[]=$row;
            }
        }

        //Now check for duplicates using email values supplied
        $count = 0;
        $emails = array();
        $emailStr = '';
        while (isset($_POST["{$this->moduleName}{$count}emailAddress{$count}"])) {
            $emailStr .= ",'" . strtoupper(trim($_POST["{$this->moduleName}{$count}emailAddress" . $count++])) . "'";
        } //while

        if (!empty($emailStr)) {
            $emailStr = substr($emailStr, 1);
            $query = 'SELECT DISTINCT er.bean_id AS id FROM email_addr_bean_rel er, ' .
                 'email_addresses ea WHERE ea.id = er.email_address_id ' .
                 'AND ea.deleted = 0 AND er.deleted = 0 AND er.bean_module = \'' . $this->moduleName . '\' ' .
                 'AND email_address_caps IN (' . $emailStr . ')';

            $result = $db->query($query);
            while (($row = $db->fetchByAssoc($result)) != null) {
                if (!isset($rows[$row['id']])) {
                    $query2 = "SELECT id, first_name, last_name, title FROM {$focus->table_name} WHERE deleted=0 AND id = '" . $row['id'] . "'";
                    $result2 = $db->query($query2);
                    $r = $db->fetchByAssoc($result2);
                    if (isset($r['id'])) {
                        $rows[]=$r;
                    }
                } //if
            }
        } //if

        return !empty($rows) ? $rows : null;
    }


    /**
     * getDuplicateQuery
     *
     * This is the function that subclasses should extend and return a SQL query used for the initial duplicate
     * check sequence.
     *
     * @see checkForDuplicates (method), ContactFormBase.php, LeadFormBase.php, ProspectFormBase.php
     * @param $focus sugarbean
     * @param $prefix String value of prefix that may be present in $_POST variables
     * @return SQL String of the query that should be used for the initial duplicate lookup check
     */
    public function getDuplicateQuery($focus, $prefix='')
    {
        return null;
    }
}
