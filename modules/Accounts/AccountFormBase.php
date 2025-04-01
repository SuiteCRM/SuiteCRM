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

/*********************************************************************************

 * Description:  base form for account
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class AccountFormBase
{
    protected $db = null;

    public function __construct()
    {
        $this->db = DBManagerFactory::getInstance();
    }

    public function checkForDuplicates($prefix)
    {
        require_once('include/formbase.php');

        $query = '';

        $name = !empty($_POST[$prefix . 'name']) ? $_POST[$prefix . 'name'] : '';
        $shippingAddressCity = !empty($_POST[$prefix . 'shipping_address_city']) ? $_POST[$prefix . 'shipping_address_city'] : '';
        $billingAddressCity = !empty($_POST[$prefix . 'billing_address_city']) ? $_POST[$prefix . 'billing_address_city'] : '';

        $baseQuery = 'SELECT id, name, website, billing_address_city FROM accounts WHERE deleted != 1 AND ';

        if (!empty($name)) {
            $nameQuoted = $this->db->quoted($name . '%');
            $query = $baseQuery ." name LIKE " . $nameQuoted;
        }

        if (!empty($billingAddressCity) || !empty($shippingAddressCity)) {
            $tempQuery = '';

            if (!empty($billingAddressCity)) {
                $billingAddressCityQuoted = $this->db->quoted($billingAddressCity . '%');
                $tempQuery .= "billing_address_city LIKE " . $billingAddressCityQuoted;
            }

            if (!empty($shippingAddressCity)) {
                $shippingAddressCityQuoted = $this->db->quoted($shippingAddressCity . '%');
                $tempQuery .= (empty($tempQuery)) ?: ' OR ';
                $tempQuery .= "shipping_address_city LIKE " . $shippingAddressCityQuoted;
            }

            $query .= (empty($query)) ? $baseQuery : ' AND ';
            $query .=   ' ('. $tempQuery . ' ) ';
        }

        if (!empty($query)) {
            $rows = array();
            $db = DBManagerFactory::getInstance();
            $result = $db->query($query);
            $i=-1;

            while (($row=$db->fetchByAssoc($result)) != null) {
                $i++;
                $rows[$i] = $row;
            }

            if ($i==-1) {
                return null;
            }

            return $rows;
        }

        return null;
    }


    public function buildTableForm($rows, $mod='Accounts')
    {
        if (!ACLController::checkAccess('Accounts', 'edit', true)) {
            return '';
        }
        global $action;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        } else {
            global $mod_strings;
        }
        global $app_strings;
        $cols = (is_countable($rows[0]) ? count($rows[0]) : 0) * 2 + 1;
        if ($action != 'ShowDuplicates') {
            $form = "<form action='index.php' method='post' id='dupAccounts'  name='dupAccounts'><input type='hidden' name='selectedAccount' value=''>";
            $form .= '<table width="100%"><tr><td>'.$mod_strings['MSG_DUPLICATE']. '</td></tr><tr><td height="20"></td></tr></table>';
            unset($_POST['selectedAccount']);
        } else {
            $form = '<table width="100%"><tr><td>'.$mod_strings['MSG_SHOW_DUPLICATES']. '</td></tr><tr><td height="20"></td></tr></table>';
        }

        if (isset($_POST['return_action']) && $_POST['return_action'] == 'SubPanelViewer') {
            $_POST['return_action'] = 'DetailView';
        }

        if (isset($_POST['return_action']) && $_POST['return_action'] == 'DetailView' && empty($_REQUEST['return_id'])) {
            unset($_POST['return_action']);
        }

        $form .= "<table width='100%' cellpadding='0' cellspacing='0' class='list view' border='0'><tr class='pagination'><td colspan='$cols'><table width='100%' cellspacing='0' cellpadding='0' border='0'><tr><td>";
        // handle buttons
        if ($action == 'ShowDuplicates') {
            $return_action = 'ListView'; // cn: bug 6658 - hardcoded return action break popup -> create -> duplicate -> cancel
            $return_action = (isset($_REQUEST['return_action']) && !empty($_REQUEST['return_action'])) ? $_REQUEST['return_action'] : $return_action;
            $form .= "<input type='hidden' name='selectedAccount' id='selectedAccount' value=''><input title='{$app_strings['LBL_SAVE_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_SAVE_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='Save';\" type='submit' name='button' value='  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  '>\n";

            if (!empty($_REQUEST['return_module']) && !empty($_REQUEST['return_action']) && !empty($_REQUEST['return_id'])) {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.module.value='".$_REQUEST['return_module']."';this.form.action.value='".$_REQUEST['return_action']."';this.form.record.value='".$_REQUEST['return_id']."'\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            } elseif (!empty($_POST['return_module']) && !empty($_POST['return_action'])) {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.module.value='".$_POST['return_module']."';this.form.action.value='". $_POST['return_action']."';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            } else {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='ListView';\" type='submit' type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            }
        } else {
            $form .= "<input type='submit' class='button' name='ContinueAccount' value='{$mod_strings['LNK_NEW_ACCOUNT']}'>\n";
        }
        $form .= "</td></tr></table></td></tr><tr>";
        if ($action != 'ShowDuplicates') {
            $form .= "<th> &nbsp;</th>";
        }
        require_once('include/formbase.php');

        $form .= getPostToForm();
        if (isset($rows[0])) {
            foreach ($rows[0] as $key=>$value) {
                if ($key != 'id') {
                    $form .= "<th>". $mod_strings[$mod_strings['db_'.$key]]. "</th>";
                }
            }

            $form .= "</tr>";
        }

        $rowColor = 'oddListRowS1';
        foreach ($rows as $row) {
            $form .= "<tr class='$rowColor'>";
            if ($action != 'ShowDuplicates') {
                $form .= "<td width='1%' nowrap><a href='javascript:void(0)' onclick='document.dupAccounts.selectedAccount.value=\"{$row['id']}\"; document.dupAccounts.submit(); '>[{$app_strings['LBL_SELECT_BUTTON_LABEL']}]</a>&nbsp;&nbsp;</td>\n";
            }
            foreach ($row as $key=>$value) {
                if ($key != 'id') {
                    if (isset($_POST['popup']) && $_POST['popup']==true) {
                        $form .= "<td scope='row'><a  href='javascript:void(0)' onclick=\"window.opener.location='index.php?module=Accounts&action=DetailView&record={$row['id']}'\">$value</a></td>\n";
                    } else {
                        $form .= "<td><a target='_blank' href='index.php?module=Accounts&action=DetailView&record={$row['id']}'>$value</a></td>\n";
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

        // handle buttons
        if ($action == 'ShowDuplicates') {
            $return_action = 'ListView'; // cn: bug 6658 - hardcoded return action break popup -> create -> duplicate -> cancel
            $return_action = (isset($_REQUEST['return_action']) && !empty($_REQUEST['return_action'])) ? $_REQUEST['return_action'] : $return_action;
            $form .= "<input type='hidden' name='selectedAccount' id='selectedAccount' value=''><input title='{$app_strings['LBL_SAVE_BUTTON_TITLE']}' class='button' onclick=\"this.form.action.value='Save';\" type='submit' name='button' value='  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  '>\n";

            if (!empty($_REQUEST['return_module']) && !empty($_REQUEST['return_action']) && !empty($_REQUEST['return_id'])) {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' class='button' onclick=\"this.form.module.value='".$_REQUEST['return_module']."';this.form.action.value='".$_REQUEST['return_action']."';this.form.record.value='".$_REQUEST['return_id']."'\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            } elseif (!empty($_POST['return_module']) && !empty($_POST['return_action'])) {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' class='button' onclick=\"this.form.module.value='".$_POST['return_module']."';this.form.action.value='". $_POST['return_action']."';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            } else {
                $form .= "<input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}'  class='button' onclick=\"this.form.action.value='ListView';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '>";
            }
        } else {
            $form .= "<input type='submit' class='button' name='ContinueAccount' value='{$mod_strings['LNK_NEW_ACCOUNT']}'></form>\n";
        }
        $form .= "</td></tr></table></td></tr></table>";
        return $form;
    }

    public function getForm($prefix, $mod='', $form='')
    {
        if (!ACLController::checkAccess('Accounts', 'edit', true)) {
            return '';
        }
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        } else {
            global $mod_strings;
        }
        global $app_strings;
        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];


        $the_form = get_left_form_header($mod_strings['LBL_NEW_FORM_TITLE']);
        $the_form .= <<<EOQ
		<form name="{$prefix}AccountSave" onSubmit="return check_form('{$prefix}AccountSave');" method="POST" action="index.php">
			<input type="hidden" name="{$prefix}module" value="Accounts">
			<input type="hidden" name="{$prefix}action" value="Save">
EOQ;
        $the_form .= $this->getFormBody($prefix, $mod, $prefix."AccountSave");
        $the_form .= <<<EOQ
		<p><input title="$lbl_save_button_title" accessKey="$lbl_save_button_key" class="button" type="submit" name="button" value="  $lbl_save_button_label  " ></p>
		</form>

EOQ;
        $the_form .= get_left_form_footer();
        $the_form .= get_validate_record_js();

        return $the_form;
    }


    public function getFormBody($prefix, $mod='', $formname='')
    {
        if (!ACLController::checkAccess('Accounts', 'edit', true)) {
            return '';
        }
        global $mod_strings;
        $temp_strings = $mod_strings;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        }
        global $app_strings;
        global $current_user;

        $lbl_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
        $lbl_account_name = $mod_strings['LBL_ACCOUNT_NAME'];
        $lbl_phone = $mod_strings['LBL_PHONE'];
        $lbl_website = $mod_strings['LBL_WEBSITE'];
        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];
        $user_id = $current_user->id;

        $form = <<<EOQ
			<p><input type="hidden" name="record" value="">
			<input type="hidden" name="email1" value="">
			<input type="hidden" name="email2" value="">
			<input type="hidden" name="assigned_user_id" value='{$user_id}'>
			<input type="hidden" name="action" value="Save">
EOQ;
        $form .= "$lbl_account_name&nbsp;<span class='required'>$lbl_required_symbol</span><br><input name='name' type='text' value=''><br>";
        $form .= "$lbl_phone<br><input name='phone_office' type='text' value=''><br>";
        $form .= "$lbl_website<br><input name='website' type='text' value='http://'><br>";
        $form .='</p>';



        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('Accounts'));
        $javascript->addRequiredFields($prefix);
        $form .=$javascript->getScript();
        $mod_strings = $temp_strings;
        return $form;
    }



    public function getWideFormBody($prefix, $mod='', $formname='', $contact='')
    {
        if (!ACLController::checkAccess('Accounts', 'edit', true)) {
            return '';
        }

        if (empty($contact)) {
            $contact = BeanFactory::newBean('Contacts');
        }
        global $mod_strings;
        $temp_strings = $mod_strings;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        }
        global $app_strings;
        global $current_user;
        $account = BeanFactory::newBean('Accounts');

        $lbl_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
        $lbl_account_name = $mod_strings['LBL_ACCOUNT_NAME'];
        $lbl_phone = $mod_strings['LBL_PHONE'];
        $lbl_website = $mod_strings['LBL_WEBSITE'];
        if (isset($contact->assigned_user_id)) {
            $user_id=$contact->assigned_user_id;
        } else {
            $user_id = $current_user->id;
        }

        //Retrieve Email address and set email1, email2
        $sugarEmailAddress = new SugarEmailAddress();
        $sugarEmailAddress->handleLegacyRetrieve($contact);
        if (!isset($contact->email1)) {
            $contact->email1 = '';
        }
        if (!isset($contact->email2)) {
            $contact->email2 = '';
        }
        if (!isset($contact->email_opt_out)) {
            $contact->email_opt_out = '';
        }
        $form="";
        $default_desc="";
        if (!empty($contact->description)) {
            $default_desc=$contact->description;
        }
        $form .= <<<EOQ
		<input type="hidden" name="{$prefix}record" value="">
		<input type="hidden" name="{$prefix}phone_fax" value="{$contact->phone_fax}">
		<input type="hidden" name="{$prefix}phone_other" value="{$contact->phone_other}">
		<input type="hidden" name="{$prefix}email1" value="{$contact->email1}">
		<input type="hidden" name="{$prefix}email2" value="{$contact->email2}">
		<input type='hidden' name='{$prefix}billing_address_street' value='{$contact->primary_address_street}'><input type='hidden' name='{$prefix}billing_address_city' value='{$contact->primary_address_city}'><input type='hidden' name='{$prefix}billing_address_state'   value='{$contact->primary_address_state}'><input type='hidden' name='{$prefix}billing_address_postalcode'   value='{$contact->primary_address_postalcode}'><input type='hidden' name='{$prefix}billing_address_country'  value='{$contact->primary_address_country}'>
		<input type='hidden' name='{$prefix}shipping_address_street' value='{$contact->alt_address_street}'><input type='hidden' name='{$prefix}shipping_address_city' value='{$contact->alt_address_city}'><input type='hidden' name='{$prefix}shipping_address_state'   value='{$contact->alt_address_state}'><input type='hidden' name='{$prefix}shipping_address_postalcode'   value='{$contact->alt_address_postalcode}'><input type='hidden' name='{$prefix}shipping_address_country'  value='{$contact->alt_address_country}'>
		<input type="hidden" name="{$prefix}assigned_user_id" value='{$user_id}'>
		<input type='hidden' name='{$prefix}do_not_call'  value='{$contact->do_not_call}'>
		<input type='hidden' name='{$prefix}email_opt_out'  value='{$contact->email_opt_out}'>
		<table width='100%' border="0" cellpadding="0" cellspacing="0">
		<tr>
		<td width="20%" nowrap scope="row">$lbl_account_name&nbsp;<span class="required">$lbl_required_symbol</span></td>
		<TD width="80%" nowrap scope="row">{$mod_strings['LBL_DESCRIPTION']}</TD>
		</tr>
		<tr>
		<td nowrap ><input name='{$prefix}name' type="text" value="$contact->account_name"></td>
		<TD rowspan="5" ><textarea name='{$prefix}description' rows='6' cols='50' >$default_desc</textarea></TD>
		</tr>
		<tr>
		<td nowrap scope="row">$lbl_phone</td>
		</tr>
		<tr>
		<td nowrap ><input name='{$prefix}phone_office' type="text" value="$contact->phone_work"></td>
		</tr>
		<tr>
		<td nowrap scope="row">$lbl_website</td>
		</tr>
		<tr>
		<td nowrap ><input name='{$prefix}website' type="text" value="http://"></td>
		</tr>
EOQ;
        //carry forward custom lead fields common to accounts during Lead Conversion
        $tempAccount = BeanFactory::newBean('Accounts');
        if (method_exists($contact, 'convertCustomFieldsForm')) {
            $contact->convertCustomFieldsForm($form, $tempAccount, $prefix);
        }
        unset($tempAccount);
        $form .= <<<EOQ
		</TABLE>
EOQ;


        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('Accounts'));
        $javascript->addRequiredFields($prefix);
        $form .=$javascript->getScript();
        $mod_strings = $temp_strings;
        return $form;
    }


    public function handleSave($prefix, $redirect=true, $useRequired=false)
    {
        require_once('include/formbase.php');

        $focus = BeanFactory::newBean('Accounts');

        if ($useRequired &&  !checkRequired($prefix, array_keys($focus->required_fields))) {
            return null;
        }
        $focus = populateFromPost($prefix, $focus);

        if (isset($GLOBALS['check_notify'])) {
            $check_notify = $GLOBALS['check_notify'];
        } else {
            $check_notify = false;
        }

        if (empty($_POST['record']) && empty($_POST['dup_checked'])) {
            $duplicateAccounts = $this->checkForDuplicates($prefix);
            if (isset($duplicateAccounts)) {
                $location='module=Accounts&action=ShowDuplicates';
                $get = '';

                // Bug 25311 - Add special handling for when the form specifies many-to-many relationships
                if (isset($_POST['relate_to']) && !empty($_POST['relate_to'])) {
                    $get .= '&Accountsrelate_to='.$_POST['relate_to'];
                }
                if (isset($_POST['relate_id']) && !empty($_POST['relate_id'])) {
                    $get .= '&Accountsrelate_id='.$_POST['relate_id'];
                }

                //add all of the post fields to redirect get string
                foreach ($focus->column_fields as $field) {
                    if (!empty($focus->$field) && !is_object($focus->$field)) {
                        $get .= "&Accounts$field=".urlencode($focus->$field);
                    }
                }

                foreach ($focus->additional_column_fields as $field) {
                    if (!empty($focus->$field)) {
                        $get .= "&Accounts$field=".urlencode($focus->$field);
                    }
                }


                if ($focus->hasCustomFields()) {
                    foreach ($focus->field_defs as $name=>$field) {
                        if (!empty($field['source']) && $field['source'] == 'custom_fields') {
                            $get .= "&Accounts$name=".urlencode($focus->$name);
                        }
                    }
                }



                $emailAddress = new SugarEmailAddress();
                $get .= $emailAddress->getFormBaseURL($focus);



                //create list of suspected duplicate account id's in redirect get string
                $i=0;
                foreach ($duplicateAccounts as $account) {
                    $get .= "&duplicate[$i]=".$account['id'];
                    $i++;
                }

                //add return_module, return_action, and return_id to redirect get string
                $urlData = array('return_module' => 'Accounts', 'return_action' => '');
                foreach (array('return_module', 'return_action', 'return_id', 'popup', 'create') as $var) {
                    if (!empty($_POST[$var])) {
                        $urlData[$var] = $_POST[$var];
                    }
                }
                $get .= "&".http_build_query($urlData);

                $_SESSION['SHOW_DUPLICATES'] = $get;
                //now redirect the post to modules/Accounts/ShowDuplicates.php
                if (!empty($_POST['is_ajax_call']) && $_POST['is_ajax_call'] == '1') {
                    ob_clean();
                    $json = getJSONobj();
                    echo $json->encode(array('status' => 'dupe', 'get' => $location));
                } elseif (!empty($_REQUEST['ajax_load'])) {
                    echo "<script>SUGAR.ajaxUI.loadContent('index.php?$location');</script>";
                } else {
                    if (!empty($_POST['to_pdf'])) {
                        $location .= '&to_pdf='.urlencode($_POST['to_pdf']);
                    }
                    header("Location: index.php?$location");
                }
                return null;
            }
        }
        if (!$focus->ACLAccess('Save')) {
            ACLController::displayNoAccess(true);
            sugar_cleanup(true);
        }

        $focus->save($check_notify);
        $return_id = $focus->id;

        $GLOBALS['log']->debug("Saved record with id of ".$return_id);


        if (!empty($_POST['is_ajax_call']) && $_POST['is_ajax_call'] == '1') {
            $json = getJSONobj();
            echo $json->encode(array('status' => 'success',
                                 'get' => ''));
            $trackerManager = TrackerManager::getInstance();
            $timeStamp = TimeDate::getInstance()->nowDb();
            if ($monitor = $trackerManager->getMonitor('tracker')) {
                $monitor->setValue('action', 'detailview');
                $monitor->setValue('user_id', $GLOBALS['current_user']->id);
                $monitor->setValue('module_name', 'Accounts');
                $monitor->setValue('date_modified', $timeStamp);
                $monitor->setValue('visible', 1);

                if (!empty($this->bean->id)) {
                    $monitor->setValue('item_id', $return_id);
                    $monitor->setValue('item_summary', $focus->get_summary_text());
                }
                $trackerManager->saveMonitor($monitor, true, true);
            }
            return null;
        }

        if (isset($_POST['popup']) && $_POST['popup'] == 'true') {
            $urlData = array("query" => true, "name" => $focus->name, "module" => 'Accounts', 'action' => 'Popup');
            if (!empty($_POST['return_module'])) {
                $urlData['module'] = $_POST['return_module'];
            }
            if (!empty($_POST['return_action'])) {
                $urlData['action'] = $_POST['return_action'];
            }
            foreach (array('return_id', 'popup', 'create', 'to_pdf') as $var) {
                if (!empty($_POST[$var])) {
                    $urlData[$var] = $_POST[$var];
                }
            }
            header("Location: index.php?".http_build_query($urlData));
            return;
        }
        if ($redirect) {
            handleRedirect($return_id, 'Accounts');
        } else {
            return $focus;
        }
    }
}
