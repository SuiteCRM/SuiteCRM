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

/*********************************************************************************

 * Description:  Base form for prospect
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

#[\AllowDynamicProperties]
class ProspectFormBase
{
    public function checkForDuplicates($prefix)
    {
        global $local_log;
        require_once('include/formbase.php');

        $focus = BeanFactory::newBean('Prospects');
        if (!checkRequired($prefix, array_keys($focus->required_fields))) {
            return null;
        }

        $dbManager = DBManagerFactory::getInstance();

        $query = '';
        $baseQuery = 'select id,first_name, last_name, title, email1, email2  from prospects where deleted!=1 and (';
        if (!empty($_POST[$prefix.'first_name']) && !empty($_POST[$prefix.'last_name'])) {
            $firstName = $dbManager->quote($_POST[$prefix.'first_name' ?? '']);
            $lastName = $dbManager->quote($_POST[$prefix.'last_name' ?? '']);
            $query = $baseQuery ."  (first_name like '". $firstName . "%' and last_name = '". $lastName ."')";
        } else {
            $lastName = $dbManager->quote($_POST[$prefix.'last_name' ?? '']);
            $query = $baseQuery ."  last_name = '". $lastName ."'";
        }
        if (!empty($_POST[$prefix.'email1'])) {
            $email1 = $dbManager->quote($_POST[$prefix.'email1' ?? '']);
            if (empty($query)) {
                $query = $baseQuery. "  email1='". $email1 . "' or email2 = '". $email1 ."'";
            } else {
                $query .= "or email1='". $email1 . "' or email2 = '". $email1 ."'";
            }
        }
        if (!empty($_POST[$prefix.'email2'])) {
            $email2 = $dbManager->quote($_POST[$prefix.'email2' ?? '']);
            if (empty($query)) {
                $query = $baseQuery. "  email1='". $email2 . "' or email2 = '". $email2 ."'";
            } else {
                $query .= "or email1='". $email2 . "' or email2 = '". $email2 ."'";
            }
        }

        if (!empty($query)) {
            $rows = array();

            $db = DBManagerFactory::getInstance();
            $result = $db->query($query.');');
            while ($row = $db->fetchByAssoc($result)) {
                $rows[] = $row;
            }
            if (count($rows) > 0) {
                return $rows;
            }
        }
        return null;
    }


    public function buildTableForm($rows, $mod='')
    {
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
            $form = '<table width="100%"><tr><td>'.$mod_strings['MSG_DUPLICATE']. '</td></tr><tr><td height="20"></td></tr></table>';
            $form .= "<form action='index.php' method='post' name='dupProspects'><input type='hidden' name='selectedProspect' value=''>";
        } else {
            $form = '<table width="100%"><tr><td>'.$mod_strings['MSG_SHOW_DUPLICATES']. '</td></tr><tr><td height="20"></td></tr></table>';
        }
        $form .= get_form_header($mod_strings['LBL_DUPLICATE'], "", '');
        $form .= "<table width='100%' cellpadding='0' cellspacing='0'>	<tr >	";
        if ($action != 'ShowDuplicates') {
            $form .= "<td > &nbsp;</td>";
        }

        require_once('include/formbase.php');
        $form .= getPostToForm();

        if (isset($rows[0])) {
            foreach ($rows[0] as $key=>$value) {
                if ($key != 'id') {
                    $form .= "<td scope='col' >". $mod_strings[$mod_strings['db_'.$key]]. "</td>";
                }
            }
            $form .= "</tr>";
        }
        $rowColor = 'oddListRowS1';
        foreach ($rows as $row) {
            $form .= "<tr class='$rowColor'>";
            if ($action != 'ShowDuplicates') {
                $form .= "<td width='1%' nowrap='nowrap' ><a href='#' onClick=\"document.dupProspects.selectedProspect.value='{$row['id']}';document.dupProspects.submit() \">[{$app_strings['LBL_SELECT_BUTTON_LABEL']}]</a>&nbsp;&nbsp;</td>\n";
            }

            $wasSet = false;

            foreach ($row as $key=>$value) {
                if ($key != 'id') {
                    if (!$wasSet) {
                        $form .= "<td scope='row' ><a target='_blank' href='index.php?module=Prospects&action=DetailView&record={$row['id']}'>$value</a></td>\n";
                        $wasSet = true;
                    } else {
                        $form .= "<td><a target='_blank' href='index.php?module=Prospects&action=DetailView&record={$row['id']}'>$value</a></td>\n";
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
        $form .= "<tr ><td colspan='$cols' class='blackline'></td></tr>";
        if ($action == 'ShowDuplicates') {
            $form .= "</table><br><input title='{$app_strings['LBL_SAVE_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_SAVE_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='Save';\" type='submit' name='button' value='  {$app_strings['LBL_SAVE_BUTTON_LABEL']}  '> <input title='{$app_strings['LBL_CANCEL_BUTTON_TITLE']}' accessKey='{$app_strings['LBL_CANCEL_BUTTON_KEY']}' class='button' onclick=\"this.form.action.value='ListView'; this.form.module.value='Prospects';\" type='submit' name='button' value='  {$app_strings['LBL_CANCEL_BUTTON_LABEL']}  '></form>";
        } else {
            $form .= "</table><br><input type='submit' class='button' name='ContinueProspect' value='{$mod_strings['LNK_NEW_PROSPECT']}'></form>";
        }
        return $form;
    }
    public function getWideFormBody($prefix, $mod='', $formname='', $prospect = '')
    {
        if (!ACLController::checkAccess('Prospects', 'edit', true)) {
            return '';
        }

        if (empty($prospect)) {
            $prospect = BeanFactory::newBean('Prospects');
        }
        global $mod_strings;
        $temp_strings = $mod_strings;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        }
        global $app_strings;
        global $current_user;
        global $app_list_strings;
        $primary_address_country_options = get_select_options_with_id($app_list_strings['countries_dom'], $prospect->primary_address_country);
        $lbl_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
        $lbl_first_name = $mod_strings['LBL_FIRST_NAME'];
        $lbl_last_name = $mod_strings['LBL_LAST_NAME'];
        $lbl_phone = $mod_strings['LBL_OFFICE_PHONE'];
        $lbl_address =  $mod_strings['LBL_PRIMARY_ADDRESS'];
        $user_id = $current_user->id;
        $lbl_email_address = $mod_strings['LBL_EMAIL_ADDRESS'];
        $form = <<<EOQ
		<input type="hidden" name="{$prefix}record" value="">
		<input type="hidden" name="{$prefix}assigned_user_id" value='{$user_id}'>
		<table border='0' celpadding="0" cellspacing="0" width='100%'>
		<tr>
		<td nowrap class='dataLabel'>$lbl_first_name</td>
		<td class='dataLabel'>$lbl_last_name&nbsp;<span class="required">$lbl_required_symbol</span></td>
		<td nowrap class='dataLabel'>&nbsp;</td>
		<td class='dataLabel'>&nbsp;</td>
		</tr>
		<tr>
		<td nowrap  class='dataField'><input name="{$prefix}first_name" type="text" value="{$prospect->first_name}"></td>
		<td class='dataField'><input name='{$prefix}last_name' type="text" value="{$prospect->last_name}"></td>
		<td class='dataField' nowrap>&nbsp;</td>
		<td class='dataField'>&nbsp;</td>
		</tr>

		<tr>
		<td class='dataLabel' nowrap>{$mod_strings['LBL_TITLE']}</td>
		<td class='dataLabel' nowrap>{$mod_strings['LBL_DEPARTMENT']}</td>
		<td class='dataLabel' nowrap>&nbsp;</td>
		<td class='dataLabel' nowrap>&nbsp;</td>
		</tr>
		<tr>
		<td class='dataField' nowrap><input name='{$prefix}title' type="text" value="{$prospect->title}"></td>
		<td class='dataField' nowrap><input name='{$prefix}department' type="text" value="{$prospect->department}"></td>
		<td class='dataField' nowra>&nbsp;</td>
		<td class='dataField' nowrap>&nbsp;</td>
		</tr>

		<tr>
		<td nowrap colspan='4' class='dataLabel'>$lbl_address</td>
		</tr>

		<tr>
		<td nowrap colspan='4' class='dataField'><input type='text' name='{$prefix}primary_address_street' size='80' value='{$prospect->primary_address_street}'></td>
		</tr>

		<tr>
		<td class='dataLabel'>{$mod_strings['LBL_CITY']}</td>
		<td class='dataLabel'>{$mod_strings['LBL_STATE']}</td>
		<td class='dataLabel'>{$mod_strings['LBL_POSTAL_CODE']}</td>
		<td class='dataLabel'>{$mod_strings['LBL_COUNTRY']}</td>
		</tr>

		<tr>
		<td class='dataField'><input name='{$prefix}primary_address_city'  maxlength='100' value='{$prospect->primary_address_city}'></td>
		<td class='dataField'><input name='{$prefix}primary_address_state'  maxlength='100' value='{$prospect->primary_address_state}'></td>
		<td class='dataField'><input name='{$prefix}primary_address_postalcode'  maxlength='100' value='{$prospect->primary_address_postalcode}'></td>
		<td class='dataField'><select name='{$prefix}primary_address_country' size='1'>{$primary_address_country_options}</select></td>
		</tr>


		<tr>
		<td nowrap class='dataLabel'>$lbl_phone</td>
		<td nowrap class='dataLabel'>{$mod_strings['LBL_MOBILE_PHONE']}</td>
		<td nowrap class='dataLabel'>{$mod_strings['LBL_FAX_PHONE']}</td>
		<td nowrap class='dataLabel'>{$mod_strings['LBL_HOME_PHONE']}</td>
		</tr>

		<tr>
		<td nowrap class='dataField'><input name='{$prefix}phone_work' type="text" value="{$prospect->phone_work}"></td>
		<td nowrap class='dataField'><input name='{$prefix}phone_mobile' type="text" value="{$prospect->phone_mobile}"></td>
		<td nowrap class='dataField'><input name='{$prefix}phone_fax' type="text" value="{$prospect->phone_fax}"></td>
		<td nowrap class='dataField'><input name='{$prefix}phone_home' type="text" value="{$prospect->phone_home}"></td>
		</tr>

		<tr>
		<td class='dataLabel' nowrap>$lbl_email_address</td>
		<td class='dataLabel' nowrap>{$mod_strings['LBL_OTHER_EMAIL_ADDRESS']}</td>
		<td class='dataLabel' nowrap>&nbsp;</td>
		<td class='dataLabel' nowrap>&nbsp;</td>
		</tr>

		<tr>
		<td class='dataField' nowrap><input name='{$prefix}email1' type="text" value="{$prospect->email1}"></td>
		<td class='dataField' nowrap><input name='{$prefix}email2' type="text" value="{$prospect->email2}"></td>
		<td class='dataField' nowrap>&nbsp;</td>
		<td class='dataField' nowrap>&nbsp;</td>
		</tr>


		<tr>
		<td nowrap colspan='4' class='dataLabel'>{$mod_strings['LBL_DESCRIPTION']}</td>
		</tr>
		<tr>
		<td nowrap colspan='4' class='dataField'><textarea cols='80' rows='4' name='{$prefix}description' >{$prospect->description}</textarea></td>
		</tr>
		</table>
		<input type='hidden' name='{$prefix}alt_address_city' value='{$prospect->alt_address_city}'><input type='hidden' name='{$prefix}alt_address_state'   value='{$prospect->alt_address_state}'><input type='hidden' name='{$prefix}alt_address_postalcode'   value='{$prospect->alt_address_postalcode}'><input type='hidden' name='{$prefix}alt_address_country'  value='{$prospect->alt_address_country}'>
		<input type='hidden' name='{$prefix}do_not_call'  value='{$prospect->do_not_call}'><input type='hidden' name='{$prefix}email_opt_out'  value='{$prospect->email_opt_out}'>
EOQ;



        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('Prospects'));
        $javascript->addField('email1', 'false', $prefix);
        $javascript->addField('email2', 'false', $prefix);
        $javascript->addRequiredFields($prefix);
        $form .=$javascript->getScript();
        $mod_strings = $temp_strings;
        return $form;
    }

    public function getFormBody($prefix, $mod='', $formname='')
    {
        if (!ACLController::checkAccess('Prospects', 'edit', true)) {
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
        $lbl_first_name = $mod_strings['LBL_FIRST_NAME'];
        $lbl_last_name = $mod_strings['LBL_LAST_NAME'];
        $lbl_phone = $mod_strings['LBL_PHONE'];
        $user_id = $current_user->id;
        $lbl_email_address = $mod_strings['LBL_EMAIL_ADDRESS'];
        if ($formname == 'EmailEditView') {
            $form = <<<EOQ
		<input type="hidden" name="{$prefix}record" value="">
		<input type="hidden" name="{$prefix}email2" value="">
		<input type="hidden" name="{$prefix}phone_work" value="">
		<input type="hidden" name="{$prefix}assigned_user_id" value='{$user_id}'>
		$lbl_first_name<br>
		<input name="{$prefix}first_name" type="text" value="" size=10><br>
		$lbl_last_name&nbsp;<span class="required">$lbl_required_symbol</span><br>
		<input name='{$prefix}last_name' type="text" value="" size=10><br>
		$lbl_email_address&nbsp;<span class="required">$lbl_required_symbol</span><br>
		<input name='{$prefix}email1' type="text" value=""><br><br>

EOQ;
        } else {
            $form = <<<EOQ
		<input type="hidden" name="{$prefix}record" value="">
		<input type="hidden" name="{$prefix}email2" value="">
		<input type="hidden" name="{$prefix}assigned_user_id" value='{$user_id}'>
		$lbl_first_name<br>
		<input name="{$prefix}first_name" type="text" value=""><br>
		$lbl_last_name&nbsp;<span class="required">$lbl_required_symbol</span><br>
		<input name='{$prefix}last_name' type="text" value=""><br>
		$lbl_phone<br>
		<input name='{$prefix}phone_work' type="text" value=""><br>
		$lbl_email_address<br>
		<input name='{$prefix}email1' type="text" value=""><br><br>

EOQ;
        }


        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('Prospects'));
        $javascript->addField('email1', 'false', $prefix);
        $javascript->addRequiredFields($prefix);

        $form .=$javascript->getScript();
        $mod_strings = $temp_strings;
        return $form;
    }
    public function getForm($prefix, $mod='')
    {
        if (!ACLController::checkAccess('Prospects', 'edit', true)) {
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

		<form name="{$prefix}ProspectSave" onSubmit="return check_form('{$prefix}ProspectSave')" method="POST" action="index.php">
			<input type="hidden" name="{$prefix}module" value="Prospects">
			<input type="hidden" name="{$prefix}action" value="Save">
EOQ;
        $the_form .= $this->getFormBody($prefix, 'Prospects', "{$prefix}ProspectSave");
        $the_form .= <<<EOQ
		<input title="$lbl_save_button_title" accessKey="$lbl_save_button_key" class="button" type="submit" name="{$prefix}button" value="  $lbl_save_button_label  " >
		</form>

EOQ;
        $the_form .= get_left_form_footer();
        $the_form .= get_validate_record_js();

        return $the_form;
    }


    public function handleSave($prefix, $redirect=true, $useRequired=false)
    {
        global $theme;




        require_once('include/formbase.php');

        global $timedate;


        $focus = BeanFactory::newBean('Prospects');
        if ($useRequired &&  !checkRequired($prefix, array_keys($focus->required_fields))) {
            return null;
        }
        $focus = populateFromPost($prefix, $focus);
        if (!$focus->ACLAccess('Save')) {
            return null;
        }
        if (!isset($GLOBALS['check_notify'])) {
            $GLOBALS['check_notify']=false;
        }

        if (!isset($_POST[$prefix.'email_opt_out'])) {
            $focus->email_opt_out = 0;
        }
        if (!isset($_POST[$prefix.'do_not_call'])) {
            $focus->do_not_call = 0;
        }

        if (empty($_POST['record']) && empty($_POST['dup_checked'])) {
            /*
            // we don't check dupes on Prospects - this is the dirtiest data in the system
            //$duplicateProspects = $this->checkForDuplicates($prefix);
            if(isset($duplicateProspects)){
            	$get='module=Prospects&action=ShowDuplicates';

            	//add all of the post fields to redirect get string
            	foreach ($focus->column_fields as $field)
            	{
            		if (!empty($focus->$field))
            		{
            			$get .= "&Prospects$field=".urlencode($focus->$field);
            		}
            	}

            	foreach ($focus->additional_column_fields as $field)
            	{
            		if (!empty($focus->$field))
            		{
            			$get .= "&Prospects$field=".urlencode($focus->$field);
            		}
            	}

            	//create list of suspected duplicate prospect id's in redirect get string
            	$i=0;
            	foreach ($duplicateProspects as $prospect)
            	{
            		$get .= "&duplicate[$i]=".$prospect['id'];
            		$i++;
            	}

            	//add return_module, return_action, and return_id to redirect get string
            	$get .= "&return_module=";
            	if(!empty($_POST['return_module'])) $get .= $_POST['return_module'];
            	else $get .= "Prospects";
            	$get .= "&return_action=";
            	if(!empty($_POST['return_action'])) $get .= $_POST['return_action'];
            	else $get .= "DetailView";
            	if(!empty($_POST['return_id'])) $get .= "&return_id=".$_POST['return_id'];

            	//now redirect the post to modules/Prospects/ShowDuplicates.php
            	header("Location: index.php?$get");
            	return null;
            }*/
        }
        global $current_user;

        $focus->save($GLOBALS['check_notify']);
        $return_id = $focus->id;

        $GLOBALS['log']->debug("Saved record with id of ".$return_id);
        if (isset($_POST['popup']) && $_POST['popup'] == 'true') {
            $get = '&module=';
            if (!empty($_POST['return_module'])) {
                $get .= $_POST['return_module'];
            } else {
                $get .= 'Prospects';
            }
            $get .= '&action=';
            if (!empty($_POST['return_action'])) {
                $get .= $_POST['return_action'];
            } else {
                $get .= 'Popup';
            }
            if (!empty($_POST['return_id'])) {
                $get .= '&return_id='.$_POST['return_id'];
            }
            if (!empty($_POST['popup'])) {
                $get .= '&popup='.$_POST['popup'];
            }
            if (!empty($_POST['create'])) {
                $get .= '&create='.$_POST['create'];
            }
            if (!empty($_POST['to_pdf'])) {
                $get .= '&to_pdf='.$_POST['to_pdf'];
            }
            $get .= '&first_name=' . $focus->first_name;
            $get .= '&last_name=' . $focus->last_name;
            $get .= '&query=true';
            header("Location: index.php?$get");
            return;
        }
        if ($redirect) {
            require_once('include/formbase.php');
            handleRedirect($return_id, 'Prospects');
        } else {
            return $focus;
        }
    }
}
