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

 * Description: Call Form Base
 * Portions created by SugarCRM are Copyright (C) SugarCRM, Inc.
 * All Rights Reserved.
 * Contributor(s): ______________________________________..
 ********************************************************************************/

require_once('include/SugarObjects/forms/FormBase.php');

#[\AllowDynamicProperties]
class CallFormBase extends FormBase
{
    public function getFormBody($prefix, $mod='', $formname='', $cal_date='', $cal_time='')
    {
        if (!ACLController::checkAccess('Calls', 'edit', true)) {
            return '';
        }
        global $mod_strings;
        $temp_strings = $mod_strings;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        }
        global $app_strings;
        global $app_list_strings;
        global $current_user;
        global $theme;


        $lbl_subject = $mod_strings['LBL_SUBJECT'];
        // Unimplemented until jscalendar language files are fixed
        // global $current_language;
        // global $default_language;
        // global $cal_codes;
        // Unimplemented until jscalendar language files are fixed
        // $cal_lang = (empty($cal_codes[$current_language])) ? $cal_codes[$default_language] : $cal_codes[$current_language];

        global $timedate;
        $cal_lang = "en";
        $cal_dateformat = $timedate->get_cal_date_format();

        $lbl_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
        $lbl_date = $mod_strings['LBL_DATE'];
        $lbl_time = $mod_strings['LBL_TIME'];
        $ntc_date_format = $timedate->get_user_date_format();
        $ntc_time_format = '('.$timedate->get_user_time_format().')';

        $user_id = $current_user->id;
        $default_status = $app_list_strings['call_status_default'];
        $default_parent_type= $app_list_strings['record_type_default_key'];
        $date = TimeDate::getInstance()->nowDb();
        $default_date_start = $timedate->to_display_date($date, false);
        $default_time_start = $timedate->to_display_time($date);
        $time_ampm = $timedate->AMPMMenu($prefix, $default_time_start);
        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_key = $app_strings['LBL_SAVE_BUTTON_KEY'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];
        $form =	<<<EOQ
			<form name="{$formname}" onSubmit="return check_form('{$formname}') "method="POST" action="index.php">
			<input type="hidden" name="{$prefix}module" value="Calls">
			<input type="hidden" name="{$prefix}action" value="Save">
				<input type="hidden" name="{$prefix}record" value="">
			<input type="hidden"  name="{$prefix}direction" value="Outbound">
			<input type="hidden" name="{$prefix}status" value="{$default_status}">
			<input type="hidden" name="{$prefix}parent_type" value="{$default_parent_type}">
			<input type="hidden" name="{$prefix}assigned_user_id" value='{$user_id}'>
			<input type="hidden" name="{$prefix}duration_hours" value="1">
			<input type="hidden" name="{$prefix}duration_minutes" value="0">
			<input type="hidden" name="{$prefix}user_id" value="{$user_id}">

		<table cellspacing="1" cellpadding="0" border="0">
<tr>
    <td colspan="2"><input type='radio' name='appointment' value='Call' class='radio' onchange='document.{$formname}.module.value="Calls";' style='vertical-align: middle;' checked> <span scope="row">{$mod_strings['LNK_NEW_CALL']}</span>
&nbsp;
&nbsp;
<input type='radio' name='appointment' value='Meeting' class='radio' onchange='document.{$formname}.module.value="Meetings";'><span scope="row">{$mod_strings['LNK_NEW_MEETING']}</span></td>
</tr>
<tr>
    <td colspan="2"><span scope="row">$lbl_subject</span>&nbsp;<span class="required">$lbl_required_symbol</span></td>
</tr>
<tr><td valign=top><input name='{$prefix}name' size='30' maxlength='255' type="text"></td>
    <td><input name='{$prefix}date_start' id='{$formname}jscal_field' maxlength='10' type="hidden" value="{$cal_date}"></td>
    <td><input name='{$prefix}time_start' type="hidden" maxlength='10' value="{$cal_time}"></td>

			<script type="text/javascript">
//		Calendar.setup ({
//			inputField : "{$formname}jscal_field", daFormat : "$cal_dateformat" ifFormat : "$cal_dateformat", showsTime : false, button : "{$formname}jscal_trigger", singleClick : true, step : 1, weekNumbers:false
//		});
		</script>



EOQ;



        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('Calls'));
        $javascript->addRequiredFields($prefix);
        $form .=$javascript->getScript();
        $form .= "<td align=\"left\" valign=top><input title='$lbl_save_button_title' accessKey='$lbl_save_button_key' class='button' type='submit' name='button' value=' $lbl_save_button_label ' ></td></tr></table></form>";
        $mod_strings = $temp_strings;
        return $form;
    }
    public function getFormHeader($prefix, $mod='', $title='')
    {
        if (!ACLController::checkAccess('Calls', 'edit', true)) {
            return '';
        }
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        } else {
            global $mod_strings;
        }






        if (!empty($title)) {
            $the_form = get_left_form_header($title);
        } else {
            $the_form = get_left_form_header($mod_strings['LBL_NEW_FORM_TITLE']);
        }
        $the_form .= <<<EOQ
		<form name="{$prefix}CallSave" onSubmit="return check_form('{$prefix}CallSave') "method="POST" action="index.php">
			<input type="hidden" name="{$prefix}module" value="Calls">
			<input type="hidden" name="{$prefix}action" value="Save">

EOQ;
        return $the_form;
    }
    public function getFormFooter($prefic, $mod='')
    {
        if (!ACLController::checkAccess('Calls', 'edit', true)) {
            return '';
        }
        global $app_strings;
        global $app_list_strings;
        $lbl_save_button_title = $app_strings['LBL_SAVE_BUTTON_TITLE'];
        $lbl_save_button_label = $app_strings['LBL_SAVE_BUTTON_LABEL'];
        $the_form = "	<p><input title='$lbl_save_button_title' class='button' type='submit' name='button' value=' $lbl_save_button_label ' ></p></form>";
        $the_form .= get_left_form_footer();
        $the_form .= get_validate_record_js();
        return $the_form;
    }

    public function getForm($prefix, $mod='')
    {
        if (!ACLController::checkAccess('Calls', 'edit', true)) {
            return '';
        }
        $the_form = $this->getFormHeader($prefix, $mod);
        $the_form .= $this->getFormBody($prefix, $mod, "{$prefix}CallSave");
        $the_form .= $this->getFormFooter($prefix, $mod);

        return $the_form;
    }


    public function handleSave($prefix, $redirect=true, $useRequired=false)
    {
        require_once('include/formbase.php');

        global $current_user;
        global $timedate;

        //BUG 17418 MFH
        if (isset($_POST[$prefix.'duration_hours'])) {
            $_POST[$prefix.'duration_hours'] = trim($_POST[$prefix.'duration_hours']);
        }

        $focus = BeanFactory::newBean('Calls');

        if ($useRequired && !checkRequired($prefix, array_keys($focus->required_fields))) {
            return null;
        }
        if (!isset($_POST[$prefix.'reminder_checked']) || $_POST[$prefix.'reminder_checked'] == 0) {
            $GLOBALS['log']->debug(__FILE__.'('.__LINE__.'): No reminder checked, resetting the reminder_time');
            $_POST[$prefix.'reminder_time'] = -1;
        }

        if (!isset($_POST[$prefix.'reminder_time'])) {
            $GLOBALS['log']->debug(__FILE__.'('.__LINE__.'): Getting the users default reminder time');
            $_POST[$prefix.'reminder_time'] = $current_user->getPreference('reminder_time');
        }

        if (!isset($_POST['email_reminder_checked']) || (isset($_POST['email_reminder_checked']) && $_POST['email_reminder_checked'] == '0')) {
            $_POST['email_reminder_time'] = -1;
        }
        if (!isset($_POST['email_reminder_time'])) {
            $_POST['email_reminder_time'] = $current_user->getPreference('email_reminder_time');
            $_POST['email_reminder_checked'] = 1;
        }

        // don't allow to set recurring_source from a form
        unset($_POST['recurring_source']);

        $time_format = $timedate->get_user_time_format();
        $time_separator = ":";
        if (preg_match('/\d+([^\d])\d+([^\d]*)/s', (string) $time_format, $match)) {
            $time_separator = $match[1];
        }

        if (!empty($_POST[$prefix.'time_hour_start']) && empty($_POST[$prefix.'time_start'])) {
            $_POST[$prefix.'time_start'] = $_POST[$prefix.'time_hour_start']. $time_separator .$_POST[$prefix.'time_minute_start'];
        }

        if (isset($_POST[$prefix.'meridiem']) && !empty($_POST[$prefix.'meridiem'])) {
            $_POST[$prefix.'time_start'] = $timedate->merge_time_meridiem($_POST[$prefix.'time_start'], $timedate->get_time_format(), $_POST[$prefix.'meridiem']);
        }

        if (isset($_POST[$prefix.'time_start']) && strlen((string) $_POST[$prefix.'date_start']) == 10) {
            $_POST[$prefix.'date_start'] = $_POST[$prefix.'date_start'] . ' ' . $_POST[$prefix.'time_start'];
        }

        // retrieve happens here
        $focus = populateFromPost($prefix, $focus);
        if (!$focus->ACLAccess('Save')) {
            ACLController::displayNoAccess(true);
            sugar_cleanup(true);
        }

        $newBean = true;
        if (!empty($focus->id)) {
            $newBean = false;
        }

        $return_id = '';
        $assignedUserId = $_POST['assigned_user_id'] ?? '';
        //add assigned user and current user if this is the first time bean is saved
        if (empty($focus->id) && !empty($_REQUEST['return_module']) && $_REQUEST['return_module'] =='Calls' && !empty($_REQUEST['return_action']) && $_REQUEST['return_action'] =='DetailView') {
            //if return action is set to detail view and return module to call, then this is from the long form, do not add the assigned user (only the current user)
            //The current user is already added to UI and we want to give the current user the option of opting out of meeting.
            if ($current_user->id != $assignedUserId) {
                $_POST['user_invitees'] .= ','.$assignedUserId.', ';
                $_POST['user_invitees'] = str_replace(',,', ',', $_POST['user_invitees']);
            }
        } else {
            //this is not from long form so add assigned and current user automatically as there is no invitee list UI.
            //This call could be through an ajax call from subpanels or shortcut bar
            $_POST['user_invitees'] .= ','.$assignedUserId.', ';

            //add current user if the assigned to user is different than current user.
            if ($current_user->id != $assignedUserId && $_REQUEST['module'] != "Calendar") {
                $_POST['user_invitees'] .= ','.$current_user->id.', ';
            }

            //remove any double commas introduced during appending
            $_POST['user_invitees'] = str_replace(',,', ',', $_POST['user_invitees']);
        }

        if ((isset($_POST['isSaveFromDetailView']) && $_POST['isSaveFromDetailView'] == 'true') ||
        (isset($_POST['is_ajax_call']) && !empty($_POST['is_ajax_call']) && !empty($focus->id) ||
        (isset($_POST['return_action']) && $_POST['return_action'] == 'SubPanelViewer') && !empty($focus->id))
    ) {
            $focus->save(true);
            $return_id = $focus->id;
        } else {
            if ($focus->status == 'Held' && $this->isEmptyReturnModuleAndAction() && !$this->isSaveFromDCMenu()) {
                //if we are closing the meeting, and the request does not have a return module AND return action set and it is not a save
                //being triggered by the DCMenu (shortcut bar) then the request is coming from a dashlet or subpanel close icon and there is no
                //need to process user invitees, just save the current values.
                $focus->save(true);
            } else {
                ///////////////////////////////////////////////////////////////////////////
                ////	REMOVE INVITEE RELATIONSHIPS
                if (!empty($_POST['user_invitees'])) {
                    $userInvitees = explode(',', trim($_POST['user_invitees'], ','));
                } else {
                    $userInvitees = array();
                }

                // Calculate which users to flag as deleted and which to add
                $deleteUsers = array();
                $focus->load_relationship('users');
                // Get all users for the call
                $q = 'SELECT mu.user_id, mu.accept_status FROM calls_users mu WHERE mu.call_id = \''.$focus->id.'\'';
                $r = $focus->db->query($q);
                $acceptStatusUsers = array();
                while ($a = $focus->db->fetchByAssoc($r)) {
                    if (!in_array($a['user_id'], $userInvitees)) {
                        $deleteUsers[$a['user_id']] = $a['user_id'];
                    } else {
                        $acceptStatusUsers[$a['user_id']] = $a['accept_status'];
                    }
                }

                if (count($deleteUsers) > 0) {
                    $sql = '';
                    foreach ($deleteUsers as $u) {
                        $sql .= ",'" . $u . "'";
                    }

                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE calls_users set deleted = 1 where user_id in ($sql) AND call_id = '". $focus->id . "'";
                    $focus->db->query($sql);
                }

                // Get all contacts for the call
                if (!empty($_POST['contact_invitees'])) {
                    $contactInvitees = explode(',', trim($_POST['contact_invitees'], ','));
                } else {
                    $contactInvitees = array();
                }

                $deleteContacts = array();
                $focus->load_relationship('contacts');
                $q = 'SELECT mu.contact_id, mu.accept_status FROM calls_contacts mu WHERE mu.call_id = \''.$focus->id.'\'';
                $r = $focus->db->query($q);
                $acceptStatusContacts = array();
                while ($a = $focus->db->fetchByAssoc($r)) {
                    if (!in_array($a['contact_id'], $contactInvitees)) {
                        $deleteContacts[$a['contact_id']] = $a['contact_id'];
                    } else {
                        $acceptStatusContacts[$a['contact_id']] = $a['accept_status'];
                    }
                }

                if (count($deleteContacts) > 0) {
                    $sql = '';
                    foreach ($deleteContacts as $u) {
                        $sql .= ",'" . $u . "'";
                    }
                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE calls_contacts set deleted = 1 where contact_id in ($sql) AND call_id = '". $focus->id . "'";
                    $focus->db->query($sql);
                }
                if (!empty($_POST['lead_invitees'])) {
                    $leadInvitees = explode(',', trim($_POST['lead_invitees'], ','));
                } else {
                    $leadInvitees = array();
                }

                // Calculate which leads to flag as deleted and which to add
                $deleteLeads = array();
                $focus->load_relationship('leads');
                // Get all leads for the call
                $q = 'SELECT mu.lead_id, mu.accept_status FROM calls_leads mu WHERE mu.call_id = \''.$focus->id.'\'';
                $r = $focus->db->query($q);
                $acceptStatusLeads = array();
                while ($a = $focus->db->fetchByAssoc($r)) {
                    if (!in_array($a['lead_id'], $leadInvitees)) {
                        $deleteLeads[$a['lead_id']] = $a['lead_id'];
                    } else {
                        $acceptStatusLeads[$a['user_id']] = $a['accept_status'];
                    }
                }

                if (count($deleteLeads) > 0) {
                    $sql = '';
                    foreach ($deleteLeads as $u) {
                        // make sure we don't delete the assigned user
                        if ($u != $focus->assigned_user_id) {
                            $sql .= ",'" . $u . "'";
                        }
                    }
                    $sql = substr($sql, 1);
                    // We could run a delete SQL statement here, but will just mark as deleted instead
                    $sql = "UPDATE calls_leads set deleted = 1 where lead_id in ($sql) AND call_id = '". $focus->id . "'";
                    $focus->db->query($sql);
                }
                ////	END REMOVE
                ///////////////////////////////////////////////////////////////////////////


                ///////////////////////////////////////////////////////////////////////////
                ////	REBUILD INVITEE RELATIONSHIPS
                $focus->users_arr = array();
                $focus->users_arr = $userInvitees;
                $focus->contacts_arr = array();
                $focus->contacts_arr = $contactInvitees;
                $focus->leads_arr = array();
                $focus->leads_arr = $leadInvitees;
                if (!empty($_POST['parent_id']) && $_POST['parent_type'] == 'Contacts') {
                    $focus->contacts_arr[] = $_POST['parent_id'];
                }
                if (!empty($_POST['parent_id']) && $_POST['parent_type'] == 'Leads') {
                    $focus->leads_arr[] = $_POST['parent_id'];
                }
                // Call the Call module's save function to handle saving other fields besides
            // the users and contacts relationships
            $focus->update_vcal = false;    // Bug #49195 : don't update vcal b/s related users aren't saved yet, create vcal cache below
            $focus->save(true);
                $return_id = $focus->id;

                // Process users
                $existing_users = array();
                if (!empty($_POST['existing_invitees'])) {
                    $existing_users =  explode(",", trim($_POST['existing_invitees'], ','));
                }

                foreach ($focus->users_arr as $user_id) {
                    if (empty($user_id) || isset($existing_users[$user_id]) || isset($deleteUsers[$user_id])) {
                        continue;
                    }

                    if (!isset($acceptStatusUsers[$user_id])) {
                        $focus->load_relationship('users');
                        $focus->users->add($user_id);
                    } else {
                        // update query to preserve accept_status
                        $qU  = 'UPDATE calls_users SET deleted = 0, accept_status = \''.$acceptStatusUsers[$user_id].'\' ';
                        $qU .= 'WHERE call_id = \''.$focus->id.'\' ';
                        $qU .= 'AND user_id = \''.$user_id.'\'';
                        $focus->db->query($qU);
                    }
                }

                // Process contacts
                $existing_contacts =  array();
                if (!empty($_POST['existing_contact_invitees'])) {
                    $existing_contacts =  explode(",", trim($_POST['existing_contact_invitees'], ','));
                }

                foreach ($focus->contacts_arr as $contact_id) {
                    if (empty($contact_id) || isset($existing_contacts[$contact_id]) || (isset($deleteContacts[$contact_id]) && $contact_id !=  $_POST['parent_id'])) {
                        continue;
                    }

                    if (!isset($acceptStatusContacts[$contact_id])) {
                        $focus->load_relationship('contacts');
                        $focus->contacts->add($contact_id);
                    } else {
                        // update query to preserve accept_status
                        $qU  = 'UPDATE calls_contacts SET deleted = 0, accept_status = \''.$acceptStatusContacts[$contact_id].'\' ';
                        $qU .= 'WHERE call_id = \''.$focus->id.'\' ';
                        $qU .= 'AND contact_id = \''.$contact_id.'\'';
                        $focus->db->query($qU);
                    }
                }
                // Process leads
                $existing_leads =  array();
                if (!empty($_POST['existing_lead_invitees'])) {
                    $existing_leads =  explode(",", trim($_POST['existing_lead_invitees'], ','));
                }

                foreach ($focus->leads_arr as $lead_id) {
                    if (empty($lead_id) || isset($existing_leads[$lead_id]) || (isset($deleteLeads[$lead_id]) && $lead_id !=  $_POST['parent_id'])) {
                        continue;
                    }

                    if (!isset($acceptStatusLeads[$lead_id])) {
                        $focus->load_relationship('leads');
                        $focus->leads->add($lead_id);
                    } else {
                        // update query to preserve accept_status
                        $qU  = 'UPDATE calls_leads SET deleted = 0, accept_status = \''.$acceptStatusLeads[$lead_id].'\' ';
                        $qU .= 'WHERE call_id = \''.$focus->id.'\' ';
                        $qU .= 'AND lead_id = \''.$lead_id.'\'';
                        $focus->db->query($qU);
                    }
                }

                // Bug #49195 : update vcal
                vCal::cache_sugar_vcal($current_user);

                // CCL - Comment out call to set $current_user as invitee
                //set organizer to auto-accept
                if ($focus->assigned_user_id == $current_user->id && $newBean) {
                    $focus->set_accept_status($current_user, 'accept');
                }

                ////	END REBUILD INVITEE RELATIONSHIPS
            ///////////////////////////////////////////////////////////////////////////
            }
        }

        if (!empty($_POST['is_ajax_call'])) {
            $json = getJSONobj();
            echo $json->encode(array('status' => 'success', 'get' => ''));
            exit;
        }

        if (isset($_REQUEST['return_module']) && $_REQUEST['return_module'] == 'Home') {
            $_REQUEST['return_action'] = 'index';
            handleRedirect('', 'Home');
        } else {
            if ($redirect) {
                handleRedirect($return_id, 'Calls');
            } else {
                return $focus;
            }
        }
    } // end handleSave();

    public function getWideFormBody($prefix, $mod='', $formname='', $wide =true)
    {
        if (!ACLController::checkAccess('Calls', 'edit', true)) {
            return '';
        }
        global $mod_strings;
        $temp_strings = $mod_strings;
        if (!empty($mod)) {
            global $current_language;
            $mod_strings = return_module_language($current_language, $mod);
        }
        global $app_strings;
        global $app_list_strings;
        global $current_user;
        global $theme;

        $lbl_subject = $mod_strings['LBL_SUBJECT'];
        // Unimplemented until jscalendar language files are fixed
        // global $current_language;
        // global $default_language;
        // global $cal_codes;
        // Unimplemented until jscalendar language files are fixed
        // $cal_lang = (empty($cal_codes[$current_language])) ? $cal_codes[$default_language] : $cal_codes[$current_language];
        $cal_lang = "en";


        $lbl_required_symbol = $app_strings['LBL_REQUIRED_SYMBOL'];
        $lbl_date = $mod_strings['LBL_DATE'];
        $lbl_time = $mod_strings['LBL_TIME'];
        global $timedate;
        $ntc_date_format = '('.$timedate->get_user_date_format(). ')';
        $ntc_time_format = '('.$timedate->get_user_time_format(). ')';
        $cal_dateformat = $timedate->get_cal_date_format();

        $user_id = $current_user->id;
        $default_status = $app_list_strings['call_status_default'];
        $default_parent_type= $app_list_strings['record_type_default_key'];
        $date = TimeDate::getInstance()->nowDb();
        $default_date_start = $timedate->to_display_date($date);
        $default_time_start = $timedate->to_display_time($date, true);
        $time_ampm = $timedate->AMPMMenu($prefix, $default_time_start);
        $form =	<<<EOQ
			<input type="hidden"  name="{$prefix}direction" value="Outbound">
			<input type="hidden" name="{$prefix}record" value="">
			<input type="hidden" name="{$prefix}status" value="{$default_status}">
			<input type="hidden" name="{$prefix}parent_type" value="{$default_parent_type}">
			<input type="hidden" name="{$prefix}assigned_user_id" value='{$user_id}'>
			<input type="hidden" name="{$prefix}duration_hours" value="1">
			<input type="hidden" name="{$prefix}duration_minutes" value="0">
			<input type="hidden" name="{$prefix}user_id" value="{$user_id}">

		<table cellspacing='0' cellpadding='0' border='0' width="100%">
<tr>
EOQ;

        if ($wide) {
            $form .= <<<EOQ
<td scope='row' width="20%"><input type='radio' name='appointment' value='Call' class='radio' checked> {$mod_strings['LNK_NEW_CALL']}</td>
<td scope='row' width="80%">{$mod_strings['LBL_DESCRIPTION']}</td>
</tr>

<tr>
<td scope='row'><input type='radio' name='appointment' value='Meeting' class='radio'> {$mod_strings['LNK_NEW_MEETING']}</td>

<td rowspan='8' ><textarea name='Appointmentsdescription' cols='50' rows='5'></textarea></td>
</tr>
EOQ;
        } else {
            $form .= <<<EOQ
<td scope='row' width="20%"><input type='radio' name='appointment' value='Call' class='radio' onchange='document.$formname.module.value="Calls";' checked> {$mod_strings['LNK_NEW_CALL']}</td>
</tr>

<tr>
<td scope='row'><input type='radio' name='appointment' value='Meeting' class='radio' onchange='document.$formname.module.value="Meetings";'> {$mod_strings['LNK_NEW_MEETING']}</td>
</tr>
EOQ;
        }
        $form .=	<<<EOQ


<tr>
<td scope='row'>$lbl_subject&nbsp;<span class="required">$lbl_required_symbol</span></td>
</tr>

<tr>
<td ><input name='{$prefix}name' maxlength='255' type="text"></td>
</tr>

<tr>
<td scope='row'>$lbl_date&nbsp;<span class="required">$lbl_required_symbol</span>&nbsp;<span class="dateFormat">$ntc_date_format</span></td>
</tr>
<tr>
<td ><input onblur="parseDate(this, '$cal_dateformat');" name='{$prefix}date_start' size="12" id='{$prefix}jscal_field' maxlength='10' type="text" value="{$default_date_start}"> <!--not_in_theme!-->
<span class="suitepicon suitepicon-module-calendar" alt="{$app_strings['LBL_ENTER_DATE']}"  id="{$prefix}jscal_trigger"></span>
</td>
</tr>

<tr>
<td scope='row'>$lbl_time&nbsp;<span class="required">$lbl_required_symbol</span>&nbsp;<span class="dateFormat">$ntc_time_format</span></td>
</tr>
<tr>
<td ><input name='{$prefix}time_start' size="12" type="text" maxlength='5' value="{$default_time_start}">$time_ampm</td>
</tr>

</table>

		<script type="text/javascript">
		Calendar.setup ({
			inputField : "{$prefix}jscal_field", daFormat : "$cal_dateformat", ifFormat : "$cal_dateformat", showsTime : false, button : "{$prefix}jscal_trigger", singleClick : true, step : 1, weekNumbers:false
		});
		</script>
EOQ;


        $javascript = new javascript();
        $javascript->setFormName($formname);
        $javascript->setSugarBean(BeanFactory::newBean('Calls'));
        $javascript->addRequiredFields($prefix);
        $form .=$javascript->getScript();
        $mod_strings = $temp_strings;
        return $form;
    }
}
