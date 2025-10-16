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


require_once 'modules/AOW_Actions/actions/actionBase.php';
class actionCreateRecord extends actionBase
{

    /**
     * @return array
     */
    public function loadJS()
    {
        return array('modules/AOW_Actions/actions/actionCreateRecord.js');
    }

    /**
     * @param $line
     * @param SugarBean|null $bean
     * @param array $params
     * @return string
     */
    public function edit_display($line, SugarBean $bean = null, $params = array())
    {
        global $app_list_strings;

        $modules = $app_list_strings['aow_moduleList'];
        $recordType = $params['record_type'] ?? '';

        $checked = 'CHECKED';
        if (isset($params['relate_to_workflow']) && !$params['relate_to_workflow']) {
            $checked = '';
        }
        $copy_email_addresses_checked = '';
        if (isset($params['copy_email_addresses']) && $params['copy_email_addresses']) {
            $copy_email_addresses_checked = 'CHECKED';
        }

        $html = "<table border='0' cellpadding='0' cellspacing='0' width='100%' data-workflow-action='create-record'>";
        $html .= '<tr>';
        $html .= '<td id="name_label" class="name_label" scope="row" valign="top"><label>' .
                 translate('LBL_RECORD_TYPE', 'AOW_Actions') .
                 '</label>:<span class="required">
*</span>&nbsp;&nbsp;';
        $html .= "<select name='aow_actions_param[".$line."][record_type]' id='aow_actions_param_record_type".$line."'  onchange='show_crModuleFields($line);'>".get_select_options_with_id($modules, $recordType). '</select></td>';
        $html .= '<td id="relate_label" class="relate_label" scope="row" valign="top"><label>' .
                 translate('LBL_RELATE_WORKFLOW', 'AOW_Actions') .
                 '</label>:';
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][relate_to_workflow]' value='0' >";
        $html .= "<input type='checkbox' id='aow_actions_param[".$line."][relate_to_workflow]' name='aow_actions_param[".$line."][relate_to_workflow]' value='1' $checked></td>";
        $html .= '<td id="copy_email_addresses_label" scope="row" valign="top">'.translate("LBL_COPY_EMAIL_ADDRESSES_WORKFLOW", "AOW_Actions").':&nbsp;&nbsp;';
        $html .= "<input type='hidden' name='aow_actions_param[".$line."][copy_email_addresses]' value='0' >";
        $html .= "<input type='checkbox' id='aow_actions_param[".$line."][copy_email_addresses]' name='aow_actions_param[".$line."][copy_email_addresses]' value='1' $copy_email_addresses_checked></td>";
        $html .= '</tr>';
        $html .= '<tr>';
        $html .= '<td colspan="4" scope="row"><table id="crLine' .
                 $line .
                 '_table" width="100%" class="lines"></table></td>';
        $html .= '</tr>';
        $html .= '<tr>';
        $html .= '<td colspan="4" scope="row"><input type="button" tabindex="116" style="display:none" class="button" value="'.translate(
            'LBL_ADD_FIELD',
            'AOW_Actions'
        ).'" id="addcrline'.$line.'" onclick="add_crLine('.$line.')" /></td>';
        $html .= '</tr>';
        $html .= '<tr>';
        $html .= '<td colspan="4" scope="row"><table id="crRelLine'.$line.'_table" width="100%" class="relationship"></table></td>';
        $html .= '</tr>';
        $html .= '<tr>';
        $html .= '<td colspan="4" scope="row"><input type="button" tabindex="116" style="display:none" class="button" value="'.translate(
            'LBL_ADD_RELATIONSHIP',
            'AOW_Actions'
        ).'" id="addcrrelline'.$line.'" onclick="add_crRelLine('.$line.')" /></td>';
        $html .= '</tr>';


        if (isset($params['record_type']) && $params['record_type'] != '') {
            require_once 'modules/AOW_WorkFlow/aow_utils.php';
            $html .= "<script id ='aow_script".$line."'>";
            $html .= 'cr_fields[' . $line . '] = "' . trim(preg_replace(
                '/\s+/',
                ' ',
                (string) getModuleFields(
                        $params['record_type'],
                        'EditView',
                        '',
                        array(),
                        array('email1', 'email2')
                    )
            )) . '";';
            $html .= 'cr_relationships[' . $line . '] = "' . trim(preg_replace(
                '/\s+/',
                ' ',
                (string) getModuleRelationships($params['record_type'])
            )) . '";';
            $html .= 'cr_module[' .$line. '] = "' .$params['record_type']. '";';
            if (isset($params['field'])) {
                foreach ($params['field'] as $key => $field) {
                    if (is_array($params['value'][$key])) {
                        $params['value'][$key] = json_encode($params['value'][$key]);
                    }

                    $html .= "load_crline('".$line."','".$field."','".str_replace(array("\r\n","\r","\n"), ' ', (string) $params['value'][$key])."','".$params['value_type'][$key]."');";
                }
            }
            if (isset($params['rel'])) {
                foreach ($params['rel'] as $key => $field) {
                    if (is_array($params['rel_value'][$key])) {
                        $params['rel_value'][$key] = json_encode($params['rel_value'][$key]);
                    }

                    $html .= "load_crrelline('".$line."','".$field."','".$params['rel_value'][$key]."','".$params['rel_value_type'][$key]."');";
                }
            }
            $html .= '</script>';
        }
        return $html;
    }

    /**
     * @param SugarBean $bean
     * @param array $params
     * @param bool $in_save
     * @return bool
     */
    public function run_action(SugarBean $bean, $params = array(), $in_save = false)
    {
        global $beanList;

        if (isset($params['record_type']) && $params['record_type'] != '') {
            if ($beanList[$params['record_type']]) {
                $record = new $beanList[$params['record_type']]();
                $this->set_record($record, $bean, $params);
                $this->set_relationships($record, $bean, $params);
                $invalidEmails = $this->copyEmailAddresses($record, $bean, $params);
                if ($invalidEmails > 0) {
                    LoggerManager::getLogger()->warn("Given bean contains $invalidEmails invalid Email address(es).");
                }
                if ($invalidEmails < 0) {
                    LoggerManager::getLogger()->error("Email address copy error occured, bean was: $bean->module_name");
                }

                if (isset($params['relate_to_workflow']) && $params['relate_to_workflow']) {
                    require_once 'modules/Relationships/Relationship.php';
                    $dbManager = DBManagerFactory::getInstance();
                    $key = Relationship::retrieve_by_modules($bean->module_dir, $record->module_dir, $dbManager);
                    if (!empty($key)) {
                        foreach ($bean->field_defs as $field=>$def) {
                            if ($def['type'] == 'link' && !empty($def['relationship']) && $def['relationship'] == $key) {
                                $bean->load_relationship($field);
                                $bean->$field->add($record->id);
                                break;
                            }
                        }
                    }
                }
                return true;
            }
        }
        return false;
    }

    /**
     * @param SugarBean $record
     * @param SugarBean $bean
     * @param array $params
     * @param bool $in_save
     */
    public function set_record(SugarBean $record, SugarBean $bean, $params = array(), $in_save = false)
    {
        global $app_list_strings, $timedate;

        $record_vardefs = $record->getFieldDefinitions();

        if (isset($params['field'])) {
            $retrieved = false;
            foreach ($params['field'] as $key => $field) {
                if ($field === '') {
                    continue;
                }
                $value = '';
                switch ($params['value_type'][$key]) {
                    case 'Field':
                        if ($params['value'][$key] === '') {
                            continue 2;
                        }
                        $fieldName = $params['value'][$key];
                        $data = $bean->field_defs[$fieldName];

                        switch ($data['type']) {
                            case 'double':
                            case 'decimal':
                            case 'currency':
                            case 'float':
                            case 'uint':
                            case 'ulong':
                            case 'long':
                            case 'short':
                            case 'tinyint':
                            case 'int':
                                $value = format_number($bean->$fieldName);
                                break;
                case 'relate':
                    if (isset($data['id_name']) && $record_vardefs[$field]['type'] === 'relate') {
                        $idName = $data['id_name'];
                        $value = $bean->$idName;
                    } else {
                        $value = $bean->$fieldName;
                    }
                break;
                            default:
                                $value = $bean->$fieldName;
                                break;
                        }
                        break;
                    case 'Date':
                        $dformat = 'Y-m-d H:i:s';
                        if ($record_vardefs[$field]['type'] === 'date') {
                            $dformat = 'Y-m-d';
                        }
                        switch ($params['value'][$key][3]) {
                            case 'business_hours':
                                require_once 'modules/AOBH_BusinessHours/AOBH_BusinessHours.php';

                                $businessHours = BeanFactory::newBean('AOBH_BusinessHours');

                                $dateToUse = $params['value'][$key][0];
                                $sign = $params['value'][$key][1];
                                $amount = $params['value'][$key][2];
                                $type = $bean->field_defs[$dateToUse]['type'] ?? 'datetime';

                                if ($sign !== 'plus') {
                                    $amount = 0-$amount;
                                }
                                if ($dateToUse === 'now') {
                                    $value = $businessHours->addBusinessHours($amount);
                                } elseif ($dateToUse === 'field') {
                                    $dateToUse = $params['field'][$key];
                                    $type = $bean->field_defs[$dateToUse]['type'] ?? 'datetime';

                                    if ($bean->$dateToUse === '') {
                                        $date = gmdate($dformat);
                                        $value = $businessHours->addBusinessHours($amount, $timedate->fromDbType($date, $record_vardefs[$field]['type']));
                                        $value = $timedate->asDbType($value, $record_vardefs[$field]['type']);
                                        break;
                                    }

                                    $date = $bean->$dateToUse;
                                    $value = $businessHours->addBusinessHours($amount, $timedate->fromDbType($date, $type));
                                } elseif ($dateToUse === 'today') {
                                    $date = gmdate('Y-m-d').' 00:00:00';
                                    $value = $businessHours->addBusinessHours($amount, $timedate->fromDb($date));
                                } else {
                                    $date = $bean->$dateToUse;
                                    if ($date === '') {
                                        $date = gmdate($dformat);
                                    }
                                    $value = $businessHours->addBusinessHours($amount, $timedate->fromDbType($date, $type));
                                }
                                $value = $timedate->asDbType($value, $type);
                                break;
                            default:
                                if ($params['value'][$key][0] === 'now') {
                                    $date = gmdate($dformat);
                                } elseif ($params['value'][$key][0] === 'field') {
                                    $dateToUse = $params['field'][$key];
                                    $date = $record->$dateToUse;
                                } elseif ($params['value'][$key][0] === 'today') {
                                    $date = $params['value'][$key][0];
                                } else {
                                    $dateToUse = $params['value'][$key][0];
                                    if (!$retrieved) {
                                        $bean->retrieve($bean->id);
                                        $retrieved = true;
                                    }
                                    $type = $bean->field_defs[$dateToUse]['type'] ?? 'datetime';
                                    $date = $timedate->fromUserType($bean->$dateToUse, $type);
                                    $date !== null ? $date->asDB() : gmdate($dformat);
                                }

                                if ($params['value'][$key][1] !== 'now') {
                                    $value = date($dformat, strtotime($date . ' '.$app_list_strings['aow_date_operator'][$params['value'][$key][1]].$params['value'][$key][2].' '.$params['value'][$key][3]));
                                } else {
                                    $value = date($dformat, strtotime($date));
                                }
                                break;
                        }
                        break;
                    case 'Round_Robin':
                    case 'Least_Busy':
                    case 'Random':
                        switch ($params['value'][$key][0]) {
                            case 'security_group':
                                require_once 'modules/SecurityGroups/SecurityGroup.php';
                                $security_group = BeanFactory::newBean('SecurityGroups');
                                $security_group->retrieve($params['value'][$key][1]);
                                $group_users = $security_group->get_linked_beans('users', 'User');
                                $users = array();
                                $r_users = array();
                                if ($params['value'][$key][2] != '') {
                                    require_once 'modules/ACLRoles/ACLRole.php';
                                    $role = BeanFactory::newBean('ACLRoles');
                                    $role->retrieve($params['value'][$key][2]);
                                    $role_users = $role->get_linked_beans('users', 'User');
                                    foreach ($role_users as $role_user) {
                                        $r_users[$role_user->id] = $role_user->name;
                                    }
                                }
                                foreach ($group_users as $group_user) {
                                    if ($params['value'][$key][2] != '' && !isset($r_users[$group_user->id])) {
                                        continue;
                                    }
                                    $users[$group_user->id] = $group_user->name;
                                }
                                break;
                            case 'role':
                                require_once 'modules/ACLRoles/ACLRole.php';
                                $role = BeanFactory::newBean('ACLRoles');
                                $role->retrieve($params['value'][$key][2]);
                                $role_users = $role->get_linked_beans('users', 'User');
                                $users = array();
                                foreach ($role_users as $role_user) {
                                    $users[$role_user->id] = $role_user->name;
                                }
                                break;
                            case 'all':
                            default:
                                $users = get_user_array(false);
                                break;
                        }

                        // format the users array
                        $users = array_values(array_flip($users));

                        if (empty($users)) {
                            $value = '';
                        } elseif (count($users) == 1) {
                            $value = $users[0];
                        } else {
                            switch ($params['value_type'][$key]) {
                                case 'Round_Robin':
                                    $value = getRoundRobinUser($users, $this->id);
                                    break;
                                case 'Least_Busy':
                                    $user_id = 'assigned_user_id';
                                    if (isset($record_vardefs[$field]['id_name']) && $record_vardefs[$field]['id_name'] != '') {
                                        $user_id = $record_vardefs[$field]['id_name'];
                                    }
                                    $value = getLeastBusyUser($users, $user_id, $record);
                                    break;
                                case 'Random':
                                default:
                                    shuffle($users);
                                    $value = $users[0];
                                    break;
                            }
                        }
                        setLastUser($value, $this->id);

                        break;
                    case 'Value':
                    default:
                        $value = $params['value'][$key];
                        break;
                }

                if ($record_vardefs[$field]['type'] === 'relate' && isset($record_vardefs[$field]['id_name'])) {
                    $field = $record_vardefs[$field]['id_name'];
                }
                $record->$field = $value;
            }
        }

        $bean_processed = isset($record->processed) ? $record->processed : false;

        $assignedUserId = $record->fetched_row['assigned_user_id'] ?? '';

        if ($in_save) {
            global $current_user;
            $record->processed = true;
            $check_notify = $record->assigned_user_id != $current_user->id && $record->assigned_user_id != $assignedUserId;
        } else {
            $check_notify = $record->assigned_user_id != $assignedUserId;
        }

        $record->process_save_dates =false;
        $record->new_with_id = false;

        /* Since we only work on non-deleted records this means the delete field
         * was set during this action.
         * Complete the deletion process by calling mark_deleted() after save() */
        $was_deleted = false;
        if ($record->deleted) {
            $record->deleted = 0;
            $was_deleted = true;
        }

        $record->save($check_notify);

        if ($was_deleted) {
            $record->mark_deleted($record->id);
        }

        $record->processed = $bean_processed;
    }

    /**
     * @param SugarBean $record
     * @param SugarBean $bean
     * @param array $params
     */
    public function set_relationships(SugarBean $record, SugarBean $bean, $params = array())
    {
        $record_vardefs = $record->getFieldDefinitions();

        require_once 'modules/Relationships/Relationship.php';
        if (isset($params['rel'])) {
            foreach ($params['rel'] as $key => $field) {
                if ($field == '' || $params['rel_value'][$key] == '') {
                    continue;
                }

                $relField = $params['rel_value'][$key];

                switch ($params['rel_value_type'][$key]) {
                    case 'Field':

                        $data = $bean->field_defs[$relField];

                        if ($data['type'] == 'relate' && isset($data['id_name'])) {
                            $relField = $data['id_name'];
                        }
                        $rel_id = $bean->$relField;
                        break;
                    default:
                        $rel_id = $relField;
                        break;
                }

                $def = $record_vardefs[$field];
                if ($def['type'] == 'link' && !empty($def['relationship'])) {
                    $record->load_relationship($field);
                    $record->$field->add($rel_id);
                }
            }
        }
    }

    /**
     *
     * @param SugarBean $toBean
     * @param SugarBean $fromBean
     * @param array $params
     * @return int Number of invalid email addresses found in $fromBean's email addresses argument. Negative numbers are error code
     */
    protected function copyEmailAddresses(SugarBean $toBean, SugarBean $fromBean, $params = array())
    {
        $ret = 0;
        if (isset($params['copy_email_addresses']) && $params['copy_email_addresses']) {
            $toBean->addresses = $fromBean->addresses;
            $toBean->email1 = $fromBean->email1;
            $toBean->email2 = $fromBean->email2;
            if (isset($fromBean->emailAddress) && $fromBean->emailAddress instanceof SugarEmailAddress) {
                $tmp_sea2 = new SugarEmailAddress();
                foreach ($fromBean->emailAddress->addresses as $currentEmailAddress) {
                    if ($this->validateCurrentEmailAddress($currentEmailAddress)) {
                        $ret++;
                    }
                    $tmp_sea2->addAddress(
                        $currentEmailAddress['email_address'],
                        $currentEmailAddress['primary_address'],
                        $currentEmailAddress['reply_to_address'],
                        $currentEmailAddress['invalid_email'],
                        $currentEmailAddress['opt_out'],
                        $currentEmailAddress['email_address_id']
                    );
                }
                $tmp_sea2->saveEmail($toBean->id, $toBean->module_name);
            } else {
                // exception
                LoggerManager::getLogger()->error('From-bean should implement emailAddress. Given bean is ' . $fromBean->module_name);
                return -1;
            }
        } else {
            // exception
            LoggerManager::getLogger()->error('Given parameter should contains index "copy_email_addresses"');
            return -2;
        }

        return $ret;
    }

    /**
     *
     * @param arra $currentEmailAddress
     * @return bool Returns TRUE if it's a valid email address parameter, FALSE otherwise.
     */
    protected function validateCurrentEmailAddress($currentEmailAddress)
    {
        $ret = true;
        if (!isset($currentEmailAddress['email_address'])) {
            LoggerManager::getLogger()->warn('Index "email_address" is not set.');
            $ret = false;
        }
        if (!isset($currentEmailAddress['primary_address'])) {
            LoggerManager::getLogger()->warn('Index "primary_address" is not set.');
            $ret = false;
        }
        if (!isset($currentEmailAddress['reply_to_address'])) {
            LoggerManager::getLogger()->warn('Index "reply_to_address" is not set.');
            $ret = false;
        }
        if (!isset($currentEmailAddress['invalid_email'])) {
            LoggerManager::getLogger()->warn('Index "invalid_email" is not set.');
            $ret = false;
        }
        if (!isset($currentEmailAddress['opt_out'])) {
            LoggerManager::getLogger()->warn('Index "opt_out" is not set.');
            $ret = false;
        }
        if (!isset($currentEmailAddress['email_address_id'])) {
            LoggerManager::getLogger()->warn('Index "email_address_id" is not set.');
            $ret = false;
        }
        return $ret;
    }
}
