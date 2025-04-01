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

require_once('service/v4/SugarWebServiceUtilv4.php');

#[\AllowDynamicProperties]
class SugarWebServiceUtilv4_1 extends SugarWebServiceUtilv4
{
    /**
     * Validate the provided session information is correct and current.  Load the session.
     *
     * @param String $session_id -- The session ID that was returned by a call to login.
     * @return true -- If the session is valid and loaded.
     * @return false -- if the session is not valid.
     */
    public function validate_authenticated($session_id)
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->validate_authenticated');
        if (!empty($session_id)) {

            // only initialize session once in case this method is called multiple times
            if (!session_id()) {
                session_id($session_id);
                session_start();
            }

            if (!empty($_SESSION['is_valid_session']) && $this->is_valid_ip_address('ip_address') && $_SESSION['type'] == 'user') {
                global $current_user;
                require_once('modules/Users/User.php');
                $current_user = BeanFactory::newBean('Users');
                $current_user->retrieve($_SESSION['user_id']);
                $this->login_success();
                $GLOBALS['log']->info('Begin: SoapHelperWebServices->validate_authenticated - passed');
                $GLOBALS['log']->info('End: SoapHelperWebServices->validate_authenticated');
                return true;
            }

            $GLOBALS['log']->debug("calling destroy");
            session_destroy();
        }
        LogicHook::initialize();
        $GLOBALS['logic_hook']->call_custom_logic('Users', 'login_failed');
        $GLOBALS['log']->info('End: SoapHelperWebServices->validate_authenticated - validation failed');
        return false;
    }


    public function check_modules_access($user, $module_name, $action='write')
    {
        if (!isset($_SESSION['avail_modules'])) {
            $_SESSION['avail_modules'] = get_user_module_list($user);
        }
        if (isset($_SESSION['avail_modules'][$module_name])) {
            if ($action == 'write' && $_SESSION['avail_modules'][$module_name] == 'read_only') {
                if (is_admin($user)) {
                    return true;
                }
                return false;
            } elseif ($action == 'write' && strcmp(strtolower($module_name), 'users') == 0 && !$user->isAdminForModule($module_name)) {
                //rrs bug: 46000 - If the client is trying to write to the Users module and is not an admin then we need to stop them
                return false;
            }
            return true;
        }
        return false;
    }

    /**
     * getRelationshipResults
     * Returns the
     *
     * @param Mixed $bean The SugarBean instance to retrieve relationships from
     * @param String $link_field_name The name of the relationship entry to fetch relationships for
     * @param Array $link_module_fields Array of fields of relationship entries to return
     * @param string $optional_where String containing an optional WHERE select clause
     * @param string $order_by String containing field to order results by
     * @param Number $offset -- where to start in the return (defaults to 0)
     * @param Number $limit -- number of results to return (defaults to all)
     * @return array|bool Returns an Array of relationship results; false if relationship could not be retrieved
     */
    public function getRelationshipResults($bean, $link_field_name, $link_module_fields, $optional_where = '', $order_by = '', $offset = 0, $limit = '')
    {
        $GLOBALS['log']->info('Begin: SoapHelperWebServices->getRelationshipResults');
        require_once('include/TimeDate.php');
        global $beanList, $beanFiles, $current_user;
        global $disable_date_format, $timedate;

        $bean->load_relationship($link_field_name);

        if (isset($bean->$link_field_name)) {
            //First get all the related beans
            $params = array();
            $params['offset'] = $offset;
            $params['limit'] = $limit;

            if (!empty($optional_where)) {
                $params['where'] = $optional_where;
            }

            if (!empty($order_by)) {
                $params['order_by'] = $order_by;
            }

            $related_beans = $bean->$link_field_name->getBeans($params);
            //Create a list of field/value rows based on $link_module_fields
            $list = array();
            $filterFields = array();
            foreach ($related_beans as $id => $bean) {
                if (empty($filterFields) && !empty($link_module_fields)) {
                    $filterFields = $this->filter_fields($bean, $link_module_fields);
                }
                $row = array();
                foreach ($filterFields as $field) {
                    if (isset($bean->$field)) {
                        if (isset($bean->field_defs[$field]['type']) && $bean->field_defs[$field]['type'] == 'date') {
                            $row[$field] = $timedate->to_display_date_time($bean->$field);
                        } else {
                            $row[$field] = $bean->$field;
                        }
                    } else {
                        $row[$field] = "";
                    }
                }
                //Users can't see other user's hashes
                if (is_a($bean, 'User') && $current_user->id != $bean->id && isset($row['user_hash'])) {
                    $row['user_hash'] = "";
                }
                $row = clean_sensitive_data($bean->field_defs, $row);
                $list[] = $row;
            }
            $GLOBALS['log']->info('End: SoapHelperWebServices->getRelationshipResults');
            return array('rows' => $list, 'fields_set_on_rows' => $filterFields);
        }
        $GLOBALS['log']->info('End: SoapHelperWebServices->getRelationshipResults - ' . $link_field_name . ' relationship does not exists');
        return false;
        // else
    } // fn
}
