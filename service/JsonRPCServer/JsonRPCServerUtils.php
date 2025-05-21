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

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

#[\AllowDynamicProperties]
class JsonRPCServerUtils
{
    /**
     * @param $query_obj
     * @param string $table
     * @param null $module
     * @return string
     */
    public function constructWhere(&$query_obj, $table = '', $module = null)
    {
        if (!empty($table)) {
            $table .= '.';
        }
        $cond_arr = array();

        if (!is_array($query_obj['conditions'])) {
            $query_obj['conditions'] = array();
        }

        foreach ($query_obj['conditions'] as $condition) {
            if ($condition['name'] === 'user_hash') {
                continue;
            }
            if ($condition['name'] === 'email1' || $condition['name'] === 'email2') {

                $email1_value = strtoupper($condition['value']);
                $email1_condition = " {$table}id in ( SELECT  er.bean_id AS id FROM email_addr_bean_rel er, " .
                    'email_addresses ea WHERE ea.id = er.email_address_id ' .
                    "AND ea.deleted = 0 AND er.deleted = 0 AND er.bean_module = '{$module}' AND email_address_caps LIKE '%{$email1_value}%' )";

                $cond_arr[] = $email1_condition;
            } else {
                if ($condition['op'] === 'contains') {
                    $cond_arr[] = $table . DBManagerFactory::getInstance()->getValidDBName($condition['name']) . " like '%" . DBManagerFactory::getInstance()->quote($condition['value']) . "%'";
                }
                if ($condition['op'] === 'like_custom') {
                    $like = '';
                    if (!empty($condition['begin'])) {
                        $like .= DBManagerFactory::getInstance()->quote($condition['begin']);
                    }
                    $like .= DBManagerFactory::getInstance()->quote($condition['value']);
                    if (!empty($condition['end'])) {
                        $like .= DBManagerFactory::getInstance()->quote($condition['end']);
                    }
                    $cond_arr[] = $table . DBManagerFactory::getInstance()->getValidDBName($condition['name']) . " like '$like'";
                } else { // starts_with
                    $cond_arr[] = $table . DBManagerFactory::getInstance()->getValidDBName($condition['name']) . " like '" . DBManagerFactory::getInstance()->quote($condition['value']) . "%'";
                }
            }
        }

        if ($table === 'users.') {
            $cond_arr[] = $table . "status='Active'";
        }
        $queryObject = $query_obj['group'] ?? '';
        $group = strtolower(trim($queryObject));
        if ($group !== 'and' && $group !== 'or') {
            $group = 'and';
        }

        return implode(" $group ", $cond_arr);
    }

    /**
     * Authenticates User
     * @return null|User
     */
    public function authenticate()
    {
        global $sugar_config;
        global $log;

        $user_unique_key = isset($_SESSION['unique_key']) ? $_SESSION['unique_key'] : '';
        $server_unique_key = isset($sugar_config['unique_key']) ? $sugar_config['unique_key'] : '';

        if ($user_unique_key !== $server_unique_key) {
            $log->debug('JSON_SERVER: user_unique_key:' . $user_unique_key . '!=' . $server_unique_key);
            session_destroy();

            return null;
        }

        if (!isset($_SESSION['authenticated_user_id'])) {
            $log->debug('JSON_SERVER: authenticated_user_id NOT SET. DESTROY');
            session_destroy();

            return null;
        }

        /**
         * @var User $current_user;
         */
        $current_user = BeanFactory::newBean('Users');

        $result = $current_user->retrieve($_SESSION['authenticated_user_id']);
        $GLOBALS['log']->debug('JSON_SERVER: retrieved user from SESSION');


        if ($result === null) {
            $GLOBALS['log']->debug('JSON_SERVER: could get a user from SESSION. DESTROY');
            session_destroy();

            return null;
        }

        return $result;
    }
}
