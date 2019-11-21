<?php
/**
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2025 SuiteCRM Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUITECRM, SUITECRM DISCLAIMS THE
 * WARRANTY OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License
 * version 3, these Appropriate Legal Notices must retain the display of the
 * "Supercharged by SuiteCRM" logo. If the display of the logos is not reasonably
 * feasible for technical reasons, the Appropriate Legal Notices must display
 * the words "Supercharged by SuiteCRM".
 */

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

use League\OAuth2\Server\RequestTypes\AuthorizationRequest;

/**
 * Class OAuth2AuthCodes
 */
class OAuth2AuthCodes extends SugarBean
{
    /**
     * @var string
     */
    public $table_name = 'oauth2authcodes';

    /**
     * @var string
     */
    public $object_name = 'OAuth2AuthCodes';

    /**
     * @var string
     */
    public $module_dir = 'OAuth2AuthCodes';

    /**
     * @var bool
     */
    public $disable_row_level_security = true;

    /**
     * @var bool
     */
    public $auth_code_is_revoked;

    /**
     * @var string
     */
    public $auth_code_expires;

    /**
     * @var string
     */
    public $auth_code;

    /**
     * @var string
     */
    public $scopes;

    /**
     * @var string
     */
    public $state;

    /**
     * @var string
     */
    public $client;

    /**
     * @see SugarBean::get_summary_text()
     */
    public function get_summary_text()
    {
        return substr($this->id, 0, 10) . '...';
    }

    /**
     * @return boolean
     * @throws Exception
     */
    public function is_revoked()
    {
        return $this->id === null || $this->auth_code_is_revoked === '1' || new \DateTime() > new \DateTime($this->auth_code_expires);
    }

    /**
     * @param AuthorizationRequest $authRequest
     * @return boolean
     */
    public function is_scope_authorized(AuthorizationRequest $authRequest)
    {
        $this->retrieve_by_string_fields([
            'client' => $authRequest->getClient()->getIdentifier(),
            'assigned_user_id' => $authRequest->getUser()->getIdentifier(),
            'auto_authorize' => '1',
        ]);

        // Check for scope changes here in future

        if($this->id === null){
            return false;
        }

        return true;
    }

    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ) {
        global $current_user, $db;

        $ret_array = parent::create_new_list_query(
            $order_by,
            $where,
            $filter,
            $params,
            $show_deleted,
            $join_type,
            true,
            $parentbean,
            $singleSelect,
            $ifListForExport
        );

        if (is_admin($current_user)) {
            if ($return_array) {
                return $ret_array;
            }

            return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
        }

        if (is_array($ret_array) && !empty($ret_array['where'])) {
            $tableName = $db->quote($this->table_name);
            $currentUserId = $db->quote($current_user->id);

            $ret_array['where'] .= " AND $tableName.assigned_user_id = '$currentUserId'";
        }

        if ($return_array) {
            return $ret_array;
        }

        return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
    }
}
