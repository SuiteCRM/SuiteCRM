<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

/**
 * OAuth provider
 */
#[\AllowDynamicProperties]
class ExternalOAuthProvider extends Basic
{
    public $module_dir = 'ExternalOAuthProvider';
    public $object_name = 'ExternalOAuthProvider';
    public $table_name = 'external_oauth_providers';
    public $disable_row_level_security = true;

    public $id;
    public $name;
    public $type;
    public $connector;
    public $redirect_uri;
    public $client_id;
    public $client_secret;
    public $scope;
    public $url_authorize;
    public $authorize_url_options;
    public $url_access_token;
    public $extra_provider_params;
    public $get_token_request_grant;
    public $get_token_request_options;
    public $refresh_token_request_grant;
    public $refresh_token_request_options;
    public $access_token_mapping;
    public $expires_in_mapping;
    public $refresh_token_mapping;
    public $token_type_mapping;
    public $redirect_uri_type;


    /**
     * @inheritDoc
     */
    public function retrieve($id = -1, $encode = true, $deleted = true)
    {
        $result = parent::retrieve($id, $encode, $deleted);

        if (!empty($result) && !$this->hasAccessToPersonalAccount()) {
            $this->logPersonalAccountAccessDenied('retrieve');

            return null;
        }

        return $result;
    }

    /**
     * @inheritDoc
     */
    public function save($check_notify = false)
    {
        if (!$this->hasAccessToPersonalAccount()) {
            $this->logPersonalAccountAccessDenied('save');
            throw new RuntimeException('Access Denied');
        }

        $this->keepWriteOnlyFieldValues();


        return parent::save($check_notify);
    }

    /**
     * Check if user has access to personal account
     * @return bool
     */
    public function hasAccessToPersonalAccount(): bool
    {
        global $current_user;

        if (is_admin($current_user)) {
            return true;
        }

        if (empty($this->type)) {
            return true;
        }

        if ($this->type !== 'personal') {
            return true;
        }

        if (empty($this->created_by)) {
            return true;
        }

        if ($this->created_by === $current_user->id) {
            return true;
        }


        return false;
    }

    /**
     * Log personal account access denied
     * @param string $action
     * @return void
     */
    public function logPersonalAccountAccessDenied(string $action): void
    {
        global $log, $current_user;

        $log->fatal("ExternalOAuthProvider | Access denied. Non-admin user trying to access personal account. Action: '" . $action . "' | Current user id: '" . $current_user->id . "' | record: '" . $this->id . "'");
    }

    /**
     * @inheritDoc
     */
    public function bean_implements($interface)
    {
        if ($interface === 'ACL') {
            return true;
        }

        return false;
    }

    /**
     * @inheritDoc
     */
    public function ACLAccess($view, $is_owner = 'not_set', $in_group = 'not_set')
    {
        global $current_user;

        $isNotAllowAction = $this->isNotAllowedAction($view);
        if ($isNotAllowAction === true) {
            return false;
        }

        if (!$this->hasAccessToPersonalAccount()) {
            $this->logPersonalAccountAccessDenied("ACLAccess-$view");

            return false;
        }

        $isPersonal = $this->type === 'personal';
        $isAdmin = is_admin($current_user);

        if ($isPersonal === true && $this->hasAccessToPersonalAccount()) {
            return true;
        }

        $isAdminOnlyAction = $this->isAdminOnlyAction($view);
        if (!$isPersonal && !$isAdmin && $isAdminOnlyAction === true) {
            return false;
        }

        return parent::ACLAccess($view, $is_owner, $in_group);
    }

    /**
     * @inheritDoc
     */
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

        if(is_admin($current_user)) {
            if ($return_array) {
                return $ret_array;
            }

            return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
        }

        if (is_array($ret_array) && !empty($ret_array['where'])) {
            $tableName = $db->quote($this->table_name);
            $currentUserId = $db->quote($current_user->id);

            $showGroupRecords = "($tableName.type IS NULL) OR ($tableName.type != 'personal' ) OR ";

            $ret_array['where'] = $ret_array['where'] . " AND ( $showGroupRecords ($tableName.type = 'personal' AND $tableName.created_by = '$currentUserId') )";
        }

        if ($return_array) {
            return $ret_array;
        }

        return $ret_array['select'] . $ret_array['from'] . $ret_array['where'] . $ret_array['order_by'];
    }

    /**
     * Do not clear write only fields
     * @return void
     */
    protected function keepWriteOnlyFieldValues(): void
    {
        if (empty($this->fetched_row)) {
            return;
        }

        foreach ($this->field_defs as $field => $field_def) {
            if (empty($field_def['display']) || $field_def['display'] !== 'writeonly') {
                continue;
            }

            if (empty($this->fetched_row[$field])) {
                continue;
            }

            if (!empty($this->$field)) {
                continue;
            }

            $this->$field = $this->fetched_row[$field];
        }
    }

    /**
     * Check if its admin only action
     * @param string $view
     * @return bool
     */
    protected function isAdminOnlyAction(string $view): bool
    {
        $adminOnlyAction = ['edit', 'delete', 'editview', 'save'];

        return in_array(strtolower($view), $adminOnlyAction);
    }

    /**
     * Check if its a security based action
     * @param string $view
     * @return bool
     */
    protected function isSecurityGroupBasedAction(string $view): bool
    {
        $securityBasedActions = ['detail', 'detailview', 'view'];

        return in_array(strtolower($view), $securityBasedActions);
    }

    /**
     * Get not allowed action
     * @param string $view
     * @return bool
     */
    protected function isNotAllowedAction(string $view): bool
    {
        $notAllowed = ['export', 'import', 'massupdate', 'duplicate'];

        return in_array(strtolower($view), $notAllowed);
    }

    /**
     * Build redirect uri based on site_url
     * @return string
     */
    public function getRedirectURI()
    {
        global $sugar_config;

        $siteUrl = $sugar_config['site_url'] ?? '';
        $siteUrl = str_ireplace('index.php', '', (string) $siteUrl);
        $siteUrl = rtrim($siteUrl, " \t\n\r\0\x0B\/");

        if ($this->redirect_uri_type === 'pretty_url') {
            return $siteUrl . '/ep/setExternalOAuthToken';
        }

        return $siteUrl . '/index.php?entryPoint=setExternalOAuthToken';
    }

    /**
     * Get provider configuration array
     * @return array
     */
    public function getConfigArray(): array
    {
        return [
            'type' => $this->connector ?? '',
            'client_id' => $this->client_id ?? '',
            'client_secret' => $this->client_secret ?? '',
            'redirect_uri' => $this->getRedirectURI() ?? '',
            'authorize_url_options' => array_merge(
                [
                    'scope' => $this->getScope(),
                ],
                $this->getAuthorizeUrlOptions()
            ),
            'extra_provider_params' => array_merge(
                [
                    'scopes' => $this->getScope(),
                    'urlAuthorize' => $this->url_authorize ?? '',
                    'urlAccessToken' => $this->url_access_token ?? '',
                ],
                $this->getExtraProviderParams()
            ),
            'get_token_request_grant' => $this->getTokenRequestGrant(), // optional
            'get_token_request_options' => $this->getTokenRequestOptions(), // optional
            'refresh_token_request_grant' => $this->getRefreshTokenRequestGrant(), // optional
            'refresh_token_request_options' => $this->getRefreshTokenRequestOptions(), // optional
            'token_mapping' => [ // optional
                'access_token' => $this->getAccessTokenMapping(),
                'expires_in' => $this->getExpiresInMapping(),
                'refresh_token' => $this->getRefreshTokenMapping(),
                'token_type' => $this->getTokenTypeMapping()
            ]
        ];
    }

    /**
     * @param $field
     * @return array
     */
    protected function deserializeMapField(string $field): array
    {
        if (empty($this->$field)) {
            return [];
        }

        try {
            $params = json_decode(html_entity_decode((string) $this->$field), true, 512, JSON_THROW_ON_ERROR);
        } catch (JsonException $e) {
            return [];
        }

        return $params ?? [];
    }

    /**
     * Get token request options
     * @return string
     */
    public function getScope(): string
    {
        $scopes = $this->deserializeMapField('scope');
        if (empty($scopes)) {
            return '';
        }

        return implode(' ', $scopes);
    }

    /**
     * Get Decoded Extra provider Params
     * @return array|mixed
     */
    public function getExtraProviderParams(): array
    {
        return $this->deserializeMapField('extra_provider_params');
    }

    /**
     * Get token request options
     * @return array|mixed
     */
    public function getTokenRequestOptions(): array
    {
        return $this->deserializeMapField('get_token_request_options');
    }

    /**
     * Get token refresh request options
     * @return array|mixed
     */
    public function getRefreshTokenRequestOptions(): array
    {
        return $this->deserializeMapField('refresh_token_request_options');
    }

    /**
     * Get authorize url options
     * @return array|mixed
     */
    public function getAuthorizeUrlOptions(): array
    {
        return $this->deserializeMapField('authorize_url_options');
    }

    /**
     * @return string
     */
    public function getRefreshTokenRequestGrant(): string
    {
        if (empty($this->refresh_token_request_grant)){
            return 'refresh_token';
        }

        return $this->refresh_token_request_grant;
    }

    /**
     * @return string
     */
    public function getTokenRequestGrant(): string
    {
        if (empty($this->get_token_request_grant)){
            return 'authorization_code';
        }

        return $this->get_token_request_grant;
    }

    /**
     * @return string
     */
    public function getAccessTokenMapping(): string
    {
        if (empty($this->access_token_mapping)){
            return 'access_token';
        }

        return $this->access_token_mapping;
    }

    /**
     * @return string
     */
    public function getRefreshTokenMapping(): string
    {
        if (empty($this->refresh_token_mapping)){
            return 'refresh_token';
        }

        return $this->refresh_token_mapping;
    }

    /**
     * @return string
     */
    public function getExpiresInMapping(): string
    {
        if (empty($this->expires_in_mapping)){
            return 'expires_in';
        }

        return $this->expires_in_mapping;
    }

    /**
     * @return string
     */
    public function getTokenTypeMapping(): string
    {
        if (empty($this->token_type_mapping)){
            return '';
        }
        return $this->token_type_mapping;
    }
}
