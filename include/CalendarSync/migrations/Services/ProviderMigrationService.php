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

/**
 * Provider Migration Service
 *
 * Handles the creation and management of OAuth providers for calendar sync migration.
 * This service is responsible for creating ExternalOAuthProvider records for Google Calendar
 * integration as part of the legacy sync migration process.
 */
class ProviderMigrationService
{

    /**
     * @var DBManager Database manager instance
     */
    protected DBManager $db;

    /**
     * @var LoggerManager Logger instance
     */
    protected LoggerManager $logger;

    /**
     * Constructor
     *
     * @throws RuntimeException If required dependencies cannot be initialized
     */
    public function __construct()
    {
        $dbManager = DBManagerFactory::getInstance();
        if (!($dbManager instanceof DBManager)) {
            throw new RuntimeException('Failed to initialize database manager');
        }
        $this->db = $dbManager;

        $logger = LoggerManager::getLogger();
        if (!$logger) {
            throw new RuntimeException('Failed to initialize logger');
        }
        $this->logger = $logger;
    }

    /**
     * Validate provider-related system requirements
     *
     * @return string[] Array of validation issues
     */
    public function validateProviderRequirements(): array
    {
        $issues = [];

        if (!class_exists('ExternalOAuthProvider')) {
            $issues[] = 'ExternalOAuthProvider module not available';
        }

        if (!$this->db->tableExists('external_oauth_providers')) {
            $issues[] = 'external_oauth_providers table does not exist';
        }

        return $issues;
    }

    /**
     * Create or get ExternalOAuthProvider for Google Calendar
     *
     * @param bool $dryRun If true, only simulate the creation
     * @return string Provider ID
     * @throws RuntimeException If provider creation fails
     */
    public function createOrGetExternalOAuthProvider(bool $dryRun = false): string
    {
        global $sugar_config;

        $existingProviderId = $this->findExistingGoogleOAuthProvider();
        if ($existingProviderId) {
            return $existingProviderId;
        }

        $googleOauthJson = $sugar_config['google_auth_json'];
        if (empty($googleOauthJson)) {
            throw new RuntimeException('Google auth configuration not found in $sugar_config');
        }

        try {
            $googleConfig = json_decode(base64_decode($googleOauthJson), true, 512, JSON_THROW_ON_ERROR);
        } catch (Throwable $e) {
        }

        if (!$googleConfig || !isset($googleConfig['web'])) {
            throw new RuntimeException('Invalid Google auth configuration format');
        }

        $webConfig = $googleConfig['web'];

        /** @var ExternalOAuthProvider $provider */
        $provider = BeanFactory::getBean('ExternalOAuthProvider');
        if (!$provider) {
            throw new RuntimeException('Unable to create ExternalOAuthProvider bean');
        }

        $provider->name = 'Google Calendar OAuth Provider';
        $provider->description = '';
        $provider->type = 'group';
        $provider->connector = 'Google';
        $provider->client_id = $webConfig['client_id'] ?? '';
        $provider->client_secret = $webConfig['client_secret'] ?? '';
        $provider->scope = '["openid", "email", "profile", "https://www.googleapis.com/auth/calendar"]';
        $provider->url_authorize = $webConfig['auth_uri'] ?? '';
        $provider->authorize_url_options = null;
        $provider->url_access_token = $webConfig['token_uri'] ?? '';
        $provider->extra_provider_params = '';
        $provider->get_token_request_grant = 'authorization_code';
        $provider->get_token_request_options = '';
        $provider->refresh_token_request_grant = 'refresh_token';
        $provider->refresh_token_request_options = '';
        $provider->access_token_mapping = 'access_token';
        $provider->expires_in_mapping = 'expires_in';
        $provider->refresh_token_mapping = 'refresh_token';
        $provider->token_type_mapping = '';
        $provider->assigned_user_id = '1';
        $provider->created_by = '1';

        if ($dryRun) {
            return 'dry-run-provider-id';
        }

        if (!$provider->save()) {
            throw new RuntimeException('Failed to save ExternalOAuthProvider');
        }

        return $provider->id;
    }

    /**
     * Find existing Google OAuth Provider
     *
     * @return string|null Provider ID if found, null otherwise
     */
    protected function findExistingGoogleOAuthProvider(): ?string
    {
        global $db;

        $query = "SELECT id 
        FROM external_oauth_providers 
        WHERE connector = 'Google' 
            AND JSON_SEARCH(scope, 'one', 'https://www.googleapis.com/auth/calendar') IS NOT NULL 
        LIMIT 1";

        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);

        return $row['id'] ?? null;
    }

}