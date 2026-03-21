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

require_once 'include/CalendarSync/migrations/value-objects/LegacyUserData.php';
require_once 'include/CalendarSync/migrations/value-objects/UserMigrationStatsDetail.php';
require_once 'include/CalendarSync/migrations/enums/MigrationStatsDetailType.php';
require_once 'modules/ExternalOAuthConnection/services/OAuthAuthorizationService.php';

/**
 * User Migration Service
 *
 * Handles the migration of users from the legacy GoogleSync system to the new
 * CalendarSync system. This includes creating ExternalOAuthConnection and
 * CalendarAccount records for each user.
 */
class UserMigrationService
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
     * @var OAuthAuthorizationService OAuth service for token management
     */
    protected OAuthAuthorizationService $oAuthService;

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

        $this->oAuthService = new OAuthAuthorizationService();
    }

    /**
     * Validate user migration system requirements
     *
     * @return string[] Array of validation issues
     */
    public function validateUserMigrationRequirements(): array
    {
        $issues = [];

        if (!class_exists('CalendarAccount')) {
            $issues[] = 'CalendarAccount module not available';
        }

        if (!class_exists('ExternalOAuthConnection')) {
            $issues[] = 'ExternalOAuthConnection module not available';
        }

        if (!$this->db->tableExists('calendar_accounts')) {
            $issues[] = 'calendar_accounts table does not exist';
        }

        if (!$this->db->tableExists('external_oauth_connections')) {
            $issues[] = 'external_oauth_connections table does not exist';
        }

        if (!class_exists('OAuthAuthorizationService')) {
            $issues[] = 'OAuthAuthorizationService not available';
        }

        return $issues;
    }

    /**
     * Find all users that need calendar sync migration (both legacy and new)
     *
     * @return LegacyUserData[] Array of user data that need migration
     */
    public function findAllUsersForMigration(): array
    {
        $legacyUsers = $this->findLegacyGoogleSyncUsers();
        $newUsers = $this->findNewGoogleSyncUsers();

        return array_merge($legacyUsers, $newUsers);
    }

    /**
     * Find all users with legacy Google Calendar sync configuration
     *
     * @return LegacyUserData[] Array of user data with legacy sync settings
     */
    protected function findLegacyGoogleSyncUsers(): array
    {
        $query = "
            SELECT DISTINCT u.id, from_base64(up.contents) as decoded_contents
            FROM users u
                INNER JOIN user_preferences up ON u.id = up.assigned_user_id
            WHERE u.deleted = '0'
                AND up.category = 'GoogleSync'
                AND up.contents IS NOT NULL
                AND from_base64(up.contents) LIKE '%GoogleApiToken%';
        ";

        $result = $this->db->query($query);
        $users = [];

        while ($row = $this->db->fetchByAssoc($result)) {
            $user = $this->getUserBean($row['id']);
            if (!$user) {
                continue;
            }

            $userData = $this->extractLegacyUserData($user);
            if ($userData) {
                $users[] = $userData;
            }
        }

        return $users;
    }

    /**
     * Get user bean with validation
     *
     * @param string $userId User ID
     * @return User|null User bean or null if invalid
     */
    protected function getUserBean(string $userId): ?User
    {
        /** @var User $user */
        $user = BeanFactory::getBean('Users', $userId);
        return ($user && $user->id) ? $user : null;
    }

    /**
     * Extract legacy Google sync data from a user
     *
     * @param User $user User bean
     * @return LegacyUserData|null User data if legacy sync is configured, null otherwise
     */
    protected function extractLegacyUserData(User $user): ?LegacyUserData
    {
        $googleApiToken = $user->getPreference('GoogleApiToken', 'GoogleSync');
        $googleApiRefreshToken = $user->getPreference('GoogleApiRefreshToken', 'GoogleSync');
        $googleSyncEnabled = $user->getPreference('syncGCal', 'GoogleSync');

        if (empty($googleApiToken)) {
            return null;
        }

        try {
            $tokenData = json_decode(base64_decode($googleApiToken), true, 512, JSON_THROW_ON_ERROR);
        } catch (JsonException $e) {
            return null;
        }

        if (!$tokenData || !isset($tokenData['access_token'])) {
            return null;
        }

        return new LegacyUserData(
            user_id: $user->id,
            user_name: $user->user_name,
            full_name: $user->full_name,
            token_data: $tokenData,
            refresh_token: $googleApiRefreshToken ? base64_decode($googleApiRefreshToken) : null,
            last_connection_status: $googleSyncEnabled === '1',
            last_connection_test: date('Y-m-d H:i:s'),
        );
    }

    /**
     * Find users with existing calendar accounts in the new system
     *
     * @return LegacyUserData[] Array of user data with new calendar accounts
     */
    protected function findNewGoogleSyncUsers(): array
    {
        $query = "
            SELECT DISTINCT u.id
            FROM users u
                INNER JOIN calendar_accounts ca ON u.id = ca.calendar_user_id
            WHERE u.deleted = '0'
                AND ca.deleted = '0'
                AND ca.source = 'google'
        ";

        $result = $this->db->query($query);
        $users = [];

        while ($row = $this->db->fetchByAssoc($result)) {
            $user = $this->getUserBean($row['id']);
            if (!$user) continue;

            $users[] = new LegacyUserData(
                user_id: $user->id,
                user_name: $user->user_name,
                full_name: $user->full_name,
                token_data: [],
                refresh_token: null,
                last_connection_status: true,
                last_connection_test: date('Y-m-d H:i:s'),
            );
        }

        return $users;
    }

    /**
     * Migrate a single user to the new calendar sync system
     *
     * @param LegacyUserData $userData User data from extractLegacyUserData
     * @param string $providerId OAuth provider ID to link
     * @param bool $dryRun If true, only simulate the migration
     * @return UserMigrationStatsDetail Migration result details
     */
    public function migrateUser(LegacyUserData $userData, string $providerId, bool $dryRun = false): UserMigrationStatsDetail
    {
        $userId = $userData->user_id;
        $userName = $userData->user_name ?? 'Unknown';

        try {
            if ($this->hasNewGoogleCalendarAccount($userId)) {
                $this->logger->info("Skipping migration for user $userName ($userId) - already has Google Calendar account");
                return new UserMigrationStatsDetail(
                    type: MigrationStatsDetailType::SKIP,
                    user_id: $userId,
                    user_name: $userName,
                    message: 'User already has new Google Calendar account'
                );
            }

            if (!$this->oAuthService->hasProvider($providerId)) {
                $this->logger->error("OAuth provider validation failed for user $userName ($userId): Provider '$providerId' not found");
                return new UserMigrationStatsDetail(
                    type: MigrationStatsDetailType::ERROR,
                    user_id: $userId,
                    user_name: $userName,
                    message: "OAuth provider '$providerId' is not supported or not properly configured"
                );
            }

            $this->logger->info("Starting migration for user $userName ($userId)");

            $oauthConnection = $this->createOAuthConnection($userData, $providerId, $dryRun);
            $this->logger->info("OAuth connection created for user $userName ($userId)");

            $this->createCalendarAccount($userData, $oauthConnection, $dryRun);
            $this->logger->info("Calendar account created for user $userName ($userId)");

            $this->disableLegacySync($userId, $dryRun);
            $this->logger->info("Legacy sync disabled for user $userName ($userId)");

            $this->logger->info("Migration completed successfully for user $userName ($userId)");
            return new UserMigrationStatsDetail(
                type: MigrationStatsDetailType::SUCCESS,
                user_id: $userId,
                user_name: $userName,
                message: 'Migration completed successfully'
            );

        } catch (RuntimeException $e) {
            $this->logger->error("OAuth migration error for user $userName ($userId): " . $e->getMessage());
            $errorMessage = $e->getMessage();
            if (str_contains($errorMessage, 're-login required') || str_contains($errorMessage, 're-authorization required')) {
                $errorMessage = 'OAuth token expired - user needs to re-authorize Google Calendar access';
            }
            return new UserMigrationStatsDetail(
                type: MigrationStatsDetailType::ERROR,
                user_id: $userId,
                user_name: $userName,
                message: $errorMessage
            );
        } catch (Throwable $e) {
            $this->logger->error("Unexpected migration error for user $userName ($userId): " . $e->getMessage());
            return new UserMigrationStatsDetail(
                type: MigrationStatsDetailType::ERROR,
                user_id: $userId,
                user_name: $userName,
                message: 'Unexpected error during migration: ' . $e->getMessage()
            );
        }
    }

    /**
     * Check if user already has a Google Calendar account in the new system
     *
     * @param string $userId User ID to check
     * @return bool True if user has new Google Calendar account
     */
    protected function hasNewGoogleCalendarAccount(string $userId): bool
    {
        $query = "
            SELECT ca.id
            FROM calendar_accounts ca
            WHERE ca.calendar_user_id = " . $this->db->quoted($userId) . "
            AND ca.source = 'google'
            AND ca.deleted = '0'
            LIMIT 1
        ";

        $result = $this->db->query($query);
        return $this->db->fetchByAssoc($result) !== false;
    }

    /**
     * Create ExternalOAuthConnection for the user with proper OAuth authorization
     *
     * @param LegacyUserData $userData User data with token information
     * @param string $providerId OAuth provider ID to link
     * @param bool $dryRun If true, only simulate the creation
     * @return ExternalOAuthConnection Created OAuth connection
     * @throws RuntimeException If creation fails
     */
    protected function createOAuthConnection(LegacyUserData $userData, string $providerId, bool $dryRun = false): ExternalOAuthConnection
    {
        if (!$this->oAuthService->hasProvider($providerId)) {
            throw new RuntimeException("OAuth provider '$providerId' is not supported or not properly configured");
        }

        /** @var ExternalOAuthConnection $connection */
        $connection = BeanFactory::getBean('ExternalOAuthConnection');
        if (!$connection) {
            throw new RuntimeException('Unable to create ExternalOAuthConnection bean');
        }

        $tokenData = $userData->token_data;
        $accessToken = $tokenData['access_token'] ?? '';
        $refreshToken = $userData->refresh_token ?? ($tokenData['refresh_token'] ?? '');

        if (empty($accessToken)) {
            throw new RuntimeException('Access token is required for OAuth connection');
        }

        $connection->name = "Google Calendar - " . ($userData->full_name ?? 'User');
        $connection->type = 'personal';
        $connection->access_token = $accessToken;
        $connection->refresh_token = $refreshToken;
        $connection->token_type = $tokenData['token_type'] ?? 'Bearer';

        $expiresIn = $tokenData['expires_in'] ?? 3600;
        $connection->expires_in = time() + (int)$expiresIn;

        $connection->assigned_user_id = $userData->user_id;
        $connection->created_by = $userData->user_id;
        $connection->external_oauth_provider_id = $providerId;

        if ($dryRun) {
            return $connection;
        }

        $originalCurrentUser = $GLOBALS['current_user'] ?? null;
        $userBean = $this->getUserBean($userData->user_id);
        if ($userBean) {
            $GLOBALS['current_user'] = $userBean;
        }

        try {
            if (!$connection->save()) {
                throw new RuntimeException('Failed to save ExternalOAuthConnection');
            }

            $this->validateAndRefreshOAuthConnection($connection);
        } finally {
            $GLOBALS['current_user'] = $originalCurrentUser;
        }

        return $connection;
    }

    /**
     * Validate and refresh OAuth connection tokens if needed
     *
     * @param ExternalOAuthConnection $connection OAuth connection to validate
     * @return void
     * @throws RuntimeException If token validation or refresh fails
     */
    protected function validateAndRefreshOAuthConnection(ExternalOAuthConnection $connection): void
    {
        try {
            $expiredStatus = $this->oAuthService->hasConnectionTokenExpired($connection);

            if ($expiredStatus['expired'] && $expiredStatus['refreshToken']) {
                $this->logger->info("OAuth token expired for connection {$connection->id}, attempting refresh");

                $refreshResult = $this->oAuthService->refreshConnectionToken($connection);

                if (!$refreshResult['success']) {
                    $errorMessage = $refreshResult['message'] ?? 'Unknown refresh error';

                    if ($refreshResult['reLogin']) {
                        throw new RuntimeException("OAuth token refresh failed - re-login required: $errorMessage");
                    } else {
                        throw new RuntimeException("OAuth token refresh failed: $errorMessage");
                    }
                }

                $this->logger->info("OAuth token successfully refreshed for connection {$connection->id}");
            } elseif ($expiredStatus['expired']) {
                throw new RuntimeException("OAuth token expired and no refresh token available - re-authorization required");
            } else {
                $this->logger->debug("OAuth token is valid for connection {$connection->id}");
            }
        } catch (Exception $e) {
            $this->logger->error("OAuth token validation failed for connection {$connection->id}: " . $e->getMessage());
            throw new RuntimeException("OAuth token validation failed: " . $e->getMessage(), 0, $e);
        }
    }

    /**
     * Create CalendarAccount for the user
     *
     * @param LegacyUserData $userData User data
     * @param ExternalOAuthConnection $oauthConnection OAuth connection to link
     * @param bool $dryRun If true, only simulate the creation
     * @return CalendarAccount Created calendar account
     * @throws RuntimeException If creation fails
     */
    protected function createCalendarAccount(LegacyUserData $userData, ExternalOAuthConnection $oauthConnection, bool $dryRun = false): CalendarAccount
    {
        /** @var CalendarAccount $account */
        $account = BeanFactory::getBean('CalendarAccount');
        if (!$account) {
            throw new RuntimeException('Unable to create CalendarAccount bean');
        }

        $account->name = "Google Calendar - $userData->full_name";
        $account->source = 'google';
        $account->type = 'personal';
        $account->oauth_connection_id = $oauthConnection->id;
        $account->oauth_connection_name = $oauthConnection->name;
        $account->calendar_user_id = $userData->user_id;
        $account->calendar_user_name = $userData->full_name;
        $account->last_connection_status = 1;
        $account->last_connection_test = date('Y-m-d H:i:s');
        $account->assigned_user_id = $userData->user_id;
        $account->created_by = $userData->user_id;

        if ($dryRun) {
            return $account;
        }

        $originalCurrentUser = $GLOBALS['current_user'] ?? null;
        $userBean = $this->getUserBean($userData->user_id);
        if ($userBean) {
            $GLOBALS['current_user'] = $userBean;
        }

        try {
            if (!$account->save()) {
                throw new RuntimeException('Failed to save CalendarAccount');
            }
        } finally {
            $GLOBALS['current_user'] = $originalCurrentUser;
        }

        return $account;
    }

    /**
     * Disable legacy Google sync for the user
     *
     * @param string $userId User ID
     * @param bool $dryRun If true, only simulate the disabling
     * @return void
     */
    protected function disableLegacySync(string $userId, bool $dryRun = false): void
    {
        $user = $this->getUserBean($userId);
        if (!$user) return;

        if ($dryRun) {
            return;
        }

        $user->setPreference('syncGCal', '0', false, 'GoogleSync');
        $user->setPreference('GoogleApiToken', null, false, 'GoogleSync');
        $user->setPreference('GoogleApiRefreshToken', null, false, 'GoogleSync');
        $user->savePreferencesToDB();
    }

    /**
     * Create empty user data for new sync users
     *
     * @param User $user User bean
     * @return LegacyUserData Empty user data
     */
    protected function createEmptyUserData(User $user): LegacyUserData
    {
        return new LegacyUserData(
            user_id: $user->id,
            user_name: $user->user_name,
            full_name: $user->full_name,
            token_data: [],
            refresh_token: null,
            last_connection_status: true,
            last_connection_test: date('Y-m-d H:i:s'),
        );
    }

    /**
     * Create OAuth connection bean with validation
     *
     * @return ExternalOAuthConnection OAuth connection bean
     * @throws RuntimeException If bean creation fails
     */
    protected function createOAuthBean(): ExternalOAuthConnection
    {
        /** @var ExternalOAuthConnection $connection */
        $connection = BeanFactory::getBean('ExternalOAuthConnection');
        if (!$connection) {
            throw new RuntimeException('Unable to create ExternalOAuthConnection bean');
        }
        return $connection;
    }

    /**
     * Create calendar account bean with validation
     *
     * @return CalendarAccount Calendar account bean
     * @throws RuntimeException If bean creation fails
     */
    protected function createCalendarBean(): CalendarAccount
    {
        /** @var CalendarAccount $account */
        $account = BeanFactory::getBean('CalendarAccount');
        if (!$account) {
            throw new RuntimeException('Unable to create CalendarAccount bean');
        }
        return $account;
    }

}