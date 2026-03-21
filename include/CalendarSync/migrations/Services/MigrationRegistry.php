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
 * Migration Registry
 *
 * Centralized service for tracking migration execution status using the config table.
 * Uses category='migrations' to identify migration-related configuration entries.
 *
 * This class provides a consistent interface for checking migration status and
 * recording migration completion across all migrations in the system.
 */
class MigrationRegistry
{

    public const MIGRATION_CATEGORY = 'migrations';
    public const GOOGLE_CALENDAR_SYNC_MIGRATION = 'google_calendar_sync_migration';
    public const CALENDAR_SYNC_HOOKS_INSTALLATION = 'calendar_sync_hooks_installation';

    protected DBManager $db;
    protected LoggerManager $logger;

    /**
     * @throws RuntimeException
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
     * Check if a migration has already been run
     *
     * @param string $migrationId Unique identifier for the migration
     * @return bool True if migration has been run, false otherwise
     */
    public function hasMigrationRun(string $migrationId): bool
    {
        try {
            $query = "SELECT COUNT(*) as the_count FROM config
                      WHERE category = " . $this->db->quoted(self::MIGRATION_CATEGORY) . "
                      AND name = " . $this->db->quoted($migrationId);

            $result = $this->db->query($query);
            if ($result === false) {
                $this->logger->warn("[MigrationRegistry][hasMigrationRun] Query failed for $migrationId, assuming not run");
                return false;
            }

            $row = $this->db->fetchByAssoc($result);
            return !empty($row['the_count']);
        } catch (Throwable $e) {
            $this->logger->warn("[MigrationRegistry][hasMigrationRun] Exception checking migration $migrationId: " . $e->getMessage());
            return false;
        }
    }

    /**
     * Record migration completion in config table
     *
     * Stores a record in the config table to indicate successful migration completion.
     * Uses INSERT IGNORE to ensure idempotency and prevent race conditions in
     * clustered/load-balanced environments.
     *
     * @param string $migrationId Unique identifier for the migration
     * @return void
     * @throws RuntimeException If database operation fails
     */
    public function recordMigrationCompletion(string $migrationId): void
    {
        $timestamp = date('Y-m-d H:i:s');
        $quotedCategory = $this->db->quoted(self::MIGRATION_CATEGORY);
        $quotedMigrationId = $this->db->quoted($migrationId);
        $quotedTimestamp = $this->db->quoted($timestamp);

        $query = "INSERT IGNORE INTO config (category, name, value)
            VALUES ($quotedCategory, $quotedMigrationId, $quotedTimestamp)";

        $result = $this->db->query($query);
        if ($result === false) {
            $error = $this->db->lastError();
            $this->logger->error("[MigrationRegistry][recordMigrationCompletion] Failed to record migration marker for $migrationId: $error");
            throw new RuntimeException("Failed to record migration completion: $error");
        }

        $this->logger->info("[MigrationRegistry][recordMigrationCompletion] Migration marker recorded: $migrationId at $timestamp");
    }

}
