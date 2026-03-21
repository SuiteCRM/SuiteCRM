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

require_once 'include/CalendarSync/migrations/value-objects/UserMigrationStatsDetail.php';

/**
 * Migration Status - comprehensive migration tracking and reporting
 */
class UserMigrationStatus
{

    public function __construct(
        private int $processed = 0,
        private int $migrated = 0,
        private int $skipped = 0,
        private int $errors = 0,
        private bool $dryRun = false,
        private array $details = [],
    ) {
    }

    /**
     * Retrieves the details.
     *
     * @return UserMigrationStatsDetail[] Returns an array containing the details.
     */
    public function getDetails(): array
    {
        return $this->details;
    }

    /**
     * Determines if the migration process has been completed.
     *
     * @return bool True if the migration has been performed and processed, false otherwise.
     */
    public function isMigrated(): bool
    {
        return !$this->isDryRun() && $this->errors === 0;
    }

    /**
     * Determines whether the current process is a dry run.
     *
     * @return bool True if the process is a dry run, false otherwise.
     */
    public function isDryRun(): bool
    {
        return $this->dryRun;
    }

    /**
     * Sets the dry run mode for the current process.
     *
     * @param bool $dryRun True to enable dry run mode, false to disable it.
     * @return void
     */
    public function setDryRun(bool $dryRun): void
    {
        $this->dryRun = $dryRun;
    }

    /**
     * Adds a detailed migration statistic and updates the corresponding counters.
     *
     * @param UserMigrationStatsDetail $detail The migration detail to be added, containing information about the user and the action type.
     * @return void
     */
    public function addDetail(UserMigrationStatsDetail $detail): void
    {
        $this->details[] = $detail;

        if ($detail->user_id || $detail->user_name) {
            $this->processed++;
        }

        switch ($detail->type) {
            case 'skip':
                $this->skipped++;
                break;
            case 'success':
                $this->migrated++;
                break;
            case 'error':
                $this->errors++;
                break;
        }
    }

    /**
     * Retrieves the number of processed items.
     *
     * @return int The total number of processed items.
     */
    public function getProcessed(): int
    {
        return $this->processed;
    }

    /**
     * Returns a string representation of the object, including details about
     * the migration process, such as processed, migrated, skipped, and errored items.
     * Also includes additional details if available.
     *
     * @return string A formatted string summarizing the migration process and details.
     */
    public function __toString(): string
    {
        $output = [];

        $output[] = sprintf(
            '%s[Processed: %d, Migrated: %d, Skipped: %d, Errors: %d]',
            $this->dryRun ? '(DRY RUN) ' : '',
            $this->processed,
            $this->migrated,
            $this->skipped,
            $this->errors
        );

        if (!empty($this->details)) {
            $output[] = '+' . str_repeat('-', 78) . '+';
        }
        foreach ($this->details as $detail) {
            $output[] = '';
            $output[] = " - " . $detail->__toString();
        }
        if (!empty($this->details)) {
            $output[] = '';
            $output[] = '+' . str_repeat('-', 78) . '+';
        }

        return implode("\n", $output);
    }

}