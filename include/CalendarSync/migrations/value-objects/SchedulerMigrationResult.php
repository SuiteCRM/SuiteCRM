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
 * Scheduler Migration Result - represents the result of scheduler migration operation
 */
class SchedulerMigrationResult
{

    public function __construct(
        public bool $success,
        public string $action,
        public string $message,
        public int $schedulersFound = 0,
        public int $schedulersUpdated = 0,
        public array $details = []
    ) {
    }

    /**
     * Create a successful skip result
     */
    public static function skip(string $message = 'No legacy schedulers found'): self
    {
        return new self(
            success: true,
            action: 'skip',
            message: $message
        );
    }

    /**
     * Create a successful migration result
     */
    public static function success(int $schedulersFound, int $schedulersUpdated, bool $isDryRun = false): self
    {
        $action = $isDryRun ? 'dry_run' : 'migrate';
        $message = $isDryRun
            ? "Dry run: Would update $schedulersFound scheduler(s)"
            : "Successfully updated $schedulersUpdated scheduler(s) to use calendarSyncJob function";

        return new self(
            success: true,
            action: $action,
            message: $message,
            schedulersFound: $schedulersFound,
            schedulersUpdated: $schedulersUpdated
        );
    }

    /**
     * Create an error result
     */
    public static function error(string $message, Throwable $exception = null): self
    {
        $details = [];
        if ($exception) {
            $details['exception'] = $exception->getMessage();
            $details['trace'] = $exception->getTraceAsString();
        }

        return new self(
            success: false,
            action: 'error',
            message: $message,
            details: $details
        );
    }

    /**
     * Check if migration was successful
     */
    public function isSuccess(): bool
    {
        return $this->success;
    }

    /**
     * Check if any schedulers were found
     */
    public function hasSchedulers(): bool
    {
        return $this->schedulersFound > 0;
    }

    /**
     * Check if this was a dry run
     */
    public function isDryRun(): bool
    {
        return $this->action === 'dry_run';
    }

    /**
     * Check if migration was skipped
     */
    public function wasSkipped(): bool
    {
        return $this->action === 'skip';
    }

    public function __toString(): string
    {
        $status = $this->success ? '✓' : '✗';
        $result = "$status Scheduler Migration: {$this->action} - {$this->message}";

        if ($this->schedulersFound > 0) {
            $result .= "\n   Found: {$this->schedulersFound} scheduler(s)";
        }

        if ($this->schedulersUpdated > 0) {
            $result .= "\n   Updated: {$this->schedulersUpdated} scheduler(s)";
        }

        if (!empty($this->details)) {
            $result .= "\n   Details: " . json_encode($this->details, JSON_PRETTY_PRINT);
        }

        return $result;
    }

}