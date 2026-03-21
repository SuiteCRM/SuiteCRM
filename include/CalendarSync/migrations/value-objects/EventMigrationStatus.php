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

require_once 'include/CalendarSync/migrations/value-objects/EventMigrationStatsDetail.php';

class EventMigrationStatus
{

    private int $processed = 0;
    private int $migrated = 0;
    private int $skipped = 0;
    private int $errors = 0;
    private array $details = [];

    public function __construct()
    {
    }

    public function incrementProcessed(): void
    {
        $this->processed++;
    }

    public function addDetail(EventMigrationStatsDetail $detail): void
    {
        $this->details[] = $detail;

        if (!$detail->event_id && !$detail->event_name) {
            return;
        }

        switch ($detail->type) {
            case MigrationStatsDetailType::SKIP:
                $this->skipped++;
                break;
            case MigrationStatsDetailType::SUCCESS:
                $this->migrated++;
                break;
            case MigrationStatsDetailType::ERROR:
                $this->errors++;
                break;
        }
    }

    public function __toString(): string
    {
        $output = [];

        $output[] = '';
        $output[] = sprintf(
            'Events: [Processed: %d, Migrated: %d, Skipped: %d, Errors: %d]',
            $this->processed,
            $this->migrated,
            $this->skipped,
            $this->errors
        );

        foreach ($this->details as $detail) {
            $output[] = '  - ' . $detail->__toString();
        }

        return implode("\n    ", $output);
    }

}