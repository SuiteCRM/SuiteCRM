<?php
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}
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

if (!is_admin($current_user)) {
    sugar_die("Unauthorized access to administration.");
}

/**
 * Helper function to safely calculate end date from start date and duration
 * Prevents fatal errors by validating DateTime objects before method chaining
 *
 * @param array $row Database row containing date_start, duration_hours, duration_minutes, id
 * @param string $recordType Type of record (call/meeting) for logging
 * @return string|false The calculated end date as database string, or false on failure
 */
function calculateEndDate(array $row, string $recordType): false|string
{
    global $timedate;
    $logger = LoggerManager::getLogger();

    $rowId = $row['id'];
    $rowDateStart = $row['date_start'] ?? null;

    /** @var ?SugarDateTime $dateStart */
    $dateStart = $timedate->fromDb($rowDateStart);

    if (!$dateStart) {
        $logger?->warn(
            sprintf(
                "RepairActivities: Invalid date_start for %s ID %s: '%s'",
                $recordType,
                $rowId,
                $rowDateStart
            )
        );
        return false;
    }

    $rowDurationHours = $row['duration_hours'] ?? null;
    $rowDurationMinutes = $row['duration_minutes'] ?? null;

    if ($rowDurationHours === null || $rowDurationMinutes === null) {
        $logger?->error(
            sprintf(
                "RepairActivities: Missing duration fields for %s ID %s: duration_hours=%s, duration_minutes=%s",
                $recordType,
                $rowId,
                $rowDurationHours ?? 'NULL',
                $rowDurationMinutes ?? 'NULL'
            )
        );
        return false;
    }

    /** @var ?SugarDateTime $modifiedDate */
    $modifiedDate = $dateStart->modify("+$rowDurationHours hours $rowDurationMinutes mins");

    if (!$modifiedDate) {
        $logger?->error(
            sprintf(
                "RepairActivities: Failed to calculate end date for %s ID %s: dateStart: %s, duration_hours=%s, duration_minutes=%s",
                $recordType,
                $rowId,
                $rowDateStart,
                $rowDurationHours,
                $rowDurationMinutes
            )
        );
        return false;
    }

    return $modifiedDate->asDb();
}

$callBean = BeanFactory::newBean('Calls');
$callQuery = "SELECT * FROM calls where calls.status != 'Held' and calls.deleted=0";

$result = $callBean->db->query($callQuery, true, "");
$row = $callBean->db->fetchByAssoc($result);
while ($row != null) {
    $date_end = calculateEndDate($row, 'call');

    if ($date_end) {
        $updateQuery = "UPDATE calls set calls.date_end='$date_end' where calls.id='{$row['id']}'";
        $call = BeanFactory::newBean('Calls');
        $call->db->query($updateQuery);
    }

    $row = $callBean->db->fetchByAssoc($result);
}

$meetingBean = BeanFactory::newBean('Meetings');
$meetingQuery = "SELECT * FROM meetings where meetings.status != 'Held' and meetings.deleted=0";

$result = $meetingBean->db->query($meetingQuery, true, "");
$row = $meetingBean->db->fetchByAssoc($result);
while ($row != null) {
    $date_end = calculateEndDate($row, 'meeting');

    if ($date_end) {
        $updateQuery = "UPDATE meetings set meetings.date_end='$date_end' where meetings.id='{$row['id']}'";
        $call = BeanFactory::newBean('Calls');
        $call->db->query($updateQuery);
    }

    $row = $callBean->db->fetchByAssoc($result);
}
echo $mod_strings['LBL_DIAGNOSTIC_DONE'];
