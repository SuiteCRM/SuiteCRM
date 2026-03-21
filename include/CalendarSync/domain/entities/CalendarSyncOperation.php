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

require_once 'include/CalendarSync/domain/enums/CalendarLocation.php';
require_once 'include/CalendarSync/domain/enums/CalendarSyncAction.php';

/**
 * Represents an operation to synchronize calendar data.
 */
class CalendarSyncOperation
{

    public function __construct(
        protected string $user_id,
        protected string $calendar_account_id,
        protected string $subject_id,
        protected CalendarLocation $location,
        protected CalendarSyncAction $action,
        protected ?CalendarAccountEvent $payload = null
    ) {
    }

    public function getUserId(): string
    {
        return $this->user_id;
    }

    public function getSubjectId(): string
    {
        return $this->subject_id;
    }

    public function getLocation(): CalendarLocation
    {
        return $this->location;
    }

    public function getAction(): CalendarSyncAction
    {
        return $this->action;
    }

    public function getCalendarAccountId(): string
    {
        return $this->calendar_account_id;
    }

    public function getPayload(): ?CalendarAccountEvent
    {
        return $this->payload;
    }

}