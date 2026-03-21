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

require_once 'include/CalendarSync/domain/enums/CalendarEventType.php';
require_once 'include/CalendarSync/domain/helpers/DateTimeHelper.php';

/**
 * Represents a calendar account event, including its details such as title,
 * location, and event schedule. This class provides methods to retrieve,
 * modify, and compare event properties.
 *
 * Performance: Maintains a content checksum for fast equality comparisons.
 */
class CalendarAccountEvent
{

    public const DATE_FORMAT = DateTimeInterface::ATOM;

    protected DateTime $date_start;
    protected ?DateTime $date_end;
    protected DateTime $last_sync;
    protected DateTime $date_modified;
    protected string $content_checksum;

    public function __construct(
        protected string $id,
        protected string $name,
        protected string $description,
        protected string $location,
        DateTime|string $date_start,
        DateTime|string|null $date_end,
        protected string $assigned_user_id,
        protected CalendarEventType $type = CalendarEventType::MEETING,
        protected ?string $linked_event_id = null,
        DateTime|string|null $last_sync = null,
        DateTime|string $date_modified = new DateTime(),
        protected bool $is_external = false,
        $datetimeHelper = new DateTimeHelper()
    ) {
        $now = new DateTime();

        $this->date_start = $datetimeHelper->createDateTime($date_start) ?? $now;
        $this->date_end = $datetimeHelper->createDateTime($date_end);
        $this->last_sync = $datetimeHelper->createDateTime($last_sync) ?? new DateTime('-1 year');
        $this->date_modified = $datetimeHelper->createDateTime($date_modified) ?? $now;

        $this->content_checksum = $this->calculateChecksum();
    }

    public function getId(): string
    {
        return $this->id;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getDateStartString(): string
    {
        return $this->getDateStart()->format(self::DATE_FORMAT);
    }

    public function getDateStart(): DateTime
    {
        return $this->date_start;
    }

    public function getDateEndString(): ?string
    {
        return $this->getDateEnd()?->format(self::DATE_FORMAT);
    }

    public function getDateEnd(): ?DateTime
    {
        return $this->date_end;
    }

    public function getAssignedUserId(): string
    {
        return $this->assigned_user_id;
    }

    public function getType(): CalendarEventType
    {
        return $this->type;
    }

    public function getLinkedEventId(): ?string
    {
        return $this->linked_event_id;
    }

    public function setLinkedEventId(?string $linkedEventId): self
    {
        $this->linked_event_id = $linkedEventId;
        return $this;
    }

    public function getLastSyncString(): ?string
    {
        return $this->getLastSync()->format(self::DATE_FORMAT);
    }

    public function getLastSync(): DateTime
    {
        return $this->last_sync;
    }

    public function setLastSync(?DateTime $lastSync = new DateTime()): self
    {
        $this->last_sync = $lastSync;
        return $this;
    }

    public function getDateModifiedString(): string
    {
        return $this->getDateModified()->format(self::DATE_FORMAT);
    }

    public function getDateModified(): DateTime
    {
        return $this->date_modified;
    }

    public function isExternal(): bool
    {
        return $this->is_external;
    }

    public function getStartDateTime(): DateTime
    {
        return $this->date_start;
    }

    public function getEndDateTime(): DateTime
    {
        return $this->date_end;
    }

    public function getTitle(): string
    {
        return $this->name;
    }

    public function getDescription(): string
    {
        return $this->description;
    }

    public function getLocation(): string
    {
        return $this->location;
    }

    public function getContentChecksum(): string
    {
        return $this->content_checksum;
    }

    public function getChecksumArray(): array
    {
        return [
            'name' => $this->name,
            'description' => $this->description,
            'location' => $this->location,
            'date_start' => $this->date_start->format('U.u'),
            'date_end' => $this->date_end?->format('U.u') ?? '0'
        ];
    }

    protected function calculateChecksum(): string
    {
        $checksumData = implode('|', array_values($this->getChecksumArray()));

        return md5($checksumData);
    }

}