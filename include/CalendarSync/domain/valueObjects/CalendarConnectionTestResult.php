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
 * Represents the result of testing a calendar connection.
 */
class CalendarConnectionTestResult
{

    public function __construct(
        private readonly bool $success = false,
        private readonly ?CalendarAccount $connection = null,
        private readonly ?string $errorMessage = null,
        private readonly ?string $errorCode = null,
        private readonly ?string $serverResponse = null,
        private readonly ?string $authenticationStatus = null,
        private readonly ?string $externalCalendarId = null
    ) {
    }

    /**
     * Determines if the operation was successful.
     *
     * @return bool Returns true if the operation was successful, otherwise false.
     */
    public function isSuccessful(): bool
    {
        return $this->success;
    }

    /**
     * Retrieves the error message associated with the current operation.
     *
     * @return string|null Returns the error message if available, otherwise null.
     */
    public function getErrorMessage(): ?string
    {
        return $this->errorMessage;
    }

    /**
     * Retrieves the external calendar ID that will be used for syncing.
     *
     * @return string|null Returns the external calendar ID if available, otherwise null.
     */
    public function getExternalCalendarId(): ?string
    {
        return $this->externalCalendarId;
    }

    /**
     * Converts the object to an associative array representation.
     *
     * @return array Returns an associative array containing the object's properties and their values.
     */
    public function toArray(): array
    {
        return [
            'success' => $this->success,
            'errorMessage' => $this->errorMessage,
            'errorCode' => $this->errorCode,
            'connectionType' => $this->connection?->source,
            'serverResponse' => $this->serverResponse,
            'authenticationStatus' => $this->authenticationStatus,
            'externalCalendarId' => $this->externalCalendarId
        ];
    }

}