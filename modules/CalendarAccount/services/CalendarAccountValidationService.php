<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class CalendarAccountValidationService
{
    private CalendarAccount $account;
    private User $currentUser;
    private array $errors = [];

    public function __construct(CalendarAccount $account, User $currentUser)
    {
        $this->account = $account;
        $this->currentUser = $currentUser;
    }

    public function validate(): bool
    {
        $this->errors = [];

        $this->validatePersonalAccountLimit();
        $this->validateGroupAccountCreation();
        $this->validateDuplicateExternalCalendar();

        return empty($this->getErrors());
    }

    public function getErrors(): array
    {
        return $this->errors;
    }

    public function getFirstError(): ?string
    {
        return !empty($this->errors) ? reset($this->errors) : null;
    }

    private function validatePersonalAccountLimit(): void
    {
        global $log;

        if ($this->account->type !== 'personal') {
            return;
        }

        $currentPersonalCalendarAccountId = $this->account->id;
        $ownerOfNewCalendarAccount = $this->account->calendar_user_id;

        require_once 'include/CalendarSync/CalendarSync.php';
        $personalAccounts = CalendarSync::getInstance()->getPersonalCalendarAccounts($ownerOfNewCalendarAccount);

        $personalAccountIds = array_unique(
            array_merge(
                array_map(static fn($account) => $account->id, $personalAccounts),
                [$currentPersonalCalendarAccountId]
            )
        );

        $userAlreadyHasPersonalAccount = count($personalAccountIds) > 1;
        if ($userAlreadyHasPersonalAccount) {
            $log->warning('[CalendarAccountValidationService][validatePersonalAccountLimit] User already has a personal calendar account');
            $this->errors[] = translate('LBL_ALREADY_HAS_PERSONAL_ACCOUNT', 'CalendarAccount');
        }
    }

    private function validateGroupAccountCreation(): void
    {
        global $log;

        $newGroupCalendarAccount = empty($this->account->id) && $this->account->type === 'group';
        if (!$newGroupCalendarAccount) {
            return;
        }

        $isAdmin = is_admin($this->currentUser);
        if (!$isAdmin) {
            $log->warning('[CalendarAccountValidationService][validateGroupAccountCreation] Only administrators can create group calendar accounts');
            $this->errors[] = translate('LBL_ADMIN_ONLY_GROUP_ACCOUNT', 'CalendarAccount');
        }
    }

    private function validateDuplicateExternalCalendar(): void
    {
        global $log;

        $externalCalendarId = $this->account->external_calendar_id ?? '';
        if (empty($externalCalendarId)) {
            return;
        }

        $currentId = $this->account->id ?? '';

        require_once 'include/CalendarSync/CalendarSync.php';
        $duplicate = CalendarSync::getInstance()->findDuplicateCalendarAccount($externalCalendarId, $currentId);

        if ($duplicate) {
            $existingAccountName = $duplicate->name ?? 'Unknown';
            $existingAccountId = $duplicate->id ?? 'Unknown';
            $errorMessage = translate('LBL_DUPLICATE_EXTERNAL_CALENDAR', 'CalendarAccount') . " '$existingAccountName'";

            $log->warn("[CalendarAccountValidationService][validateDuplicateExternalCalendar] Duplicate found: $existingAccountId");
            $this->errors[] = $errorMessage;
        }
    }
}
