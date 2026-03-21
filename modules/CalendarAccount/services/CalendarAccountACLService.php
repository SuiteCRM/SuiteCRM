<?php

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class CalendarAccountACLService
{
    private CalendarAccount $account;
    private User $currentUser;

    public function __construct(CalendarAccount $account, User $currentUser)
    {
        $this->account = $account;
        $this->currentUser = $currentUser;
    }

    public function hasAccess(string $view): bool
    {
        if (!$view) {
            return true;
        }

        if ($this->isNotAllowedAction($view)) {
            $this->logAccessDenied($view);
            return false;
        }

        if ($this->isAllUsersAllowedAction($view)) {
            return true;
        }

        $isAdmin = is_admin($this->currentUser);

        if ($isAdmin && $this->isAdminAllowedAction($view)) {
            return true;
        }

        $isGroupAccount = $this->account->type !== 'personal';
        if ($isGroupAccount && $this->isSecurityGroupBasedAction($view)) {
            require_once 'modules/SecurityGroups/SecurityGroup.php';
            $hasGroupAccess = SecurityGroup::groupHasAccess('CalendarAccount', $this->account->id, $view);
            if ($hasGroupAccess) {
                return true;
            }
        }

        $isOwner = $isAdmin || $this->account->isOwner($this->currentUser->id);
        if ($isOwner && $this->isOwnerAllowedAction($view)) {
            return true;
        }

        $this->logAccessDenied($view);
        return false;
    }

    private function isNotAllowedAction(string $view): bool
    {
        $notAllowed = ['export', 'import', 'massupdate', 'duplicate'];
        $simplifiedAction = $this->simplifyAction($view);
        return in_array($simplifiedAction, $notAllowed);
    }

    private function isAllUsersAllowedAction(string $view): bool
    {
        $allowed = ['list'];
        $simplifiedAction = $this->simplifyAction($view);
        return in_array($simplifiedAction, $allowed);
    }

    private function isAdminAllowedAction(string $view): bool
    {
        $allowed = ['view', 'edit', 'delete'];
        $simplifiedAction = $this->simplifyAction($view);
        return in_array($simplifiedAction, $allowed);
    }

    private function isOwnerAllowedAction(string $view): bool
    {
        $allowed = ['view', 'edit'];
        $simplifiedAction = $this->simplifyAction($view);
        return in_array($simplifiedAction, $allowed);
    }

    private function isSecurityGroupBasedAction(string $view): bool
    {
        $allowed = ['view'];
        $simplifiedAction = $this->simplifyAction($view);
        return in_array($simplifiedAction, $allowed);
    }

    private function simplifyAction(string $view): string
    {
        $action = strtolower($view);
        return match ($action) {
            'list', 'index', 'listview' => 'list',
            'edit', 'save', 'popupeditview', 'editview' => 'edit',
            'view', 'detail', 'detailview', 'retrieve' => 'view',
            default => $action,
        };
    }

    private function logAccessDenied(string $view): void
    {
        global $log;

        $log->fatal("CalendarAccount | Access denied. Non-admin user trying to access personal account. Action: '$view' | Current user id: '{$this->currentUser->id}' | record: '{$this->account->id}'");
    }
}
