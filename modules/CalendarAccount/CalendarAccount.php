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
 * Calendar Account
 */
#[\AllowDynamicProperties]
class CalendarAccount extends Basic
{

    public $new_schema = true;
    public $module_dir = 'CalendarAccount';
    public $object_name = 'CalendarAccount';
    public $table_name = 'calendar_accounts';

    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_link;
    public $SecurityGroups;

    public $disable_row_level_security = true;

    public $source;
    public $type;

    // OAuth2 Fields
    public $oauth_connection_id;
    public $oauth_connection_name;

    // Basic Auth Fields
    public $username;
    public $password;
    public $server_url;

    // API Key Fields
    public $api_key;
    public $api_endpoint;

    // Connection Status Fields
    public $last_connection_status;
    public $last_connection_test;
    public $last_sync_date;
    public $last_sync_attempt_date;
    public $last_sync_attempt_status;
    public $last_sync_attempt_message;
    public $external_calendar_id;

    // Calendar User Fields
    public $calendar_user_id;
    public $calendar_user_name;

    /**
     * @inheritDoc
     */
    public function retrieve($id = -1, $encode = true, $deleted = true): CalendarAccount|null
    {
        /** @var CalendarAccount $result */
        $result = parent::retrieve($id, $encode, $deleted);

        return $result?->ACLAccess('retrieve') ? $result : null;
    }

    /**
     * @inheritDoc
     * @throws Exception
     */
    public function save($check_notify = false): string
    {
        global $current_user, $log;

        if (!$this->ACLAccess('save')) {
            $log->warning('[CalendarAccount][save] User does not have permission to save');
            throw new RuntimeException(translate('LBL_NO_ACCESS', 'CalendarAccount'));
        }

        require_once 'modules/CalendarAccount/services/CalendarAccountValidationService.php';
        $validator = new CalendarAccountValidationService($this, $current_user);

        if (!$validator->validate()) {
            $error = $validator->getFirstError();
            $log->warning("[CalendarAccount][save] Validation failed: $error");
            throw new RuntimeException($error);
        }

        $this->keepWriteOnlyFieldValues();
        $this->clearAuthFieldsOnSourceChange();

        return parent::save($check_notify);
    }

    /**
     * @inheritDoc
     */
    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
            default:
                return parent::bean_implements($interface);
        }
    }

    /**
     * @inheritDoc
     */
    public function ACLAccess($view, $is_owner = 'not_set', $in_group = 'not_set'): bool
    {
        if (!$view){
            return true;
        }

        global $current_user;

        require_once 'modules/CalendarAccount/services/CalendarAccountACLService.php';
        if (!(new CalendarAccountACLService($this, $current_user))->hasAccess($view)) {
            return false;
        }

        return parent::ACLAccess($view, $is_owner, $in_group);
    }

    /**
     * @inheritDoc
     */
    public function create_new_list_query(
        $order_by,
        $where,
        $filter = array(),
        $params = array(),
        $show_deleted = 0,
        $join_type = '',
        $return_array = false,
        $parentbean = null,
        $singleSelect = false,
        $ifListForExport = false
    ): array|string
    {
        global $current_user, $db;

        $ret_array = parent::create_new_list_query(
            $order_by,
            $where,
            $filter,
            $params,
            $show_deleted,
            $join_type,
            true,
            $parentbean,
            $singleSelect,
            $ifListForExport
        );

        if (is_array($ret_array) && !empty($ret_array['where']) && !is_admin($current_user)) {
            $tableName = $db->quote($this->table_name);
            $currentUserId = $db->quote($current_user->id);

            $wrap = static fn($condition) => "($condition)";

            $personalAccountsConditions = implode(' AND ', array_map($wrap,[
                "$tableName.type = 'personal'",
                "$tableName.calendar_user_id = '$currentUserId'",
            ]));

            $groupAccountsConditions = implode(' AND ', array_map($wrap, [
                "$tableName.type != 'personal'",
                SecurityGroup::getGroupWhere($tableName, $this->module_dir, $current_user->id)
            ]));

            $access = implode(' OR ', array_map($wrap, [
                $personalAccountsConditions,
                $groupAccountsConditions
            ]));

            $ret_array['where'] .= " AND ($access)";
        }

        if ($return_array) {
            return $ret_array;
        }

        return "{$ret_array['select']}{$ret_array['from']}{$ret_array['where']}{$ret_array['order_by']}";
    }

    /**
     * Clear authentication fields when source changes
     * @return void
     */
    protected function clearAuthFieldsOnSourceChange(): void
    {
        global $log;

        $attributeSource = $this->fetched_row['source'] ?? '';
        $fieldSource = $this->source ?? '';

        if ($attributeSource === $fieldSource) {
            return;
        }

        $fieldsToHide = [];
        try {
            require_once 'include/CalendarSync/CalendarSync.php';
            $fieldsToHide = CalendarSync::getInstance()->getFieldsToHide($fieldSource);
        } catch (Throwable $e) {
            $log->warn("[CalendarAccount][clearAuthFieldsOnSourceChange] Could not get fields to hide for source: $fieldSource - {$e->getMessage()}");
            return;
        }

        foreach ($fieldsToHide as $fieldToHide) {
            $this->$fieldToHide = '';

            $fieldDef = $this->field_defs[$fieldToHide] ?? [];
            $isRelate = ($fieldDef['type'] ?? '') === 'relate';
            $relateFieldToHide = $fieldDef['id_name'] ?? '';
            if ($isRelate && !empty($relateFieldToHide)) {
                $this->$relateFieldToHide = '';
            }
        }

        $this->last_connection_status = '0';
        $this->last_connection_test = '';
        $this->last_sync_attempt_status = '';
        $this->last_sync_attempt_date = '';
        $this->last_sync_attempt_message = '';
        $this->last_sync_date = '';
        $this->external_calendar_id = '';
    }

    /**
     * Do not clear write only fields
     * @return void
     */
    protected function keepWriteOnlyFieldValues(): void
    {
        if (empty($this->fetched_row)) {
            return;
        }

        foreach ($this->field_defs as $field => $field_def) {
            if (empty($field_def['display']) || $field_def['display'] !== 'writeonly') {
                continue;
            }

            if (empty($this->fetched_row[$field])) {
                continue;
            }

            if (!empty($this->$field)) {
                continue;
            }

            $this->$field = $this->fetched_row[$field];
        }
    }

    /**
     * @inheritDoc
     */
    public function isOwner($user_id): bool
    {
        if (!isset($this->id)  || $this->id === "[SELECT_ID_LIST]") {
            return true;
        }

        if (!empty($this->calendar_user_id) && $this->calendar_user_id === $user_id) {
            return true;
        }

        return false;
    }

    /**
     * Update sync metadata fields without affecting sensitive field encryption state.
     * Use this for internal sync operations that need to update timestamps
     * without triggering full save side effects.
     *
     * @param array $fields Associative array of field => value to update
     * @return void
     */
    public function updateSyncMetadata(array $fields): void
    {
        global $log, $timedate;

        $allowedFields = [
            'last_sync_date',
            'last_sync_attempt_date',
            'last_sync_attempt_status',
            'last_sync_attempt_message',
            'last_connection_status',
            'last_connection_test',
            'external_calendar_id'
        ];

        $updateParts = [];
        foreach ($fields as $field => $value) {
            if (!in_array($field, $allowedFields, true)) {
                $log->warn("[CalendarAccount][updateSyncMetadata] Attempted to update non-allowed field: $field");
                continue;
            }

            $quotedValue = $this->db->quoted($value);
            $updateParts[] = "$field = $quotedValue";

            try {
                $this->$field = $timedate->asUser($timedate->fromDb($value));
            } catch (Throwable $e) {
            }
        }

        if (empty($updateParts)) {
            return;
        }

        $updateParts[] = "date_modified = " . $this->db->quoted($timedate->nowDb());

        $updatePartsImplode = implode(', ', $updateParts);
        $idQuoted = $this->db->quoted($this->id);
        $sql = "UPDATE $this->table_name SET $updatePartsImplode WHERE id = $idQuoted";

        $this->db->query($sql);

        $log->debug("[CalendarAccount][updateSyncMetadata] Updated metadata for account $this->id: " . implode(', ', array_keys($fields)));
    }

}