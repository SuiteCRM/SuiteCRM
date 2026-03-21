/*
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

/* global $, CalendarAuthVisibility */
// Depends on auth_fields_visibility.js

$(document).ready(function () {
    'use strict';

    if (typeof CalendarAuthVisibility === 'undefined') {
        return;
    }

    var sourceSelector = $('#source');
    sourceSelector.change(function () {
        CalendarAuthVisibility.onSourceChange($(this).val(), true);
        resetSyncStatusFields();
    });

    CalendarAuthVisibility.setMode(true);
    CalendarAuthVisibility.onSourceChange(sourceSelector.val(), false);

    CalendarAuthVisibility.handleCalendarUserField();

    translateEnumFields();
});

function translateEnumFields() {
    'use strict';

    if (typeof window.CalendarAccountFields === 'undefined') {
        return;
    }

    var getLabel = window.CalendarAccountFields.getLabel;

    var fieldMappings = [
        {
            fieldId: 'last_connection_status',
            labelMap: {
                '1': 'LBL_YES',
                '0': 'LBL_NO'
            }
        },
        {
            fieldId: 'last_sync_attempt_status',
            labelMap: {
                'in_progress': 'LBL_SYNC_STATUS_IN_PROGRESS',
                'success': 'LBL_SYNC_STATUS_SUCCESS',
                'warning': 'LBL_SYNC_STATUS_WARNING',
                'error': 'LBL_SYNC_STATUS_ERROR'
            }
        },
        {
            fieldId: 'last_sync_attempt_message',
            labelMap: {
                'sync_complete': 'LBL_SYNC_MSG_SYNC_COMPLETE',
                'up_to_date': 'LBL_SYNC_MSG_UP_TO_DATE',
                'meetings_failed': 'LBL_SYNC_MSG_MEETINGS_FAILED',
                'sync_partial': 'LBL_SYNC_MSG_SYNC_PARTIAL',
                'sync_error': 'LBL_SYNC_MSG_SYNC_ERROR',
                'token_expired': 'LBL_SYNC_MSG_TOKEN_EXPIRED',
                'connection_error': 'LBL_SYNC_MSG_CONNECTION_ERROR',
                'calendar_not_found': 'LBL_SYNC_MSG_CALENDAR_NOT_FOUND'
            }
        }
    ];

    fieldMappings.forEach(function (mapping) {
        var element = document.getElementById(mapping.fieldId);
        if (!element) {
            return;
        }

        var rawValue = element.textContent.trim();
        if (!rawValue) {
            return;
        }

        var labelKey = mapping.labelMap[rawValue];
        if (!labelKey) {
            return;
        }

        var translated = getLabel(labelKey);
        if (translated && translated !== labelKey && translated !== 'undefined') {
            element.textContent = translated;
        }
    });
}

function resetSyncStatusFields() {
    'use strict';

    var fieldsToReset = [
        'last_connection_status',
        'last_connection_test',
        'last_sync_attempt_status',
        'last_sync_attempt_date',
        'last_sync_attempt_message',
        'last_sync_date',
        'external_calendar_id'
    ];

    fieldsToReset.forEach(function (fieldId) {
        var span = document.getElementById(fieldId);
        if (span) {
            span.textContent = '';
        }

        var input = document.querySelector('input[name="' + fieldId + '"]');
        if (input) {
            input.value = '';
        }
    });
}
