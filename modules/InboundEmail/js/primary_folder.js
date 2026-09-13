/*
 * SuiteCRM is a customer relationship management program developed by SuiteCRM Ltd.
 * Copyright (C) 2026 SuiteCRM Ltd.
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

(function () {
    'use strict';

    /**
     * Hides or shows the primary_folder row (both label and value cells) based on
     * whether any monitored folders are present.
     *
     * @param {HTMLElement} selectEl
     * @param {boolean}     hasFolders
     */
    function setPrimaryFolderVisibility(selectEl, hasFolders) {
        // The select sits inside a <td>; its preceding sibling <td> is the label cell.
        var valueCell = selectEl.parentNode;
        var labelCell = valueCell && valueCell.previousElementSibling;
        var display   = hasFolders ? '' : 'none';
        if (valueCell) {
            valueCell.style.display = display;
        }
        if (labelCell) {
            labelCell.style.display = display;
        }
    }

    /**
     * Populates the primary_folder <select> with options derived from the mailbox field.
     *
     * @param {boolean} preserveSelection - true  → keep the value currently shown in the select
     *                                              (called after the mailbox popup closes).
     *                                      false → pre-select using the data-selected attribute
     *                                              set server-side on initial page load.
     */
    function updatePrimaryFolderSelect(preserveSelection) {
        var mailboxEl = document.getElementById('mailbox');
        var selectEl  = document.getElementById('primary_folder');

        if (!mailboxEl || !selectEl) {
            return;
        }

        var folders = (mailboxEl.value || '')
            .split(',')
            .map(function (s) { return s.trim(); })
            .filter(Boolean);

        // Fall back to the first folder when no explicit value is saved yet
        var targetValue = preserveSelection
            ? selectEl.value
            : (selectEl.getAttribute('data-selected') || folders[0] || '');

        selectEl.innerHTML = '';

        folders.forEach(function (folder) {
            var opt = new Option(folder, folder, false, folder === targetValue);
            selectEl.add(opt);
        });

        setPrimaryFolderVisibility(selectEl, folders.length > 0);
    }

    function init() {
        var mailboxEl = document.getElementById('mailbox');
        if (mailboxEl) {
            mailboxEl.addEventListener('change', function () {
                updatePrimaryFolderSelect(true);
            });
        }
        updatePrimaryFolderSelect(false);
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }
}());
