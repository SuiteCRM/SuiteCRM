{*
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
*}

<table class="calendar-sync-container">
    <tr>
        <td colspan='100'>
            <h2>{$MOD.LBL_CALENDAR_SYNC_SETTINGS}</h2>
        </td>
    </tr>
    <tr>
        <td colspan='100'>
            {$MOD.LBL_CALENDAR_SYNC_DESCRIPTION}
        </td>
    </tr>
    <tr>
        <td>
            <br>
        </td>
    </tr>
    <tr>
        <td colspan='100'>

            {if $message}
                <div
                        class="alert {if $success}alert-success{else}alert-danger{/if}"
                        role="alert"
                >
                    {$message|escape:'html'}
                </div>
            {/if}

            <form name="CalendarSyncSettings" method="POST" action="index.php">
                <input type="hidden" name="module" value="Administration">
                <input type="hidden" name="action" value="CalendarSyncSettings">
                <input type="hidden" name="form_action" value="save">
                <input type="hidden" name="return_module" value="Administration">
                <input type="hidden" name="return_action" value="index">

                <table class="actionsContainer">
                    <tr>
                        <td>
                            <input
                                    title="{$APP.LBL_SAVE_BUTTON_TITLE}"
                                    class="button primary"
                                    type="submit"
                                    name="button"
                                    value="{$APP.LBL_SAVE_BUTTON_LABEL}"
                            >
                            <input
                                    title="{$APP.LBL_CANCEL_BUTTON_TITLE}"
                                    class="button"
                                    onclick="this.form.action.value='index'; this.form.module.value='Administration';"
                                    type="submit"
                                    name="button"
                                    value="{$APP.LBL_CANCEL_BUTTON_LABEL}"
                            >
                        </td>
                    </tr>
                </table>

                <div class='add_table calendar-sync-form-section'>
                    <div class="themeSettings edit view calendar-sync-settings">
                        {if $status.exists}
                            <div class="calendar-sync-separator">
                                <h4>{$MOD.LBL_CALENDAR_SYNC_STATUS_INFO}</h4>
                            </div>

                            <div class="calendar-sync-field-row">
                                <div class="calendar-sync-label">
                                    <label>{$MOD.LBL_CALENDAR_SYNC_SCHEDULER_STATUS}</label>
                                </div>
                                <div class="calendar-sync-input">
                                    <span class="{if $status.enabled}text-success{else}text-muted{/if}">
                                        {if $status.enabled}{$MOD.LBL_CALENDAR_SYNC_ACTIVE}{else}{$MOD.LBL_CALENDAR_SYNC_INACTIVE}{/if}
                                    </span>
                                </div>
                            </div>

                            <div class="calendar-sync-field-row">
                                <div class="calendar-sync-label">
                                    <label>{$MOD.LBL_CALENDAR_SYNC_LAST_SCHEDULED_RUN}</label>
                                </div>
                                <div class="calendar-sync-input">
                                    {if $status.lastScheduledRunTime}
                                        <span class="text-muted">{$status.lastScheduledRunTime}</span>
                                    {else}
                                        <span class="text-muted">{$MOD.LBL_CALENDAR_SYNC_NEVER}</span>
                                    {/if}
                                </div>
                            </div>

                            <div class="calendar-sync-field-row">
                                <div class="calendar-sync-label">
                                    <label>{$MOD.LBL_CALENDAR_SYNC_LAST_MANUAL_RUN}</label>
                                </div>
                                <div class="calendar-sync-input">
                                    {if $lastManualRunTime}
                                        <span class="text-muted">{$lastManualRunTime}</span>
                                    {else}
                                        <span class="text-muted">{$MOD.LBL_CALENDAR_SYNC_NEVER}</span>
                                    {/if}
                                </div>
                            </div>

                            <div class="calendar-sync-divider"></div>
                        {/if}

                        <div class="calendar-sync-separator">
                            <h4>{$MOD.LBL_CALENDAR_SYNC_BATCH_CONTROL}</h4>
                        </div>

                        <div class="calendar-sync-field-row">
                            <div class="calendar-sync-label">
                                <label for="max_accounts_per_sync">{$MOD.LBL_CALENDAR_SYNC_MAX_ACCOUNTS_PER_SYNC}</label>
                            </div>
                            <div class="calendar-sync-input">
                                <input
                                        type="number"
                                        name="max_accounts_per_sync"
                                        id="max_accounts_per_sync"
                                        class="form-control calendar-sync-number-input"
                                        value="{$calendarSyncConfig.max_accounts_per_sync}"
                                        min="1"
                                        max="200"
                                >
                                <span class="calendar-sync-description">{$MOD.LBL_CALENDAR_SYNC_MAX_ACCOUNTS_PER_SYNC_DESC}</span>
                            </div>
                        </div>

                        <div class="calendar-sync-field-row">
                            <div class="calendar-sync-label">
                                <label for="max_operations_per_account">{$MOD.LBL_CALENDAR_SYNC_MAX_OPERATIONS_PER_ACCOUNT}</label>
                            </div>
                            <div class="calendar-sync-input">
                                <input
                                        type="number"
                                        name="max_operations_per_account"
                                        id="max_operations_per_account"
                                        class="form-control calendar-sync-number-input"
                                        value="{$calendarSyncConfig.max_operations_per_account}"
                                        min="1"
                                        max="1000"
                                >
                                <span class="calendar-sync-description">{$MOD.LBL_CALENDAR_SYNC_MAX_OPERATIONS_PER_ACCOUNT_DESC}</span>
                            </div>
                        </div>

                        <div class="calendar-sync-divider"></div>

                        <div class="calendar-sync-separator">
                            <h4>{$MOD.LBL_CALENDAR_SYNC_ADVANCED_SETTINGS}</h4>
                        </div>

                        <div class="calendar-sync-field-row">
                            <div class="calendar-sync-label">
                                <label for="sync_window_past_days">{$MOD.LBL_CALENDAR_SYNC_WINDOW_PAST_DAYS}</label>
                            </div>
                            <div class="calendar-sync-input">
                                <input
                                        type="number"
                                        name="sync_window_past_days"
                                        id="sync_window_past_days"
                                        class="form-control calendar-sync-number-input"
                                        value="{$calendarSyncConfig.sync_window_past_days}"
                                        min="0"
                                        max="365"
                                >
                                <span class="calendar-sync-description">{$MOD.LBL_CALENDAR_SYNC_WINDOW_PAST_DAYS_DESC}</span>
                            </div>
                        </div>

                        <div class="calendar-sync-field-row">
                            <div class="calendar-sync-label">
                                <label for="sync_window_future_days">{$MOD.LBL_CALENDAR_SYNC_WINDOW_FUTURE_DAYS}</label>
                            </div>
                            <div class="calendar-sync-input">
                                <input
                                        type="number"
                                        name="sync_window_future_days"
                                        id="sync_window_future_days"
                                        class="form-control calendar-sync-number-input"
                                        value="{$calendarSyncConfig.sync_window_future_days}"
                                        min="1"
                                        max="730"
                                >
                                <span class="calendar-sync-description">{$MOD.LBL_CALENDAR_SYNC_WINDOW_FUTURE_DAYS_DESC}</span>
                            </div>
                        </div>

                        <div class="calendar-sync-field-row">
                            <div class="calendar-sync-label">
                                <label for="conflict_resolution">{$MOD.LBL_CALENDAR_SYNC_CONFLICT_RESOLUTION}</label>
                            </div>
                            <div class="calendar-sync-input">
                                <select
                                        name="conflict_resolution"
                                        id="conflict_resolution"
                                        class="form-control calendar-sync-select"
                                >
                                    {foreach from=$conflictResolutionOptions key=value item=label}
                                        <option
                                                value="{$value}"
                                                {if $calendarSyncConfig.conflict_resolution == $value}selected{/if}
                                        >
                                            {$label}
                                        </option>
                                    {/foreach}
                                </select>
                                <span class="calendar-sync-description">{$MOD.LBL_CALENDAR_SYNC_CONFLICT_RESOLUTION_DESC}</span>
                            </div>
                        </div>

                        <div class="calendar-sync-divider"></div>

                        <div class="calendar-sync-separator">
                            <h4>{$MOD.LBL_CALENDAR_SYNC_LOGIC_HOOKS_CONTROL}</h4>
                        </div>

                        <div class="calendar-sync-field-row">
                            <div class="calendar-sync-label">
                                <label for="enable_calendar_sync_logic_hooks">{$MOD.LBL_CALENDAR_SYNC_ENABLE_LOGIC_HOOKS}</label>
                            </div>
                            <div class="calendar-sync-input">
                                <input
                                        type="hidden"
                                        name="enable_calendar_sync_logic_hooks"
                                        value="0"
                                >
                                <input
                                        type="checkbox"
                                        name="enable_calendar_sync_logic_hooks"
                                        id="enable_calendar_sync_logic_hooks"
                                        class="form-control calendar-sync-checkbox"
                                        value="1"
                                        {if $calendarSyncConfig.enable_calendar_sync_logic_hooks}checked{/if}
                                >
                                <span class="calendar-sync-description">{$MOD.LBL_CALENDAR_SYNC_ENABLE_LOGIC_HOOKS_DESC}</span>
                            </div>
                        </div>

                    </div>
                </div>

                <table class="actionsContainer">
                    <tr>
                        <td>
                            <input
                                    title="{$APP.LBL_SAVE_BUTTON_TITLE}"
                                    class="button primary"
                                    type="submit"
                                    name="button"
                                    value="{$APP.LBL_SAVE_BUTTON_LABEL}"
                            >
                            <input
                                    title="{$APP.LBL_CANCEL_BUTTON_TITLE}"
                                    class="button"
                                    onclick="this.form.action.value='index'; this.form.module.value='Administration';"
                                    type="submit"
                                    name="button"
                                    value="{$APP.LBL_CANCEL_BUTTON_LABEL}"
                            >
                        </td>
                    </tr>
                </table>
            </form>

            <div class="calendar-sync-manual-trigger-section">
                <div class="alert alert-warning" role="alert">
                    <h4 class="alert-heading">{$MOD.LBL_CALENDAR_SYNC_MANUAL_TRIGGER_TITLE}</h4>
                    <p>{$MOD.LBL_CALENDAR_SYNC_MANUAL_TRIGGER_DESC}</p>
                    <hr>
                    <form method="POST" action="index.php" style="margin: 0;">
                        <input type="hidden" name="module" value="Administration">
                        <input type="hidden" name="action" value="CalendarSyncSettings">
                        <input type="hidden" name="form_action" value="manual_trigger">
                        <input type="hidden" name="return_module" value="Administration">
                        <input type="hidden" name="return_action" value="CalendarSyncSettings">

                        <button
                                type="submit"
                                class="btn btn-warning calendar-sync-manual-trigger-btn"
                                title="{$MOD.LBL_CALENDAR_SYNC_MANUAL_TRIGGER_TOOLTIP}"
                                onclick="return confirm('{$MOD.LBL_CALENDAR_SYNC_MANUAL_TRIGGER_CONFIRM|escape:'javascript'}');"
                        >
                            <strong>{$MOD.LBL_CALENDAR_SYNC_MANUAL_TRIGGER_BUTTON}</strong>
                        </button>
                    </form>
                </div>
            </div>

        </td>
    </tr>
</table>

{literal}
    <style>
        .calendar-sync-container {
            width: 100%;
        }

        .calendar-sync-form-section {
            margin-block: 20px;
        }

        .calendar-sync-settings {
            margin-bottom: 0;
            padding: 20px;
            border: 1px solid #ddd;
            background-color: #f9f9f9;
            border-radius: 4px;
        }

        .calendar-sync-field-row {
            display: flex;
            align-items: center;
            margin-bottom: 15px;
        }

        .calendar-sync-field-row:last-child {
            margin-bottom: 0;
        }

        .calendar-sync-label {
            width: 20%;
            margin-right: 20px;
            font-weight: 500;
        }

        .calendar-sync-input {
            width: 80%;
        }

        .calendar-sync-description {
            margin-left: 10px;
            color: #666;
            font-size: 0.9em;
        }

        .calendar-sync-select {
            width: 450px;
        }

        .calendar-sync-number-input {
            width: 150px;
        }

        .calendar-sync-separator {
            margin: 0 0 15px 0;
            padding: 10px 0;
            border-top: 1px solid #ddd;
        }

        .calendar-sync-divider {
            height: 1px;
            margin-bottom: 24px;
        }

        .calendar-sync-separator h4 {
            margin: 0;
            color: #333;
            font-size: 1.1em;
            font-weight: 600;
        }

        .calendar-sync-manual-trigger-section {
            margin-top: 20px;

            margin-bottom: 20px;
        }

        .calendar-sync-manual-trigger-btn {
            padding: 10px 20px;
            font-size: 16px;
            border-radius: 5px;
            border: none;
            background-color: #f0ad4e;
            color: white;
            cursor: pointer;
            transition: background-color 0.3s;
        }

        .calendar-sync-manual-trigger-btn:hover {
            background-color: #ec971f;
        }
    </style>
{/literal}