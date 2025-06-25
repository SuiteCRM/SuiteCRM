{* 
 * This file is part of SinergiaCRM.
 * SinergiaCRM is a work developed by SinergiaTIC Association, based on SuiteCRM.
 * Copyright (C) 2013 - 2023 SinergiaTIC Association
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation.
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
 * You can contact SinergiaTIC Association at email address info@sinergiacrm.org.
 *}
{if !empty($FIELD.LINK_URL)}
    <div>
        <a href="{$FIELD.LINK_URL}">{$FIELD.LINK_TEXT}</a>
    </div>
{/if}
{if empty($FIELD.LINK_URL)}
    <div>
        <strong>{$FIELD.ACTION}</strong>
    </div>
{/if}
<br>
{if !empty($FIELD.ASSIGNED_USER_NAME)}
    <div>
        <strong>{$APP.LBL_ASSIGNED_TO_NAME}:</strong>
        {$FIELD.ASSIGNED_USER_NAME}
    </div>
{/if}
<br>

{if !empty($FIELD.DATE_MODIFIED)}
    <div>
        <strong>{$APP.LBL_DATE_LAST_ACTION}:</strong>
        {$FIELD.DATE_MODIFIED}
    </div>
{/if}
{if !empty($FIELD.ACTION) && !empty($FIELD.ITEM_ID)}
    <div>
        <strong>{$APP.LBL_ACTION}:</strong>
        {$FIELD.ACTION}
    </div>
{/if}
<br>
{if !empty($FIELD.MODULE_NAME)}
    <div>
        <strong>{$APP.LBL_MODULE}:</strong>
        {$FIELD.MODULE_NAME}
    </div>
{/if}
{if empty($FIELD.ITEM_ID) || empty($FIELD.LINK_URL)}
    <div>
        <strong>{$APP.LBL_ITEM_SUMMARY}:</strong>
        {$FIELD.ITEM_SUMMARY}
    </div>
{/if}
{if !empty($FIELD.ITEM_ID)}
    <div>
        <strong>{$APP.LBL_ITEM_ID}:</strong>
        {$FIELD.ITEM_ID}
    </div>
{/if}

<br>
{if !empty($FIELD.SESSION_ID)}
    <div>
        <strong>{$APP.LBL_SESSION_ID}:</strong>
        {$FIELD.SESSION_ID}
    </div>
{/if}