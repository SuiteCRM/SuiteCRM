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
{if !empty($FIELD.NAME)}
    <div>
        <a href="index.php?action=DetailView&module={$MODULE_NAME}&record={$FIELD.ID}">{$FIELD.NAME}</a>
    </div>
{/if}
<br>

{if !empty($FIELD.PARENT_ID)}
    <div>
        <strong>{$MOD.LBL_RELATED_TO}</strong>
        <a href="index.php?module={$FIELD.PARENT_TYPE}&action=DetailView&record={$FIELD.PARENT_ID}">{$APP_LIST.moduleList[$FIELD.PARENT_TYPE]} - {$FIELD.PARENT_NAME}</a>
    </div>
{/if}
<br>
{if !empty($FIELD.TYPE)}
    <div>
        <strong>{$PARAM.LBL_TYPE}:</strong>
        {$FIELD.TYPE}
    </div>
{/if}
{if !empty($FIELD.DIRECTION)}
    <div>
        <strong>{$PARAM.LBL_DIRECTION}:</strong>
        {$FIELD.DIRECTION}
    </div>
{/if}
{if !empty($FIELD.STATUS)}
    <div>
        <strong>{$PARAM.LBL_STATUS}:</strong>
        {$FIELD.STATUS}
    </div>
{/if}
{if !empty($FIELD.SENT_DATE)}
    <div>
        <strong>{$PARAM.LBL_SENT_DATE}:</strong>
        {$FIELD.SENT_DATE}
    </div>
{/if}
{if !empty($FIELD.PHONE)}
    <div>
        <strong>{$PARAM.LBL_PHONE}:</strong>
        {$FIELD.PHONE}
    </div>
{/if}
{if !empty($FIELD.TEMPLATE)}
    <div>
        <strong>{$PARAM.LBL_TEMPLATE}:</strong>
        {$FIELD.TEMPLATE}
    </div>
{/if}
{if !empty($FIELD.DATE_ENTERED)}
    <div>
        <strong>{$PARAM.LBL_DATE_ENTERED}:</strong>
        {$FIELD.DATE_ENTERED}
    </div>
{/if}
<br>
{if !empty($FIELD.STIC_CONVERSATIONS_STIC_MESSAGES_NAME)}
    <div>
        <strong>{$PARAM.LBL_STIC_CONVERSATIONS_STIC_MESSAGES}:</strong>
        <a href="index.php?module=stic_Conversations&action=DetailView&record={$FIELD.STIC_CONVERSATIONS_IDA}">{$FIELD.STIC_CONVERSATIONS_STIC_MESSAGES_NAME}</a>
    </div>
{/if}
{if !empty($FIELD.MESSAGE)}
    <div>
        <strong>{$PARAM.LBL_MESSAGE}:</strong>
        {$FIELD.MESSAGE}
    </div>
{/if}
