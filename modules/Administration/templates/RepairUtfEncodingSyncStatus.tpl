{*
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2018 SalesAgility Ltd.
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Affero General Public License version 3 as published by the
 * Free Software Foundation with the addition of the following permission added
 * to Section 15 as permitted in Section 7(a): FOR ANY PART OF THE COVERED WORK
 * IN WHICH THE COPYRIGHT IS OWNED BY SUGARCRM, SUGARCRM DISCLAIMS THE WARRANTY
 * OF NON INFRINGEMENT OF THIRD PARTY RIGHTS.
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
 * You can contact SugarCRM, Inc. headquarters at 10050 North Wolfe Road,
 * SW2-130, Cupertino, CA 95014, USA. or at email address contact@sugarcrm.com.
 *
 * The interactive user interfaces in modified source and object code versions
 * of this program must display Appropriate Legal Notices, as required under
 * Section 5 of the GNU Affero General Public License version 3.
 *
 * In accordance with Section 7(b) of the GNU Affero General Public License version 3,
 * these Appropriate Legal Notices must retain the display of the "Powered by
 * SugarCRM" logo and "Supercharged by SuiteCRM" logo. If the display of the logos is not
 * reasonably feasible for technical reasons, the Appropriate Legal Notices must
 * display the words "Powered by SugarCRM" and "Supercharged by SuiteCRM".
 */
*}

<div class="view" >
    <h2 class="pt-0">{$MOD.LBL_REPAIR_UTF_ENCODING}</h2>

    <div class="container-fluid">
        <div class="row">
            <div class="col-sm-2">
                <strong>{$MOD.LBL_EXECUTION_STATUS}</strong>
            </div>
            <div class="col-sm-1">
                <span class="label label-warning">{$MOD.LBL_IN_PROGRESS}</span>
            </div>
        </div>
        <div class="row">
            <div class="col-sm-2">
                <strong>{$MOD.LBL_EXECUTION_MODE}</strong>
            </div>
            <div class="col-sm-1">
                <span class="label label-warning">{$MOD.LBL_SYNCHRONOUS}</span>
            </div>
        </div>
    </div>

    {if $mode eq 'sync'}
        <hr/>
        <div class="alert alert-warning sm" role="alert">
            <h4 class="alert-heading">{$MOD.LBL_WARNING}</h4>
            <p>{$MOD.LBL_SYNC_LONG_EXECUTION_WARNING}</p>
            <p>{$MOD.LBL_SYNC_RUNNING_INFORMATION_OUTPUT}</p>
            <p>{$MOD.LBL_SYNC_RUNNING_INFORMATION_LOGS}</p>
        </div>

    {/if}
</div>
<h3 class="pt-0">{$MOD.LBL_OUTPUT}</h3>

