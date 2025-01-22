{*
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2022 SalesAgility Ltd.
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

{{capture name=idname assign=idname}}{{sugarvar key='name'}}{{/capture}}

{{if !empty($displayParams.idName)}}
    {{assign var=idname value=$displayParams.idName}}
{{/if}}

<style>
    {literal}
        .string-map-label {
            font-weight: bold;
            padding-right: 10px;
        }
    {/literal}
</style>

<div class="string-map-container {{if empty($displayParams.idName)}}{{sugarvar key='name'}}-string-map{{else}}{{$displayParams.idName}}-string-map{{/if}}">

    {assign var=json value={{sugarvar key='value' string=true}}}
    {assign var=values value=$json|json_decode}

    <div class="container-fluid string-map-values-list">
        {if !empty($values) }
            {foreach from=$values key=k item=v}
                <div class="row string-map-entry-row">
                    {{if $vardef.show_keys === true}}
                        <div class="col-sm-6 col-xs-12 string-map-key-col">
                            {{if !empty($entry_value_label)}}<span class="string-map-label">{{$entry_key_label}}:</span>{{/if}}<span>{$k}</span>
                        </div>
                    {{/if}}
                    <div class="col-sm-6 col-xs-12 string-map-value-col">
                        {{if !empty($entry_value_label)}}<span class="string-map-label">{{$entry_value_label}}:</span>{{/if}}<span>{$v}</span>
                    </div>
                </div>
            {/foreach}
        {/if}
    </div>
</div>
