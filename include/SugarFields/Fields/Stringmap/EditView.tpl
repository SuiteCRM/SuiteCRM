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


<div class="string-map-container" id="{{if empty($displayParams.idName)}}{{sugarvar key='name'}}-string-map{{else}}{{$displayParams.idName}}-string-map{{/if}}">
    {assign var=json value={{sugarvar key='value' string=true}}}
    {assign var=values value=$json|json_decode}

    <script src='{sugar_getjspath file="include/SugarFields/Fields/Stringmap/js/stringmap.js"}'></script>
    <script src='{sugar_getjspath file="include/SugarFields/Fields/Stringmap/js/stringmap-factory.js"}'></script>
    <style>
    {literal}
        .string-map-entry-row {
            display: flex;
            justify-content: start;
        }
        .string-map-key-col, .string-map-value-col {
            flex-grow: 1;
        }
        .string-map-actions {
            display: flex;
            justify-content: end;
        }
        .string-map-action .add-btn, .string-map-button-col .remove-btn {
            width: 30px;
        }
    {/literal}
    </style>

    <div class="container-fluid string-map-values-list">
          {if !empty($values) }
            {foreach from=$values key=k item=v}
                <div class="string-map-entry-row">
                    {{if $vardef.show_keys === true}}
                        <div class="string-map-key-col">
                            <input type="text" name="{{$idname}}-key[]" value="{$k}" placeholder="{{$entry_key_label}}">
                        </div>
                    {{/if}}
                    <div class="string-map-value-col">
                        <input type="text" name="{{$idname}}-value[]" value="{$v}" {{if !empty($vardef.show_keys)}} {if empty($k) && empty($v)}placeholder="{{$entry_value_label}}"{/if} {{/if}}>
                    </div>
                    <div class="string-map-button-col">
                        <button type="button" class="btn btn-sm btn-primary remove-btn" onclick=""> - </button>
                    </div>
                </div>
            {/foreach}
        {/if}
    </div>
    <div class="container-fluid string-map-actions">
        <div class="string-map-action">
            <button type="button" class="btn btn-sm btn-primary add-btn" onclick=""> + </button>
        </div>
    </div>

    {literal}
        <script>
          $(document).ready(function(){
            var fieldIdentifier = '{/literal}{{if empty($displayParams.idName)}}{{sugarvar key='name'}}{{else}}{{$displayParams.idName}}{{/if}}{literal}';

            var options = {
              containerSelector: '#' + fieldIdentifier + '-string-map',
              showKeys: {/literal}{{if $vardef.show_keys === true}}true{{else}}false{{/if}}{literal},
              idName: fieldIdentifier,
              keyPlaceholder: '{/literal}{{$entry_key_label}}{literal}',
              valuePlaceholder: '{/literal}{{$entry_value_label}}{literal}'
            };

            var stringMap = StringMapFactory.make(options);
            stringMap.initEdit();

            $(fieldIdentifier + '-string-map').data('string-map', stringMap);
          });
        </script>
    {/literal}
</div>
