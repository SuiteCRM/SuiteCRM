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
<div class="filter-container">
    <label for="filter-resources-label">{$MOD.LBL_FILTER_RESOURCES}</label>
    <select name="filter-resources" id="filter-resources" class="filter-resources" multiple>
        <option value=""></option>
        {foreach from=$RESOURCESGROUP item=RESOURCES key=GROUP}
            <optgroup label="{$GROUP}">
                {foreach from=$RESOURCES item=RESOURCE}
                    {if $RESOURCE.selected}
                        <option selected value="{$RESOURCE.id}">{$RESOURCE.name}</option>
                    {else}
                        <option value="{$RESOURCE.id}">{$RESOURCE.name}</option>
                    {/if}
                {/foreach}
            </optgroup>
        {/foreach}
    </select>
    <button id='button_clear' class='button lastChild'>
        <span id='span_clear' class='suitepicon suitepicon-action-clear'></span>
    </button>
</div>

{literal}
    <style>
        .option {
            display: inline-block;
            border-radius: 3px;
            padding: 1px 3px;
            margin: 0 3px 3px 0;
            cursor: pointer;
            border: 0 solid rgba(0, 0, 0, 0);
        }
        
        .selectize-control .selectize-dropdown-content .option-optgroup {
            position: relative;
            font-weight: bold;
            background-color: #353535 !important;
            color: white !important;
        }

        .option-optgroup:hover, .option-optgroup:active {
            /* background-color: #1b1b1b;    */
        }

        .selectize-dropdown-content {
            margin-left: 10px;
        }

        .filter-container {
            display: none;
            justify-content: center;
            padding: 0 10px;
            max-width: 1400px;
            margin: 0px auto 2em;
        }

        .filter-resources {
            margin-left: 10px;
            margin-right: 10px;
        }

        .suitepicon-action-clear {
            line-height: unset;
        }

        .filter-info-sign {
            margin-top: 2px;
            margin-left: 4px;
        }
    </style>
{/literal}
