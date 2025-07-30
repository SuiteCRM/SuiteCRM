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
{* This template is showed both in Bookings' ListView and Bookings Calendar reservations popups *}

<h2 id="resourcesTitle">{$MOD.LBL_RESOURCES}  <button id="openCenterPopup" type="button" class="button">{$MOD.LBL_CENTERS_BUTTON}</button>
</h2>

<div class="filter-box">
    <div id="resourceSearchFields" class="filter-content">
        <div id="selectedCentersContainer">
            <div id="selectedCentersList"></div>
        </div>
        
        <div class="filter-row">
            <div class="filter-item">
                <label for="resourcePlaceUserType">{$MOD.LBL_RESOURCES_USER_TYPE}</label>
                <select id="resourcePlaceUserType" name="resourcePlaceUserType" multiple></select>
            </div>
            
            <div class="filter-item">
                <label for="resourcePlaceType">{$MOD.LBL_RESOURCES_PLACE_TYPE}</label>
                <select id="resourcePlaceType" name="resourcePlaceType" multiple></select>
            </div>
        </div>
        
        <div class="filter-row">
            <div class="filter-item">
                <label for="resourceGender">{$MOD.LBL_RESOURCES_GENDER}</label>
                <select id="resourceGender" name="resourceGender" multiple></select>
            </div>
            
            <div class="filter-item">
                <label for="resourceName">{$MOD.LBL_RESOURCES_NAME}</label>
                <input type="text" id="resourceName" name="resourceName">
            </div>
        </div>
        
        <div class="filter-row">
            <div class="filter-item">
                <label for="numberOfPlaces">{$MOD.LBL_NUMBER_OF_PLACES}</label>
                <input type="number" id="numberOfPlaces" name="numberOfPlaces">
            </div>
        </div>
        <div class="filter-actions grouped-buttons">
            <button id="loadCenterResourcesButton" type="button" class="button">
                {$MOD.LBL_ADD_BUTTON}
            </button>
            <button id="resetResourcesButton" type="button" class="button">
                {$MOD.LBL_UNDO_BUTTON}
            </button>
            <button id="deleteResourcesButton" type="button" class="button">
                {$APP.LBL_DELETE_BUTTON}
            </button>
            <span id="resourceCount"></span>
        </div>
    </div>
</div>
<br>
<table id="resourceLine" class="resource-table">
    <tr>
        {foreach from=$config_resource_fields key=field item=label}
            <th class="resource_column {if $field eq 'name'}resource_name{/if}
                {if $field eq 'hourly_rate' || $field eq 'daily_rate'}hidden-xs hidden-sm{/if}">
                {$label}
            </th>
        {/foreach}
        <th class="resource_column"></th>
    </tr>
</table>
<div style="padding-top: 2px">
    <input type="button" class="button" value="{$MOD.LBL_RESOURCES_ADD}" id="addResourceLine" />
</div>
<br>
{literal}
<script>
$(document).ready(function() {
    // Initialize Selectize on all multiple select elements
    $('#stic_resources_places_gender_list, #stic_resources_places_type_list, #stic_resources_places_users_list').selectize({
        plugins: ['remove_button'],
        delimiter: ',',
        persist: false,
        allowEmptyOption: false,
        create: false
    });
});
</script>
    <style>
        .resource-table .resouce_data_group>input {
            width: calc(100% - 85px);
        }

        .resource-table {
            width: 100%;
        }

        #resourceLine th {
            white-space: initial;
        }

        #resourceLine input.resource_color,
        #resourceLine input.resource_status,
        #resourceLine input.resource_hourly_rate,
        #resourceLine input.resource_daily_rate {
            max-width: 90px !important;
        }

        .resource-table th.resource_name {
            width: calc(20% + 85px);
            min-width: 250px;
        }

        .resource_data {
            color: grey;
            width: 95%;
            border-color: grey !important;
        }

        .filter-box {
            border: 1px solid #ddd;
            border-radius: 4px;
            background-color: #f9f9f9;
            padding: 15px;
            margin-bottom: 20px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        }
        
        .filter-content {
            display: flex;
            flex-direction: column;
            gap: 10px;
        }
        
        .filter-row {
            display: flex;
            flex-wrap: wrap;
            gap: 15px;
            margin-bottom: 10px;
        }
        
        .filter-item {
            flex: 1;
            min-width: 200px;
        }
        
        .filter-item label {
            display: block;
            margin-bottom: 5px;
            font-weight: bold;
        }
        
        .filter-item select,
        .filter-item input {
            width: 100%;
            padding: 6px 8px;
            border: 1px solid #ccc;
            border-radius: 4px;
        }
        .filter-actions {
            display: flex;
            align-items: center;
            gap: 8px; /* espaciado entre botones */
            margin-top: 5px;
        }
        
        #resourceCount {
            font-weight: bold;
        }
        #numberOfPlaces {
            width: 100px;
        }
        #resourceName {
            height: 32px; 
            box-sizing: border-box;
            padding: 8px 10px;
            border: 1px solid #d0d0d0;
            border-radius: 3px;
            width: 90%;
            font-size: 13px;
        }

        #selectedCentersContainer {
            margin-bottom: 10px;
        }
    </style>
{/literal}
{{include file='include/EditView/footer.tpl'}}