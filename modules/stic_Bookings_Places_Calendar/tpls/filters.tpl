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
<form name="form_filters" id="form_filters" method="POST" action="index.php?module=stic_Bookings_Places_Calendar&action=SaveFilters">
    <div class="filters-container">
        <div class="filter-row">
            <div class="filter-group">
                <label for="stic_center_name">{$MOD.LBL_FILTERS_STIC_CENTER}</label>
                <div class="input-group">
                    <input type='text' class='form-control sqsEnabled' name='stic_center_name'
                        id='stic_center_name' autocomplete='off'
                        value='{$stic_center_name}' title='' tabindex='3'>
                    <input type='hidden' name='stic_center_id'
                        id='stic_center_id'
                        value='{$stic_center_id}'>
                    <span class='input-group-btn'>
                        <button title='{$MOD.LBL_SELECT_BUTTON_TITLE}' type='button'
                            class='btn btn-default' name='btn_1'
                            onclick='openSelectPopup("stic_Centers", "stic_center")'>
                            <span class='suitepicon suitepicon-action-select'></span>
                        </button>
                        <button type='button' name='btn_clear' class='btn btn-default'
                            onclick='clearRow(this.form, "stic_center")'>
                            <span class='suitepicon suitepicon-action-clear'></span>
                        </button>
                    </span>
                </div>
            </div>
        </div>

        <div class="filter-row">
            <div class="filter-group">
                <label for="stic_resources_places_gender_list">{$MOD.LBL_FILTERS_STIC_PLACE_GENDER}</label>
                <select multiple id="stic_resources_places_gender_list" 
                        name="stic_resources_places_gender_list[]" 
                        tabindex="102">
                    {$stic_resources_places_gender_list}
                </select>
            </div>
        </div>

        <div class="filter-row">
            <div class="filter-group">
                <label for="stic_resources_places_type_list">{$MOD.LBL_FILTERS_STIC_PLACE_TYPE}</label>
                <select multiple id="stic_resources_places_type_list" 
                        name="stic_resources_places_type_list[]" 
                        tabindex="102">
                    {$stic_resources_places_type_list}
                </select>
            </div>
        </div>

        <div class="filter-row">
            <div class="filter-group">
                <label for="stic_resources_places_users_list">{$MOD.LBL_FILTERS_STIC_PLACE_USER_TYPE}</label>
                <select multiple id="stic_resources_places_users_list" 
                        name="stic_resources_places_users_list[]" 
                        tabindex="102">
                    {$stic_resources_places_users_list}
                </select>
            </div>
        </div>
    </div>
</form>

{literal}
<script>
$(document).ready(function() {
    // Initialize Selectize on all multiple select elements
    $('#stic_resources_places_gender_list, #stic_resources_places_type_list, #stic_resources_places_users_list').selectize({
        plugins: ['remove_button'],
        delimiter: ',',
        persist: false,
        allowEmptyOption: true,
        create: false
    });
});
</script>

<style>
.filters-container {
    display: flex;
    flex-direction: column;
    gap: 15px;
    padding: 15px;
}

.filter-row {
    display: flex;
    flex-direction: column;
}

.filter-group {
    display: flex;
    flex-direction: column;
    gap: 10px;
}

.filter-group label {
    margin-bottom: 5px;
    font-weight: bold;
}

.input-group {
    display: flex;
    			width: 290px !important;

}

.input-group .form-control {
    flex-grow: 1;
}

.input-group-btn {
    display: flex;
}

.input-group-btn .btn {
    margin-left: 5px;
}

.form-control, .btn {
    height: 34px;
}

.selectize-control {
    width: 100%;
}
#form_filters {
    display: none;
}
</style>
{/literal}
