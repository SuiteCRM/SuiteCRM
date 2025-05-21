<?php
/**
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
 */

function getCustomView($customizationBean)
{
    require_once 'modules/stic_Custom_Views/Utils.php';
    return getRelatedBeanObject($customizationBean, "stic_custom_views_stic_custom_view_customizations");
}

function getLangStrings()
{
    $html = "";
    // Load related lang strings
    $moduleNames = array('stic_Custom_View_Customizations', 'stic_Custom_View_Conditions', 'stic_Custom_View_Actions');
    foreach ($moduleNames as $moduleName) {
        if (!is_file("cache/jsLanguage/{$moduleName}/{$GLOBALS['current_language']}.js")) {
            require_once 'include/language/jsLanguage.php';
            jsLanguage::createModuleStringsCache($moduleName, $GLOBALS['current_language']);
        }
        $html .= getVersionedScript("cache/jsLanguage/{$moduleName}/{$GLOBALS['current_language']}.js", $GLOBALS['sugar_config']['js_lang_version']);
    }
    return $html;
}

function displayConditionLines($focus, $field, $value, $view)
{
    require_once 'modules/stic_Custom_Views/Utils.php';

    $html =
        "<table cellpadding='0' cellspacing='0' border='0' style='table-layout:fixed; width:100%;'" .
        "id='sticCustomView_ConditionLines' class='sticCustomView_Lines'></table>" .
        "<div style='padding-top: 10px; padding-bottom:10px;'>" .
        "<input type='button' class='button' tabindex='116' " .
        "value=\"" . translate('LBL_ADD_CONDITION', "stic_Custom_View_Customizations") . "\" " .
        "id='btn_ConditionLine' onclick='insertConditionLine()'/>" .
        "</div>";

    $conditionBeanArray = getRelatedBeanObjectArray($focus, "stic_custom_view_customizations_stic_custom_view_conditions");
    if (!empty($conditionBeanArray)) {
        // Sort conditions
        usort($conditionBeanArray, 'compareConditions');

        $html .= "<script>";
        foreach ($conditionBeanArray as $conditionBean) {
            $conditionBean->value = htmlspecialchars_decode($conditionBean->value);
            if ($conditionBean->condition_type == "") {
                $conditionBean->condition_type = "value";
            }
            $html .= "loadConditionLine(\"" . addslashes(json_encode($conditionBean->toArray())) . "\");";
        }
        $html .= "</script>";
    }
    return $html;
}

function displayActionLines(SugarBean $focus, $field, $value, $view)
{
    require_once 'modules/stic_Custom_Views/Utils.php';

    $html =
        "<table cellpadding='0' cellspacing='0' border='0' style='table-layout:fixed; width:100%;'" .
        "id='sticCustomView_ActionLines' class='sticCustomView_Lines'></table>" .
        "<div style='padding-top: 10px; padding-bottom:10px;'>" .
        "<input type='button' class='button' tabindex='116' " .
        "value=\"" . translate('LBL_ADD_ACTION', "stic_Custom_View_Customizations") . "\" " .
        "id='btn_ActionLine' onclick='insertActionLine()'/>" .
        "</div>";

    $actionBeanArray = getRelatedBeanObjectArray($focus, "stic_custom_view_customizations_stic_custom_view_actions");
    if (!empty($actionBeanArray)) {
        // Sort actions
        usort($actionBeanArray, 'compareActions');
        $html .= "<script>";
        foreach ($actionBeanArray as $actionBean) {
            $actionBean->value = htmlspecialchars_decode($actionBean->value);
            $html .= "loadActionLine(\"" . addslashes(json_encode($actionBean->toArray())) . "\");";
        }
        $html .= "</script>";
    }
    return $html;
}

function compareConditions($a, $b)
{
    return $a->condition_order - $b->condition_order;
}

function compareActions($a, $b)
{
    return $a->action_order - $b->action_order;
}
