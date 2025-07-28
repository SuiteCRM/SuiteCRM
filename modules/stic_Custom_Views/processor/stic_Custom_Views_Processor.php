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
class stic_Custom_Views_Processor 
{
    public $module = "";
    public $view = "";

    public function __construct($module, $view)
    {
        $this->module = $module;
        $this->view = $view;    
    }

    public function getHtmlScriptForCustomViews() {
        require_once 'modules/stic_Custom_Views/Utils.php';

        global $current_user;

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Getting Script for Custom Views; Module:'.$this->module.'; View:'.$this->view);

        $availableViews = $GLOBALS['app_list_strings']['stic_custom_views_views_list'] ?? [];
        if (!array_key_exists($this->view, $availableViews)) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'No view available; Module:'.$this->module.'; View:'.$this->view);
            return "";
        }

        // Steps:
        //  1- Find all stic_Custom_Views defined for the module and view
        //  2- Filter stic_Custom_Views to apply with user permissions
        //  3- Sort stic_Custom_Views: less restrictions -> more restrictions
        //  4- Get all Customizations: [Conditions, Actions]
        //  5- Sort Customizations
        //  6- Convert to json
        //  7- Write a js call to processSticCustomView when loaded

        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Username:'.$current_user->user_name);
        $isAdmin = $current_user->isAdmin();

        $groups = SecurityGroup::getUserSecurityGroups($current_user->id);
        $groupsIds = array();
        foreach ($groups as $group) {
            $groupsIds[] = $group["id"];
        }

        $acl = BeanFactory::newBean('ACLRoles');
        $roles = $acl->getUserRoles($current_user->id, false);
        $rolesIds = array();
        foreach ($roles as $rol) {
            $rolesIds[] = $rol->id;
        }
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'IsAdmin:'.$isAdmin.'; Security Groups:'.print_r($groupsIds, true).'; Roles:'.print_r($rolesIds, true));

        // Find all stic_Custom_Views defined for the module and view
        $db = DBManagerFactory::getInstance();
        $sql = "SELECT DISTINCT views.id, views.user_type, views.security_groups, views.security_groups_exclude, views.roles, views.roles_exclude
                FROM stic_custom_views views
                    INNER JOIN stic_custom_views_stic_custom_view_customizations_c views_custom
                        ON views_custom.stic_custo45d1m_views_ida = views.id
                    INNER JOIN stic_custom_view_customizations custom
                        ON views_custom.stic_custobdd5zations_idb = custom.id
                WHERE
                    views.deleted = 0
                    AND views.status = 'active'
                    AND views_custom.deleted = 0
                    AND custom.deleted = 0
                    AND custom.status = 'active'
                    AND views.view_module = '{$this->module}'
                    AND views.view_type = '{$this->view}'";

        $result = $db->query($sql, true);
        if (!$result) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Invalid result with query: '.$sql);
            return '';
        }

        // Filter Custom Views for current user
        $validCustomViews = array();
        while ($row = $db->fetchByAssoc($result)) {
            $okUserType = $row["user_type"] == "all" ||
                ($isAdmin && $row["user_type"] == "administrator") ||
                (!$isAdmin && $row["user_type"] == "regular_user");
            if (!$okUserType) {
                continue;
            }

            $okGroup = empty($row["security_groups"]) ||
            $this->string_contains_any($row["security_groups"], $groupsIds);
            $okGroup &= empty($row["security_groups_exclude"]) ||
            !$this->string_contains_any($row["security_groups_exclude"], $groupsIds);
            if (!$okGroup) {
                continue;
            }

            $okRole = empty($row["roles"]) ||
            $this->string_contains_any($row["roles"], $rolesIds);
            $okRole &= empty($row["roles_exclude"]) ||
            !$this->string_contains_any($row["roles_exclude"], $rolesIds);
            if (!$okRole) {
                continue;
            }

            // Here Customization match all: UserType, Role, Role-Exclude, SecurityGroup and SecurityGroup-Exclude
            $validCustomViews[] = $row;
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'CustomView for user:'.print_r($row, true));
        }

        if (empty($validCustomViews)) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'No CustomViews defined for: Module:'.$this->module.'; View:'.$this->view.'; Username:'.$current_user->user_name);
            return '';
        }

        // Sort stic_Custom_Views: less restrictions -> more restrictions
        usort($validCustomViews, array($this, 'compareCustomViewRestrictions'));

        // Get all customizations: [Conditions, Actions]
        $customizations = array();
        foreach ($validCustomViews as $customView) {
            $customViewBean = BeanFactory::getBean('stic_Custom_Views', $customView["id"]);
            $allCustomizationBeanArray = getRelatedBeanObjectArray($customViewBean, 'stic_custom_views_stic_custom_view_customizations');
            $customizationBeanArray = array();
            foreach ($allCustomizationBeanArray as $customizationBean) {
                if (strtolower($customizationBean->status) == "active") {
                    $customizationBeanArray[] = $customizationBean;
                }
            }
            // Sort Customizations
            usort($customizationBeanArray, array($this, 'compareCustomizations'));

            foreach ($customizationBeanArray as $customizationBean) {
                $conditionBeanArray = getRelatedBeanObjectArray($customizationBean, 'stic_custom_view_customizations_stic_custom_view_conditions');
                $actionsBeanArray = getRelatedBeanObjectArray($customizationBean, 'stic_custom_view_customizations_stic_custom_view_actions');

                $conditions = array();
                foreach ($conditionBeanArray as $conditionBean) {
                    $value_typeArray = explode("|", $conditionBean->value_type);
                    $value_type = $value_typeArray[0];
                    if ($value_type != "enum" && $value_type != "multienum" && $value_type != "dynamicenum") {
                        $value_typeArray[1] = "";
                    }
                    $value_list = $value_typeArray[1] ?? "";
                    $condition_type = $conditionBean->condition_type;
                    if($condition_type=="value") {
                        $value = $this->value_to_display($conditionBean->value, $value_type);
                    } else {
                        $value = $conditionBean->value;
                    }
                    if ($condition_type == "") {
                        $condition_type = "value";
                    }
                    $conditions[] = array(
                        "condition_order" => $conditionBean->condition_order,
                        "field" => $conditionBean->field,
                        "operator" => $conditionBean->operator,
                        "condition_type" => $condition_type,
                        //"value" => $conditionBean->value,
                        "value" => htmlspecialchars_decode($value),
                        "value_type" => $value_type,
                        "value_list" => $value_list,
                        "date_format" => strtoupper($current_user->getPreference('datef')) . " HH:mm",
                    );
                }
                // Sort conditions
                usort($conditions, array($this, 'compareConditions'));

                $actions = array();
                foreach ($actionsBeanArray as $actionBean) {
                    $value_typeArray = explode("|", $actionBean->value_type);
                    $value_type = $value_typeArray[0] ?? "";
                    $value_list = $value_typeArray[1] ?? "";
                    if ($actionBean->action == "fixed_value") {
                        $value = $this->value_to_display($actionBean->value, $value_type);
                    } else {
                        $value = $actionBean->value;
                    }
                    $actions[] = array(
                        "action_order" => $actionBean->action_order,
                        "type" => $actionBean->type,
                        "element" => $actionBean->element,
                        "action" => $actionBean->action,
                        //"value" => $actionBean->value,
                        "value" => htmlspecialchars_decode($value),
                        "value_type" => $value_type,
                        "value_list" => $value_list,
                        "element_section" => $actionBean->element_section,
                    );
                }
                // Sort actions
                usort($actions, array($this, 'compareActions'));

                if (count($actions) > 0) {
                    // Add Customization
                    $customizations[] = array("conditions" => $conditions, "actions" => $actions);
                }
            }
        }
        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'Found CustomViews defined for: Module:'.$this->module.'; View:'.$this->view.'; Username:'.$current_user->user_name);
        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'CustomViews:'.print_r($customizations, true));

        // Convert to json
        $customizationsJson = json_encode($customizations);
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'jsonCustomViews:'.$customizationsJson);

        // Set Current user in js
        // Write a js call to processSticCustomView when loaded
        $html =
            "<script type=\"text/javascript\" language=\"JavaScript\">" .
                "SUGAR.sticCV_currentUser = \"".$current_user->id."|".$current_user->user_name."\";".
                "$(document).ready(function () {" .
                    "sticCustomizeView.For(\"{$this->view}\").processSticCustomView(\"" . addslashes($customizationsJson) . "\");" .
                "});" .
            "</script>";

        return $html;
    }

    /**
     * Converts the given value to a displayable format based on the specified value type.
     *
     * @param mixed $value The value to be converted.
     * @param string $value_type The type of the value.
     * @return mixed The converted value.
     */
    private function value_to_display($value, $value_type)
    {
        global $timedate, $current_user, $sugar_config;

        switch ($value_type) {
            case "currency":
                return currency_format_number($value);

            case 'double':
            case 'decimal':
            case 'float':
                $user_dec_sep = (!empty($current_user->id) ? $current_user->getPreference('dec_sep') : null);
                $dec_sep = empty($user_dec_sep) ? $sugar_config['default_decimal_seperator'] : $user_dec_sep;
                return str_replace('.', $dec_sep, $value);

            case "date":
                return $timedate->asUser($timedate->fromDbFormat($value, TimeDate::DB_DATE_FORMAT), $current_user);
            case "datetime":
            case "datetimecombo":
                return $timedate->asUser($timedate->fromDbFormat($value, TimeDate::DB_DATETIME_FORMAT), $current_user);
        }
        return $value;
    }

    /**
     * Checks if the given string contains any of the substrings from the provided array.
     *
     * @param string $str The string to be checked.
     * @param array $arr An array of substrings to search for.
     * @return bool True if the string contains any of the substrings, false otherwise.
     */
    private function string_contains_any($str, array $arr)
    {
        foreach ($arr as $a) {
            if (stripos($str, (string) $a) !== false) {
                return true;
            }
        }
        return false;
    }

    /**
     * Compares Custom View restrictions for sorting purposes.
     *
     * This function is intended for use with sorting arrays of custom views when applying restrictions
     *
     * @param array $a The first Custom View to compare.
     * @param array $b The second Custom View to compare.
     * @return int Returns a negative value if $a should be placed before $b, a positive value if $b should be placed before $a, or 0 if they are equivalent.
     */    
    private function compareCustomViewRestrictions($a, $b)
    {
        if (!empty($a["roles"]) && !empty($b["roles"])) {
            return 0;
        }
        if (empty($a["roles"]) && !empty($b["roles"])) {
            return -1;
        }
        if (!empty($a["roles"]) && empty($b["roles"])) {
            return 1;
        }
        if (!empty($a["roles_exclude"]) && !empty($b["roles_exclude"])) {
            return 0;
        }
        if (empty($a["roles_exclude"]) && !empty($b["roles_exclude"])) {
            return -1;
        }
        if (!empty($a["roles_exclude"]) && empty($b["roles_exclude"])) {
            return 1;
        }
        if (!empty($a["security_groups"]) && !empty($b["security_groups"])) {
            return 0;
        }
        if (empty($a["security_groups"]) && !empty($b["security_groups"])) {
            return -2;
        }
        if (!empty($a["security_groups"]) && empty($b["security_groups"])) {
            return 2;
        }
        if (!empty($a["security_groups_exclude"]) && !empty($b["security_groups_exclude"])) {
            return 0;
        }
        if (empty($a["security_groups_exclude"]) && !empty($b["security_groups_exclude"])) {
            return -2;
        }
        if (!empty($a["security_groups_exclude"]) && empty($b["security_groups_exclude"])) {
            return 2;
        }
        if ($a["user_type"] == $b["user_type"]) {
            return 0;
        }
        if ($a["user_type"] == "all") {
            return -3;
        }
        if ($b["user_type"] == "all") {
            return 3;
        }
        return 0;
    }

    /**
     * Compares Customizations for sorting purposes.
     *
     * This function is intended for use with sorting arrays of Customizations, by order
     *
     * @param array $a The first Customization to compare.
     * @param array $b The second Customization to compare.
     * @return int Returns a negative value if $a should be placed before $b, a positive value if $b should be placed before $a, or 0 if they are equivalent.
     */    
    private function compareCustomizations($a, $b)
    {
        return $a->customization_order - $b->customization_order;
    }

    /**
     * Compares Conditions for sorting purposes.
     *
     * This function is intended for use with sorting arrays of Conditions, by order
     *
     * @param array $a The first Condition to compare.
     * @param array $b The second Condition to compare.
     * @return int Returns a negative value if $a should be placed before $b, a positive value if $b should be placed before $a, or 0 if they are equivalent.
     */     
    private function compareConditions($a, $b)
    {
        return $a["condition_order"] - $b["condition_order"];
    }

    /**
     * Compares Actions for sorting purposes.
     *
     * This function is intended for use with sorting arrays of Actions, by order
     *
     * @param array $a The first Action to compare.
     * @param array $b The second Action to compare.
     * @return int Returns a negative value if $a should be placed before $b, a positive value if $b should be placed before $a, or 0 if they are equivalent.
     */ 
    private function compareActions($a, $b)
    {
        return $a["action_order"] - $b["action_order"];
    }

}