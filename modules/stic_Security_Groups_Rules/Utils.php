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

// Including necessary dependencies
global $current_user, $app_strings, $sugar_config, $app_list_strings;
require_once 'modules/SecurityGroups/SecurityGroup.php';
require_once 'modules/MySettings/TabController.php';
require_once 'SticInclude/Utils.php';

// Class stic_Security_Groups_RulesUtils
class stic_Security_Groups_RulesUtils
{

/**
 * Generates and returns an array with related module options.
 *
 * @param string $mainModule The main module.
 * @return array Returns an array for each related module, including the module name, relationship name, related field, and the translated label of the relationship.
 */
    public static function getRelatedModulesList($mainModule)
    {
        // Access global application list strings for label translations
        global $app_list_strings;

        // Create a new bean instance for the main module
        $mainModuleBean = BeanFactory::newBean($mainModule);

        // Initialize an array to hold the options
        $options = array();

        // Retrieve systemTabs to filter out irrelevant modules
        $systemTabs = (new TabController())->get_system_tabs();

        // Check if the main module bean is initialized
        if (!empty($mainModuleBean)) {
            // Iterate over related fields of the main module
            foreach ($mainModuleBean->get_related_fields() as $val) {

                // Skip processing if the module is not in systemTabs or lacks necessary properties
                if (!in_array(($val['module'] ?? null), $systemTabs) || !isset($val['ext2'])) {
                    continue;
                }

                // Translate and set the module label
                $moduleLabel = translate($val['vname'], $mainModule);

                // Append the translated module name to the label if it differs from the list label
                $destModuleLabel = '';
                if (!empty($val['module']) && $moduleLabel != $app_list_strings['moduleList'][$val['module']]) {
                    $destModuleLabel = " ({$app_list_strings['moduleList'][$val['module']]})";
                }

                // Add the related module's information to the options array
                $options[] = [
                    'id' => $mainModule . $val['id_name'],
                    'relationship' => $val['id_name'],
                    'field' => $mainModule . '__' . $val['id_name'],
                    'module' => $val['module'],
                    'label' => $moduleLabel . $destModuleLabel,
                ];
            }

            // Retrieve linked fields from the main module bean
            $linkedFields = $mainModuleBean->get_linked_fields();

            // Use array_filter to apply the callback function
            $filteredFields = array_filter($linkedFields, function ($element) {
                // Exclude elements where 'side' is set to 'right'
                return !(isset($element['side']) && $element['side'] === 'right');
            });

            // Iterate over the field definitions of the main module bean
            foreach ($filteredFields as $val) {
                // Reset destination module label and module label
                unset($destModuleLabel, $moduleLabel);

                $moduleLabel = translate(($val['vname'] ?? ''), $mainModule);

                // Retrieve certain properties directly from $GLOBALS['dictionary'] if not present
                $tmpRel = $GLOBALS['dictionary'][$val['relationship']]['relationships'][$val['relationship']] ?? null;
                if ($tmpRel && !empty($tmpRel['join_key_rhs']) && !empty($tmpRel['join_key_lhs'])) {
                    // Get the relationship field name from the dictionary
                    $val['field'] = $tmpRel['lhs_module'] == $mainModule ? $tmpRel['join_key_rhs'] : $tmpRel['join_key_lhs'];
                    // Get the module name from the dictionary
                    $val['module'] = $tmpRel['lhs_module'] == $mainModule ? $tmpRel['rhs_module'] : $tmpRel['lhs_module'];
                }

                // Skip relationship modules that are not in systemTabs
                if (empty($val['module']) || !in_array($val['module'], $systemTabs)) {continue;}

                // Check if the module is defined and differs from the module list label
                if (!empty($val['module']) && $moduleLabel != $app_list_strings['moduleList'][$val['module']]) {
                    $destModuleLabel = " ({$app_list_strings['moduleList'][$val['module']]})";
                }

                // Handle link and relate type fields, excluding certain modules
                if (isset($val['type']) && in_array($val['type'], ['link', 'relate']) && !in_array($val['module'], ['SecurityGroups', 'Users'])) {
                    if (empty($val['link'])) {
                        // Handle link type relationships
                        if (!in_array($val['relationship'], array_column($options, 'relationship'))) {
                            $options[] = [
                                'id' => $mainModule . $val['relationship'],
                                'relationship' => $val['relationship'],
                                'field' => $val['field'] ?? null,
                                'module' => $val['module'] ?? null,
                                'label' => translate($val['vname'] ?? '', $mainModule) . ($destModuleLabel ?? ''),
                            ];
                        }
                    } elseif (!in_array($val['link'], array_column($options, 'relationship'))) {
                        // Handle other relationships
                        $options[] = [
                            'id' => $mainModule . $val['relationship'],
                            'relationship' => $val['link'],
                            'field' => $val['id_name'],
                            'module' => $val['module'],
                            'label' => translate($val['vname'], $mainModule) . $destModuleLabel,
                        ];
                    }
                }
            }
        }

        // Sort options alphabetically by label
        usort($options, function ($a, $b) {return strcmp($a['label'], $b['label']);});

        // Return the options array
        return $options;
    }

    /**
     * Sets the custom related module list.
     *
     * @param string $mainModule The main module.
     */
    public static function setCustomRelatedModuleList($mainModule)
    {
        $options = array_column(self::getRelatedModulesList($mainModule), 'label', 'field');
        $overridedListName = 'dynamic_related_module_list';
        global $app_list_strings;
        $app_list_strings[$overridedListName] = $options;
    }

    /**
     * Sets a list of all related modules with their labels.
     * This function retrieves and populates a list with all existing module relationships and their labels,
     * filtering out specific types and modules.
     */
    public static function setAllRelatedModuleList()
    {
        // Access global application list strings
        global $app_list_strings;

        // Retrieve system tabs
        $systemTabs = (new TabController())->get_system_tabs();

        // Initialize options array
        $options = [];

        // Loop through each system tab to get related modules
        foreach ($systemTabs as $key => $mainModule) {

            // Get related modules for the main module
            $thisModuleRels = self::getRelatedModulesList($mainModule);
            // Check if related modules list is an array
            if (is_array($thisModuleRels)) {
                foreach ($thisModuleRels as $rel) {
                    // Add non-empty module id and label to options
                    if (!empty($rel['field'])) {
                        $options[$rel['field']] = $rel['label'];
                    }
                }
            }
        }

        // Sort options alphabetically
        uasort($options, function ($a, $b) {return strcmp($a, $b);});

        // Assign sorted options to the application list strings
        $app_list_strings['dynamic_related_module_list'] = $options;
    }

    /**
     * Sets a custom list of security groups.
     * This function retrieves all active security groups from the database
     * and populates a global list with their IDs and names.
     */
    public static function setCustomSecurityGroupList()
    {
        global $db, $app_list_strings;
        $overridedListName = 'dynamic_security_group_list';
        $sql = "SELECT id, name as 'value' FROM securitygroups WHERE deleted = 0";

        unset($app_list_strings[$overridedListName]);
        $result = $db->query($sql);

        $app_list_strings[$overridedListName][''] = '';
        while ($row = $db->fetchByAssoc($result)) {
            $app_list_strings[$overridedListName][$row['id']] = $row['value'];
        }
    }

    /**
     * Retrieves a list of security group IDs associated with a given record ID.
     *
     * This function executes a database query to fetch distinct security group IDs from the
     * 'securitygroups_records' and 'securitygroups' tables. It selects IDs where the record's
     * ID matches the specified 'relatedRecordID', and where both 'deleted' and 'noninheritable' flags are set to 0.
     *
     * @param int|string $relatedRecordID The ID of the record for which related security group IDs are to be fetched.
     * @return array An array of security group IDs associated with the specified record ID.
     * @global object $databaseConnection A global database connection object used to execute the query.
     */
    public static function getRelatedSecurityGroupIDs($relatedRecordID)
    {
        global $db;
        $securityGroupIDs = [];
        $queryResult = $db->query("SELECT DISTINCT securitygroup_id
                                               FROM securitygroups_records
                                               LEFT JOIN securitygroups ON securitygroups_records.securitygroup_id = securitygroups.id
                                               WHERE securitygroups_records.record_id = '{$relatedRecordID}'
                                               AND securitygroups_records.deleted = 0
                                               AND (securitygroups.noninheritable = 0 OR securitygroups.noninheritable IS NULL)
                                               AND securitygroups.deleted=0");

        foreach ($queryResult as $row) {
            $securityGroupIDs[] = $row['securitygroup_id'];
        }

        return $securityGroupIDs;
    }

    /**
     * Retrieve the defined rule for a specified module.
     *
     * This method searches the database for a rule associated with a module
     * and returns a bean with the rule configuration.
     *
     * @param String $moduleName The name of the module for which the rule is sought.
     * @return Object The bean of the found rule, or null if not found.
     */
    public static function getModuleRule($moduleName)
    {
        global $db;
        // Query to obtain the rule ID based on the module name
        $ruleId = $db->getOne("SELECT id FROM stic_security_groups_rules WHERE name='{$moduleName}' AND deleted=0 AND active=true");

        // Retrieve and return the rule bean using the obtained ID
        $rulesBean = BeanFactory::getBean('stic_Security_Groups_Rules', $ruleId);
        return $rulesBean;
    }

    /**
     * Return a list of groups that this user belongs to.
     *
     * This method retrieves the security groups associated with a given user
     * in SuiteCRM. It constructs a SQL query to fetch group details such as the
     * group ID and name from the database, ensuring to exclude any deleted 
     * and noninheritable groups (either directly or through the Users and Security Groups relationship).
     * The method returns an associative array where each key is a group ID, and
     * the value is an array containing the group's ID and name.
     *
     * @param string $userId The user's ID.
     * @return array An associative array of groups with their IDs and names.
     */
    public static function getSticUserInheritableSecurityGroups($userId)
    {
        // Get an instance of the database manager
        $db = DBManagerFactory::getInstance();

        // Safely escape the user ID to prevent SQL injection
        $userId = $db->quote($userId);

        // Construct the SQL query to fetch group details
        $query = "SELECT
                securitygroups.id,
                securitygroups.name
              FROM
                securitygroups_users
              INNER JOIN securitygroups ON
                securitygroups_users.securitygroup_id = securitygroups.id
                AND securitygroups.deleted = 0
                AND (securitygroups.noninheritable = 0 OR securitygroups.noninheritable IS NULL)
              WHERE
                securitygroups_users.user_id = '{$userId}'
                AND securitygroups_users.deleted = 0
                AND (securitygroups_users.noninheritable = 0 OR securitygroups_users.noninheritable IS NULL)
              ORDER BY
                securitygroups.name ASC";

        // Execute the query and handle any errors
        $result = $db->query($query, true, 'Error finding the full membership list for a user: ');

        // Initialize an array to store group details
        $group_array = array();

        // Fetch each row from the query result and add it to the group array
        while (($row = $db->fetchByAssoc($result)) != null) {
            $group_array[$row['id']] = $row;
        }

        // Return the array of group details
        return $group_array;
    }

    /**
     * Function for SuiteCRM, handling security group inheritance.
     * This function applies custom security group inheritance rules to newly created records
     * based on conditions such as the assigned user, creator, and parent records.
     *
     * @param SugarBean $bean The current bean being processed.
     */
    public static function applyCustomInheritance($bean)
    {
        global $current_user, $db;

        // Skip processing for the 'SugarFeed' module
        if (in_array($bean->module_name, ['SugarFeed'])) {
            return;
        }

        // Retrieve module-specific inheritance rules
        $rulesBean = self::getModuleRule($bean->module_name);

        // Check if there is a defined and active inheritance rule for the module
        if (!empty($rulesBean) && $rulesBean->active == 1) {

            // Initialize an array to store candidate security groups for inheritance
            $securityGroupsCandidatesToInherit = [];

            // Inherit security groups from the assigned user, if enabled
            if ($rulesBean->inherit_assigned == 1) {
                $userGroups = self::getSticUserInheritableSecurityGroups($bean->assigned_user_id);
                if (!empty($userGroups)) {
                    foreach ($userGroups as $group) {
                        $securityGroupsCandidatesToInherit = array_merge(
                            $securityGroupsCandidatesToInherit,
                            [['record_id' => $bean->id, 'securitygroup_id' => $group['id']]]
                        );
                    }
                }
            }

            // Inherit security groups from the creator user, if enabled
            if ($rulesBean->inherit_creator == 1) {
                $userGroups = self::getSticUserInheritableSecurityGroups($bean->created_by);
                if (!empty($userGroups)) {
                    foreach ($userGroups as $group) {
                        $securityGroupsCandidatesToInherit = array_merge(
                            $securityGroupsCandidatesToInherit,
                            [['record_id' => $bean->id, 'securitygroup_id' => $group['id']]]
                        );
                    }
                }
            }

            // Inherit security groups from parent records, if enabled
            $allRelatedModules = self::getRelatedModulesList($bean->module_dir);

            // Proccess $field value to ensure only contains related fields name
            $allRelatedModules = array_map(function ($module) {
                if (strpos($module['field'], '__') !== false) {
                    // Extrae y asigna la parte derecha del string directamente
                    $module['field'] = explode('__', $module['field'])[1];
                }
                return $module;
            }, $allRelatedModules);

            foreach (unencodeMultienum($rulesBean->inherit_from_modules) as $value) {
                if (strpos($value, '__') !== false) {
                    $filteredRelatedModules[] = explode('__', $value, 2)[1];
                } else {
                    $filteredRelatedModules[] = $value;
                }
            }

            foreach ($allRelatedModules as $value) {
                if ($rulesBean->inherit_parent == 1 || in_array($value['field'], $filteredRelatedModules)) {

                    // Obtain id from parent record
                    $relatedId = ($bean->{$value['field']})??null;

                    // If it is not a string, it indicates we're accessing from a subpanel.
                    // In such cases, retrieve the id in one of the three following ways, or continue.

                    // 1) Check if the related field is present in the subpanel's form
                    if (!is_string($relatedId) && !empty($relatedId)) {
                        $relatedId = key($bean->{$value['field']}->rows);
                    }

                    // 2) Check if the relationship is specified with a relationship name
                    if (!is_string($relatedId) && isset($bean->{$value['relationship']})) {
                        $relatedId = key($bean->{$value['relationship']}->getBeans());
                    }

                    // 3) Check if the relationship is specified using the lowercase module name (common in native relationships)
                    $relName = strtolower($value['module']);
                    if (!is_string($relatedId) && isset($bean->{$relName})) {
                        $relatedId = key($bean->{$relName}->getBeans());

                    }
                    
                    if (!empty($relatedId)) {
                        $currentRecordGroups = self::getRelatedSecurityGroupIDs($relatedId);
    
                        foreach ($currentRecordGroups as $val2) {
                            $securityGroupsCandidatesToInherit = array_merge(
                                $securityGroupsCandidatesToInherit,
                                [['record_id' => $bean->id, 'securitygroup_id' => $val2]]
                            );
                        }
                    }
                }
            }

            // Create an array of security groups that are not inheritable under any circumstances for the current module
            $notInheritableGroups = array();
            if (!empty($rulesBean->non_inherit_from_security_groups)) {
                $notInheritableGroups = unencodeMultienum($rulesBean->non_inherit_from_security_groups);
            }

            // Add globally defined non-inheritable security groups
            $nonInheritableSQL = "SELECT DISTINCT id FROM securitygroups WHERE deleted=0 AND noninheritable=1";
            $result = $bean->db->query($nonInheritableSQL);
            while ($row = $db->fetchByAssoc($result)) {
                $notInheritableGroups[] = $row['id'];
            }

            // Remove duplicate candidates groups
            $serializedArray = array_map('serialize', $securityGroupsCandidatesToInherit);
            $securityGroupsCandidatesToInherit = array_map('unserialize', array_unique($serializedArray));

            // Add security groups to the record, excluding the non-inheritable ones
            $securityGroupBean = BeanFactory::newBean('SecurityGroups');
            foreach ($securityGroupsCandidatesToInherit as $key => $item) {
                if (!in_array($item['securitygroup_id'], $notInheritableGroups)) {
                    $securityGroupBean->addGroupToRecord($bean->module_name, $item['record_id'], $item['securitygroup_id']);
                }
            }

            // Assign default groups defined in the general configuration for the module
            SecurityGroup::assign_default_groups($bean, false);

        }
    }
}
