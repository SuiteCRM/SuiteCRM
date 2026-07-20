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
if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

class stic_AWF_FormsUtils {

    private static $relationshipsCache = [];

    /**
     * Retrieves fields and relationships of given Module to given SuiteCRM modules.
     * Result: [name, text, textSingular, inStudio, icon, fields:[Field], relationships:[Relationship]]
     *   Field: {
     *     name, text, type, required, options, inViews
     *   }
     *   Relationship: {
     *     name, text, module_orig, field_orig, relationship, module_dest
     *   }
     * 
     */
    public static function getModuleInformation($moduleName, $availableModules) {
        global $app_list_strings, $dictionary;

        $result = [
            'name' => $moduleName,
            'text' => '',
            'textSingular' => '',
            'inStudio' => false,
            'icon' => '',
            'fields' => [],
            'relationships' => []
        ];
        // Set Text
        $result['text'] = translate($moduleName);
        $result['textSingular'] = $app_list_strings['moduleListSingular'][$moduleName] ?? $result['text'];

        // Fill Studio information
        require_once 'modules/ModuleBuilder/Module/StudioBrowser.php';
        $sb = new StudioBrowser();
        $nodes = $sb->getNodes();
        foreach ($nodes as $node) {
            if ($node['module'] == $moduleName) {
                $result['inStudio'] = true;
                $result['icon'] = $node['icon'];
                break;
            }
        }

        // Discover relationships from $dictionary (canonical source + link field fallback)
        $result['relationships'] = self::getRelationshipsFromDictionary($moduleName, $availableModules);

        // Discover fields for the module
        try {
            $objectOrig = BeanFactory::getObjectName($moduleName);
            VardefManager::loadVardef($moduleName, $objectOrig);
        } catch (\Exception $e) {
            $GLOBALS['log']->warn(__METHOD__ . ": Error loading vardefs for '{$moduleName}': " . $e->getMessage());
            return $result;
        }
        $fieldDefs = $dictionary[$objectOrig]['fields'] ?? [];

        // Collect link field relationship names for later relate-field cleanup
        $validRelNames = array_keys($result['relationships']);

        foreach ($fieldDefs as $fieldName => $arr) {
            if (isset($result['fields'][$fieldName])) continue;
            if (!isset($arr['type'])) continue;
            if ($arr['type'] == 'link') continue;

            // Exclude non Studio editable fields
            if (isset($arr['studio'])) {
                if (is_array($arr['studio']) && isset($arr['studio']['editview']) && $arr['studio']['editview'] === false) continue;
                if ($arr['studio'] === false || $arr['studio'] === 'false') continue;
            }

            // Exclude ID type fields
            if ($arr['type'] == 'id' || (isset($arr['dbType']) && strtolower($arr['dbType']) == 'id')) continue;

            // Exclude system fields
            $excludedFields = ['currency_name', 'currency_symbol', 'date_entered', 'date_modified',
                'modified_user_id', 'modified_by_name', 'created_by', 'created_by_name', 'deleted'];
            if (in_array($fieldName, $excludedFields)) continue;

            // Exclude non procesable field types
            $excludedTypes = ['html', 'iframe', 'image', 'file', 'attachment', 'address', 'wysiwyg',
                'parent', 'parent_type', 'team_id', 'team_set_id', 'team_list', 'team_count'];
            if (in_array($arr['type'], $excludedTypes)) continue;

            $isEmail = self::isEmailField($arr, $fieldName);
            $merge_filter = $isEmail ? 'enabled' : ($arr['merge_filter'] ?? '');

            $result['fields'][$fieldName] = [
                'name' => $fieldName,
                'text' => rtrim(trim(translate($arr['vname'] ?? '', $moduleName)), ":"),
                'type' => $arr['type'],
                'required' => isset($arr['required']) && $arr['required'],
                'default' => $arr['default'] ?? null,
                'options' => $arr['options'] ?? '',
                'module' => $arr['module'] ?? '',
                'id_name' => $arr['id_name'] ?? '',
                'merge_filter' => $merge_filter,
                'inViews' => false,
            ];

            // For relate fields with 'link' property: link options to their relationship name
            if ($arr['type'] === 'relate' && isset($arr['link'])) {
                $linkName = $arr['link'];
                $linkRelName = $fieldDefs[$linkName]['relationship'] ?? '';
                if (!empty($linkRelName) && isset($result['relationships'][$linkRelName])) {
                    $result['fields'][$fieldName]['options'] = $linkRelName;
                    $result['fields'][$fieldName]['link_name'] = $linkName;
                }
            }

            // For standalone relate fields (no link, but has id_name): link to their virtual relationship
            if ($arr['type'] === 'relate' && !isset($arr['link']) && !empty($arr['id_name'])) {
                $virtualName = 'virtual__' . $fieldName;
                if (isset($result['relationships'][$virtualName])) {
                    $result['fields'][$fieldName]['options'] = $virtualName;
                    $result['fields'][$fieldName]['link_name'] = $virtualName;
                }
            }
        }

        // Remove relate fields whose relationship is not available
        $fieldsToRemove = [];
        foreach ($result['fields'] as $fieldName => $arr) {
            if ($arr['type'] == 'relate' && !empty($arr['options']) && !in_array($arr['options'], $validRelNames)) {
                $fieldsToRemove[] = $fieldName;
            }
        }
        foreach ($fieldsToRemove as $fieldName) {
            unset($result['fields'][$fieldName]);
        }

        // Complete field info with inViews (is in detailview or editview)
        if($result['inStudio']) {
            require_once 'modules/ModuleBuilder/parsers/ParserFactory.php';
            $views = ['detailview', 'editview'];
            foreach($views as $view) {
                $parser = ParserFactory::getParser($view, $moduleName, null);
                foreach ($parser->_viewdefs['panels'] as $panel) {
                    foreach ($panel as $row) {
                        foreach ($row as $field) {
                            if (isset($result['fields'][$field])) {
                                $result['fields'][$field]['inViews'] = true;
                            }
                        }
                    }
                }
            }
        }

        // Sort fields by text
        uasort($result['fields'], function($a, $b) {
            return strcmp($a['text'], $b['text']);
        });

        return $result;
    }

    /**
     * Returns relationships for a module using $dictionary['relationships'] (canonical source)
     * with fallback to link-type fields from $dictionary['fields'].
     * Each relationship entry includes: name, text, module_orig, module_dest, relationship, link_name
     */
    private static function getRelationshipsFromDictionary(string $moduleName, array $availableModules): array {
        global $dictionary;

        $cacheKey = $moduleName . '_' . md5(serialize(array_keys($availableModules)));
        if (isset(self::$relationshipsCache[$cacheKey])) {
            return self::$relationshipsCache[$cacheKey];
        }

        $result = [];
        $bean = BeanFactory::newBean($moduleName);
        if (!$bean) {
            return $result;
        }

        try {
            $objectName = BeanFactory::getObjectName($moduleName);
            VardefManager::loadVardef($moduleName, $objectName);
        } catch (\Exception $e) {
            $GLOBALS['log']->warn(__METHOD__ . ": Error loading vardefs for '{$moduleName}': " . $e->getMessage());
            return $result;
        }

        $fields = $dictionary[$objectName]['fields'] ?? [];
        $relDefs = $dictionary[$objectName]['relationships'] ?? [];

        $processed = [];
        $sources = [
            'relationships' => $relDefs,
            'link_fields'   => array_filter($fields, function($f) { return ($f['type'] ?? '') === 'link' && !empty($f['relationship']); }),
        ];

        foreach ($sources as $sourceType => $entries) {
            foreach ($entries as $key => $def) {
                if ($sourceType === 'relationships') {
                    $relName = $key;
                    $lhs = $def['lhs_module'] ?? '';
                    $rhs = $def['rhs_module'] ?? '';
                    $vname = $def['vname'] ?? '';
                    $linkFieldName = '';
                    $relType = $def['relationship_type'] ?? 'many-to-many';
                } else {
                    $relName = $def['relationship'];
                    $lhs = $moduleName;
                    $rhs = $def['module'] ?? '';
                    $vname = $def['vname'] ?? '';
                    $linkFieldName = $def['name'] ?? $key;
                    $relType = $def['relationship_type'] ?? 'many-to-many';

                    if (empty($rhs)) {
                        try {
                            if ($bean->load_relationship($linkFieldName)) {
                                $rhs = $bean->$linkFieldName->getRelatedModuleName();
                            }
                        } catch (\Exception $e) {
                            continue;
                        }
                    }
                }

                if (isset($processed[$relName])) {
                    // If the relationships source already created an entry but
                    // without link_name (empty string), fill it from this link_field entry.
                    if ($sourceType === 'link_fields' && isset($result[$relName]) && empty($result[$relName]['link_name'])) {
                        $result[$relName]['link_name'] = $linkFieldName;
                    }
                    continue;
                }
                if (empty($rhs) || $rhs === 'EmailAddress') continue;

                // Determine which side is the current module
                $otherModule = null;
                $isOrig = false;
                if ($lhs === $moduleName && isset($availableModules[$rhs])) {
                    $otherModule = $rhs;
                    $isOrig = true;
                } elseif ($rhs === $moduleName && isset($availableModules[$lhs])) {
                    $otherModule = $lhs;
                    $isOrig = false;
                } else {
                    continue;
                }

                $processed[$relName] = true;

                // Text: translate vname, with fallbacks
                $text = '';
                if (!empty($vname)) {
                    $text = translate($vname, $moduleName);
                    if ($text === $vname) {
                        $text = translate($vname, $otherModule);
                    }
                    $text = rtrim(trim($text), ':');
                }
                if (empty($text) || $text === $vname) {
                    // Fallback 1: try relate field's vname (more descriptive for 1-N)
                    $fieldVname = '';
                    foreach ($fields as $fName => $fDef) {
                        if (($fDef['type'] ?? '') !== 'relate') continue;
                        $match = (!empty($linkFieldName) && ($fDef['link'] ?? '') === $linkFieldName)
                              || (($fDef['module'] ?? '') === $otherModule);
                        if ($match) {
                            $fieldVname = $fDef['vname'] ?? '';
                            break;
                        }
                    }
                    if (!empty($fieldVname)) {
                        $text = translate($fieldVname, $moduleName);
                        $text = rtrim(trim($text), ':');
                    }
                    // Fallback 2: destination module name
                    if (empty($text) || $text === $fieldVname) {
                        $text = translate($otherModule) ?: $relName;
                    }
                }

                $result[$relName] = [
                    'name'              => $relName,
                    'text'              => $text,
                    'module_orig'       => $isOrig ? $moduleName : $otherModule,
                    'module_dest'       => $isOrig ? $otherModule : $moduleName,
                    'relationship'      => $relName,
                    'link_name'         => $linkFieldName,
                    'relationship_type' => $relType,
                ];
            }
        }

        // Build set of id_name values already covered by existing relationships
        $coveredIdNames = [];
        foreach ($relDefs as $relName => $relDef) {
            if (isset($processed[$relName])) {
                if (isset($relDef['relationship_type']) && $relDef['relationship_type'] === 'one-to-many') {
                    if (($relDef['rhs_module'] ?? '') === $moduleName && !empty($relDef['rhs_key'])) {
                        $coveredIdNames[$relDef['rhs_key']] = true;
                    }
                }
            }
        }
        // Also check result entries that may already have id_name set
        foreach ($result as $relData) {
            if (!empty($relData['id_name'])) {
                $coveredIdNames[$relData['id_name']] = true;
            }
        }

        // Third source: standalone relate fields with id_name but without link (virtual relationships)
        foreach ($fields as $fieldName => $f) {
            if (($f['type'] ?? '') !== 'relate') continue;
            if (empty($f['id_name'])) continue;
            if (!empty($f['link'])) continue;
            if (empty($f['module']) || !isset($availableModules[$f['module']])) continue;
            if (isset($coveredIdNames[$f['id_name']])) continue;

            $virtualName = 'virtual__' . $fieldName;

            $vname = $f['vname'] ?? '';
            $text = '';
            if (!empty($vname)) {
                $text = rtrim(trim(translate($vname, $moduleName)), ':');
            }
            if (empty($text)) {
                $text = translate($f['module']) ?: $fieldName;
            }
            $suffix = translate('LBL_AWF_FIELD_SUFFIX', 'stic_AWF_Forms');
            $text .= ' (' . $suffix . ')';

            $result[$virtualName] = [
                'name'              => $virtualName,
                'text'              => $text,
                'module_orig'       => $moduleName,
                'module_dest'       => $f['module'],
                'relationship'      => $virtualName,
                'link_name'         => $fieldName,
                'id_name'           => $f['id_name'],
                'is_virtual_relate' => true,
                'type'              => '1-N',
                'relationship_type' => 'one-to-many',
            ];

            $processed[$virtualName] = true;
        }

        // Build a map of canonical relationship text by module_dest to detect redundant inverse virtuals.
        $canonicalTextByDest = [];
        foreach ($result as $relName => $relData) {
            if (empty($relData['is_virtual_relate'])) {
                $dest = $relData['module_dest'];
                if (!isset($canonicalTextByDest[$dest])) {
                    $canonicalTextByDest[$dest] = $relData['text'];
                }
            }
        }

        // Fourth source: inverse virtual relationships — scan other modules for standalone
        // relate fields pointing to the current module, so the relationship is visible from
        // both the owning module (the N side with the FK) and the target module (the 1 side).
        foreach ($availableModules as $otherModuleName => $otherModuleInfo) {
            if ($otherModuleName === $moduleName) continue;

            try {
                $otherObjectName = BeanFactory::getObjectName($otherModuleName);
                if (empty($otherObjectName)) {
                    $GLOBALS['log']->debug(__METHOD__ . ": Fourth source — empty objectName for '{$otherModuleName}', skipping.");
                    continue;
                }
                VardefManager::loadVardef($otherModuleName, $otherObjectName);
            } catch (\Exception $e) {
                $GLOBALS['log']->debug(__METHOD__ . ": Fourth source — Exception loading vardefs for '{$otherModuleName}': " . $e->getMessage());
                continue;
            }

            $otherFields = $dictionary[$otherObjectName]['fields'] ?? [];

            foreach ($otherFields as $fieldName => $f) {
                if (($f['type'] ?? '') !== 'relate') continue;
                if (empty($f['id_name'])) continue;
                if (!empty($f['link'])) continue;
                if (($f['module'] ?? '') !== $moduleName) continue;

                $virtualName = 'virtual__' . $fieldName;
                if (isset($processed[$virtualName])) continue;

                $vname = $f['vname'] ?? '';
                $baseText = '';
                if (!empty($vname)) {
                    $baseText = rtrim(trim(translate($vname, $otherModuleName)), ':');
                }
                if (empty($baseText)) {
                    $baseText = translate($moduleName) ?: $fieldName;
                }

                // Skip if a canonical relationship already covers the same text to the same module_dest.
                // This prevents redundant inverse virtuals (e.g. Leads' report_to_name → Contacts
                // when contact_direct_reports already provides "Informa a" within Contacts).
                if (isset($canonicalTextByDest[$moduleName]) && $canonicalTextByDest[$moduleName] === $baseText) {
                    $GLOBALS['log']->debug(__METHOD__ . ": Fourth source — SKIPPING '{$virtualName}' (field '{$fieldName}' in '{$otherModuleName}'): text '{$baseText}' already covered by canonical to '{$moduleName}'.");
                    continue;
                }

                $suffix = translate('LBL_AWF_FIELD_SUFFIX', 'stic_AWF_Forms');
                $text = $baseText . ' (' . $suffix . ')';

                $GLOBALS['log']->debug(__METHOD__ . ": Fourth source — FOUND relate field '{$fieldName}' in '{$otherModuleName}' pointing to '{$moduleName}', creating virtual '{$virtualName}' with text '{$text}'.");

                $result[$virtualName] = [
                    'name'               => $virtualName,
                    'text'               => $text,
                    'module_orig'        => $otherModuleName,
                    'module_dest'        => $moduleName,
                    'relationship'       => $virtualName,
                    'link_name'          => $fieldName,
                    'id_name'            => $f['id_name'],
                    'is_virtual_relate'  => true,
                    'is_inverse_virtual' => true,
                    'type'               => '1-N',
                    'relationship_type'  => 'one-to-many',
                ];

                $processed[$virtualName] = true;
            }
        }

        // Deduplicate by target module: if multiple relationships point to the same module_dest,
        // keep only the first non-virtual one (avoids duplicate Notes/Tasks entries in module selectors).
        // Virtual relationships (is_virtual_relate = true) are always kept, since they represent
        // distinct standalone relate fields that coexist with canonical relationships to the same module.
        $seenDest = [];
        foreach ($result as $relName => $relData) {
            $dest = $relData['module_dest'];
            $isVirtual = $relData['is_virtual_relate'] ?? false;
            if (isset($seenDest[$dest]) && !$isVirtual) {
                unset($result[$relName]);
            } else {
                $seenDest[$dest] = true;
            }
        }

        return self::$relationshipsCache[$cacheKey] = $result;
    }

    /**
     * Determines if a given field definition corresponds to a CRM Email field.
     */
    public static function isEmailField($fieldDef, $fieldName) 
    {
        if (isset($fieldDef['type']) && $fieldDef['type'] === 'email') {
            return true;
        }
        if (isset($fieldDef['type']) && $fieldDef['type'] === 'varchar' && 
            isset($fieldDef['source']) && $fieldDef['source'] === 'non-db' &&
            strpos($fieldName, 'email') !== false) {
            return true;
        }
        return false;
    }

    /**
     * Retrieves the relationships of given Module to given SuiteCRM modules.
     * Result: [Relationship]
     * Relationship: {
     *   name, text, module_orig, field_orig, relationship, module_dest
     * }
     */
    public static function getRelationships($moduleName, $availableModules) {
        $result = self::getRelationshipsFromDictionary($moduleName, $availableModules);

        uasort($result, function($a, $b) {
            return strcmp($a['text'], $b['text']);
        });

        return $result;
    }


    /**
     * Retrieves the relationships between given SuiteCRM modules.
     * Result: [Relationship]
     * Relationship: {
     *   name, text, module_orig, field_orig, relationship, module_dest
     * }
     */
    public static function getRelationshipsBetween($availableModules) {
        $result = [];
        foreach ($availableModules as $moduleName => $moduleInfo) {
            $rels = self::getRelationshipsFromDictionary($moduleName, $availableModules);
            foreach ($rels as $relName => $relData) {
                if (!isset($result[$relName])) {
                    $result[$relName] = $relData;
                }
            }
        }

        uasort($result, function($a, $b) {
            return strcmp($a['text'], $b['text']);
        });

        return $result;
    }

    /**
     * Get all modules enabled in Administration with a valid Bean
     * Result: [EnabedModule]
     * EnabledModule: {
     *   name, text, textSingular, inStudio, icon
     * }
     */
    public static function getEnabledModules() {
        global $app_list_strings, $beanList;

        $blackList = [
            'Home',
            'AOW_WorkFlow',
            'AOR_Reports', 'AOR_Scheduled_Reports',
            'KReports',
            'AOS_PDF_Templates',
            'DHA_PlantillasDocumentos',
            'AM_ProjectTemplates',
            // 'Documents',
            'Emails', 'EmailTemplates',
            'jjwg_Maps', 'jjwg_Markers', 'jjwg_Areas', 'jjwg_Address_Cache',
            'ProspectLists',
            'SecurityGroups', 'Roles', 'stic_Security_Groups_Rules',
            'SavedSearch', 
            'Spots',
            'Schedulers', 'SchedulersJobs',
            'Surveys', 'SurveyQuestions', 'SurveyResponses', 'SurveyQuestionOptions', 'SurveyQuestionResponses',
            'stic_Sepe_Actions', 'stic_Sepe_Files', 'stic_Sepe_Incidents',
            'stic_Signatures', 'stic_Signature_Log', 'stic_Signers',
            'stic_Messages', 'stic_Message_Marketing', 'stic_MessagesMan', 'stic_Conversations',
            'stic_Validation_Actions', 'stic_Validation_Results',
            'stic_AWF_Forms', 'stic_AWF_Responses', 'stic_AWF_Response_Details', 'stic_AWF_Links', 'stic_AWF_Deferred_Tickets', 'stic_AWF_Incoming_Events',
            'stic_Web_Forms',
            'stic_Settings',
            'Calendar', 'ResourceCalendar', 'stic_Bookings_Calendar', 'stic_Bookings_Places_Calendar', 'Reminders', 'Reminders_Invitees',
            'AOBH_BusinessHours',
            'AOK_KnowledgeBase', 'AOK_Knowledge_Base_Categories',
            'stic_Incorpora_Locations',
            'FP_Event_Locations'
        ];

        // Get Enabled Modules
        require_once("modules/MySettings/TabController.php");
        $controller = new TabController();
        $tabs = $controller->get_tabs_system();
        
        $enabled = [];
        foreach ($tabs[0] as $key=>$value) {
            if (!isset($beanList[$key]) || in_array($key, $blackList)) {
                continue;
            }
            $text = translate($key);
            $textSingular = $app_list_strings['moduleListSingular'][$key] ?? $text;
            $enabled[$key] = ["name" => $key, "text" => $text, "textSingular" => $textSingular, "inStudio" => false, "icon" => ""];
        }

        // Complete information from Studio
        require_once 'modules/ModuleBuilder/Module/StudioBrowser.php';
        $sb = new StudioBrowser();
        $nodes = $sb->getNodes();
        foreach ($nodes as $module) {
            if(isset($enabled[$module['module']])) {
                $enabled[$module['module']]['inStudio'] = true;
                $enabled[$module['module']]['icon'] = $module['icon'];
            }
        }

        // Sort modules by text
        uasort($enabled, function($a, $b) {
            return strcmp($a['text'], $b['text']);
        });

        return $enabled;
    }

    /**
     * Retrieves the Id and text of required records
     * Results: [{ id, text }]
     */
    public static function getRecordsTextById($module, $ids = []) {
        $results = [];
        if (empty($module) || empty($ids)) {
            return $results;
        }

        foreach ($ids as $id) {
            $bean = BeanFactory::getBean($module, $id);

            if (empty($bean) || empty($bean->id)) {
                continue;
            }

            $displayField = self::detectDisplayField($bean);
            $text = isset($bean->$displayField) ? $bean->$displayField : $bean->id;

            $results[] = [
                'id' => $bean->id,
                'text' => $text,
            ];
        }

        return $results;
    }

    /**
     * Gets the field name for the text of a record
     */
    public static function detectDisplayField($bean) {
        $fields = $bean->field_defs;

        $priorityFields = ['name', 'document_name', 'subject', 'full_name', 'first_name', 'last_name', 'title'];
        foreach ($priorityFields as $f) {
            if (isset($fields[$f])) {
                return $f;
            }
        }
        return 'id';
    }

    public static function cancelExpiredTickets() {
        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Running cancelExpiredTickets');

        $db = DBManagerFactory::getInstance();

        $dateNow = $db->convert($db->quoted(date('Y-m-d H:i:s')), 'datetime');

        // Release zombie tickets: reset 'processing' tickets stuck for >30 minutes back to 'pending'
        $sqlReleaseZombies = "UPDATE stic_awf_deferred_tickets
                SET status = 'pending', date_modified = {$dateNow}
                WHERE status = 'processing' AND date_modified < DATE_SUB(NOW(), INTERVAL 30 MINUTE) AND deleted = 0";
        $resultZombies = $db->query($sqlReleaseZombies);
        $zombieCount = $db->getAffectedRowCount($resultZombies);
        if ($zombieCount > 0) {
            $GLOBALS['log']->warn('Line ' . __LINE__ . ': ' . __METHOD__ . ': Released ' . $zombieCount . ' zombie deferred tickets (processing > 30min)');
        }

        // Cancel expired pending tickets
        $sql = "UPDATE stic_awf_deferred_tickets
                SET status = 'cancelled', date_modified = {$dateNow}
                WHERE status = 'pending' AND expiration_date < {$dateNow} AND deleted = 0";
        $result = $db->query($sql);

        $affectedRows = $db->getAffectedRowCount($result);
        if ($affectedRows > 0) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': Cancelled ' . $affectedRows . ' expired deferred tickets');

            // Update associated responses from 'awaiting_action' to 'error'
            $sqlUpdateResponses = "UPDATE stic_awf_responses r
                INNER JOIN stic_awf_deferred_tickets t ON t.stic_awf_responses_id_c = r.id
                SET r.status = 'error', r.date_modified = {$dateNow}
                WHERE t.status = 'cancelled' AND t.deleted = 0
                AND r.status = 'awaiting_action' AND r.deleted = 0";
            $db->query($sqlUpdateResponses);
        }

        return true;
    }

    public static function getCustomBaseColor() {
        $db = DBManagerFactory::getInstance();
        $color = $db->getOne("select value from stic_settings where name='GENERAL_CUSTOM_THEME_COLOR' and deleted=0");

        if (!is_string($color) || !preg_match('/#([a-fA-F0-9]{3}){1,2}\b/m', $color)) {
            $color = '';
        }

        if (!empty($color)){
            return $color;
        } else {
            return '#b5bc31';
        }
    }

    /**
     * Checks if a bean was newly created during the current execution flow.
     */
    public static function wasBeanCreatedInThisContext(string $beanId, ExecutionContext $context): bool 
    {
        foreach ($context->actionResults as $result) {
            foreach ($result->modifiedBeans as $modBean) {
                if ($modBean->beanId === $beanId && $modBean->modificationType === BeanModificationType::CREATED) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Recalculates auto-generated names for newly created beans.
     */
    public static function recalculateNameIfNeeded(SugarBean $bean, DataBlockResolved $block, ExecutionContext $context): void
    {
        $nameFieldInBlock = $block->getFieldValue('name');
        $nameIsUserDefined = $nameFieldInBlock && !empty($nameFieldInBlock->value);

        if (!$nameIsUserDefined && self::wasBeanCreatedInThisContext($bean->id, $context)) {
            $bean->retrieve($bean->id);
            $bean->name = '';
            $bean->save();
        }
    }

    /**
     * Finds and populates the display relate field (e.g. account_name) associated with a physical FK field (e.g. account_id)
     */
    public static function populateRelateDisplayField(SugarBean $bean, string $idName, string $targetBeanId): void
    {
        $nameField = null;
        foreach ($bean->field_defs as $fieldName => $def) {
            if (isset($def['type'], $def['id_name']) && $def['type'] === 'relate' && $def['id_name'] === $idName) {
                $nameField = $fieldName;
                break;
            }
        }
        if ($nameField) {
            $parentModule = $bean->field_defs[$nameField]['module'] ?? '';
            $rname = $bean->field_defs[$nameField]['rname'] ?? 'name';
            if ($parentModule) {
                $parentBean = BeanFactory::getBean($parentModule, $targetBeanId);
                if ($parentBean && $parentBean->id === $targetBeanId && isset($parentBean->$rname)) {
                    $bean->$nameField = $parentBean->$rname;
                    $GLOBALS['log']->debug(__METHOD__ . ": Populated relate field '{$nameField}' = '{$parentBean->$rname}'.");
                }
            }
        }
    }

    /**
     * Detects and saves the form type ('crm' or 'web') for legacy forms if not set.
     *
     * @param SugarBean $formBean
     * @param array $configData
     * @return void
     */
    public static function detectAndSaveFormType(SugarBean $formBean, array $configData): void
    {
        if (empty($formBean->form_type)) {
            $hasCheckSession = false;
            $mainFlowActions = $configData['flows']['0']['actions'] ?? [];
            if (!empty($mainFlowActions)) {
                // Gets the first element and evaluates if its name matches CheckSessionAction
                $firstAction = reset($mainFlowActions);
                $hasCheckSession = ($firstAction['name'] ?? '') === 'CheckSessionAction';
            }
            $formBean->form_type = $hasCheckSession ? 'crm' : 'web';
            $formBean->save();
        }
    }
}
