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
class ExternalReporting
{
    // Default language, it will be used if the instance language cannot be obtained from the settings.
    private $lang = 'es';

    // Variable to store debug messages
    private $info = '';

    // It is important during the development phase to not use previously used versions in order to avoid breaking existing reports.
    private $versionPrefix = 'sda';

    // Fields that we always exclude in our operations
    private $evenExcludedFields = ['template_ddown_c', 'currency_name', 'assigned_user_id', 'parent_name', 'deleted', 'created_by', 'created_by_name', 'created_by_link', 'modified_user_link', 'modified_by_name'];

    // Modules that we always included, although they are hidden in the CRM
    private $evenIncludedModules = [];

    // Modules that we always exclude
    private $evenExcludedModules = [
        'Administration',
        'AM_ProjectTemplates',
        'AOBH_BusinessHours',
        'AOK_Knowledge_Base_Categories',
        'AOK_KnowledgeBase',
        'AOR_Reports',
        'AOR_Scheduled_Reports',
        'AOS_PDF_Templates',
        'AOW_WorkFlow',
        'Bugs',
        'Calendar',
        'DHA_PlantillasDocumentos',
        'Documents', // Excluida por STIC#578
        'EmailTemplates',
        'FP_events',
        'Home',
        'jjwg_Address_Cache',
        'jjwg_Areas',
        'jjwg_Maps',
        'jjwg_Markers',
        'KReports',
        'ProspectLists',
        'Prospects',
        'ResourceCalendar',
        'SecurityGroups',
        'Spots',
        'stic_Bookings_Calendar',
        'stic_Sepe_Files',
        'stic_Settings',
        'stic_Validation_Actions',
        'stic_Web_Forms',
        'stic_Incorpora_Locations',
        'stic_Validation_Results',
        'stic_Custom_Views',
    ];

    // Autorelationships that we always exclude. 
    // We use the module && field name instead of the relationship name, because the iteration is done through the fields and not through the relationships

    private $evenExcludedAutoRelationships = [
        'Contacts:reports_to_link',
        'Users:reports_to_link',
        
    ];
    // Autorleationships that we always include

    private $viewPrefix;
    private $listViewPrefix;
    private $maxNonAdminUsers;
    private $hostName;
    private $baseHostname;
    private $sdaSettings = [];
    private $langCode;
    private $autoRelationshipsRegistered = []; // Array to store all modules auto relationships already registered for use in ACLs

    public function __construct()
    {
        global $sugar_config;

        $this->hostName = $sugar_config['host_name'];
        $this->baseHostname = explode('.', $this->hostName)[0];

        $this->sdaSettings['publishAsTable'] = $sugar_config['stic_sinergiada']['publish_as_table'] ?? [];

        // If a specific language is not provided, the language defined for the instance will be used.
        if (!empty($_REQUEST['lang'])) {
            $this->lang = $_REQUEST['lang'];
            switch ($_REQUEST['lang']) {
                case 'ca':
                    $this->langCode = 'ca_ES';
                    break;
                case 'en':
                    $this->langCode = 'en_US';
                    break;
                case 'gl':
                    $this->langCode = 'gl_ES';
                    break;
                default:
                    $this->langCode = 'es_ES';
                    break;
            }
        } else {
            $this->langCode = $sugar_config['default_language'];
            $this->lang = explode('_', $this->langCode)[0];
        }

        $this->viewPrefix = "{$this->versionPrefix}";
        $this->listViewPrefix = "{$this->viewPrefix}_l";

    }
    /**
     * Main function responsible for creating and managing MariaDB views and tables based on CRM modules.
     *
     * Here's a breakdown of its operations:
     * 1) For each enabled CRM module (excluding specified ones), it creates a MariaDB view or table. These are derived from the module's primary table and, if present, the _cstm table, excluding records marked as deleted.
     * 2) For every 1:n or n:1 relationship where the current module represents the "n" side, a column is added to the respective view or table.
     * 3) Generates essential exchange tables and views (prefixed with "sda_def_") to facilitate EDA's data interpretation and management.
     *
     * @param Boolean $callUpdateModel Indicates whether to call updateModel (true) or not (false).
     * @param String $rebuildFilter Specifies the rebuild scope: 'views' for views only, 'tables' for tables only, and 'all' or any other value for both tables and views.
     * @return String Provides debug information in HTML format, displayed only when $_REQUEST['print_debug'] equals 1.
     */

    public function createViews($callUpdateModel = true, $rebuildFilter = 'all')
    {

        $startTime = microtime(true);
        $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "SinergiaDA rebuild script starts!");

        global $app_list_strings, $sugar_config;

        $archivo = __FILE__; // Ruta del archivo actual
        $fechaModificacion = filemtime($archivo);
        $fechaFormateada = date('Y-m-d H:i:s', $fechaModificacion);

        $this->info = "<strong>La fecha de modificación del archivo es: " . $fechaFormateada . "</strong><br>";

        $this->info .= '<link rel="stylesheet" type="text/css" href="cache/themes/SuiteP/css/Stic/style.css" />';

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': Running Createviews() function');

        $db = DBManagerFactory::getInstance();

        // Check number of non-admin users enabled
        // Get configured limit for non-admin user processing
        if (is_numeric($sugar_config['stic_sinergiada']['max_users_processed'] ?? null)) {
            $maxNonAdminUsers = $sugar_config['stic_sinergiada']['max_users_processed'];
            $normalUsersEnabled = $db->query("SELECT
                                                    distinct u.id
                                                FROM users u
                                                INNER JOIN users_cstm uc ON uc.id_c = u.id
                                                    WHERE
                                                        u.is_admin = 0
                                                        AND u.deleted = 0
                                                        AND u.status = 'Active'
                                                        AND uc.sda_allowed_c=1;"
            );
            // If the number of non-admin users enabled is greater than the limit allowed, the operation is aborted
            // to protect the system from possible performance problems
            if (!empty($normalUsersEnabled) && is_object($normalUsersEnabled) && $normalUsersEnabled->num_rows > $maxNonAdminUsers) {
                $errorString = return_module_language($_SESSION['authenticated_user_language'], 'Administration')['LBL_STIC_SINERGIADA_MAX_USERS_ERROR'];
                $errorString = str_replace('__max_users__', $maxNonAdminUsers, $errorString);
                $errorString = str_replace('__enabled_users__', $normalUsersEnabled->num_rows, $errorString);
                $this->info .= "[FATAL: {$errorString}]";
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Number of non-admin users enabled is greater than the limit allowed. Is not possible to enable more than {$maxNonAdminUsers} non-admin users. There is a total of {$normalUsersEnabled->num_rows} non-admin users enabled.');
                return $this->info;
            }
        }

        // Before create any view, delete previous old views
        $this->deleteOldViews();

        $this->info .= "<div><a href='index.php?module=Administration&action=createReportingMySQLViews&print_debug=1'>All modules</a> </div>";

        // Reset general metadata tables
        $this->resetMetadataTables();

        // Reset user metadata views
        $this->resetMetadataViews();

        $this->info .= "<h2>Creating MySQL/MariaDB views & tables</h2>";

        // For test & debug mode
        $onlyIncludeModule = isset($_REQUEST['only']) ? $_REQUEST['only'] : '';

        // Get module list in current lang
        $langAppListStrings = return_app_list_strings_language($this->langCode);

        // Filter only allowed modules in CRM
        include_once 'modules/MySettings/TabController.php';
        $controller = new TabController();
        $visibleModules = $controller->get_system_tabs();

        $modulesList = array_diff($visibleModules, $this->evenExcludedModules);

        // The modules that should always be included are added
        $modulesList = array_merge($modulesList, $this->evenIncludedModules);

        $modulesList = array_unique($modulesList);

        // We force add the users module, since, otherwise, it will never appear on the list of visible modules
        $modulesList['Users'] = 'Users';

        // If the Project module is enabled, we directly include the project tasks module,
        // since otherwise it would never be included, because it is not included directly in the application menu
        if (in_array('Project', $modulesList)) {
            $modulesList['ProjectTask'] = 'ProjectTask';
        }

        // If Surveys module is enabled, we automatically activate the related Surveys modules
        if (in_array('Surveys', $modulesList)) {
            $modulesList['SurveyResponses'] = 'SurveyResponses';
            $modulesList['SurveyQuestions'] = 'SurveyQuestions';
            $modulesList['SurveyQuestionOptions'] = 'SurveyQuestionOptions';
            $modulesList['SurveyQuestionResponses'] = 'SurveyQuestionResponses';
        }

        natsort($modulesList);

        foreach ($modulesList as $moduleName) {
            // Reset module index list
            unset($indexesToCreate);

            // Reset auto relationships
            $autoRelationships = [];

            // View creation query
            $createViewQuery = [];

            $moduleStart = microtime(true);
            // If $onlyIncludeModule is set, only this module will be processed and others will be ignored for debugging purposes).
            if ($moduleName != $onlyIncludeModule && !empty($onlyIncludeModule)) {
                continue;
            };

            // The module is processed or omitted, depending on the received parameter
            switch ($rebuildFilter) {
                case 'tables':
                    if (!in_array($moduleName, $this->sdaSettings['publishAsTable'])
                        && $this->sdaSettings['publishAsTable'][0] != '0'
                    ) {
                        $this->info .= "<li>{$moduleName} (Omitted)</li>";
                        continue 2;
                    }
                    break;
                case 'views':
                    if (in_array($moduleName, $this->sdaSettings['publishAsTable'])
                        || $this->sdaSettings['publishAsTable'][0] == '1'
                    ) {
                        $this->info .= "<li>{$moduleName} (Omitted)</li>";
                        continue 2;
                    }
                    break;
                default:

                    break;
            }

            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Processing module {$moduleName}");

            // Get only allowed fields in detail view
            $detailViewVisibleFields = $this->detailViewVisibleFields($moduleName);

            // Create array to save lists used in module
            $listNames = array();

            // Fix when table name & module name are not equal.
            switch ($moduleName) {
                case 'CampaignLog':
                    $tableName = 'campaign_log';
                    break;
                case 'ProjectTask':
                    $tableName = 'project_task';
                    break;

                default:
                    $tableName = strtolower($moduleName);
                    break;
            }

            if (!$this->checkSdaTables($tableName)) {
                continue;
            }

            // Get translated module name
            $txModuleName = $langAppListStrings['moduleList'][$moduleName];
            $this->info .= "<li module='{$moduleName}'><a href='#'>	{$txModuleName} ({$moduleName})</a></li><div id='{$moduleName}' style='display:none;'>";

            // Sanitize text
            $viewName = $this->sanitizeText("{$this->viewPrefix}_{$tableName}");

            // Get module bean for use later
            $moduleBean = BeanFactory::getBean($moduleName);

            // recover module labels
            $modStrings = return_module_language($this->langCode, $moduleName);

            // Initialize a variable to concatenate the different Left Join corresponding to Relationships fields
            $leftJoins = '';

            $fieldList = [];

            // We save here the aliases we are using to avoid duplicates when there is more than one relationship with the same module
            $usedAlias = [];
            // Process the content of the field according to the type
            foreach ($moduleBean->getFieldDefinitions() as $fieldK => $fieldV) {
                // We reset certain variables to avoid errors
                unset($fieldSrc, $relatedModuleName, $secureName, $edaAggregations, $sdaHiddenField, $excludeColumnFromMetadada, $isAutoRelationship);

                // To avoid exceptional cases where the table name is defined in uppercase
                // (like in the relationship between Contacts and Cases) we convert the table name to lowercase
                $fieldV['table'] = strtolower($fieldV['table'] ?? '');
                
                $fieldName = $fieldV['name'];

                $fieldPrefix = ($fieldV['source'] ?? null) == 'custom_fields' ? 'c' : 'm';

                // If the field is excluded, skip it
                if (in_array($fieldV['name'], $this->evenExcludedFields)) {
                    continue;
                }

                // Conditionally controls the visibility of fields in the detail view:
                // * Shows fields present in the detail view.
                // * Always shows the "full_name" & "id" field, regardless of its presence in the detail view.
                if (in_array($fieldV['name'], $detailViewVisibleFields) || in_array($fieldV['name'], ['id', 'full_name'])) {
                    $sdaHiddenField = false;
                } else {
                    $sdaHiddenField = true;
                }

                $fieldV['label'] = $this->sanitizeText($modStrings[$fieldV['vname'] ?? null] ?? null);

                // Attempts to assign a translated label to the field.
                // If no translation is found, it tries to translate it directly.
                // The field is skipped if no translation is obtained.
                if (empty($fieldV['label']) && $fieldV['name'] != 'id') {
                    $directTranslate = translate($fieldV['vname'] ?? '', $fieldV['module'] ?? null);
                    if (!empty($directTranslate)) {
                        $fieldV['label'] = $this->sanitizeText($directTranslate);
                    } else {
                        continue;
                    }
                }

                // There are some exceptions that must be applied in specific modules that have not been seen how to solve otherwise
                switch ($moduleName) {
                    case 'Cases':
                        // exclude this field because there is another with same name
                        if (($fieldV['id_name'] ?? null) == 'account_id') {
                            continue 2;
                        }
                        break;
                    default:
                        break;
                }

                // Let's establish specific actions according to the field type
                switch ($fieldV['type']) {
                    case 'id':
                        if ($fieldV['name'] == 'id') {
                            // It is the normal ID field of a module
                            $fieldV['alias'] = 'id';
                            $fieldV['label'] = "ID $txModuleName";

                            $fieldSrc = "m.{$fieldV['name']} AS id";

                            $indexesToCreate[] = 'id';

                        } else {
                            // It is a related field of the ID type and we will not process it
                            continue 2;
                        }
                        break;
                    case 'function':
                    case 'assigned_user_name':
                    case 'file':
                    case 'parent_type':
                    case 'parent_name':
                    case 'parent':
                    case 'kreporter':
                    case 'image':
                    case 'link':
                    case 'mailchimprating':
                    case 'Signature':
                    case 'Esignature':
                    case 'wysiwyg':
                    case 'time':
                    case 'iframe':
                    case 'currency_id':
                    case 'emailbody':
                    case 'none':
                    case 'ColourPicker':
                    case 'collection':
                        continue 2;
                        break;
                    case 'relate':
                        if (
                            (in_array($fieldV['module'] ?? null, $this->evenExcludedModules) && $fieldV['name'] != 'assigned_user_name')

                        ) {
                            continue 2;
                        } else {

                            $relatedModuleName = $app_list_strings['moduleList'][$fieldV['module'] ?? ''] ?? '';

                            // The relationship between contacts and accounts does not have the 'link' property,
                            // which is necessary for retrieval of the relationship values, so we add it directly.
                            if ($fieldName == 'account_id' && $moduleName == 'Contacts') {
                                $fieldV['link'] = 'accounts_contacts';
                            }

                            if (isset($fieldV['link']) && !empty($fieldV['link']) && $fieldV['name'] != 'assigned_user_name') {

                                // Build and obtain the translated value from the other side of the relationship so it can be properly displayed in SinergiaDA
                                $joinModuleRelLabel = 'LBL_' . strtoupper($fieldV['link']) . '_FROM_' . strtoupper($moduleName) . '_TITLE';
                                $joinLabel = translate($joinModuleRelLabel, $fieldV['module'] ?? '');
                                $joinLabel = empty($joinLabel) || $joinLabel == $joinModuleRelLabel ? $txModuleName : $joinLabel;

                                // Check if the relationship is an autorelationship & prepare autorelationship data for use later
                                if (($fieldV['module'] ?? null) == $moduleName) {
                                    
                                    // Check if the autorelationship is excluded & skip it if it is
                                    if(in_array("{$fieldV['module']}:{$fieldV['link']}", $this->evenExcludedAutoRelationships)){
                                        continue 2;
                                    }

                                    $fieldV['isAutoRelationship'] = true;
                                    $fieldV['rLabel'] = translate('LBL_' . strtoupper($fieldV['link']) . '_FROM_' . strtoupper($moduleName) . '_R_TITLE', $fieldV['module']);
                                    $fieldV['lLabel'] = translate('LBL_' . strtoupper($fieldV['link']) . '_FROM_' . strtoupper($moduleName) . '_L_TITLE', $fieldV['module']);
                                    $fieldV['autoRelJoinModuleRelLabel'] = 'LBL_' . strtoupper($fieldV['link']) . '_FROM_' . strtoupper($moduleName) . '_R_TITLE';
                                    $autoRelationships[$fieldV['link']] = $fieldV;
                                    $this->autoRelationshipsRegistered[$fieldV['link']] = $fieldV['table'];
                                }

                                $res = $this->createRelateLeftJoin($fieldV, $tableName, $joinLabel, ($fieldV['isAutoRelationship'] ?? null));

                                if (empty($res)) {
                                    continue 2;
                                }

                                $fieldSrc = " IFNULL({$res['field']},'') ";

                                // Prepare parent_id field for autorelationships
                                if (isset($res['fieldForAutoRelationshipsNSide'])) {
                                    $autoRelationships[$fieldV['link']]['parentIdfieldSrc'] = ", IFNULL({$res['fieldForAutoRelationshipsNSide']},'') as parent_id";
                                }

                                // Common left join for all relationships
                                if (!empty($res['leftJoin'])) {
                                    $leftJoins .= "\n\t{$res['leftJoin']} ";
                                }

                                $fieldV['alias'] = substr($fieldV['id_name'], 0, 64);

                                // Inner join for autorelationships N side
                                if (!empty($res['innerJoinForAutoRelationshipsNSide'])) {
                                    $autoRelationships[$fieldV['link']]['innerJoin'] = "{$res['innerJoinForAutoRelationshipsNSide']} ";
                                }

                                if (in_array($fieldV['alias'], $usedAlias)) {
                                    $relName = $fieldV['link'];
                                    $fieldV['alias'] = $this->sanitizeText("{$fieldV['alias']}_{$relName}");
                                }
                                $usedAlias[] = $fieldV['alias'];

                                // add column to index list
                                $indexesToCreate[] = $fieldV['alias'];

                                if (!empty($fieldSrc)) {
                                    $fieldList['related'][$fieldK] = $fieldSrc . " AS {$fieldV['alias']}";
                                } else {
                                    $fieldList['failedRelations'][$fieldK] = $fieldSrc . " AS {$fieldV['alias']}";
                                }

                            } else {
                                /**
                                 * Management of 'relate' type fields:
                                 *
                                 * 1) Creation of a column with the ID of the record in the related module (e.g., contacts_id_c).
                                 *    This column establishes a complete relationship with the other module, allowing the use
                                 *    of any column from the related module. This column is not created in case the relationship
                                 *    is with another record of the same module (autorelationship).
                                 *
                                 * 2) Creation of a column with the name of the related record, or 'first_name' + 'last_name'
                                 *    if it's a 'Person' type module.
                                 *    This column allows for easy use of the related record's name, without the
                                 *    need for establishing additional relationships.
                                 */

                                $secureName = preg_replace('([^A-Za-z0-9])', '_', $app_list_strings['moduleList'][$fieldV['module']]) . ' (' . $fieldV['label'] . ')';

                                // It is necessary to detect whether or not the field is custom, and in this case, we obtain it looking at the property of the field containing the ID of the Relationship Table ("...id_c")
                                $fieldV['source'] = $moduleBean->getFieldDefinitions()[$fieldV['id_name']]['source'] ?? $fieldV['source'] ?? null;

                                $fieldPrefix = $fieldV['source'] == 'custom_fields' ? 'c' : 'm';

                                $fieldV['alias'] = $fieldV['id_name'];

                                // In the case of the Users module, the 'module' value may appear empty, if so, we force it.
                                $fieldV['module'] = empty($fieldV['module']) && $fieldV['table'] == 'users' ? 'Users' : $fieldV['module'];
                                $relatedTableName = strtolower($fieldV['module']);
                                $leftJoinAlias = "{$fieldV['name']}_{$relatedTableName}";

                                // Add id field
                                $fieldSrc = " IFNULL({$fieldPrefix}.{$fieldV['id_name']},'') AS {$fieldV['alias']}, ";

                                // add column to index list
                                $indexesToCreate[] = "{$fieldV['id_name']}";

                                //Add relate record name
                                if (in_array($fieldV['module'], ['Contacts', 'Leads']) || (Beanfactory::newBean($fieldV['module'])->field_defs['last_name'] ?? null) && $fieldV['module'] != 'Users') {
                                    $relatedName = " concat_ws(' ', {$leftJoinAlias}.first_name, {$leftJoinAlias}.last_name) ";
                                } elseif ($fieldV['module'] == 'Users') {
                                    $relatedName = "{$leftJoinAlias}.user_name";
                                } else {
                                    $relatedName = "{$leftJoinAlias}.name";
                                }

                                $fieldSrc .= " IFNULL($relatedName,'') AS {$fieldV['name']}";

                                $leftJoins .= " \n\tLEFT JOIN {$relatedTableName} AS {$leftJoinAlias} ON {$leftJoinAlias}.id = {$fieldPrefix}.{$fieldV['id_name']} AND {$leftJoinAlias}.deleted=0";

                                // Related fields are treated as normal fields. Since the Source property is indicated Non-DB,
                                // If is a base fields, we directly include them into "base" array to prevent subsequently excluding.
                                if ($fieldPrefix == 'm') {
                                    $fieldList['base'][$fieldK] = $fieldSrc;
                                }

                                $fieldV['targetModule'] = strtolower($fieldV['module']);

                                // Añadimos directamente los metadatos en sda_def_columns para la columna que contiene el nombre del regitro relacionado
                                // al mismo tiempo nos aseguramos de que no se añade a sda_def_columns la columna que contiene el id
                                $excludeColumnFromMetadada = true;
                                $this->addMetadataRecord(
                                    'sda_def_columns',
                                    [
                                        'table' => "{$this->viewPrefix}_{$tableName}",
                                        'column' => $fieldV['name'],
                                        'type' => 'text',
                                        'aggregations' => 'count,count_distinct,none',
                                        'label' => $fieldV['label'],
                                        'description' => addslashes($fieldV['label']),
                                        'sda_hidden' => 0,
                                        'stic_type' => $fieldV['type'] . '-name',
                                    ]
                                );

                                // Add metadata record for relate fields
                                $this->addMetadataRecord(
                                    'sda_def_relationships',
                                    [
                                        'id' => "{$tableName}_{$fieldV['name']}",
                                        'source_table' => "{$this->viewPrefix}_{$tableName}",
                                        'source_column' => $fieldV['id_name'],
                                        'target_table' => "{$this->viewPrefix}_{$fieldV['targetModule']}",
                                        'target_column' => 'id',
                                        'info' => 'relate',
                                        'label' => "{$fieldV['label']}|{$txModuleName}",
                                    ]
                                );
                            }
                        }
                        break;

                    case 'multienum':

                        // Create listViewName for use in metadata & view creation
                        $listViewName = substr(join('_', [$tableName, $fieldV['name'], $fieldV['options']]), 0, 58);

                        $createdListView = $this->createEnumView($fieldV['options'], $listViewName);

                        // If there is a valid drop-down list we continue, otherwise we move on to the next column
                        if (!empty($createdListView)) {
                            $listNames[] = $createdListView;
                        } else {
                            continue 2;
                        }

                        $fieldV['alias'] = $fieldV['name'];
                        $fieldV['bridgeTableName'] = mb_strcut("{$this->viewPrefix}_{$tableName}__{$fieldV['name']}", 0, 64);
                        $fieldSrc = "IFNULL({$fieldPrefix}.{$fieldV['name']},'') AS {$fieldName}";

                        $this->addMetadataRecord(
                            'sda_def_enumerations',
                            [
                                'source_table' => "{$this->viewPrefix}_{$tableName}",
                                'source_column' => $fieldV['name'],
                                'master_table' => "{$this->listViewPrefix}_{$listViewName}",
                                'bridge_table' => $fieldV['bridgeTableName'],
                                'source_bridge' => 'id',
                                'target_bridge' => 'code',
                                'info' => 'enum_list',
                                'stic_type' => $fieldV['type'],
                            ]
                        );
                        break;
                    case 'enum':
                    case 'dynamicenum':
                    case 'bool':
                    case 'radioenum':

                        $fieldV['alias'] = $fieldV['name'];

                        $listName = $fieldV['type'] == 'bool' ? 'stic_boolean_list' : $fieldV['options'];

                        // Create listViewName for use in metadata & view creation
                        $listViewName = substr(join('_', [$tableName, $fieldV['name'], $listName]), 0, 58);

                        $fieldSrc = " IFNULL(CAST({$fieldPrefix}.{$fieldV['name']} AS CHAR),'') AS {$fieldName}";

                        $createdListView = $this->createEnumView($listName, $listViewName);

                        // If there is a valid drop-down list or if it corresponds to that of a boolean field
                        // we continue, otherwise we move on to the next column
                        if (!empty($createdListView) || $listName == 'stic_boolean_list') {
                            $listNames[] = $createdListView;
                        } else {
                            continue 2;
                        }

                        $this->addMetadataRecord(
                            'sda_def_enumerations',
                            [
                                'source_table' => "{$this->viewPrefix}_{$tableName}",
                                'source_column' => $fieldV['name'],
                                'master_table' => "{$this->listViewPrefix}_{$listViewName}",
                                'info' => 'enum_list',
                                'stic_type' => $fieldV['type'],
                            ]
                        );
                        break;
                    case 'datetime':
                    case 'datetimecombo':
                        $fieldV['alias'] = $fieldV['name'];
                        $tzDateValue = "CONVERT_TZ({$fieldPrefix}.{$fieldV['name']}, 'UTC', 'Europe/Madrid')";
                        $fieldSrc = "IFNULL({$tzDateValue},'') AS {$fieldName}";
                        break;

                    case 'date':
                    case 'varchar':
                    case 'phone':
                    case 'text':
                    case 'readonly':
                    case 'fullname':
                    case 'name':
                    case 'url':
                    case 'html':
                    case 'user_name':
                    case 'ColorPicker':
                    case 'email':
                        $fieldV['alias'] = $fieldV['name'];
                        if ($fieldV['name'] == 'email1' && $fieldV['type'] == 'varchar' && $fieldV['source'] == 'non-db') {
                            // Special field for main email
                            $fieldSrc = "IFNULL(ea.email_address,'') AS {$fieldV['name']}";

                            // add left join for email field
                            $leftJoins .= " LEFT JOIN email_addr_bean_rel eabr ON m.id = eabr.bean_id AND eabr.bean_module = '{$moduleName}' AND eabr.deleted = 0 AND eabr.primary_address = 1 LEFT JOIN email_addresses ea ON eabr.email_address_id = ea.id AND ea.deleted = 0 ";
                        } elseif ($fieldV['name'] == 'full_name') {
                            // special query for full_name
                            $fieldSrc = "IFNULL(CONCAT_WS(' ',m.first_name, m.last_name),'') as {$fieldV['name']}";
                        } else {
                            $fieldSrc = "IFNULL({$fieldPrefix}.{$fieldV['name']},'') AS {$fieldName}";
                        }
                        break;

                    // Numeric types
                    case 'decimal':
                    case 'int':
                    case 'currency':
                    case 'float':
                        $fieldV['alias'] = $fieldV['name'];

                        if ($fieldV['type'] == 'float' && in_array($fieldV['name'], ['jjwg_maps_lat_c', 'jjwg_maps_lng_c'])) {
                            // We use a specific configuration for the latitude and longitude fields of the JJWG Maps module
                            $decConfig = '11,8';
                        } else {
                            $decConfig = '20,4';
                        }

                        // Numeric type columns are converted to decimal to ensure they remain in this type in the view,
                        // avoiding errors in min and max aggregations due to ordering
                        $fieldSrc = "CONVERT(IFNULL({$fieldPrefix}.{$fieldV['name']},''), decimal({$decConfig})  ) AS {$fieldName}";

                        break;

                    default:
                        $this->info .= "<div class='error' style='color:red;'>ERROR: [FATAL: Unprocessed field type. {$fieldV['type']} | Módule: {$moduleName} - Field: {$fieldV['name']}] </div>";
                        $this->info .= "[FATAL: Unprocessed field type. {$fieldV['type']} | Módule: {$moduleName} - Field: {$fieldV['name']}]";
                        $this->info .= print_r($fieldV, true);

                        break;
                }

                // Map to appropriate eda type
                switch ($fieldV['type']) {
                    case 'decimal':
                    case 'int':
                    case 'currency':
                    case 'float':
                        $edaType = !in_array($fieldV['name'], ['jjwg_maps_lat_c', 'jjwg_maps_lng_c']) ? 'numeric' : 'coordinate';
                        $edaPrecision = $fieldV['type'] == 'currency' ? 2 : 0;
                        $edaPrecision = $fieldV['precision'] ?? $edaPrecision;
                        break;
                    case 'date':
                    case 'datetime':
                    case 'datetimecombo':
                        $edaType = 'date';
                        break;
                    case 'enum':
                    case 'multienum':
                    case 'dynamicenum':
                    case 'bool':
                    case 'radioenum':
                        $edaType = 'enumeration';
                        break;
                    default:
                        $edaType = 'text';
                        break;
                }

                //  set aggregation according EDA field type if it has not been previously defined
                if (empty($edaAggregations)) {
                    switch ($edaType) {
                        case 'numeric':
                            $edaAggregations = 'sum,avg,max,min,count,count_distinct,none';
                            break;
                        case 'text':
                        case 'enumeration':
                        case 'date':
                            $edaAggregations = 'count,count_distinct,none';
                            break;

                        default:
                            $edaAggregations = 'none';
                            break;
                    }
                }

                // We add the values calculated to Array $fieldv to use them later in other methods
                $fieldV['edaType'] = $edaType;

                if (isset($fieldSrc)) {
                    // Add to the array of normal base and custom fields
                    if (!empty($fieldV['source']) && $fieldV['source'] == 'custom_fields') {
                        $fieldList['custom'][$fieldK] = $fieldSrc;
                        $addColumnToMetadata = 1;
                    } else if (!empty($fieldV['source']) && $fieldV['source'] == 'non-db' && $fieldV['name'] != 'full_name' && $fieldV['name'] != 'email1') {
                        // This source is not processed, so we are moving them away
                        $fieldList['non-db'][$fieldK] = $fieldSrc;
                        $addColumnToMetadata = 0;
                    } else {
                        if ($fieldV['type'] != 'relate') {
                            $fieldList['base'][$fieldK] = $fieldSrc;
                            $addColumnToMetadata = 1;
                        }
                    }

                    if (isset($excludeColumnFromMetadada) && $excludeColumnFromMetadada == true) {
                        $addColumnToMetadata = 0;
                    }
                    // Only the columns that are really going to be added to the views are added to the SDA_def_Columns table
                    if ($addColumnToMetadata == 1) {
                        // Add field metadata
                        $this->addMetadataRecord(
                            'sda_def_columns',
                            [
                                'table' => "{$this->viewPrefix}_{$tableName}",
                                'column' => $fieldV['alias'],
                                'type' => $edaType,
                                'decimals' => $edaPrecision ?? 0,
                                'aggregations' => empty($edaAggregations) ? 'none' : $edaAggregations,
                                'label' => html_entity_decode($fieldV['label'], ENT_QUOTES),
                                'description' => addslashes($fieldV['label']),
                                'sda_hidden' => $sdaHiddenField ?: 0,
                                'stic_type' => $fieldV['type'],
                            ]
                        );
                    }
                }

                unset($edaPrecision);
            }

            // VIRTUAL FIELDS
            // Include existing files in modules and custom/modules/ to create virtual fields in SinergiaCRM
            $sourceFiles = [
                "modules/$moduleName/SDAVardefs.php",
                "custom/modules/$moduleName/Ext/SDAVardefs/SDAVardefs.ext.php",
            ];

            foreach ($sourceFiles as $file) {
                if (file_exists($file)) {
                    require_once $file;

                    $fieldsToProcess = [];

                    if (basename($file) === 'SDAVardefs.ext.php') {
                        // For custom files, use $dictionary[$moduleName]['SDAVirtualFields']
                        $fieldsToProcess = $dictionary[$moduleName]['SDAVirtualFields'] ?? [];
                    } else {
                        // For standard files, use $SDAVirtualFields
                        $fieldsToProcess = $SDAVirtualFields ?? [];
                    }

                    if (!empty($fieldsToProcess) && is_array($fieldsToProcess)) {
                        foreach ($fieldsToProcess as $fieldName => $fieldData) {
                            // Get the translated label or use the original if not available
                            $virtualFieldLabel = $modStrings[$fieldData['label']] ?? $fieldData['label'];

                            // Check if the virtual field label is empty
                            if (empty($virtualFieldLabel)) {
                                $this->info .= "<div style='color:red;'>VIRTUAL FIELD ERROR: <b>[{$file}]</b> - The virtual field was not processed because there is no translation available for {$this->langCode}</div>";
                                $this->info .= "[FATAL: Virtual Field without label $viewName - $file]";
                                continue;
                            }

                            // Get the translated description or use the original if not available
                            $virtualFieldDescription = $modStrings[$fieldData['description']] ?? $fieldData['description'];

                            // Add the virtual field to the fieldList array
                            $fieldList['virtual'][$fieldName] = " {$fieldData['expression']} AS '{$fieldName}'";

                            // Add metadata record for the virtual field
                            $this->addMetadataRecord(
                                'sda_def_columns',
                                [
                                    'table' => "{$this->viewPrefix}_{$tableName}",
                                    'column' => $fieldName,
                                    'type' => $fieldData['type'],
                                    'decimals' => $fieldData['precision'] ?? 0,
                                    'aggregations' => $fieldData['aggregations'] ?? 'none',
                                    'label' => html_entity_decode($virtualFieldLabel, ENT_QUOTES),
                                    'description' => addslashes($virtualFieldDescription),
                                    'sda_hidden' => $fieldData['hidden'] ?? 0,
                                    'stic_type' => 'virtual',
                                ]
                            );
                        }
                    } else {
                        $this->info .= "<div style='color:orange;'>WARNING: The file {$file} does not contain a valid array of virtual fields.</div>";
                    }

                    // Clear the variables after processing to avoid conflicts with the next file
                    unset($SDAVirtualFields, $dictionary);
                }
            }
            // END VIRTUAL FIELDS

            // Add module metadata
            $this->addMetadataRecord(
                'sda_def_tables',
                [
                    'table' => "{$this->viewPrefix}_{$tableName}",
                    'label' => $txModuleName,
                    'description' => addslashes($txModuleName),
                ]
            );

            // Add auto relationships metadata if exists
            unset($qualifiedLabel);
            if (!empty($autoRelationships)) {
                foreach ($autoRelationships as $key => $value) {
                    if ($txModuleName != $value['rLabel']) {
                        $qualifiedLabel = "{$txModuleName} ({$value['rLabel']})";
                    } else {
                        $qualifiedLabel = "{$txModuleName} ({$value['label']})";
                    }

                    $this->addMetadataRecord(
                        'sda_def_tables',
                        [
                            'table' => "{$this->viewPrefix}_{$tableName}_{$key}",
                            'label' => $qualifiedLabel,
                            'description' => addslashes($qualifiedLabel),
                        ]

                    );
                }
            }

            // Sql Header. Depending on the value of the SDA_MODE_MODE setting we create tables or views mysql
            if (
                in_array($moduleName, $this->sdaSettings['publishAsTable'])
                || (!empty($this->sdaSettings['publishAsTable'][0]) && $this->sdaSettings['publishAsTable'][0] == '1')
            ) {
                $tableMode = 'table';
                $createViewQueryHeader = " CREATE OR REPLACE TABLE {$viewName} ENGINE=InnoDB AS SELECT ";
            } else {
                $tableMode = 'view';
                $createViewQueryHeader = " CREATE OR REPLACE VIEW {$viewName} AS SELECT ";
            }

            // Add base table fields
            $createViewQueryFields = ''; // important
            foreach ($fieldList['base'] as $bKey => $bValue) {
                $createViewQueryFields .= " {$bValue}, ";
            }

            // Add custom table fields
            if (!empty($fieldList['custom'])) {
                foreach ($fieldList['custom'] as $cKey => $cValue) {
                    $createViewQueryFields .= " {$cValue}, ";
                }
            }

            // Add virtual fields
            if (!empty($fieldList['virtual'])) {
                foreach ($fieldList['virtual'] as $vKey => $vValue) {
                    $createViewQueryFields .= " {$vValue}, ";
                }
            }

            // Special fields to allow count records
            $createViewQueryFields .= " 1 as N, "; // N field for counters

            // Add N column to metadata
            $this->addMetadataRecord(
                'sda_def_columns',
                [
                    'table' => "{$this->viewPrefix}_{$tableName}",
                    'column' => 'N',
                    'type' => 'numeric',
                    'aggregations' => 'sum,avg,max,min,count,count_distinct,none',
                    'decimals' => 0,
                    'label' => "(n) {$txModuleName}",
                    'description' => 'Special field allow count',
                    'stic_type' => 'none',
                ]
            );

            // Relations
            if (!empty($fieldList['related'])) {
                foreach ($fieldList['related'] as $cValue) {
                    $createViewQueryFields .= " {$cValue}, ";
                }
            }
            $createViewQueryFields = rtrim($createViewQueryFields, ', ');

            // Create FROM
            unset($createViewQueryFrom);
            if (isset($fieldList['custom'])) {
                $createViewQueryFrom = " FROM  {$tableName} m LEFT JOIN {$tableName}_cstm c ON m.id=c.id_c ";
            } else {
                $createViewQueryFrom = " FROM  {$tableName} AS m ";
            }

            // Create left joins
            $createViewQueryLeftJoins = " {$leftJoins} ";

            // Create WHERE
            $createViewQueryWhere = " WHERE m.deleted = 0 ";

            // Create Group By
            $createViewQueryGroupBy = " GROUP BY m.id ";

            // Create the SQL instruction with the pieces created above for main module view
            $createViewQuery[] = "{$createViewQueryHeader}  {$createViewQueryFields} {$createViewQueryFrom} {$createViewQueryLeftJoins} {$createViewQueryWhere} {$createViewQueryGroupBy}";

            // Process autorelationships
            if (!empty($autoRelationships)) {
                foreach ($autoRelationships as $key => $value) {
                    // Set mode for autorelationships (table or view) according to the module settings
                    if (
                        in_array($moduleName, $this->sdaSettings['publishAsTable'])
                        || (!empty($this->sdaSettings['publishAsTable'][0]) && $this->sdaSettings['publishAsTable'][0] == '1')
                    ) {
                        $mode = 'TABLE';
                    } else {
                        $mode = 'VIEW';
                    }

                    // Create the SQL instruction with some modifications for each autorelationship view
                    $createViewQuery[] = "CREATE OR REPLACE {$mode} {$viewName}_{$key} AS
                    SELECT   {$createViewQueryFields} {$value['parentIdfieldSrc']}
                    {$createViewQueryFrom}
                    {$createViewQueryLeftJoins} {$value['innerJoin']}
                    {$createViewQueryWhere}";

                }

                // Add sda_def_columns metadata record for auto relationships
                $this->cloneColumnRecordsForAutorelationships($autoRelationships);
            }

            foreach ($createViewQuery as $query) {
                // Execute the query
                if (!$db->query($query)) {
                    $lastSQLError = array_pop(explode(':', $db->last_error));

                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error has occurred: [{$lastSQLError}] running Query: [{$query}]");

                    $this->info .= "<div class='error' style='color:red;'>ERROR: <textarea style='width:100%;height:300px;border:1px solid red;'> {$query} </textarea>({$lastSQLError})</div>";
                    $this->info .= "[FATAL: Unable to create view $viewName]";

                } else {
                    $this->info .= '<div style="color:green;">OK: <textarea style="width:100%;height:300px;border:1px solid green;">' . $query . '</textarea>  </div>';
                    $this->info .= '<div style="font-size:80%"><b>Listas creadas:</b> ' . join(' | ', array_unique($listNames)) . '</div>';
                };
            }
            $this->info .= "<h2>Base fields</h2>";
            $this->info .= print_r($fieldList['base'] ?? '', true);
            $this->info .= "<h2>Custom fields</h2>";
            $this->info .= print_r($fieldList['custom'] ?? '', true);
            $this->info .= "<h2>Virtual Fields</h2>";
            $this->info .= print_r($fieldList['virtual'] ?? '', true);

            $this->info .= "</div>";
            $isTable = $tableMode == 'table' ? ' <b style=color:orange>[Table]</b> ' : ' <b style=color:green>[View]</b> ';
            $moduleTotalTime = round(microtime(true) - $moduleStart, 2);
            $this->info .= "<script>document.querySelectorAll('[module={$moduleName}] a').forEach(function(element) {
            element.textContent += ' ({$moduleTotalTime} s.)';
            element.innerHTML += '{$isTable}';
            element.style.color='blue';
            });</script>";
        }

        // Get & populate users ACL metadata (must run after $modulesList is created)
        $this->getAndSaveUserACL($modulesList);

        // We create the views Join Multienum, right now that we already have all the views and the complete metadata table.
        $this->createMultiEnumJoinViews();

        // Clone the permissions records for autorelationships
        $this->clonePermissionRecordsForAutorelationships();

        $this->checkSdaColumns();
        $this->checkSdaTablesInViews();

        if ($callUpdateModel) {
            $this->updateModelCall();
        }

        $this->info .= '<script>
        // select all li elements with the attribute module
        var liElements = document.querySelectorAll("li[module]");

        // Recorre todos los elementos li seleccionados
        for (var i = 0; i < liElements.length; i++) {
          var li = liElements[i];

          // Obtiene el valor del atributo module
          var moduleValue = li.getAttribute("module");

          // Busca el div con id igual al valor de module
          var targetDiv = document.getElementById(moduleValue);


          // Si se encuentra el div
          if (targetDiv) {
            // Comprueba si el div contiene un div con la clase error
            var errorDivs = targetDiv.getElementsByClassName("error");

            // Selecciona el elemento a dentro del li
            var aElement = li.querySelector("a");

            // Si hay elementos con la clase error dentro del div
            if (errorDivs.length > 0) {
              // Aplica los estilos al elemento a
              aElement.style.backgroundColor = "red";
              aElement.style.color = "white";
            }

            // Agrega el evento de clic al elemento li
            li.addEventListener("click", function () {
              var targetId = this.getAttribute("module");
              var targetDiv = document.getElementById(targetId);
              if (targetDiv.style.display === "none") {
                targetDiv.style.display = "block";
              } else {
                targetDiv.style.display = "none";
              }

              // Obtiene el primer campo de entrada dentro del div
              var inputField = targetDiv.querySelector("input");

              // Si se encuentra el campo de entrada, le da el foco
              if (inputField) {
                inputField.focus();
              }
            });
          }
        }
        </script>';

        $endTime = microtime(true);
        $totalTime = round($endTime - $startTime, 2);
        $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "SinergiaDA rebuild script finished in {$totalTime} seconds.");
        $this->info .= "<b>Rebuild ejecutado en {$totalTime} segundos. </b>";

        //Adding config values
        $this->addMetadataRecord('sda_def_config', ['key' => 'rebuild_file_date', 'value' => $fechaFormateada]);
        $this->addMetadataRecord('sda_def_config', ['key' => 'rebuild_duration', 'value' => $totalTime]);

        // Add other config values that do not depend on the variables of this function
        $this->addConfigValues();

        // The text output is shown only if REQUEST print_debug exists
        if (isset($_REQUEST['print_debug'])) {
            echo $this->info;
            die();
        }

        return $this->info;

    }

    /**
     * Updates the model from SinergiaDA instance fetching content from a specified URL.
     *
     * @return void
     */
    private function updateModelCall()
    {
        global $sugar_config;

        $this->info .= '<div style="background-color:#eee;padding:5px;font-style:helvetica, arial;"><h2>Update Model Call</h2>';

        $seedString = $sugar_config['stic_sinergiada']['seed_string'] ?? '';

        $token = gmdate('Y') . $seedString . intVal(gmdate('d')) . intVal(gmdate('H'));

        $this->info .= "<li>Token source: $token";
        $token = md5($token);
        $this->info .= "<li>Token md5: $token";

        // Builds the URL to be called to execute the updateModel method in SinergiaDA,
        // depending on whether a specific URL has been indicated or if a standard location will be used.
        if ($sugar_config['stic_sinergiada_public']['url'] ?? null) {
            $url = "{$sugar_config['stic_sinergiada_public']['url']}/edapi/updatemodel/update?tks=$token";
        } else {
            $url = "https://{$this->baseHostname}.sinergiada.org/edapi/updatemodel/update?tks=$token";
        }

        $link = "<a href='$url' target='_blank'>$url</a>";
        $link2 = addslashes("Retry <a href='$url' target='_blank'>&#9842;</a>");

        $this->info .= "<li>URL: {$link}";

        // Use curl to get url content
        $ch = curl_init($url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        $contenido = curl_exec($ch);
        curl_close($ch);

        // Check if the content is empty.
        if (!empty($contenido)) {
            // If the content is not empty, display it.
            $this->info .= "<li>Response: <strong>{$contenido}</strong>";

            if (preg_match('/\bstatus\b.*\bok\b/i', $contenido)) {
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . ' The Updatemodel method has returned state OK');
                // break; // Exit the loop if the status is "ok".
            } else {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . ' The Updatemodel method has returned state Error');
            }
        } else {
            // If the content is empty, display an error message.
            $this->info .= "[FATAL: The Updatemodel method has not been executed {$link2}]";
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . 'The Updatemodel method in the SDA instance has not been executed');
        }
        $this->info .= "</div>";

    }

    /**
     * Sanitizes a text by removing unnecessary characters
     * @param string $text The text to be sanitized
     * @return string The sanitized text
     */
    private function sanitizeText($text)
    {
        $text = trim((string) $text, ' _:');
        return $text;
    }

    /**
     * Creates a LEFT JOIN for a relationship field, handling both standard and special cases.
     *
     * This function retrieves information about a relationship based on the field's `link` property.
     * If the relationship uses a join table (`join_table`, `join_key_lhs`, and `join_key_rhs` are defined),
     * it creates a LEFT JOIN based on whether the current module is the left or right side of the relationship.
     * If no join table is used, it checks if the relationship is a one-to-many relationship for the
     * current table and builds the join accordingly. If no suitable relationship is found, it does not return a join.
     * 
     *
     * @param array $field The field array containing information about the current field
     * @param string $tableName The name of the table being processed
     * @param string $tableLabel The label of the other side relationships
     * @param bool $isAutoRelationship Indicates if the relationship is an auto relationship
     *
     * @return array|null Returns an associative array containing:
     *                    - 'field': The field to be used in the SELECT clause.
     *                    - 'leftJoin': The LEFT JOIN SQL clause.
     *                    - 'innerJoinForAutoRelationshipsNSide': (Optional) INNER JOIN clause for
     *                      auto-relationships on the N-side.
     *                    - 'fieldForAutoRelationshipsNSide': (Optional) Field for auto-relationships
     *                      on the N-side.
     *                    Returns null if no valid relationship is found.
     */

    private function createRelateLeftJoin($field, $tableName, $tableLabel, $isAutoRelationship = false)
    {
        $db = DBManagerFactory::getInstance();

        $tableLabel = empty($tableLabel) ? '-' : $tableLabel;
        // **Retrieve relationship information:**
        $rel = $db->fetchOne("select * from relationships where relationship_name='{$field['link']}'");

        // **Check if necessary information is present for standard join:**
        if (!empty($rel['join_table']) && !empty($rel['join_key_lhs']) && !empty($rel['join_key_rhs'])) {
            // Standard join using join table

            // **Determine join side based on current module:**
            if (!empty($rel['lhs_module']) && !empty($field['module']) && $rel['lhs_module'] == $field['module']) {
                // Current module is on the left side

                // Check if the relationship is an auto relationship to compose the target table name
                if ($isAutoRelationship !== true) {
                    // if not an auto relationship
                    $targetTable = "{$this->viewPrefix}_{$field['table']}";
                    $label = "{$field['label']}|{$tableLabel}";

                    // Add metadata record
                    $this->addMetadataRecord(
                        'sda_def_relationships',
                        [
                            'id' => $field['link'],
                            'source_table' => "{$this->viewPrefix}_{$rel['rhs_table']}",
                            'source_column' => $field['id_name'],
                            'target_table' => $targetTable,
                            'target_column' => 'id',
                            'info' => 'link_lhs' . ($isAutoRelationship ? '_auto_relationship' : ''),
                            'label' => $label,
                        ]
                    );

                    return [
                        'field' => "{$rel['join_table']}.{$rel['join_key_lhs']}",
                        'leftJoin' => " LEFT JOIN {$rel['join_table']} ON {$rel['join_table']}.{$rel['join_key_rhs']}=m.id AND {$rel['join_table']}.deleted=0 ",
                    ];
                } else {
                    // if an auto relationship
                    $targetTable = "{$this->viewPrefix}_{$field['table']}_{$field['link']}";
                    $label = "{$tableLabel} ({$field['rLabel']})|{$tableLabel}";
                    $label = "{$tableLabel}|{$tableLabel} ({$field['rLabel']})";

                    // Add metadata record
                    $this->addMetadataRecord(
                        'sda_def_relationships',
                        [
                            'id' => $field['link'],
                            'source_table' => $targetTable,
                            'source_column' => 'parent_id',
                            'target_table' => "{$this->viewPrefix}_{$rel['rhs_table']}",
                            'target_column' => 'id',
                            'info' => 'link_lhs' . ($isAutoRelationship ? '_auto_relationship' : ''),
                            'label' => $label,
                        ]
                    );

                    return [
                        'field' => " NULL ",
                        'leftJoin' => "  ",
                        'innerJoinForAutoRelationshipsNSide' => " INNER JOIN {$rel['join_table']} ON {$rel['join_table']}.{$rel['join_key_lhs']}=m.id AND {$rel['join_table']}.deleted=0 ",
                        'fieldForAutoRelationshipsNSide' => "{$rel['join_table']}.{$rel['join_key_rhs']}",
                    ];

                }

            } elseif (!empty($rel['rhs_module']) && !empty($field['module']) && $rel['rhs_module'] == $field['module']) {
                // Current module is on the right side

                // Add metadata record
                $this->addMetadataRecord(
                    'sda_def_relationships',
                    [
                        'id' => $field['link'],
                        'source_table' => "{$this->viewPrefix}_{$rel['lhs_table']}",
                        'source_column' => $field['id_name'],
                        'target_table' => "{$this->viewPrefix}_{$field['table']}",
                        'target_column' => 'id',
                        'info' => 'link_rhs',
                        'label' => "{$field['label']}|{$tableLabel}",
                    ]
                );

                return [
                    'field' => "{$rel['join_table']}.{$rel['join_key_rhs']} ",
                    'leftJoin' => " LEFT JOIN {$rel['join_table']} ON {$rel['join_table']}.{$rel['join_key_lhs']}=m.id AND {$rel['join_table']}.deleted=0 ",
                ];
            }
        } else {
            // **Handle cases where no join table is used:**

            // Check for one-to-many relationship with the current table
            $sql = "SELECT * FROM relationships WHERE (lhs_table='{$tableName}' OR rhs_table='{$tableName}') AND (lhs_table='{$field['table']}' OR rhs_table='{$field['table']}') AND relationship_type='one-to-many'";
            $rel = $db->fetchOne($sql);

            if ($rel) {
                // One-to-many relationship - use direct join
                $res['field'] = "m.{$field['id_name']}";
                $res['leftJoin'] = " LEFT JOIN {$field['table']} ON {$field['table']}.id=m.{$field['id_name']} AND {$field['table']}.deleted=0 ";

                // Add metadata record
                $this->addMetadataRecord(
                    'sda_def_relationships',
                    [
                        'id' => $field['link'],
                        'source_table' => "{$this->viewPrefix}_{$tableName}",
                        'source_column' => $field['id_name'],
                        'target_table' => "{$this->viewPrefix}_{$field['table']}",
                        'target_column' => 'id',
                        'info' => 'no-join-table-relationship ',
                        'label' => "{$field['label']}|{$tableLabel}",
                    ]
                );

                return $res;

            }

            return;
        }
    }

    /**
     * Retrieves the fields that are visible in the detail view for a given module
     *
     * This function retrieves the fields that are visible in the detail view for a given module by including the relevant metadata files and
     * parsing through the view definition panels. It first checks for the existence of the file "modules/{$moduleName}/metadata/detailviewdefs.php"
     * and then checks for the existence of "custom/modules/{$moduleName}/metadata/detailviewdefs.php". It then iterates through the
     * view definition panels and collects the fields that are visible in the detail view.
     *
     * @param string $moduleName The name of the module for which to retrieve the visible fields
     *
     * @return array An array of the fields that are visible in the detail view
     */
    private function detailViewVisibleFields($moduleName)
    {
        // 1º) Check for the presence of the file "modules/{$moduleName}/metadata/detailviewdefs.php"
        if (file_exists("modules/{$moduleName}/metadata/detailviewdefs.php")) {
            include_once "modules/{$moduleName}/metadata/detailviewdefs.php";
        }

        // 2º) Check for the presence of the file "custom/modules/{$moduleName}/metadata/detailviewdefs.php"
        if (file_exists("custom/modules/{$moduleName}/metadata/detailviewdefs.php")) {
            include_once "custom/modules/{$moduleName}/metadata/detailviewdefs.php";
        }

        $fieldsAvailable = array();
        //Iterate through the view definition panels
        foreach (($viewdefs[$moduleName]['DetailView']['panels'] ?? []) as $panel) {
            foreach ($panel as $row) {
                foreach ($row as $fieldArray) {
                    if (isset($fieldArray['name'])) {
                        //collect the fields that are visible in the detail view
                        $fieldsAvailable[] = $fieldArray['name'];
                    } else {
                        if (!empty($fieldArray)) {
                            $fieldsAvailable[] = $fieldArray;
                        }
                    }
                }
            }
        }

        return $fieldsAvailable;
    }
    /**
     * Common function for insert values in metadata tables.
     *
     * @param String $table Table name
     * @param Array $values Array with keys & values to insert in $table
     * @return void
     */
    private function addMetadataRecord($table, $values)
    {
        $db = DBManagerFactory::getInstance();

        // All values to be inserted in MySQL escape to avoid syntax errors
        foreach ($values as $key => $value) {
            $values[$key] = addslashes($value);
        }

        $columnKeys = '`' . implode('`,`', array_keys($values)) . '`';

        $columnValues = "'" . implode("','", array_values($values)) . "'";

        $insertSQL = "INSERT INTO `{$table}` ";
        $insertSQL .= " ($columnKeys ) ";
        $insertSQL .= " VALUES ($columnValues)";

        if (!$db->query($insertSQL)) {
            die("<div style=color:red;>" . $db->last_error . "</div>");
        };
    }

    /**
     * Reconstruye las vistas que forman parte de los metadatos
     *
     * @return void
     */
    private function resetMetadataViews()
    {
        global $sugar_config;

        $db = DBManagerFactory::getInstance();

        $sqlMetadata = [];
        // 1) eda_def_users
        // All users are added, indicating whether or not they are active, in the "Active" field, to
        // that EDA can handle the user synchronization process
        $sqlMetadata[] = "CREATE or REPLACE VIEW `sda_def_users` AS
                          SELECT
                            id,
                            user_name AS user_name,
                            CONCAT_WS(' ', first_name, last_name) as name,
                            (
                                SELECT
                                    email_address
                                FROM
                                    email_addresses ea
                                    JOIN email_addr_bean_rel eab ON ea.id = eab.email_address_id
                                WHERE
                                    eab.bean_id = u.id
                                    AND primary_address = 1
                                    AND eab.bean_module = 'Users'
                                    AND ea.deleted = 0
                                    AND eab.deleted = 0
                                LIMIT
                                    1
                            ) email,
                            u.user_hash AS password,
                            if(u.status='Active' AND uc.sda_allowed_c=1 ,1,0) as 'active'
                        FROM
                            users u
                            INNER JOIN users_cstm uc on u.id =uc.id_c
                        WHERE
                            deleted = 0
                        AND user_hash IS NOT NULL;";

        // 2) eda_def_groups
        $sqlMetadata[] = "CREATE or REPLACE VIEW `sda_def_groups` AS
                                  SELECT CONCAT('SCRM_',name) as name FROM securitygroups WHERE deleted=0
                                  UNION SELECT 'EDA_ADMIN'
                                  ;";
        // 3) eda_def_users_groups
        $sqlMetadata[] = "CREATE or REPLACE VIEW `sda_def_user_groups` AS
                                -- Select 1: Regular users
                            	SELECT
                                    user_name,
                                    CONCAT('SCRM_', s.name) as name
                                FROM
                                    users u
                                JOIN users_cstm uc ON uc.id_c = u.id
                                JOIN securitygroups_users su ON u.id = su.user_id
                                JOIN securitygroups s ON s.id = su.securitygroup_id
                                WHERE
                                    u.is_admin = 0
                                    AND u.deleted = 0
                                    AND su.deleted=0
                                    AND s.deleted=0
                                    AND uc.sda_allowed_c = 1
                                    AND u.status = 'Active'
                                    AND u.user_hash IS NOT NULL
                                    -- Select 2: Administrator users should always belong to the EDA_ADMIN group.
                            UNION SELECT
                                    user_name,
                                    'EDA_ADMIN'
                                FROM
                                    users u
                                JOIN users_cstm uc ON uc.id_c = u.id
                                WHERE
                                    u.is_admin = 1
                                    AND u.deleted = 0
                                    AND u.status = 'Active'
                                    AND u.user_hash IS NOT NULL
                                AND uc.sda_allowed_c =1;";

        // 4) eda_def_security_group_records

        // Set a switch to determine whether to populate the sda_def_security_group_records view based
        // on the value of $sugar_config['stic_sinergiada']['group_permissions_enabled']
        if (($sugar_config['stic_sinergiada']['group_permissions_enabled'] ?? null) != true) {
            $limitQueryClause = ' limit 0 ';
        } else {
            $limitQueryClause = '';
        }

        $sqlMetadata[] = "CREATE or REPLACE VIEW `sda_def_security_group_records` AS
                            SELECT
                                CONCAT('{$this->viewPrefix}_', LCASE(module)) as `table`,
                                record_id,
                                CONCAT('SCRM_',s.name) as `group`
                            FROM
                                securitygroups_records sr
                                JOIN securitygroups s on sr.securitygroup_id=s.id
                            WHERE sr.deleted=0
                            {$limitQueryClause};
                            ";

        // run sql queries
        foreach ($sqlMetadata as $value) {
            if (!$db->query($value)) {
                die("<div style=color:red;>Error running  {$sqlMetadata} <br>" . $db->last_error . "</div>");
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error has occurred: [{$db->last_error}] running Query: [{$sqlMetadata}]");
            };
        }

        // foreach ($sqlMetadata as $value) {
        //     if (!$db->query($value)) {
        //         die("<div style=color:red;>Error running  {$sqlMetadata} <br>" . $db->last_error . "</div>");
        //     };
        // }
    }

    /**
     * Drop and create metadata tables
     *
     * @return void
     */
    private function resetMetadataTables()
    {
        $db = DBManagerFactory::getInstance();

        $sqlMetadata = [];

        // 1) eda_def_tables
        $sqlMetadata[] = 'CREATE TABLE IF NOT EXISTS `sda_def_tables` (
                            `table` VARCHAR(64) NOT NULL,
                            `label` VARCHAR(100) NOT NULL,
                            `description` VARCHAR(255) NOT NULL,
                            `visible` BIT DEFAULT 1
                        ) ENGINE = InnoDB;';

        // 2) eda_def_columns
        $sqlMetadata[] = 'CREATE TABLE IF NOT EXISTS `sda_def_columns` (
                            `table` VARCHAR(64) NOT NULL,
                            `column` VARCHAR(64) NOT NULL,
                            `type` VARCHAR(25) NOT NULL,
                            `label` VARCHAR(100) NOT NULL,
                            `description` VARCHAR(255) NOT NULL,
                            `decimals` INT NOT NULL,
                            `aggregations` VARCHAR(100) DEFAULT \'none\',
                            `visible` BIT DEFAULT 1,
                            `sda_hidden` INT DEFAULT 0,
                            `stic_type` VARCHAR(20)
                        ) ENGINE = InnoDB;';
        // 3) eda_def_relationships
        $sqlMetadata[] = 'CREATE TABLE IF NOT EXISTS `sda_def_relationships` (
                            `id` VARCHAR(64) NOT NULL,
                            `source_table` VARCHAR(64) NOT NULL,
                            `source_column` VARCHAR(64) NOT NULL,
                            `target_table` VARCHAR(64) NOT NULL,
                            `target_column` VARCHAR(64) NOT NULL,
                            `label` VARCHAR(255) NOT NULL,
                            `info` VARCHAR(255) NOT NULL
                        ) ENGINE = InnoDB;';

        // 4) eda_def_enumerations
        $sqlMetadata[] = 'CREATE TABLE IF NOT EXISTS `sda_def_enumerations` (
                            -- `id` CHAR(64) NOT NULL,
                            `source_table` VARCHAR(64) NOT NULL,
                            `source_column` VARCHAR(64) NOT NULL,
                            `master_table` VARCHAR(64) NOT NULL,
                            `master_id` VARCHAR(64) NOT NULL DEFAULT "code",
                            `master_column` VARCHAR(64) NOT NULL DEFAULT "value",
                            `bridge_table` VARCHAR(64) NOT NULL,
                            `source_bridge` VARCHAR(64) NOT NULL,
                            `target_bridge` VARCHAR(64) NOT NULL,
                            `stic_type` VARCHAR(20) NOT NULL,
                            `info` VARCHAR(255) NOT NULL
                        ) ENGINE = InnoDB;';

        // 5) eda_def_permissions

        // remove old objects if exists

        $sqlMetadata[] = 'CREATE TABLE `sda_def_permissions` (
                            `id` bigint(20) NOT NULL AUTO_INCREMENT,
                            `user_name` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin,
                            `group` varchar(64) CHARACTER SET utf8mb4 COLLATE utf8mb4_bin DEFAULT NULL,
                            `table` varchar(64) NOT NULL,
                            `column` varchar(64) NOT NULL,
                            `global` tinyint(1) NOT NULL,
                            `stic_permission_source` varchar(20) NOT NULL,
                            PRIMARY KEY (`id`),
                            KEY `sda_def_permissions_user_name_IDX` (`user_name`) USING BTREE,
                            KEY `sda_def_permissions_group_IDX` (`group`) USING BTREE,
                            KEY `sda_def_permissions_table_IDX` (`table`) USING BTREE,
                            KEY `sda_def_permissions_stic_permission_source_IDX` (`stic_permission_source`) USING BTREE,
                            KEY `idx_table_user` (`table`, `user_name`),
                            KEY `idx_table_group` (`table`, `group`)
                        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;';

        // 6) sda_def_config
        $sqlMetadata[] = 'CREATE TABLE IF NOT EXISTS `sda_def_config` (
                            `key` VARCHAR(64) NOT NULL,
                            `value` VARCHAR(64) NOT NULL
                        ) ENGINE = InnoDB;';

        foreach ($sqlMetadata as $key => $value) {
            if (!$db->query($value)) {
                die("<div style=color:red;>Error running  {$sqlMetadata} <br>" . $db->last_error . "</div>");
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error has occurred: [{$db->last_error}] running Query: [{$sqlMetadata}]");

            };
        }
    }

    /**
     * Adds configuration values to the sda_def_config table.
     * This function aggregates different types of configuration values,
     * including CRM version, current datetime, and custom settings from $sugar_config['stic_sinergiada'].
     */
    public function addConfigValues()
    {
        global $sugar_config;
        $tmpConfigValues = [];

        // Add the CRM version to the config values
        $tmpConfigValues['sinergiacrm_version'] = $sugar_config['sinergiacrm_version'];

        // Add the current date and time to the config values
        $tmpConfigValues['last_rebuild'] = date('Y-m-d H:i:s');

        // Iterate over 'stic_sinergiada' settings and add them to the config values
        foreach ($sugar_config['stic_sinergiada'] as $key => $value) {
            if (is_array($value)) {
                // If the setting is an array, add each sub-element with a composite key
                foreach ($value as $key2 => $value2) {
                    $tmpConfigValues["sda_{$key}_{$key2}"] = $value2;
                }
            } else {
                // If the setting is not an array, add it directly with a simple key
                $tmpConfigValues["sda_{$key}"] = $value;
            }
        }

        // Add the default language to the config values
        $tmpConfigValues['default_language'] = substr($sugar_config['default_language'], 0, 2);

        // Add each gathered config value to the metadata record
        foreach ($tmpConfigValues as $key => $value) {
            $this->addMetadataRecord('sda_def_config', [
                'key' => $key,
                'value' => $value,
            ]);
        }
    }

    /**
     * Function to create an MariaDB view for a specific $app_list_strings
     * @param string $listName The name of the SuiteCRM list ($app_mod_string)
     * @param string $listViewName then name of the MariaDB view to create.
     * @return mixed The name of the list if the view is successfully created, void otherwise
     */
    private function createEnumView($listName, $listViewName)
    {

        // Get the global variable $app_list_strings
        global $app_list_strings;

        // Get instance of DBManagerFactory
        $db = DBManagerFactory::getInstance();

        // Get the current list or return if not exists
        $currentList = $app_list_strings[$listName] ?? null;
        if (!$currentList) {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "The referenced dropdown list [{$listName}] is not available. Ommited");
            return;
        }

        // Start building the SQL command
        $sqlCommand = "CREATE OR REPLACE VIEW {$this->listViewPrefix}_{$listViewName} AS ";
        $isFirst = false;

        // Loop through the current list
        foreach ($currentList as $key => $value) {
            // If it's the first iteration, add SELECT statement
            if ($isFirst == false) {
                $sqlCommand .= "SELECT '{$key}' as 'code', '{$db->quote($value)}' as 'value' ";
                $isFirst = true;
            }
            // Add UNION SELECT statement for the rest of the iterations
            $sqlCommand .= "UNION SELECT '{$key}', '{$db->quote($value)}' ";
        }

        // Execute the SQL command
        if (!$db->query($sqlCommand)) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error has occurred: [{$db->last_error}] running Query: [{$sqlCommand}]");

        } else {
            return $listViewName;
        };
    }

    /**
     * Function to delete old views from the database
     * @return void
     */
    public function deleteOldViews()
    {
        global $sugar_config;
        // List of prefixes to be deleted
        $prefixesToDelete = ['sda_'];

        // Get instance of DBManagerFactory
        $db = DBManagerFactory::getInstance();
        $counterView = 0;
        $counterTable = 0;

        // Loop through the prefixes
        foreach ($prefixesToDelete as $prefix) {

            // Get all the views with matching prefixes for the current database
            $res = $db->query("SELECT table_name FROM information_schema.views WHERE table_name LIKE '{$prefix}%' AND table_schema='{$sugar_config['dbconfig']['db_name']}'");
            // Loop through the views
            while ($view = $db->fetchByAssoc($res, false)) {
                // Delete the view
                if ($db->query("DROP view {$view['table_name']}")) {
                    $counterView++;
                }
            }

            // Get all the tables & views with matching prefixes
            $res = $db->query("select table_name, table_type from information_schema.tables where table_name like '{$prefix}%'");
            // Loop through the views
            while ($view = $db->fetchByAssoc($res, false)) {
                if ($view['table_type'] == 'VIEW') {
                    // Delete the view
                    if ($db->query("DROP VIEW IF EXISTS {$view['table_name']}")) {
                        $counterTable++;
                    }
                } elseif ($view['table_type'] == 'BASE TABLE') {
                    // Delete the table
                    if ($db->query("DROP TABLE IF EXISTS {$view['table_name']}")) {
                        $counterTable++;
                    }
                }

            }

        }
        $this->info .= "Eliminando {$counterView} vistas y {$counterTable} tablas obsoletas.";
    }

    /**
     * Function to create join views for multi-enum fields in SuiteCRM
     *
     * This function retrieves all multi-enum fields from the 'sda_def_enumerations' table, and for each field,
     * it retrieves the corresponding list of values from the 'master_table' specified in the field's record.
     * For each value in the list, it finds the ids of the records in the 'source_table' that contain the value in the 'source_column'
     * of the multi-enum field, and creates a UNION SELECT statement for each id. These statements are then concatenated
     * and used to create a join view with the name "{$multiField['source_table']}__{$multiField['source_column']}"
     * The function will print if the view was created successfully or if there was an error while creating it.
     *
     * @return bool true if bridge view has created, false if not
     */
    public function createMultiEnumJoinViews()
    {
        $this->info .= "<h2>Creating enum join tables</h2>";

        // Get instance of DBManagerFactory
        $db = DBManagerFactory::getInstance();

        // Get all the multi-enum fields
        $res = $db->query("SELECT * FROM sda_def_enumerations WHERE stic_type='multienum' ");
        while ($multiField = $db->fetchByAssoc($res, false)) {

            $list = $multiField['master_table'];
            $this->info .= "<li>{$list}</li>";

            $unionSelectPiece = array();
            $resList = $db->query("select * from {$list}");

            // Loop through the elements in the list
            while ($listElement = $db->fetchByAssoc($resList, false)) {

                // Skip empty values
                if (empty($db->quote($listElement['value']))) {
                    continue;
                }

                // Add the ids that contain the value of the list
                $unionSelectPiece[] = "SELECT
                 id as id,
                 '{$listElement['code']}' as 'code'
                 FROM
                    {$multiField['source_table']}
                 WHERE ({$multiField['source_column']} LIKE '%^{$listElement['code']}^%' OR {$multiField['source_column']} = '{$listElement['code']}')
                 ";
            }

            $multienumJoinTableSQL = implode(' UNION ', $unionSelectPiece);

            // Create the join view. It's important to use the same method as when processing each individual multienum field
            $multienumBridgeTableName = mb_strcut("{$multiField['source_table']}__{$multiField['source_column']}", 0, 64);

            $sqlCommand = "CREATE OR REPLACE VIEW `$multienumBridgeTableName` AS {$multienumJoinTableSQL}";

            if (!$db->query($sqlCommand)) {

                $this->info .= ("<div style=color:red;>Error al crear la vista {$multienumBridgeTableName}:" . $sqlCommand . '<hr>' . array_pop(explode(':', $db->last_error)) . "</div>");
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error has occurred: [{$db->last_error}] running Query: [{$sqlCommand}]");
                $this->info .= "[FATAL: Unable to create view {$multienumBridgeTableName}]";
            } else {
                $this->info .= ("<div style=color:green;> - OK: {$multienumBridgeTableName}  </div>");
            };
        }
    }

    /**
     * Retrieves and saves Access Control List (ACL) permissions for users in the system.
     *
     * This function processes ACL permissions for users and creates corresponding entries in the
     * permissions metadata table. It handles different types of permissions including:
     * - Security group based permissions
     * - Owner based permissions
     * - Global permissions
     *
     * @param array $modules Array of modules to process permissions for
     * @return void
     * @global array $sugar_config Global SuiteCRM configuration array
     * @throws SQLException If there are database errors during processing
     */
    public function getAndSaveUserACL($modules)
    {
        // Track total processing time
        $startTime = microtime(true);
        $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': Starting ACL processing');

        global $sugar_config;
        $db = DBManagerFactory::getInstance();
        include_once 'modules/ACLActions/ACLAction.php';

        // Define mapping of access levels to their identifier constants
        $aclSourcesList = [
            100 => 'ACL_ALLOW_ADMIN_DEV',
            99 => 'ACL_ALLOW_ADMIN',
            90 => 'ACL_ALLOW_ALL',
            89 => 'ACL_ALLOW_ENABLED',
            80 => 'ACL_ALLOW_GROUP',
            75 => 'ACL_ALLOW_OWNER',
            1 => 'ACL_ALLOW_NORMAL',
            0 => 'ACL_ALLOW_DEFAULT',
        ];

        // Preload user groups if group permissions are enabled for better performance
        $userGroups = [];
        if ($sugar_config['stic_sinergiada']['group_permissions_enabled']) {
            $groupsQuery = "SELECT user_name, name as 'group'
                       FROM sda_def_user_groups";
            $groupsResult = $db->query($groupsQuery);
            while ($group = $db->fetchByAssoc($groupsResult)) {
                $userGroups[$group['user_name']][] = $group['group'];
            }
        }

        // Query to get active non-admin users
        $nonAdminQuery = "SELECT id, user_name, is_admin
                     FROM users
                     JOIN users_cstm ON users.id = users_cstm.id_c
                     WHERE status='Active' AND deleted=0
                     AND sda_allowed_c=1 AND is_admin=0";

        $nonAdminUsers = $db->query($nonAdminQuery);
        $userQueries = [$nonAdminUsers];

        // Process each user query result set
        foreach ($userQueries as $users) {
            while ($user = $db->fetchByAssoc($users, false)) {
                $userStartTime = microtime(true);

                // Process each module's permissions for current user
                foreach ($modules as $moduleName => $moduleData) {
                    // Skip Users module for non-admin users
                    if ($user['is_admin'] == 0 && $moduleName == 'Users') {
                        continue;
                    }

                    $userActions = ACLAction::getUserActions($user['id'], false, $moduleName);
                    $value = $userActions[$moduleName]['module'] ?? $userActions['module'] ?? null;

                    // Only process valid permission configurations
                    if (!empty($value) && $user['is_admin'] == 0 &&
                        $value['access']['aclaccess'] >= 0 &&
                        $value['view']['aclaccess'] >= 0) {

                        $aclSource = $aclSourcesList[$value['view']['aclaccess']];

                        // Handle special module name cases
                        $key = $moduleName == 'ProjectTask' ? 'Project_Task' : $moduleName;
                        $key = $key == 'CampaignLog' ? 'Campaign_Log' : $key;

                        $currentTable = $this->viewPrefix . '_' . strtolower($key);

                        // Process permissions based on access level
                        switch ($value['view']['aclaccess']) {
                            case '80': // Security group permissions
                                if (!empty($userGroups[$user['user_name']])) {
                                    foreach ($userGroups[$user['user_name']] as $group) {
                                        $permissionsBatch[] = [
                                            'user_name' => $user['user_name'],
                                            'group' => $group,
                                            'table' => $currentTable,
                                            'column' => 'id',
                                            'stic_permission_source' => $aclSource,
                                            'global' => 0,
                                        ];
                                    }
                                }
                                break;

                            case '75': // Owner-based permissions
                                $permissionsBatch[] = [
                                    'user_name' => $user['user_name'],
                                    'group' => null,
                                    'table' => $currentTable,
                                    'column' => 'assigned_user_name',
                                    'stic_permission_source' => $aclSource,
                                    'global' => 0,
                                ];
                                break;

                            default: // Global permissions
                                $permissionsBatch[] = [
                                    'user_name' => $user['user_name'],
                                    'group' => null,
                                    'table' => $currentTable,
                                    'column' => 'users_id',
                                    'stic_permission_source' => $aclSource,
                                    'global' => 1,
                                ];
                                break;
                        }
                    }
                }

                // Record processing time for current user
                $userEndTime = microtime(true);
                $userProcessingTime = round($userEndTime - $userStartTime, 4);
            }

            // Insert accumulated permissions in batch
            if (!empty($permissionsBatch)) {
                $this->batchInsertPermissions($permissionsBatch);
                unset($permissionsBatch);
            }
        }

        // Log total execution time
        $endTime = microtime(true);
        $totalProcessingTime = round($endTime - $startTime, 4);

        $GLOBALS['log']->stic('Line ' . __LINE__ . ': ' . __METHOD__ . ': Completed ACL processing for users in ' . $totalProcessingTime . ' seconds');
    }

    /**
     * Performs batch insertions of permission records into the database.
     *
     * This function handles the bulk insertion of permission records into the sda_def_permissions table,
     * implementing batch processing to optimize database performance. It includes:
     * - Validation of record structure
     * - Sanitization of input values
     * - Batch size management to prevent memory issues
     * - Error handling and logging
     *
     * @param array $permissionsBatch Array of permission records to insert. Each record should contain:
     *        - user_name: string
     *        - group: string
     *        - table: string
     *        - column: string
     *        - stic_permission_source: string
     *        - global: string
     * @return void
     * @throws Exception If database errors occur during insertion
     */
    private function batchInsertPermissions($permissionsBatch)
    {
        // Get database instance
        $db = DBManagerFactory::getInstance();

        // Define base SQL for all insertions
        $baseSQL = "INSERT INTO sda_def_permissions
           (user_name, `group`, `table`, `column`, stic_permission_source, `global`)
           VALUES ";

        // Set batch processing parameters
        $subBatchSize = 100000; // Maximum records per batch
        $values = [];
        $count = 0;

        // Process each permission record
        foreach ($permissionsBatch as $record) {
            // Ensure record has all required fields
            if (count($record) !== 6) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Invalid record structure: ' . print_r($record, true));
                continue;
            }

            // Sanitize and quote record values
            $sanitizedValues = [];
            foreach ($record as $value) {
                $sanitizedValues[] = "'" . str_replace("'", "''", $value) . "'";
            }

            // Add formatted record to batch
            $values[] = "(" . implode(",", $sanitizedValues) . ")";
            $count++;

            // Execute insertion when batch limit is reached
            if ($count >= $subBatchSize) {
                $this->executeInsertBatch($db, $baseSQL, $values);

                // Reset batch tracking
                $values = [];
                $count = 0;
            }
        }

        // Process remaining records if any
        if (!empty($values)) {
            $this->executeInsertBatch($db, $baseSQL, $values);
        }
    }

    /**
     * Helper function to execute a batch of INSERT statements.
     *
     * @param object $db Database instance
     * @param string $baseSQL Base SQL statement
     * @param array $values Array of formatted value strings
     * @return void
     */
    private function executeInsertBatch($db, $baseSQL, $values)
    {
        $sql = $baseSQL . implode(',', $values);
        try {
            if (!$db->query($sql)) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': SQL Error: ' . $db->last_error);
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Failed SQL: ' . $sql);
            }
        } catch (Exception $e) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': Insert error: ' . $e->getMessage());
        }
    }

    /**
     * Check the columns in the sda_def_columns table against the columns in the views.
     * Display the columns that do not exist in the views.
     *
     * @return void
     */
    public function checkSdaColumns()
    {
        $this->info .= "<b>SDA_DEF_COLUMNS columns that do not exist in the views</b>";
        // Get an instance of the DBManager
        $db = DBManagerFactory::getInstance();
        // Query to get all the rows from the sda_def_columns table
        $query = "SELECT `table`, `column` FROM sda_def_columns WHERE stic_type != 'virtual'";
        $result = $db->query($query);

        // Loop through each row
        while ($row = $db->fetchByAssoc($result)) {
            // Query to check if the column exists in the view
            $columnExists = $db->getOne("SELECT count(*)
        FROM information_schema.columns
        WHERE table_name = '{$row["table"]}'
        AND column_name='{$row["column"]}'");
            // If the column does not exist, display a message
            if ($columnExists == 0) {
                $this->info .= "[FATAL: Table: " . $row["table"] . " - column: " . $row["column"] . " does not exist]";

            }
        }
    }
    public function checkSdaTables($tableToCheck)
    {
        // Get an instance of the DBManager
        $db = DBManagerFactory::getInstance();
        // Query to get all the rows from the sda_def_columns table
        $query = "SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = DATABASE();";
        $result = $db->query($query);
        while ($row = $db->fetchByAssoc($result)) {
            if ($row['table_name'] == $tableToCheck) {
                return true;
            }
        }
        return false;
    }

/**
 * Checks the tables in the different definition tables against the existing views in the database.
 * Prevents attempts to generate tables that do not exist in the views.
 *
 * @return void
 */
    public function checkSdaTablesInViews()
    {
        $this->info .= "<br><b>SDA_DEF_COLUMNS tables that do not exist in the views</b><br>";

        $db = DBManagerFactory::getInstance();
        $missingTables = "SELECT DISTINCT * FROM (
            SELECT `table`, 'sda_def_columns', 'table' AS column_name  FROM sda_def_columns
            UNION SELECT `table`,'sda_def_tables', 'table' FROM sda_def_tables
            UNION SELECT source_table,'sda_def_enumerations','source_table' FROM sda_def_enumerations
            UNION SELECT master_table,'sda_def_enumerations', 'master_table' FROM sda_def_enumerations
            UNION SELECT `table`, 'sda_def_permissions','table' FROM sda_def_permissions
            UNION SELECT source_table,'sda_def_relationships','source_table' FROM sda_def_relationships
            UNION SELECT target_table,'sda_def_relationships','target_table' FROM sda_def_relationships)
            AS source WHERE (
                    `table` NOT IN ( SELECT table_name FROM information_schema.views WHERE table_schema = DATABASE())
                AND `table` NOT IN ( SELECT table_name FROM information_schema.tables WHERE table_schema = DATABASE())
                );";

        $result = $db->query($missingTables);
        if ($result !== false) {
            if ($result->num_rows > 0) {
                while ($row = $db->fetchByassoc($result)) {
                    $queryDelete = "DELETE FROM {$row['sda_def_columns']} WHERE {$row['column_name']} = '{$row['table']}';";

                    $deleteResult = $db->query($queryDelete);

                    if ($deleteResult !== false) {
                        $this->info .= "<br>Registro {$row['table']} ha sido eliminado de {$row['sda_def_columns']}<br>";
                        $GLOBALS['log']->debug('Línea ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Registro {$row['table']} ha sido eliminado de {$row['sda_def_columns']}");
                    } else {
                        $this->info .= "<br>Error al eliminar el registro {$row['table']} de {$row['sda_def_columns']}.<br>";
                        $GLOBALS['log']->debug('Línea ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error al eliminar el registro {$row['table']} de {$row['sda_def_columns']}");
                    }
                }
            } else {
                $this->info .= "No existen tablas sin relaciones en las vistas.";
            }
        } else {
            $this->info .= "Error al ejecutar la consulta.";
            $GLOBALS['log']->debug('Línea ' . __LINE__ . ': ' . __METHOD__ . ': ' . "Error al ejecutar la consulta: $missingTables");
        }
    }

    /**
     * Clones sda_def_columns metadata records for auto-relationships.
     *
     * This method iterates over the provided auto-relationships and clones the existing metadata columns records
     * from base view for each autorelationship view. It updates the table name in the cloned records
     * to reflect the new relationship.
     *
     * @param array $autoRelationships Array of auto-relationship in the module
     */
    public function cloneColumnRecordsForAutorelationships($autoRelationships)
    {
        $db = DBManagerFactory::getInstance();
        foreach ($autoRelationships as $relationship) {
            $this->info .= "<li>{$relationship['source_table']} -> {$relationship['target_table']}</li>";
            $query = "SELECT * FROM sda_def_columns WHERE `table` = '{$this->viewPrefix}_{$relationship['table']}'";
            $result = $db->query($query);
            while ($row = $db->fetchByAssoc($result)) {
                $row['table'] = "{$this->viewPrefix}_{$relationship['table']}_{$relationship['link']}";
                $this->addMetadataRecord('sda_def_columns', $row);
            }
        }

    }



    /**
     * Clones permission records for auto-relationships.
     *
     * This method iterates through the registered auto-relationships and clones
     * the permission records from the `sda_def_permissions` table for each
     * relationship. The cloned records will have their `table` field updated to
     * include the relationship name and their `id` field set to null.
     *
     * @return void
     */
    public function clonePermissionRecordsForAutorelationships()
    {
        $db = DBManagerFactory::getInstance();
        foreach ($this->autoRelationshipsRegistered as $relationship => $table) {
            $query = "SELECT * FROM sda_def_permissions WHERE `table` = '{$this->viewPrefix}_{$table}'";
            $result = $db->query($query);
            while ($row = $db->fetchByAssoc($result)) {
                $row['table'] = "{$this->viewPrefix}_{$table}_{$relationship}";
                $row['id'] = null;
                $this->addMetadataRecord('sda_def_permissions', $row);
            }
        }
    }

}

/**
 * Checks if a security group or user has access to a specific action in a given module.
 *
 * This function determines whether a security group, identified by its name, or an user, identified by its id has the necessary
 * permissions to perform a specific action in a given module. It looks up the roles associated
 * with the group or user and checks the highest access levels available for those roles.
 *
 * @param string $group_name The name of the security group to check.
 * @param string $userId The id of the user to check.
 * @param string $category The name of the module or category (e.g., 'Accounts', 'Contacts').
 * @param string $action The specific action to check (e.g., 'view', 'edit', 'delete').
 * @param string $type The type of ACL, defaults to 'module'.
 *
 * @return bool Returns true if the group has access, false otherwise.
 *
 * @global object $db SuiteCRM's global database object.
 *
 * @throws SQLException If there's an error in executing the SQL queries.
 */
function groupHasAccess($group_name, $userId, $category, $action, $type = 'module')
{
    global $db;

    // Escape the group name to prevent SQL injection
    $group_name = $db->quote($group_name);

    // Get the roles associated with this security group or user
    $query = "SELECT role_id FROM (
                SELECT role_id FROM securitygroups_acl_roles
                WHERE securitygroup_id IN (SELECT DISTINCT securitygroup_id FROM securitygroups_users sgu WHERE sgu.user_id='$userId' AND sgu.deleted = false)
                UNION SELECT role_id FROM acl_roles_users aru
                WHERE aru.user_id='$userId' AND deleted=false ) m
             LIMIT 1
                ";
    $result = $db->query($query);

    $roles = array();
    while ($row = $db->fetchByAssoc($result)) {
        $roles[] = $row['role_id'];
    }

    if (empty($roles)) {
        return false; // If there are no roles, there's no access
    }

    // Check permissions for these roles
    $roleIds = implode("','", $roles);
    $query = "SELECT acl_actions.*, acl_roles_actions.access_override
              FROM acl_actions
              LEFT JOIN acl_roles_actions ON acl_roles_actions.action_id = acl_actions.id
                  AND acl_roles_actions.role_id IN ('$roleIds')
              WHERE acl_actions.category = '$category'
                AND acl_actions.name = '$action'
                AND acl_actions.acltype = '$type'
                AND acl_actions.deleted = 0";

    $result = $db->query($query);

    $highestAccess = -1;
    while ($row = $db->fetchByAssoc($result)) {
        // Use access_override if set, otherwise use the default aclaccess
        $access = $row['access_override'] ?? $row['aclaccess'];
        $highestAccess = max($highestAccess, $access);
    }

    // Determine if the access is sufficient
    // ACL_ALLOW_GROUP should be defined elsewhere in the system
    return $highestAccess >= ACL_ALLOW_GROUP;
}
