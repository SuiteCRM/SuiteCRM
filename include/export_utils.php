<?php
/**
 *
 * SugarCRM Community Edition is a customer relationship management program developed by
 * SugarCRM, Inc. Copyright (C) 2004-2013 SugarCRM Inc.
 *
 * SuiteCRM is an extension to SugarCRM Community Edition developed by SalesAgility Ltd.
 * Copyright (C) 2011 - 2021 SalesAgility Ltd.
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

use SuiteCRM\CleanCSV;

if (!defined('sugarEntry') || !sugarEntry) {
    die('Not A Valid Entry Point');
}

/**
 * gets the system default delimiter or an user-preference based override
 * @return string the delimiter
 */
function getDelimiter()
{
    global $sugar_config;
    global $current_user;

    $delimiter = ','; // default to "comma"
    $userDelimiter = $current_user->getPreference('export_delimiter');
    $delimiter = empty($sugar_config['export_delimiter']) ? $delimiter : $sugar_config['export_delimiter'];
    $delimiter = empty($userDelimiter) ? $delimiter : $userDelimiter;

    return $delimiter;
}

/**
 * Prints the encoded CSV to the output buffer with the right headers for downloading.
 *
 * @param string $csv The CSV, UTF-8 encoded
 * @param string $name The name of the document
 */
function printCSV($csv, $name) {
    global $locale, $sugar_config;

    // Excel correctly detects the CSV encoding for utf8+bom files. utf16(+bom) only works on Excel for Windows,
    // but fails with Excel on macOS.
    if (!empty($sugar_config['export_excel_compatible'])) {
        $charset = 'UTF-8';
        $data = $locale->addBOM($csv, $charset);
    } else {
        $charset = $locale->getExportCharset();
        $data = $locale->translateCharset($csv, 'UTF-8', $charset);
    }

    header("Pragma: cache");
    header("Content-type: text/comma-separated-values; charset=" . $charset);
    header("Content-Disposition: attachment; filename=\"{$name}.csv\"");
    header("Content-transfer-encoding: binary");
    header("Expires: Mon, 26 Jul 1997 05:00:00 GMT");
    header("Last-Modified: " . TimeDate::httpTime());
    header("Cache-Control: post-check=0, pre-check=0", false);
    header("Content-Length: " . mb_strlen($data, '8bit'));

    print $data;
}


/**
 * builds up a delimited string for export
 * @param string type the bean-type to export
 * @param array records an array of records if coming directly from a query
 * @return string delimited string for export
 */
function export($type, $records = null, $members = false, $sample=false)
{
    global $locale;
    global $beanList;
    global $beanFiles;
    global $current_user;
    global $app_strings;
    global $app_list_strings;
    global $timedate;
    global $mod_strings;
    global $current_language;
    global $log;
    global $sugar_config;
    $sampleRecordNum = 5;

    //Array of fields that should not be exported, and are only used for logic
    $remove_from_members = array("ea_deleted", "ear_deleted", "primary_address");
    $focus = 0;

    $db = DBManagerFactory::getInstance();
    if (empty($db)){
        $log->fatal('export: not able to get db instance');
        throw new RuntimeException('Unexpected error. See logs.');
    }

    if (empty($beanList[$db->quote($type)])) {
        $log->security("export: trying to access an invalid module '" . $db->quote($type) . "'");
        throw new RuntimeException('Unexpected error. See logs.');
    }

    $bean = $beanList[$db->quote($type)];

    require_once($beanFiles[$bean]);
    $focus = new $bean;
    $searchFields = array();

    $records = $db->quote($records);
    $recordsArray = [];

    if (!empty($records)) {
        $recordsArray = explode(',', $records);
    }

    if (!empty($recordsArray)) {
        $quotedRecords = [];

        foreach ($recordsArray as $record) {
            $quotedRecords[] = $db->quote($record);
        }

        $records = "'" . implode("','", $quotedRecords) . "'";
        $where = "{$focus->table_name}.id in ($records)";
    } elseif (isset($_REQUEST['all'])) {
        $where = '';
    } else {
        if (!empty($_REQUEST['current_post'])) {
            $ret_array = generateSearchWhere($type, $_REQUEST['current_post']);

            $where = $ret_array['where'];
            $searchFields = $ret_array['searchFields'];
        } else {
            $where = '';
        }
    }
    $order_by = "";
    if ($focus->bean_implements('ACL')) {
        if (!ACLController::checkAccess($focus->module_dir, 'export', true)) {
            ACLController::displayNoAccess();
            sugar_die('');
        }

        $accessWhere = $focus->buildAccessWhere('export');
        if (!empty($accessWhere)) {
            $where .= empty($where) ? $accessWhere : ' AND ' . $accessWhere;
        }
    }
    // Export entire list was broken because the where clause already has "where" in it
    // and when the query is built, it has a "where" as well, so the query was ill-formed.
    // Eliminating the "where" here so that the query can be constructed correctly.
    if ($members == true) {
        $query = $focus->create_export_members_query($records);
    } else {
        $beginWhere = substr(trim($where), 0, 5);
        if ($beginWhere == "where") {
            $where = substr(trim($where), 5, strlen((string) $where));
        }

        $query = $focus->create_export_query($order_by, $where);
    }

    $result = '';
    $populate = false;
    if ($sample) {
        $result = $db->limitQuery($query, 0, $sampleRecordNum, true, $app_strings['ERR_EXPORT_TYPE'].$type.": <BR>.".$query);
        if ($focus->_get_num_rows_in_query($query)<1) {
            $populate = true;
        }
    } else {
        $result = $db->query($query, true, $app_strings['ERR_EXPORT_TYPE'].$type.": <BR>.".$query);
    }


    $fields_array = $db->getFieldsArray($result, true);

    //set up the order on the header row
    $fields_array = get_field_order_mapping($focus->module_dir, $fields_array);



    //set up labels to be used for the header row
    $field_labels = array();
    foreach ($fields_array as $key=>$dbname) {
        //Remove fields that are only used for logic
        if ($members && (in_array($dbname, $remove_from_members))) {
            continue;
        }

        //If labels should not be exportable skip them
        if (isset($focus->field_name_map[$key])  && isset($focus->field_name_map[$key]['exportable'])
            && $focus->field_name_map[$key]['exportable'] === false) {
            continue;
        }

        //default to the db name of label does not exist
        $field_labels[$key] = translateForExport($dbname, $focus);
    }

    $content = '';

    // setup the "header" line with proper delimiters
    $content .= "\"".implode("\"".getDelimiter()."\"", array_values($field_labels))."\"\r\n";
    $pre_id = '';

    if ($populate) {
        //this is a sample request with no data, so create fake datarows
        $content .= returnFakeDataRow($focus, $fields_array, $sampleRecordNum);
    } else {
        $records = array();

        //process retrieved record
        while ($val = $db->fetchByAssoc($result, false)) {
            if ($members) {
                $focus = BeanFactory::getBean($val['related_type']);
            } else { // field order mapping is not applied for member-exports, as they include multiple modules
                //order the values in the record array
                $val = get_field_order_mapping($focus->module_dir, $val);
            }

            $new_arr = array();
            if ($members) {
                if ($pre_id == $val['id']) {
                    continue;
                }
                if ($val['ea_deleted']==1 || $val['ear_deleted']==1) {
                    $val['primary_email_address'] = '';
                }
                unset($val['ea_deleted']);
                unset($val['ear_deleted']);
                unset($val['primary_address']);
            }
            $pre_id = $val['id'];

            foreach ($val as $key => $value) {
                //getting content values depending on their types
                $fieldNameMapKey = $fields_array[$key];

                //Dont export fields that have been explicitly marked not to be exportable
                if (isset($focus->field_name_map[$fieldNameMapKey])  && isset($focus->field_name_map[$fieldNameMapKey]['exportable']) &&
                $focus->field_name_map[$fieldNameMapKey]['exportable'] === false) {
                    continue;
                }

                if (isset($focus->field_name_map[$fieldNameMapKey])  && $focus->field_name_map[$fieldNameMapKey]['type']) {
                    $fieldType = $focus->field_name_map[$fieldNameMapKey]['type'];
                    switch ($fieldType) {
                    //if our value is a currency field, then apply the users locale
                    case 'currency':
                        require_once('modules/Currencies/Currency.php');
                        $value = currency_format_number($value);
                        break;
                    // Fix Issue 9326 - Adding Decimal and Float case to retrieve user-defined decimal separator
                    case 'decimal':
                    case 'float':
                        $user_dec_sep = (!empty($current_user->id) ? $current_user->getPreference('dec_sep') : null);
                        $dec_sep = empty($user_dec_sep) ? $sugar_config['default_decimal_seperator'] : $user_dec_sep;
                        $value = str_replace('.', $dec_sep, (string) $value);
                        break;

                    //if our value is a datetime field, then apply the users locale
                    case 'datetime':
                    case 'datetimecombo':
                        $value = $timedate->to_display_date_time($db->fromConvert($value, 'datetime'));
                        $value = preg_replace('/([pm|PM|am|AM]+)/', ' \1', (string) $value);
                        break;

                    //kbrill Bug #16296
                    case 'date':
                        $value = $timedate->to_display_date($db->fromConvert($value, 'date'), false);
                        break;

                    // Bug 32463 - Properly have multienum field translated into something useful for the client
                    case 'multienum':
            $valueArray = unencodeMultiEnum($value);

                        if (isset($focus->field_name_map[$fields_array[$key]]['options']) && isset($app_list_strings[$focus->field_name_map[$fields_array[$key]]['options']])) {
                            foreach ($valueArray as $multikey => $multivalue) {
                                if (isset($app_list_strings[$focus->field_name_map[$fields_array[$key]]['options']][$multivalue])) {
                                    $valueArray[$multikey] = $app_list_strings[$focus->field_name_map[$fields_array[$key]]['options']][$multivalue];
                                }
                            }
                        }
            $value = implode(",", $valueArray);

                        break;

        // Fix Issue 9153 - Exporting DynamicDropdown fields return keys
        case 'dynamicenum':
        case 'enum':
            if (isset($focus->field_name_map[$fields_array[$key]]['options']) &&
                isset($app_list_strings[$focus->field_name_map[$fields_array[$key]]['options']]) &&
                isset($app_list_strings[$focus->field_name_map[$fields_array[$key]]['options']][$value])
            ) {
                $value = $app_list_strings[$focus->field_name_map[$fields_array[$key]]['options']][$value];
            }

            break;
                }
                }

                // Keep as $key => $value for post-processing
                $cleanCSV = new CleanCSV();
                $new_arr[$key] = preg_replace("/\"/", "\"\"", $cleanCSV->escapeField($value) ?? '');
            }

            // Use Bean ID as key for records
            $records[$pre_id] = $new_arr;
        }

        // Check if we're going to export non-primary emails
        if ($focus->hasEmails() && in_array('email_addresses_non_primary', $fields_array)) {
            // $records keys are bean ids
            $keys = array_keys($records);

            // Split the ids array into chunks of size 100
            $chunks = array_chunk($keys, 100);

            foreach ($chunks as $chunk) {
                // Pick all the non-primary mails for the chunk
                $query =
                    "
                      SELECT eabr.bean_id, ea.email_address
                      FROM email_addr_bean_rel eabr
                      LEFT JOIN email_addresses ea ON ea.id = eabr.email_address_id
                      WHERE eabr.bean_module = '{$focus->module_dir}'
                      AND eabr.primary_address = '0'
                      AND eabr.bean_id IN ('" . implode("', '", $chunk) . "')
                      AND eabr.deleted != '1'
                      ORDER BY eabr.bean_id, eabr.reply_to_address, eabr.primary_address DESC
                    ";

                $result = $db->query($query, true, $app_strings['ERR_EXPORT_TYPE'] . $type . ": <BR>." . $query);

                while ($val = $db->fetchByAssoc($result, false)) {
                    if (empty($records[$val['bean_id']]['email_addresses_non_primary'])) {
                        $records[$val['bean_id']]['email_addresses_non_primary'] = $val['email_address'];
                    } else {
                        // No custom non-primary mail delimeter yet, use semi-colon
                        $records[$val['bean_id']]['email_addresses_non_primary'] .= ';' . $val['email_address'];
                    }
                }
            }
        }

        $customRelateFields = array();
        $selects = array();
        foreach ($records as $record) {
            foreach ($record as $recordKey => $recordValue) {
                if (preg_match('/{relate\s+from=""([^"]+)""\s+to=""([^"]+)""}/', (string) $recordValue, $matches)) {
                    $marker = $matches[0];
                    $relatedValue = '';

                    $splits = explode('.', $matches[1]);
                    $currentModule = $splits[0];
                    $currentField = $splits[1];
                    $currentBean = BeanFactory::getBean($currentModule);
                    $currentTable = $currentBean->table_name;

                    $splits = explode('.', $matches[2]);
                    $relatedModule = $splits[0];
                    $relatedField = $splits[1];
                    $relatedBean = BeanFactory::getBean($relatedModule);
                    $relatedTable = $relatedBean->table_name;

                    $relatedLabel = "$relatedTable.name AS related_label, NULL AS related_label1";
                    if (isset($relatedBean->field_defs['name']['source']) && $relatedBean->field_defs['name']['source'] == 'non-db') {
                        //$relatedLabel = 'NULL AS related_label, NULL AS related_label1';
                        if (
                            !isset($relatedBean->field_defs['first_name']['source']) || $relatedBean->field_defs['first_name']['source'] != 'non-db' &&
                            !isset($relatedBean->field_defs['last_name']['source']) || $relatedBean->field_defs['last_name']['source'] != 'non-db'
                        ) {
                            $relatedLabel = "$relatedTable.last_name AS related_label, $relatedTable.first_name AS related_label1";
                        }
                    }

                    $relatedTableCustomJoin = '';
                    $relatedFieldSelect = "NULL AS related_value";
                    if (!isset($existsTables["{$relatedTable}_cstm"])) {
                        $existsTables["{$relatedTable}_cstm"] = $db->tableExists("{$relatedTable}_cstm");
                    }
                    if ($existsTables["{$relatedTable}_cstm"]) {
                        $relatedTableCustomJoin = "
                        JOIN {$relatedTable}_cstm ON {$relatedTable}_cstm.id_c = {$currentTable}_cstm.$relatedField
                        ";
                        $relatedFieldSelect = "{$currentTable}_cstm.$relatedField AS related_value";
                    }

                    $relatedTableJoin = "LEFT JOIN $relatedTable ON $relatedTable.id = {$currentTable}_cstm.id_c";
                    if (isset($currentBean->field_defs[$relatedField])) {
                        $relatedTableJoin = "LEFT JOIN $relatedTable ON $relatedTable.id = {$currentTable}_cstm.$relatedField";
                    }

                    //-- $relatedTable.id AS related_id,
                    //-- {$currentTable}_cstm.id_c AS current_id_c,
                    //-- {$relatedTable}_cstm.id_c AS related_id_c,
                    $selects[] = "(SELECT $currentTable.id AS current_id,'$currentModule' AS current_module,'$currentField' AS current_field,'$relatedModule' AS related_module,'$relatedField' AS related_field,$relatedFieldSelect,$relatedLabel FROM $currentTable JOIN {$currentTable}_cstm ON {$currentTable}_cstm.id_c=$currentTable.id $relatedTableCustomJoin $relatedTableJoin WHERE $currentTable.id='{$record['id']}')";
                }
            }
        }

        $selects = array_unique($selects);


        // grab custom related fields information

        // query max length optimization, measured by mssql FreeTDS connection too
        $queryMaxLength = 620000;
        $query = '';
        $i = 0;
        $selectsCount = count($selects)-1;
        foreach ($selects as $select) {
            $queryTemp = $query.($i==0 ? $select : " UNION $select");
            if ($i==$selectsCount || strlen($queryTemp) > $queryMaxLength) {
                $result = $db->query($query, 'export error on custom related type: '.$query);
                while ($val = $db->fetchByAssoc($result, false)) {
                    $customRelateFields[$val['current_module']][$val['current_id']][$val['related_module']][$val['related_field']] = trim($val['related_label'].' '.$val['related_label1']);
                }
                $query = $select;
            } else {
                $query = $queryTemp;
            }
            $i++;
        }


        foreach ($records as $record) {
            $line = implode("\"" . getDelimiter() . "\"", $record);
            $line = "\"" . $line;
            $line .= "\"\r\n";
            $line = parseRelateFields($line, $record, $customRelateFields);
            $content .= $line;
        }
    }

    return $content;
}

/**
 * Parse custom related fields
 * @param string $line CSV line
 * @param array $record of current line
 * @return mixed string CSV line
 */
function parseRelateFields($line, $record, $customRelateFields)
{
    while (preg_match('/{relate\s+from=""([^"]+)""\s+to=""([^"]+)""}/', $line, $matches)) {
        $marker = $matches[0];
        $relatedValue = '';

        $splits = explode('.', $matches[1]);
        $currentModule = $splits[0];
        $currentField = $splits[1];

        $splits = explode('.', $matches[2]);
        $relatedModule = $splits[0];
        $relatedField = $splits[1];

        if ($currentModule != $record['related_type']) {
            $GLOBALS['log']->debug('incorrect related type in export');
        } else {
            if (isset($customRelateFields[$currentModule][$record['id']][$relatedModule][$relatedField])) {
                $relatedValue = $customRelateFields[$currentModule][$record['id']][$relatedModule][$relatedField];
            } else {
                $relatedValue = '';
            }
        }

        $line = str_replace($marker, $relatedValue, $line);
    }
    return $line;
}

function generateSearchWhere($module, $query)
{
    $ret_array = [];
    $metafiles = [];
    $where = '';
    //this function is similar with function prepareSearchForm() in view.list.php
    $seed = loadBean($module);
    if (file_exists('modules/'.$module.'/SearchForm.html')) {
        if (file_exists('modules/' . $module . '/metadata/SearchFields.php')) {
            require_once('include/SearchForm/SearchForm.php');
            $searchForm = new SearchForm($module, $seed);
        } elseif (!empty($_SESSION['export_where'])) { //bug 26026, sometimes some module doesn't have a metadata/SearchFields.php, the searchfrom is generated in the ListView.php.
            // Currently, massupdate will not generate the where sql. It will use the sql stored in the SESSION. But this will cause bug 24722, and it cannot be avoided now.
            $where = $_SESSION['export_where'];
            $whereArr = explode(" ", trim($where));
            if ($whereArr[0] === trim('where')) {
                $whereClean = array_shift($whereArr);
            }
            $where = implode(" ", $whereArr);
            //rrs bug: 31329 - previously this was just returning $where, but the problem is the caller of this function
            //expects the results in an array, not just a string. So rather than fixing the caller, I felt it would be best for
            //the function to return the results in a standard format.
            $ret_array['where'] = $where;
            $ret_array['searchFields'] =array();
            return $ret_array;
        } else {
            return;
        }
    } else {
        require_once('include/SearchForm/SearchForm2.php');

        if (file_exists('custom/modules/'.$module.'/metadata/metafiles.php')) {
            require('custom/modules/'.$module.'/metadata/metafiles.php');
        } elseif (file_exists('modules/'.$module.'/metadata/metafiles.php')) {
            require('modules/'.$module.'/metadata/metafiles.php');
        }

        if (file_exists('custom/modules/'.$module.'/metadata/searchdefs.php')) {
            require_once('custom/modules/'.$module.'/metadata/searchdefs.php');
        } elseif (!empty($metafiles[$module]['searchdefs'])) {
            require_once($metafiles[$module]['searchdefs']);
        } elseif (file_exists('modules/'.$module.'/metadata/searchdefs.php')) {
            require_once('modules/'.$module.'/metadata/searchdefs.php');
        }

        //fixing bug #48483: Date Range search on custom date field then export ignores range filter
        // first of all custom folder should be checked
        if (file_exists('custom/modules/'.$module.'/metadata/SearchFields.php')) {
            require_once('custom/modules/'.$module.'/metadata/SearchFields.php');
        } elseif (!empty($metafiles[$module]['searchfields'])) {
            require_once($metafiles[$module]['searchfields']);
        } elseif (file_exists('modules/'.$module.'/metadata/SearchFields.php')) {
            require_once('modules/'.$module.'/metadata/SearchFields.php');
        }
        if (empty($searchdefs) || empty($searchFields)) {
            //for some modules, such as iframe, it has massupdate, but it doesn't have search function, the where sql should be empty.
            return;
        }
        $searchForm = new SearchForm($seed, $module);
        $searchForm->setup($searchdefs, $searchFields, 'SearchFormGeneric.tpl');
    }
    $queryJson = json_decode(html_entity_decode($query), true);
    $searchForm->populateFromArray($queryJson);
    $where_clauses = $searchForm->generateSearchWhere(true, $module);
    if ((is_countable($where_clauses) ? count($where_clauses) : 0) > 0) {
        $where = '('. implode(' ) AND ( ', $where_clauses) . ')';
    }
    $GLOBALS['log']->info("Export Where Clause: {$where}");
    $ret_array['where'] = $where;
    $ret_array['searchFields'] = $searchForm->searchFields;
    return $ret_array;
}
/**
  * calls export method to build up a delimited string and some sample instructional text on how to use this file
  * @param string type the bean-type to export
  * @return string delimited string for export with some tutorial text
  */
     function exportSample($type)
     {
         global $app_strings;

         //first grab the
         $_REQUEST['all']=true;

         //retrieve the export content
         $content = export($type, null, false, true);

         // Add a new row and add details on removing the sample data
         // Our Importer will stop after he gets to the new row, ignoring the text below
         return $content . "\n" . $app_strings['LBL_IMPORT_SAMPLE_FILE_TEXT'];
     }
 //this function will take in the bean and field mapping and return a proper value
 function returnFakeDataRow($focus, $field_array, $rowsToReturn = 5)
 {
     if (empty($focus) || empty($field_array)) {
         return ;
     }

     //include the file that defines $sugar_demodata
     include('install/demoData.en_us.php');

     $person_bean = false;
     if (isset($focus->first_name)) {
         $person_bean = true;
     }

     global $timedate;
     $returnContent = '';
     $counter = 0;
     $new_arr = array();

     //iterate through the record creation process as many times as defined.  Each iteration will create a new row
     while ($counter < $rowsToReturn) {
         $counter++;
         //go through each field and populate with dummy data if possible
         foreach ($field_array as $field_name) {
             if (empty($focus->field_name_map[$field_name]) || empty($focus->field_name_map[$field_name]['type'])) {
                 //type is not set, fill in with empty string and continue;
                 $returnContent .= '"",';
                 continue;
             }
             $field = $focus->field_name_map[$field_name];
             //fill in value according to type
             $type = $field['type'];

             switch ($type) {

                 case "id":
                 case "assigned_user_name":
                     //return new guid string
                    $returnContent .= '"'.create_guid().'",';
                     break;
                 case "int":
                     //return random number`
                    $returnContent .= '"'.mt_rand(0, 4).'",';
                     break;
                 case "name":
                     //return first, last, user name, or random name string
                     if ($field['name'] == 'first_name') {
                         $count = (is_countable($sugar_demodata['first_name_array']) ? count($sugar_demodata['first_name_array']) : 0) - 1;
                         $returnContent .= '"'.$sugar_demodata['last_name_array'][mt_rand(0, $count)].'",';
                     } elseif ($field['name'] == 'last_name') {
                         $count = (is_countable($sugar_demodata['last_name_array']) ? count($sugar_demodata['last_name_array']) : 0) - 1;
                         $returnContent .= '"'.$sugar_demodata['last_name_array'][mt_rand(0, $count)].'",';
                     } elseif ($field['name'] == 'user_name') {
                         $count = (is_countable($sugar_demodata['first_name_array']) ? count($sugar_demodata['first_name_array']) : 0) - 1;
                         $returnContent .= '"'.$sugar_demodata['last_name_array'][mt_rand(0, $count)].'_'.mt_rand(1, 111).'",';
                     } else {
                         //return based on bean
                         if ($focus->module_dir =='Accounts') {
                             $count = (is_countable($sugar_demodata['company_name_array']) ? count($sugar_demodata['company_name_array']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['company_name_array'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='Bugs') {
                             $count = (is_countable($sugar_demodata['bug_seed_names']) ? count($sugar_demodata['bug_seed_names']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['bug_seed_names'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='Notes') {
                             $count = (is_countable($sugar_demodata['note_seed_names_and_Descriptions']) ? count($sugar_demodata['note_seed_names_and_Descriptions']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['note_seed_names_and_Descriptions'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='Calls') {
                             $count = (is_countable($sugar_demodata['call_seed_data_names']) ? count($sugar_demodata['call_seed_data_names']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['call_seed_data_names'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='Tasks') {
                             $count = (is_countable($sugar_demodata['task_seed_data_names']) ? count($sugar_demodata['task_seed_data_names']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['task_seed_data_names'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='Meetings') {
                             $count = (is_countable($sugar_demodata['meeting_seed_data_names']) ? count($sugar_demodata['meeting_seed_data_names']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['meeting_seed_data_names'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='ProductCategories') {
                             $count = (is_countable($sugar_demodata['productcategory_seed_data_names']) ? count($sugar_demodata['productcategory_seed_data_names']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['productcategory_seed_data_names'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='ProductTypes') {
                             $count = (is_countable($sugar_demodata['producttype_seed_data_names']) ? count($sugar_demodata['producttype_seed_data_names']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['producttype_seed_data_names'][mt_rand(0, $count)].'",';
                         } elseif ($focus->module_dir =='ProductTemplates') {
                             $count = (is_countable($sugar_demodata['producttemplate_seed_data']) ? count($sugar_demodata['producttemplate_seed_data']) : 0) - 1;
                             $returnContent .= '"'.$sugar_demodata['producttemplate_seed_data'][mt_rand(0, $count)].'",';
                         } else {
                             $returnContent .= '"Default Name for '.$focus->module_dir.'",';
                         }
                     }
                    break;
                 case "relate":
                     if ($field['name'] == 'team_name') {
                         //apply team names and user_name
                         $teams_count = (is_countable($sugar_demodata['teams']) ? count($sugar_demodata['teams']) : 0) - 1;
                         $users_count = (is_countable($sugar_demodata['users']) ? count($sugar_demodata['users']) : 0) - 1;

                         $returnContent .= '"'.$sugar_demodata['teams'][mt_rand(0, $teams_count)]['name'].','.$sugar_demodata['users'][mt_rand(0, $users_count)]['user_name'].'",';
                     } else {
                         //apply GUID
                         $returnContent .= '"'.create_guid().'",';
                     }
                     break;
                 case "bool":
                     //return 0 or 1
                     $returnContent .= '"'.mt_rand(0, 1).'",';
                     break;

                 case "text":
                     //return random text
                     $returnContent .= '"Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Maecenas porttitor congue massa. Fusce posuere, magna sed pulvinar ultricies, purus lectus malesuada libero, sit amet commodo magna eros quis urna",';
                     break;

                 case "team_list":
                     $teams_count = (is_countable($sugar_demodata['teams']) ? count($sugar_demodata['teams']) : 0) - 1;
                     //give fake team names (East,West,North,South)
                     $returnContent .= '"'.$sugar_demodata['teams'][mt_rand(0, $teams_count)]['name'].'",';
                     break;

                 case "date":
                     //return formatted date
                     $timeStamp = strtotime('now');
                     $value =    date($timedate->dbDayFormat, $timeStamp);
                     $returnContent .= '"'.$timedate->to_display_date_time($value).'",';
                     break;

                 case "datetime":
                 case "datetimecombo":
                     //return formatted date time
                     $timeStamp = strtotime('now');
                     //Start with db date
                     $value =    date($timedate->dbDayFormat.' '.$timedate->dbTimeFormat, $timeStamp);
                     //use timedate to convert to user display format
                     $value = $timedate->to_display_date_time($value);
                     //finally forma the am/pm to have a space so it can be recognized as a date field in excel
                     $value = preg_replace('/([pm|PM|am|AM]+)/', ' \1', (string) $value);
                     $returnContent .= '"'.$value.'",';

                     break;
                case "phone":
                    $value = '('.mt_rand(300, 999).') '.mt_rand(300, 999).'-'.mt_rand(1000, 9999);
                      $returnContent .= '"'.$value.'",';
                     break;
                 case "varchar":
                                     //process varchar for possible values
                                     if ($field['name'] == 'first_name') {
                                         $count = (is_countable($sugar_demodata['first_name_array']) ? count($sugar_demodata['first_name_array']) : 0) - 1;
                                         $returnContent .= '"'.$sugar_demodata['last_name_array'][mt_rand(0, $count)].'",';
                                     } elseif ($field['name'] == 'last_name') {
                                         $count = (is_countable($sugar_demodata['last_name_array']) ? count($sugar_demodata['last_name_array']) : 0) - 1;
                                         $returnContent .= '"'.$sugar_demodata['last_name_array'][mt_rand(0, $count)].'",';
                                     } elseif ($field['name'] == 'user_name') {
                                         $count = (is_countable($sugar_demodata['first_name_array']) ? count($sugar_demodata['first_name_array']) : 0) - 1;
                                         $returnContent .= '"'.$sugar_demodata['last_name_array'][mt_rand(0, $count)].'_'.mt_rand(1, 111).'",';
                                     } elseif ($field['name'] == 'title') {
                                         $count = (is_countable($sugar_demodata['titles']) ? count($sugar_demodata['titles']) : 0) - 1;
                                         $returnContent .= '"'.$sugar_demodata['titles'][mt_rand(0, $count)].'",';
                                     } elseif (strpos((string) $field['name'], 'address_street')>0) {
                                         $count = (is_countable($sugar_demodata['street_address_array']) ? count($sugar_demodata['street_address_array']) : 0) - 1;
                                         $returnContent .= '"'.$sugar_demodata['street_address_array'][mt_rand(0, $count)].'",';
                                     } elseif (strpos((string) $field['name'], 'address_city')>0) {
                                         $count = (is_countable($sugar_demodata['city_array']) ? count($sugar_demodata['city_array']) : 0) - 1;
                                         $returnContent .= '"'.$sugar_demodata['city_array'][mt_rand(0, $count)].'",';
                                     } elseif (strpos((string) $field['name'], 'address_state')>0) {
                                         $state_arr = array('CA','NY','CO','TX','NV');
                                         $count = count($state_arr) - 1;
                                         $returnContent .= '"'.$state_arr[mt_rand(0, $count)].'",';
                                     } elseif (strpos((string) $field['name'], 'address_postalcode')>0) {
                                         $returnContent .= '"'.mt_rand(12345, 99999).'",';
                                     } else {
                                         $returnContent .= '"",';
                                     }
                     break;
                case "url":
                     $returnContent .= '"https://suitecrm.com",';
                     break;

                case "enum":
                    //get the associated enum if available
                    global $app_list_strings;

                    if (isset($focus->field_name_map[$field_name]['type']) && !empty($focus->field_name_map[$field_name]['options'])) {
                        if (!empty($app_list_strings[$focus->field_name_map[$field_name]['options']])) {

                            //get the values into an array
                            $dd_values = $app_list_strings[$focus->field_name_map[$field_name]['options']];
                            $dd_values = array_values($dd_values);

                            //grab the count
                            $count = count($dd_values) - 1;

                            //choose one at random
                            $returnContent .= '"'.$dd_values[mt_rand(0, $count)].'",';
                        } else {
                            //name of enum options array was found but is empty, return blank
                            $returnContent .= '"",';
                        }
                    } else {
                        //name of enum options array was not found on field, return blank
                        $returnContent .= '"",';
                    }
                     break;
                default:
                    //type is not matched, fill in with empty string and continue;
                    $returnContent .= '"",';

             }
         }
         $returnContent .= "\r\n";
     }
     return $returnContent;
 }




 //expects the field name to translate and a bean of the type being translated (to access field map and mod_strings)
 function translateForExport($field_db_name, $focus)
 {
     global $mod_strings,$app_strings;

     if (empty($field_db_name) || empty($focus)) {
         return false;
     }

     //grab the focus module strings
     $temp_mod_strings = $mod_strings;
     global $current_language;
     $mod_strings = return_module_language($current_language, $focus->module_dir);
     $fieldLabel = '';

     //!! first check to see if we are overriding the label for export.
     if (!empty($mod_strings['LBL_EXPORT_'.strtoupper($field_db_name)])) {
         //entry exists which means we are overriding this value for exporting, use this label
         $fieldLabel = $mod_strings['LBL_EXPORT_'.strtoupper($field_db_name)];
     }
     //!! next check to see if we are overriding the label for export on app_strings.
     elseif (!empty($app_strings['LBL_EXPORT_'.strtoupper($field_db_name)])) {
         //entry exists which means we are overriding this value for exporting, use this label
         $fieldLabel = $app_strings['LBL_EXPORT_'.strtoupper($field_db_name)];
     }//check to see if label exists in mapping and in mod strings
     elseif (!empty($focus->field_name_map[$field_db_name]['vname']) && !empty($mod_strings[$focus->field_name_map[$field_db_name]['vname']])) {
         $fieldLabel = $mod_strings[$focus->field_name_map[$field_db_name]['vname']];
     }//check to see if label exists in mapping and in app strings
     elseif (!empty($focus->field_name_map[$field_db_name]['vname']) && !empty($app_strings[$focus->field_name_map[$field_db_name]['vname']])) {
         $fieldLabel = $app_strings[$focus->field_name_map[$field_db_name]['vname']];
     }//field is not in mapping, so check to see if db can be uppercased and found in mod strings
     elseif (!empty($mod_strings['LBL_'.strtoupper($field_db_name)])) {
         $fieldLabel = $mod_strings['LBL_'.strtoupper($field_db_name)];
     }//check to see if db can be uppercased and found in app strings
     elseif (!empty($app_strings['LBL_'.strtoupper($field_db_name)])) {
         $fieldLabel = $app_strings['LBL_'.strtoupper($field_db_name)];
     } else {
         //we could not find the label in mod_strings or app_strings based on either a mapping entry
         //or on the db_name itself or being overwritten, so default to the db name as a last resort
         $fieldLabel = $field_db_name;
     }
     //strip the label of any columns
     $fieldLabel= preg_replace("/([:]|\xEF\xBC\x9A)[\\s]*$/", '', trim($fieldLabel));

     //reset the bean mod_strings back to original import strings
     $mod_strings = $temp_mod_strings;
     return $fieldLabel;
 }

//call this function to return the desired order to display columns for export in.
//if you pass in an array, it will reorder the array and send back to you.  It expects the array
//to have the db names as key values, or as labels
function get_field_order_mapping($name='', $reorderArr = '', $exclude = true)
{

    //define the ordering of fields, note that the key value is what is important, and should be the db field name
    $field_order_array = array();
    $field_order_array['accounts'] = array( 'name'=>'Name', 'id'=>'ID', 'website'=>'Website', 'email_address' =>'Email Address', 'email_addresses_non_primary' => 'Non Primary E-mails', 'phone_office' =>'Office Phone', 'phone_alternate' => 'Alternate Phone', 'phone_fax' => 'Fax', 'billing_address_street' => 'Billing Street', 'billing_address_city' => 'Billing City', 'billing_address_state' => 'Billing State', 'billing_address_postalcode' => 'Billing Postal Code', 'billing_address_country' => 'Billing Country', 'shipping_address_street' => 'Shipping Street', 'shipping_address_city' => 'Shipping City', 'shipping_address_state' => 'Shipping State', 'shipping_address_postalcode' => 'Shipping Postal Code', 'shipping_address_country' => 'Shipping Country', 'description' => 'Description', 'account_type' => 'Type', 'industry' =>'Industry', 'annual_revenue' => 'Annual Revenue', 'employees' => 'Employees', 'sic_code' => 'SIC Code', 'ticker_symbol' => 'Ticker Symbol', 'parent_id' => 'Parent Account ID', 'ownership' =>'Ownership', 'campaign_id' =>'Campaign ID', 'rating' =>'Rating', 'assigned_user_name' =>'Assigned to',  'assigned_user_id' =>'Assigned User ID', 'team_id' =>'Team Id', 'team_name' =>'Teams', 'team_set_id' =>'Team Set ID', 'date_entered' =>'Date Created', 'date_modified' =>'Date Modified', 'modified_user_id' =>'Modified By', 'created_by' =>'Created By', 'deleted' =>'Deleted');
    $field_order_array['contacts'] = array( 'first_name' => 'First Name', 'last_name' => 'Last Name', 'id'=>'ID', 'salutation' => 'Salutation', 'title' => 'Title', 'department' => 'Department', 'account_name' => 'Account Name', 'email_address' => 'Email Address', 'email_addresses_non_primary' => 'Non Primary E-mails for Import', 'phone_mobile' => 'Phone Mobile','phone_work' => 'Phone Work', 'phone_home' => 'Phone Home',  'phone_other' => 'Phone Other','phone_fax' => 'Phone Fax', 'primary_address_street' => 'Primary Address Street', 'primary_address_city' => 'Primary Address City', 'primary_address_state' => 'Primary Address State', 'primary_address_postalcode' => 'Primary Address Postal Code', 'primary_address_country' => 'Primary Address Country', 'alt_address_street' => 'Alternate Address Street', 'alt_address_city' => 'Alternate Address City', 'alt_address_state' => 'Alternate Address State', 'alt_address_postalcode' => 'Alternate Address Postal Code', 'alt_address_country' => 'Alternate Address Country', 'description' => 'Description', 'birthdate' => 'Birthdate', 'lead_source' => 'Lead Source', 'campaign_id' => 'campaign_id', 'do_not_call' => 'Do Not Call', 'portal_name' => 'Portal Name', 'portal_active' => 'Portal Active', 'portal_password' => 'Portal Password', 'portal_app' => 'Portal Application', 'reports_to_id' => 'Reports to ID', 'assistant' => 'Assistant', 'assistant_phone' => 'Assistant Phone', 'picture' => 'Picture', 'assigned_user_name' => 'Assigned User Name', 'assigned_user_id' => 'Assigned User ID', 'team_name' => 'Teams', 'team_id' => 'Team id', 'team_set_id' => 'Team Set ID', 'date_entered' =>'Date Created', 'date_modified' =>'Date Modified', 'modified_user_id' =>'Modified By', 'created_by' =>'Created By', 'deleted' =>'Deleted');
    $field_order_array['leads']    = array( 'first_name' => 'First Name', 'last_name' => 'Last Name', 'id'=>'ID', 'salutation' => 'Salutation', 'title' => 'Title', 'department' => 'Department', 'account_name' => 'Account Name', 'account_description' =>  'Account Description', 'website' =>  'Website', 'email_address' =>  'Email Address', 'email_addresses_non_primary' => 'Non Primary E-mails for Import', 'phone_mobile' =>  'Phone Mobile', 'phone_work' =>  'Phone Work', 'phone_home' =>  'Phone Home', 'phone_other' =>  'Phone Other', 'phone_fax' =>  'Phone Fax', 'primary_address_street' =>  'Primary Address Street', 'primary_address_city' =>  'Primary Address City', 'primary_address_state' =>  'Primary Address State', 'primary_address_postalcode' =>  'Primary Address Postal Code', 'primary_address_country' =>  'Primary Address Country', 'alt_address_street' =>  'Alt Address Street', 'alt_address_city' =>  'Alt Address City', 'alt_address_state' =>  'Alt Address State', 'alt_address_postalcode' =>  'Alt Address Postalcode', 'alt_address_country' =>  'Alt Address Country', 'status' =>  'Status', 'status_description' =>  'Status Description', 'lead_source' =>  'Lead Source', 'lead_source_description' =>  'Lead Source Description', 'description'=>'Description', 'converted' =>  'Converted', 'opportunity_name' =>  'Opportunity Name', 'opportunity_amount' =>  'Opportunity Amount', 'refered_by' =>  'Referred By', 'campaign_id' =>  'campaign_id', 'do_not_call' =>  'Do Not Call', 'portal_name' =>  'Portal Name', 'portal_app' =>  'Portal Application', 'reports_to_id' =>  'Reports To ID', 'assistant' =>  'Assistant', 'assistant_phone' =>  'Assistant Phone', 'birthdate'=>'Birthdate', 'contact_id' =>  'Contact ID', 'account_id' =>  'Account ID', 'opportunity_id' =>  'Opportunity ID',  'assigned_user_name' =>  'Assigned User Name', 'assigned_user_id' =>  'Assigned User ID', 'team_name' =>  'Teams', 'team_id' =>  'Team id', 'team_set_id' =>  'Team Set ID', 'date_entered' =>  'Date Created', 'date_modified' =>  'Date Modified', 'created_by' =>  'Created By ID', 'modified_user_id' =>  'Modified By ID', 'deleted' =>  'Deleted');
    $field_order_array['opportunities'] = array( 'name' => 'Opportunity Name', 'id'=>'ID', 'amount' => 'Opportunity Amount', 'currency_id' => 'Currency', 'date_closed' => 'Expected Close Date', 'sales_stage' => 'Sales Stage', 'probability' => 'Probability (%)', 'next_step' => 'Next Step', 'opportunity_type' => 'Opportunity Type', 'account_name' => 'Account Name', 'description' => 'Description', 'amount_usdollar' => 'Amount', 'lead_source' => 'Lead Source', 'campaign_id' => 'campaign_id', 'assigned_user_name' => 'Assigned User Name', 'assigned_user_id' => 'Assigned User ID', 'team_name' => 'Teams', 'team_id' => 'Team id', 'team_set_id' => 'Team Set ID', 'date_entered' => 'Date Created', 'date_modified' => 'Date Modified', 'created_by' => 'Created By ID', 'modified_user_id' => 'Modified By ID', 'deleted' => 'Deleted');
    $field_order_array['notes'] =         array( 'name' => 'Name', 'id'=>'ID', 'description' => 'Description', 'filename' => 'Attachment', 'parent_type' => 'Parent Type', 'parent_id' => 'Parent ID', 'contact_id' => 'Contact ID', 'portal_flag' => 'Display in Portal?', 'assigned_user_name' =>'Assigned to', 'assigned_user_id' => 'assigned_user_id', 'team_id' => 'Team id', 'team_set_id' => 'Team Set ID', 'date_entered' => 'Date Created', 'date_modified' => 'Date Modified',  'created_by' => 'Created By ID', 'modified_user_id' => 'Modified By ID', 'deleted' => 'Deleted' );
    $field_order_array['bugs'] =   array('bug_number' => 'Bug Number', 'id'=>'ID', 'name' => 'Subject', 'description' => 'Description', 'status' => 'Status', 'type' => 'Type', 'priority' => 'Priority', 'resolution' => 'Resolution', 'work_log' => 'Work Log', 'found_in_release' => 'Found In Release', 'fixed_in_release' => 'Fixed In Release', 'found_in_release_name' => 'Found In Release Name', 'fixed_in_release_name' => 'Fixed In Release', 'product_category' => 'Category', 'source' => 'Source', 'portal_viewable' => 'Portal Viewable', 'system_id' => 'System ID', 'assigned_user_id' => 'Assigned User ID', 'assigned_user_name' => 'Assigned User Name', 'team_name'=>'Teams', 'team_id' => 'Team id', 'team_set_id' => 'Team Set ID', 'date_entered' =>'Date Created', 'date_modified' =>'Date Modified', 'modified_user_id' =>'Modified By', 'created_by' =>'Created By', 'deleted' =>'Deleted');
    $field_order_array['tasks'] =   array( 'name'=>'Subject', 'id'=>'ID', 'description'=>'Description', 'status'=>'Status', 'date_start'=>'Date Start', 'date_due'=>'Date Due','priority'=>'Priority', 'parent_type'=>'Parent Type', 'parent_id'=>'Parent ID', 'contact_id'=>'Contact ID', 'assigned_user_name' =>'Assigned to', 'assigned_user_id'=>'Assigned User ID', 'team_name'=>'Teams', 'team_id'=>'Team id', 'team_set_id'=>'Team Set ID', 'date_entered'=>'Date Created', 'date_modified'=>'Date Modified', 'created_by'=>'Created By ID', 'modified_user_id'=>'Modified By ID', 'deleted'=>'Deleted');
    $field_order_array['calls'] =   array( 'name'=>'Subject', 'id'=>'ID', 'description'=>'Description', 'status'=>'Status', 'direction'=>'Direction', 'date_start'=>'Date', 'date_end'=>'Date End', 'duration_hours'=>'Duration Hours', 'duration_minutes'=>'Duration Minutes', 'reminder_time'=>'Reminder Time', 'parent_type'=>'Parent Type', 'parent_id'=>'Parent ID', 'outlook_id'=>'Outlook ID', 'assigned_user_name' =>'Assigned to', 'assigned_user_id'=>'Assigned User ID', 'team_name'=>'Teams', 'team_id'=>'Team id', 'team_set_id'=>'Team Set ID', 'date_entered'=>'Date Created', 'date_modified'=>'Date Modified', 'created_by'=>'Created By ID', 'modified_user_id'=>'Modified By ID', 'deleted'=>'Deleted');
    $field_order_array['meetings'] =array( 'name'=>'Subject', 'id'=>'ID', 'description'=>'Description', 'status'=>'Status', 'location'=>'Location', 'date_start'=>'Date', 'date_end'=>'Date End', 'duration_hours'=>'Duration Hours', 'duration_minutes'=>'Duration Minutes', 'reminder_time'=>'Reminder Time', 'type'=>'Meeting Type', 'external_id'=>'External ID', 'password'=>'Meeting Password', 'join_url'=>'Join Url', 'host_url'=>'Host Url', 'displayed_url'=>'Displayed Url', 'creator'=>'Meeting Creator', 'parent_type'=>'Related to', 'parent_id'=>'Related to', 'outlook_id'=>'Outlook ID','assigned_user_name' =>'Assigned to','assigned_user_id' => 'Assigned User ID', 'team_name' => 'Teams', 'team_id' => 'Team id', 'team_set_id' => 'Team Set ID', 'date_entered' => 'Date Created', 'date_modified' => 'Date Modified', 'created_by' => 'Created By ID', 'modified_user_id' => 'Modified By ID', 'deleted' => 'Deleted');
    $field_order_array['cases'] =array( 'case_number'=>'Case Number', 'id'=>'ID', 'name'=>'Subject', 'description'=>'Description', 'status'=>'Status', 'type'=>'Type', 'priority'=>'Priority', 'resolution'=>'Resolution', 'work_log'=>'Work Log', 'portal_viewable'=>'Portal Viewable', 'account_name'=>'Account Name', 'account_id'=>'Account ID', 'assigned_user_id'=>'Assigned User ID', 'team_name'=>'Teams', 'team_id'=>'Team id', 'team_set_id'=>'Team Set ID', 'date_entered'=>'Date Created', 'date_modified'=>'Date Modified', 'created_by'=>'Created By ID', 'modified_user_id'=>'Modified By ID', 'deleted'=>'Deleted');
    $field_order_array['prospects'] =array( 'first_name'=>'First Name', 'last_name'=>'Last Name', 'id'=>'ID', 'salutation'=>'Salutation', 'title'=>'Title', 'department'=>'Department', 'account_name'=>'Account Name', 'email_address'=>'Email Address', 'email_addresses_non_primary' => 'Non Primary E-mails for Import', 'phone_mobile' => 'Phone Mobile', 'phone_work' => 'Phone Work', 'phone_home' => 'Phone Home', 'phone_other' => 'Phone Other', 'phone_fax' => 'Phone Fax',  'primary_address_street' => 'Primary Address Street', 'primary_address_city' => 'Primary Address City', 'primary_address_state' => 'Primary Address State', 'primary_address_postalcode' => 'Primary Address Postal Code', 'primary_address_country' => 'Primary Address Country', 'alt_address_street' => 'Alternate Address Street', 'alt_address_city' => 'Alternate Address City', 'alt_address_state' => 'Alternate Address State', 'alt_address_postalcode' => 'Alternate Address Postal Code', 'alt_address_country' => 'Alternate Address Country', 'description' => 'Description', 'birthdate' => 'Birthdate', 'assistant'=>'Assistant', 'assistant_phone'=>'Assistant Phone', 'campaign_id'=>'campaign_id', 'tracker_key'=>'Tracker Key', 'do_not_call'=>'Do Not Call', 'lead_id'=>'Lead Id', 'assigned_user_name'=>'Assigned User Name', 'assigned_user_id'=>'Assigned User ID', 'team_id' =>'Team Id', 'team_name' =>'Teams', 'team_set_id' =>'Team Set ID', 'date_entered' =>'Date Created', 'date_modified' =>'Date Modified', 'modified_user_id' =>'Modified By', 'created_by' =>'Created By', 'deleted' =>'Deleted');

    $fields_to_exclude = array();
    $fields_to_exclude['accounts'] = array('account_name');
    $fields_to_exclude['bugs'] = array('system_id');
    $fields_to_exclude['cases'] = array('system_id', 'modified_by_name', 'modified_by_name_owner', 'modified_by_name_mod', 'created_by_name', 'created_by_name_owner', 'created_by_name_mod', 'assigned_user_name', 'assigned_user_name_owner', 'assigned_user_name_mod', 'team_count', 'team_count_owner', 'team_count_mod', 'team_name_owner', 'team_name_mod', 'account_name_owner', 'account_name_mod', 'modified_user_name',  'modified_user_name_owner', 'modified_user_name_mod');
    $fields_to_exclude['notes'] = array('first_name','last_name', 'file_mime_type','embed_flag');
    $fields_to_exclude['tasks'] = array('date_start_flag', 'date_due_flag');

    //of array is passed in for reordering, process array
    if (!empty($name) && !empty($reorderArr) && is_array($reorderArr)) {

        //make sure reorderArr has values as keys, if not then iterate through and assign the value as the key
        $newReorder = array();
        foreach ($reorderArr as $rk=> $rv) {
            if (is_int($rk)) {
                $newReorder[$rv]=$rv;
            } else {
                $newReorder[$rk]=$rv;
            }
        }

        //if module is not defined, lets default the order to another module of the same type
        //this would apply mostly to custom modules
        if (!isset($field_order_array[strtolower($name)]) && isset($_REQUEST['module'])) {
            $exemptModuleList = array('ProspectLists');
            if (in_array($name, $exemptModuleList)) {
                return $newReorder;
            }

            //get an instance of the bean
            global $beanList;
            global $beanFiles;

            $bean = $beanList[$_REQUEST['module']];
            require_once($beanFiles[$bean]);
            $focus = new $bean;


            //if module is of type person
            if ($focus instanceof Person) {
                $name = 'contacts';
            }
            //if module is of type company
            else {
                if ($focus instanceof Company) {
                    $name = 'accounts';
                }
                //if module is of type Sale
                else {
                    if ($focus instanceof Sale) {
                        $name = 'opportunities';
                    }//if module is of type File
                    else {
                        if ($focus instanceof Issue) {
                            $name = 'bugs';
                        }//all others including type File can use basic
                        else {
                            $name = 'Notes';
                        }
                    }
                }
            }
        }

        //lets iterate through and create a reordered temporary array using
        //the  newly formatted copy of passed in array
        $temp_result_arr = array();
        $lname = strtolower($name);
        if (!empty($field_order_array[$lname])) {
            foreach ($field_order_array[$lname] as $fk=> $fv) {

                //if the value exists as a key in the passed in array, add to temp array and remove from reorder array.
                //Do not force into the temp array as we don't want to violate acl's
                if (array_key_exists($fk, $newReorder)) {
                    $temp_result_arr[$fk] = $newReorder[$fk];
                    unset($newReorder[$fk]);
                }
            }
        }
        //add in all the left over values that were not in our ordered list
        //array_splice($temp_result_arr, count($temp_result_arr), 0, $newReorder);
        foreach ($newReorder as $nrk=>$nrv) {
            $temp_result_arr[$nrk] = $nrv;
        }


        if ($exclude) {
            //Some arrays have values we wish to exclude
            if (isset($fields_to_exclude[$lname])) {
                foreach ($fields_to_exclude[$lname] as $exclude_field) {
                    unset($temp_result_arr[$exclude_field]);
                }
            }
        }

        //return temp ordered list
        return $temp_result_arr;
    }

    //if no array was passed in, pass back either the list of ordered columns by module, or the entireorder array
    if (empty($name)) {
        return $field_order_array;
    } else {
        return $field_order_array[strtolower($name)];
    }
}
