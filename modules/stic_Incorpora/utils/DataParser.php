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
#[\AllowDynamicProperties]
class DataParser
{

    private $listDef = array();

    /**
     * Initialize the class defining the module that will be working on
     *
     * @param [type] $module
     */
    public function __construct($module)
    {
        include 'modules/stic_Incorpora/utils/FieldsDef.php';
        switch ($module) {
            case 'Accounts': {
                    $this->listDef = $accountDef;
                    break;
                }
            case 'Contacts': {
                    $this->listDef = $contactDef;
                    break;
                }
            case 'stic_Job_Offers': {
                    $this->listDef = $offerDef;
                    break;
                }
        }
        $this->module = $module;
        $this->incCRMArray = $this->getIncCRMArray();
        $this->CRMIncArray = $this->getCRMIncArray();
    }

    /**
     * This function is in charge of preparing the data that will be sent to Incorpora
     * 
     * It parses all fields and, depending its properties definition,it applies a diferent solution.
     * 
     * It uses the definitions defined in modules/stic_Incorpora/utils/FieldsDef.php and defined previously in the constat listDef
     * in the constructor
     *
     * @param Object $bean Record to be transformed
     * @param Array $incorporaUserParams Parameters of the user that makes the request
     * @return Array $incData An Array containing the data to be sent to Incorpora
     */
    public function parseDataToIncorpora($bean, $incorporaUserParams)
    {
        $errors = '';
        global $current_user, $timedate, $mod_strings, $app_list_strings;
        $fieldsDef = $this->listDef;
        $incData = array();
        foreach ($fieldsDef as $beanField => $incField) {
            if (!isset($incField['skipAlta']) || !$incField['skipAlta']) {
                switch ($incField[1] ?? null) {
                    case 'location':
                        $incData[$incField[0][0]] = $bean->{$incField[2][0]};
                        $incData[$incField[0][1]] = $bean->{$incField[2][1]};
                        $incData[$incField[0][2]] = $bean->{$incField[2][2]};
                        break;

                    case 'dropdownDependent':
                        $dropdown = explode("_", $bean->$beanField);
                        $incData[$incField[0]] = end($dropdown);
                        if (empty($bean->$beanField)) {
                            $incData[$incField[0]] = 0;
                        }
                        break;

                    case 'multienum': 
                        // Some multienum fields are only accepted with a single value for New or Edit type sync.
                        if (!isset($incField['altaMultienum']) || !$incField['altaMultienum']) {
                            $incData[$incField[0]] = explode(',', str_replace('^', '', $bean->$beanField))[0];
                        } else {
                            $incData[$incField[0]] =str_replace('^', '', $bean->$beanField);
                        }

                        break;

                    // case 'yesno':
                    //     switch ($bean->$beanField) {
                    //         case 'SI': {
                    //                 $incData[$incField[0]] = 1;
                    //                 break;
                    //             }
                    //         case 'NO': {
                    //                 $incData[$incField[0]] = 0;
                    //                 break;
                    //             }
                    //         default: {
                    //                 $incData[$incField[0]] = 0;
                    //                 break;
                    //             }
                    //     }
                    //     break;

                    case 'date':
                        $incData[$incField[0]] = $bean->$beanField;
                        if (empty($bean->$beanField)) {
                            $incData[$incField[0]] = '';
                        }
                        break;
                    case 'transformedListNewEdit':
                    case 'transformedList':
                        include 'modules/stic_Incorpora/utils/TransformedLists.php';
                        if (!empty($bean->$beanField)) {
                            if (isset($incField[2]) && $incField[2]) {
                                $list = $incField[2];
                                $incData[$incField[0]] = array_search($bean->$beanField, ${$list});
                            } else {
                                $errors .= $mod_strings['LBL_RESULTS_DATAPARSER_LIST_NOT_SET'] . $incField[0] . ' / ' . $beanField;
                            }
                        } else {
                            $incData[$incField[0]] = '';
                        }
                        break;
                        // When we create a new record in Incorpora, we automatically assign the reference details (Group, Entity, Officer)
                        // to the bean.
                    case 'group':
                    case 'entity':
                    case 'officer':
                        // First we assign the value into the incorpora field
                        $incData[$incField[0]] = $incorporaUserParams[$incField[1]];
                        // Then we add the reference details into the bean record
                        $sufix = $bean->field_defs['inc_id_c'] ? '_c' : '';
                        $beanField = 'inc_reference_' . $incField[1] . $sufix;
                        if (isset($incField[1]) && $incField[1] === 'entity') {
                            $bean->$beanField = $incorporaUserParams['group'] . '_' . $incorporaUserParams[$incField[1]];
                        } else {
                            $bean->$beanField = $incorporaUserParams[$incField[1]];
                        }
                        break;
                    case 'relation':
                        if ($bean->load_relationship($beanField)) {
                            $relatedBeans = $bean->$beanField->getBeans();
                            $relatedBean = array_pop($relatedBeans);
                            if (!$relatedBean->{$incField[2]}) {
                                $errors .= $mod_strings['LBL_RESULTS_DATAPARSER_RELATED_RECORD_WITHOUT_INCORPORA_ID'] . $incField[0] . ' / ' . translate($relatedBean->field_defs[$incField[2]]['vname'], $relatedBean->module_name) .' (' . $app_list_strings['moduleList'][$relatedBean->module_name] .')';
                            }
                            $incData[$incField[0]] = $relatedBean->{$incField[2]};
                        } else {
                            $incData[$incField[0]] = '';
                        }
                        break;
                    case 'integer':
                        $incData[$incField[0]] = intval($bean->$beanField);
                        break;
                    case 'noData':
                        $incData[$incField[0]] = 0;
                        break;
                    case 'noDataDate':
                        $incData[$incField[0]] = '';
                        break;
                    default:
                        if (empty($bean->$beanField)) {
                            $incData[$incField[0]] = '';
                        } else {
                            $incData[$incField[0]] = $bean->$beanField;
                        }
                        break;
                }
            }
        }
        if (!empty($errors)) {
            $sufix = $bean->field_defs['inc_id_c'] ? '_c' : '';
            $incSynchronizationErrors = 'inc_synchronization_errors' . $sufix;
            $incSynchronizationLog = 'inc_synchronization_log' . $sufix;
            $incSynchronizationDate = 'inc_synchronization_date' . $sufix;

            $syncDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));
            $bean->$incSynchronizationDate = $timedate->asUser($syncDate, $current_user);
            $bean->$incSynchronizationErrors = true;
            $log['logs'][$bean->id]['error'] = true;

            $log['logs'][$bean->id]['msg'] = $errors;
            $bean->$incSynchronizationLog = $log['logs'][$bean->id]['msg'];
            $bean->save();
            $log['logs'][$bean->id]['cod'] = $dataIncorpora->salidaComun->codRespuesta;
            $log['logs'][$bean->id]['url'] = static::createLinkToDetailView($this->module, $bean->id, $bean->name);
            $this->log = $log;
            // $GLOBALS['log']->fatal(__METHOD__.' '.__LINE__.' ', $errors );

            return false;
        } else {
            return $incData;
        }
    }

    /**
     * It parses the data that comes from Incorpora and saves it in the SugarBean
     *
     * @param Object $dataIncorpora Data received from Incorpora
     * @param Object $bean Record where the data will be saved
     * @param boolean $override True if the data from the bean will be overwritten
     * @return Array $log Results of the parsing
     */
    public function parseDataFromIncorpora($dataIncorpora, $bean, $override)
    {
        global $mod_strings, $app_list_strings, $current_user, $timedate;
        $errors = '';
        $log = array();
        $fieldsDef = $this->CRMIncArray;
        // include 'modules/stic_Incorpora/utils/FieldsDef.php';
        foreach ($fieldsDef as $key => $value) {
            if (!isset($value['skipConsulta']) || !$value['skipConsulta']) {
                switch ($value[1] ?? null) {
                    case 'location':
                        if (isset($bean->$key)) {
                            $sufix = $bean->field_defs['inc_id_c'] ? '_c' : '';
                            $sticIncorporaLocationsId = 'stic_incorpora_locations_id' . $sufix;

                            if ($override || empty($bean->$sticIncorporaLocationsId)) {
                                $locationBeanList = BeanFactory::getBean('stic_Incorpora_Locations');

                                if (isset($value[5]) && $value[5] == 'consultaDiferentFields') {
                                    $state = $value[6][0];
                                    $municipality = $value[6][1];
                                    $town = $value[6][2];
                                } else {
                                    $state = $value[0][0];
                                    $municipality = $value[0][1];
                                    $town = $value[0][2];
                                    
                                }
                                $locationState = $dataIncorpora->$state;
                                $locationMunicipality = $dataIncorpora->$municipality;
                                $locationTown = $dataIncorpora->$town;
                                $locationBean = $locationBeanList->retrieve_by_string_fields(
                                    array(
                                        'state_code' => $locationState,
                                        'municipality_code' => $locationMunicipality,
                                        'town_code' => $locationTown,
                                    )
                                );

                                if ($locationBean->id) {
                                    $bean->$sticIncorporaLocationsId = $locationBean->id;
                                } else {
                                    $errors .= $mod_strings['LBL_RESULTS_DATAPARSER_NO_LOCATION_ID'] . ($dataIncorpora->{$value[0]} ?? '');
                                    $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . $mod_strings['LBL_RESULTS_DATAPARSER_NO_LOCATION_ID']);
                                }
                            }
                        }
                        break;

                    case 'dropdownDependent':
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                $bean->$key = strval($bean->{$value[2]}) . "_" . strval($dataIncorpora->{$value[0]} ?? '');
                            }
                        }
                        break;

                    case 'multienum':
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                $bean->$key = '^' . str_replace(",", "^,^", ($dataIncorpora->{$value[0]} ?? '')) . '^';
                            }
                        }
                        break;
                    case 'transformedListConsulta':
                    case 'transformedList':
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                include 'modules/stic_Incorpora/utils/TransformedLists.php';
                                if (isset($value[2]) && $value[2]) {
                                    $list = $value[2];
                                    // STIC Custom 20250401 JBL - Fix Fatal error
                                    // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
                                    // $bean->$key = array_key_exists($dataIncorpora->{$value[0]}, ${$list}) ? ${$list}[$dataIncorpora->{$value[0]}] : ${$list}['**not_listed**'];
                                    $propertyName = $value[0];
                                    try {
                                        $propertyValue = $dataIncorpora->$propertyName;
                                        $bean->$key = array_key_exists($propertyValue, ${$list}) 
                                            ? ${$list}[$propertyValue] 
                                            : ${$list}['**not_listed**'];
                                    } catch (Error $e) {
                                        // Property does not exists: Use not_listed value
                                        $bean->$key = ${$list}['**not_listed**'];
                                    }
                                    // END STIC Custom
                                } else {
                                    $errors .= $mod_strings['LBL_RESULTS_DATAPARSER_LIST_NOT_SET'] . $incField[0] . ' / ' . $beanField;
                                }
                            }
                        }
                        break;
                    case 'relation':
                        if (isset($value[5]) && $value[5] != 'consultaDiferentFields') {
                            $incField = $value[0];
                        } else {
                            $incField = $value[6];
                        }
                        if ($bean->load_relationship($key)) {
                            $relatedBeans = $bean->$key->getBeans();
                            $relatedBean = array_pop($relatedBeans);
                            $sufix = $relatedBean->field_defs['inc_id_c'] ? '_c' : '';
                            $incId = 'inc_id' . $sufix;
                            if (($dataIncorpora->$incField ?? '') != ($relatedBean->$incId ?? '')) {
                                $errors .= $mod_strings['LBL_RESULTS_DATAPARSER_MISMATCH_RELATED_INCORPORA_ID'].'' . $incField . ': [' . $dataIncorpora->$incField . '] -- CRM:  '. translate($relatedBean->field_defs[$incId]['vname'], $relatedBean->module_name) .' (' . $app_list_strings['moduleList'][$relatedBean->module_name] . '): [' . $relatedBean->$incId.']';
                            }
                        } else {
                            $errors .= $mod_strings['LBL_RESULTS_DATAPARSER_MISSING_RELATED_RECORD'] . $incField . ': ' . $dataIncorpora->$incField . ' / '. translate($relatedBean->field_defs[$incId]['vname'], $relatedBean->module_name) .' (' . $app_list_strings['moduleList'][$relatedBean->module_name] . '): N/A';
                        }
                        break;
                    case 'dropdown': // We use this because Incorpora return -1 when empty value for dropdown lists
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                if (($dataIncorpora->{$value[0]} ?? '') == '-1') {
                                    $bean->$key = '';
                                } else {
                                    $bean->$key = $dataIncorpora->{$value[0]} ?? null;
                                }
                            }
                        }
                        break;
                    case 'consultaDiferentFields':
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                $bean->$key = $dataIncorpora->{$value[2]} ?? null;
                            }
                        }
                        break;
                        // TODO: Still not defined how the data is returned from Incorpora WS. Sometimes empresaGrupo, other empresaGrupoReferencia..
                        // case 'entity': 
                        //     if (isset($bean->$key)) {
                        //         if ($override || empty($bean->$key)) {
                        //             switch ($bean->module_name) {
                        //                 case 'Accounts':
                        //                     $bean->$key = $dataIncorpora->
                        //             }
                        //         }
                        //     }
                        //     break;
                    case 'concat':
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                $bean->$key = ($dataIncorpora->{$value[0]} ?? '') .' '. ($dataIncorpora->{$value[2]} ?? '');
                            }
                        }
                        break;
                    case 'overrideZero':
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key) || $bean->$key == 0) {
                                $bean->$key = $dataIncorpora->{$value[0]} ?? null;
                            }
                        }
                        break;
                    case 'noData':
                    case 'noDataDate';
                        // Fields NODATA. Nothing to do
                        break;
                    default:
                        if (isset($bean->$key)) {
                            if ($override || empty($bean->$key)) {
                                $bean->$key = $dataIncorpora->{$value[0]} ?? null;
                            }
                        }
                        break;
                }
            }
        }
        $sufix = $bean->field_defs['inc_id_c'] ? '_c' : '';
        $incSynchronizationErrors = 'inc_synchronization_errors' . $sufix;
        $incSynchronizationLog = 'inc_synchronization_log' . $sufix;
        $incSynchronizationDate = 'inc_synchronization_date' . $sufix;
        $incIncorporaRecord = 'inc_incorpora_record' . $sufix;

        $syncDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));
        $bean->$incSynchronizationDate = $timedate->asUser($syncDate, $current_user);
        $bean->$incSynchronizationErrors = true;

        if (!empty($errors)) {
            $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . 'There were errors in the synchronization', $errors);
            $log['logs'][$bean->id]['msg'] = $errors;
            $bean->$incSynchronizationErrors = true;
            $log['logs'][$bean->id]['error'] = true;
        } else {
            $log['logs'][$bean->id]['msg'] = $dataIncorpora->salidaComun->msgRespuesta;
            $bean->$incSynchronizationErrors = false;
            $bean->$incIncorporaRecord = true;
            $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . 'Sync record successful', $log['logs'][$bean->id]['msg']);

        }
        $bean->$incSynchronizationLog = $log['logs'][$bean->id]['msg'];
        $bean->save();
        $log['logs'][$bean->id]['cod'] = $dataIncorpora->salidaComun->codRespuesta;
        $log['logs'][$bean->id]['url'] = static::createLinkToDetailView($this->module, $bean->id, $bean->name);
        return $log;
    }

    /**
     * Parses the control message sent form Incorpora. This control message doesn't contain information from the record but
     * the information related with the synchronization details
     *
     * @param Object $response The response from Incorpora
     * @param Object $bean The SugarBean that will be saved the synchronization details
     * @param String $syncOption It contains the type of synchronization we are doing
     * @return $log Details of the response
     */
    public function parseResponseIncorpora($response, $bean, $syncOption)
    {
        global $current_user, $timedate, $mod_strings;

        $sufix = $bean->field_defs['inc_id_c'] ? '_c' : '';
        $incId = 'inc_id' . $sufix;
        $incSynchronizationErrors = 'inc_synchronization_errors' . $sufix;
        $incSynchronizationLog = 'inc_synchronization_log' . $sufix;
        $incSynchronizationDate = 'inc_synchronization_date' . $sufix;
        $incIncorporaRecord = 'inc_incorpora_record' . $sufix;

        $log = array();
        if (isset($response['type']) && $response['type'] === 'AUT') {
            $log['aut'] = $response;
            return $log;
        }
        switch ($syncOption) {

            case 'crm_edit_inc':
            case 'crm_inc': {
                    switch ($response['cod']) {
                        case 0:
                            $incNewId = $response['new_id'];
                            $log['logs'][$bean->id]['msg'] = $response['msg'];
                            $bean->$incId = $incNewId;
                            $bean->$incSynchronizationErrors = false;
                            $bean->$incIncorporaRecord = true;
                            break;
                        case 1:
                            $incCod = $response['cod'];
                            $bean->$incSynchronizationErrors = true;
                            $log['logs'][$bean->id]['error'] = true;
                            $log['logs'][$bean->id]['msg'] = $response['msg'] . " - ". $mod_strings['LBL_RESULTS_DATAPARSER_INCORPORA_ERROR_NOT_DEFINED'];
                        break;

                        case 218:
                            $log['logs'][$bean->id]['msg'] = $response['msg'] .' - '. $mod_strings['LBL_RESULTS_NEW_RECORD_ALREADY_EXISTS'];
                            $bean->$incSynchronizationErrors = false;
                            $bean->$incIncorporaRecord = true;
                            $incNewId = $this->extractField($response['msg']);
                            $bean->$incId = $incNewId;
                            break;

                        default:
                            $incCod = $response['cod'];
                            $bean->$incSynchronizationErrors = true;
                            $log['logs'][$bean->id]['error'] = true;

                            if ($incCod > 199 && $incCod < 300) {
                                $incFieldValue = $this->extractField($response['msg']);
                                $incField = explode(':', $incFieldValue);
                                if (strpos($incField[0], '/')) {
                                    $incFieldArray = explode('/', $incField[0]);
                                    $crmFieldsLabel = ' - CRM: ';
                                    foreach ($incFieldArray as $incFieldItem) {
                                        $crmField = $this->incCRMArray[trim($incFieldItem)];
                                        $crmFieldsLabel .= ' - '.translate($bean->field_defs[$crmField]['vname'], $this->module) . ' (' . $crmField . ')';
                                    }
                                }
                                else {
                                    $incField = $incField[0];
                                    $crmField = $this->incCRMArray[trim($incField)];
                                    $crmFieldsLabel = ' - CRM: ' . translate($bean->field_defs[$crmField]['vname'], $this->module) . ' (' . $crmField . ')';
                                }
                                $log['logs'][$bean->id]['msg'] = $response['msg'] . $crmFieldsLabel;
                            } else if ($incCod > 99 && $incCod < 200) {
                                $incField = $this->extractField($response['msg']);
                                $incField = preg_replace('/[^\p{L}\p{N}\s]/u', '', $incField);
                                $crmField = isset($this->incCRMArray[trim($incField)]) ? $this->incCRMArray[trim($incField)] : null;

                                $crmFieldsLabel = ' - CRM: ' . translate($bean->field_defs[$crmField]['vname'], $this->module) . ' (' . $crmField . ')';
                                $log['logs'][$bean->id]['msg'] = $response['msg'] . $crmFieldsLabel;
                            } else {
                                $log['logs'][$bean->id]['msg'] = $response['msg'];
                            }
                            break;
                    }

                    $syncDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));
                    $bean->$incSynchronizationDate = $timedate->asUser($syncDate, $current_user);
                    $bean->$incSynchronizationLog = $log['logs'][$bean->id]['msg'];
                    $log['logs'][$bean->id]['url'] = static::createLinkToDetailView($this->module, $bean->id, $bean->name);
                    $log['logs'][$bean->id]['cod'] = $response['cod'];
                    $bean->save();
                    return $log;

                    break;
                }
            case 'inc_crm':
                switch ($response['cod']) {
                    case 0: {
                            return false;
                            break;
                        }
                    default: {
                            $log['logs'][$bean->id] = $response;
                            return $log;
                            break;
                        }
                }
                break;
        }
        return true;
    }

    public function getLogMsg()
    {
        return $this->log;
    }

    protected function getCRMIncArray()
    {
        $var = array();
        $fieldsDef = $this->listDef;
        foreach ($fieldsDef as $key => $value) {
            $var[$key] = $value;
        }
        return $var;
    }

    protected function getIncCRMArray()
    {
        $var = array();
        $fieldsDef = $this->listDef;
        foreach ($fieldsDef as $key => $value) {
            // STIC Custom 20250326 JBL - Avoid Uncaught Type Error
            // https://github.com/SinergiaTIC/SinergiaCRM/pull/315
            if (is_array($value) && isset($value[0]) && !is_array($value[0])) {
            // END STIC Custom
                $var[$value[0]] = $key;
            }
        }        
        return $var;
    }

    protected function extractField($str)
    {
        $start = strpos($str, '[');
        $end = strpos($str, ']', $start + 1);
        $length = $end - $start;
        return substr($str, $start + 1, $length - 1);
    }

    public static function createLinkToDetailView($module, $id, $text)
    {
        global $sugar_config;
        $site_url = rtrim($sugar_config['site_url'], "/"); // Elimina el carácter / si existe, luego se lo incluiremos siempre
        return "<a href=\"{$site_url}/index.php?module={$module}&action=DetailView&record={$id}\">$text</a>";
    }
}
