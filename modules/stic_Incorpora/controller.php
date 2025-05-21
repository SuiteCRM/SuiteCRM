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

/**
 * This class is the controller of the stic_Incorpora module. This module gathers all the necessary
 * functions and views to manage the data synchronization with Incorpora remote database, through their
 * Web Services.
 *
 * In order to build the views with the user info and parameteres, this controller uses the variable
 * view_object_map. This variable is redirected to the tpl smarty template, with the name of $MAP,
 * and it can be manipulated as needed
 */
#[\AllowDynamicProperties]
class stic_IncorporaController extends SugarController
{
    private $summary = array(
        'crm_ids' => 0,
        'inc_ids' => 0,
        'no_inc_ids' => 0,
    );
    private $log = array();
    private $option;

    /**
     * This action runs when the user wants to syncronize the data with Incorpora from the ViewList
     * of any Module.
     *
     * It runs a query that returns all the records that where selected by the user in the ViewList. Returning
     * its SugarCRM ID and Incorpora ID, if any. And get the User connection params from the user profile details.
     *
     * Then it sets the current view and the next action.
     *
     * @return void
     */
    public function action_fromMassUpdate()
    {
        global $db;
        $ids = array();

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Syncronization Incorpora action from ListView/MassUpdate ');

        // Retrieving and setting user syncronization params
        $this->setIncorporaUserParams();

        // This may only happen if the URL is introduced manually
        if (!$this->returnModule = $_REQUEST['return_module']) {
            echo "There are missing some URL parameters. Expected 'return_module'.";
            die();
        }
        $bean = BeanFactory::getBean($this->returnModule);
        $moduleTable = $bean->table_name;
        switch ($this->returnModule) {
            case 'stic_Job_Offers':
                $tableQuery = 'FROM ' . $moduleTable;
                $incIdFieldSql = 'inc_id';
                $incIdField = 'inc_id';
                break;

            default:
                $tableQuery = 'FROM ' . $moduleTable . ' JOIN ' . $moduleTable . '_cstm ON id=' . $moduleTable . '_cstm.id_c';
                $incIdFieldSql = $moduleTable . '_cstm.inc_id_c';
                $incIdField = 'inc_id_c';
                break;
        }

        // Building and running the query that retrieves all the record that were selected in ListView
        $sql = "SELECT id, $incIdFieldSql $tableQuery WHERE {$moduleTable}.deleted=0";
        $where = '';
        if (isset($_REQUEST['select_entire_list']) && $_REQUEST['select_entire_list'] == '1' && isset($_REQUEST['current_query_by_page'])) {
            require_once 'include/export_utils.php';
            $retArray = generateSearchWhere($bean->module_name, $_REQUEST['current_query_by_page']);
            $where = '';
            if (!empty($retArray['where'])) {
                $where = " AND " . $retArray['where'];
            }
        } else {
            $ids = explode(',', $_REQUEST['uid']);
            $idList = implode("','", $ids);
            $where = " AND id in ('{$idList}')";
        }
        $sql .= $where;
        $resultado = $db->query($sql);
        unset($ids);
        $ids = array();

        while ($row = $db->fetchByAssoc($resultado)) {
            // Building the Summary count table
            $ids[] = $row['id'];
            $this->summary['crm_ids']++;
            $incIds[$row['id']] = $row[$incIdField];
            if ($row[$incIdField]) {
                $this->summary['inc_ids']++;
            } else {
                $this->summary['no_inc_ids']++;
            }
        }

        // Sending the params that the UI will use
        $this->view_object_map['SUMMARY'] = $this->summary;
        $this->view_object_map['IDS'] = $ids;
        $this->view_object_map['INC_IDS'] = $incIds;

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Syncronization Incorpora action from ListView/MassUpdate finished with Summary: ', $this->summary);

        $this->view = "syncoptions"; //call for the view file in views dir
        $this->mapStepNavigation('results'); //next action to be run
    }

    /**
     * This action runs when the user wants to syncronize data to Incorpora from the DetailView of a record
     *
     * It retrieves from the URL parameters the SugarCRM Id and the Incorpora Id, if any. And get the User
     * connection params from the user profile details.
     *
     * Then it sets the current view and the next action.
     *
     * @return void
     */
    public function action_fromDetailView()
    {
        $ids = array();
        $incIds = array();
        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Syncronization Incorpora action from DetailView ');

        // Retrieving and setting user syncronization params
        $this->setIncorporaUserParams();

        // This may only happen if the URL is introduced manually
        if (!($this->returnModule = $_REQUEST['return_module']) || !($recordId = $_REQUEST['record'])) {
            echo "There are missing some URL parameters. Expected 'record' and 'return_module'.";
            die();
        }
        switch ($this->returnModule) {
            case 'stic_Job_Offers':
                $incIdField = 'inc_id';
                break;
            default:
                $incIdField = 'inc_id_c';
                break;
        }

        // Retrieving the Incorpora ID related to the record, if there is any
        $bean = BeanFactory::getBean($this->returnModule, $recordId);
        $GLOBALS['log']->fatal(__METHOD__ . ' ' . __LINE__ . ' ' . ' Retrived bean: ', $bean->module_name, $bean->id);

        // Building the Summary count table
        $ids[] = $recordId;
        $this->summary['crm_ids']++;
        if ($bean->$incIdField) {
            $incIds[$recordId] = $bean->$incIdField;
            $this->summary['inc_ids']++;
        } else {
            $this->summary['no_inc_ids']++;
        }

        // Sending the params that the UI will use
        $this->view_object_map['SUMMARY'] = $this->summary;
        $this->view_object_map['INC_IDS'] = $incIds;
        $this->view_object_map['IDS'] = $ids;

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Syncronization Incorpora action from DetailView finished with Summary: ', $this->summary);

        $this->view = "syncoptions"; //call for the view file in views dir
        $this->mapStepNavigation('results'); //next action to be run
    }

    /**
     * This action uses the details chosen by the user to syncronize the data with Incorpora.act_item
     *
     * It stablishes connection with the WS of Incorpora and runs the SOAP functions to Retrieve, Set or
     * Send data to the remote database.
     *
     * Finally it uses the results to build the log that will be shown to the User throught the result view.
     *
     * @return void
     */
    public function action_results()
    {
        $connectionParams = array();
        require_once 'modules/stic_Incorpora/utils/Incorpora.php';

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Syncronization Incorpora Results action ');

        // This may only happen if the URL is introduced manually
        if (!$this->returnModule = $_REQUEST['return_module']) {
            echo "There are missing some URL parameters. Expected 'return_module'.";
            die();
        }

        // Retrieving the data from last action
        $this->summary = $_REQUEST['summary'] ?? ['crm_ids' => 0, 'inc_ids' => 0, 'no_inc_ids' => 0];
        $ids = $_REQUEST['ids'] ?? [];
        $inc_ids = $_REQUEST['inc_ids'] ?? [];
        $this->option = $_REQUEST['sync_option'] ?? false;
        $override = $_REQUEST['override'] ?? false;

        // Retrieving user parameters if it's PRO or TEST.
        // For the correct visualization in the UI, we need to 
        if ($test = ($_REQUEST['test'] ?? false)) {
            $this->incorporaUserParams = array(
                'group' => $_REQUEST['reference_group_test_code'],
                'entity' => $_REQUEST['reference_entity_test_code'],
                'officer' => $_REQUEST['reference_officer_test_code'],
            );
            $connectionParams['username'] = $_REQUEST['user_test'];
        } else {
            $this->incorporaUserParams = array(
                'group' => $_REQUEST['reference_group_pro_code'],
                'entity' => $_REQUEST['reference_entity_pro_code'],
                'officer' => $_REQUEST['reference_officer_pro_code'],
            );
            $connectionParams['username'] = $_REQUEST['user_pro'];
        }
        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Incorpora User Connection Params, without password ', $this->incorporaUserParams);
        $connectionParams['password'] = $_REQUEST['password'];

        // Stablishing connection to Incorpora using Incorpora class
        $Incorpora = new Incorpora($this->returnModule);
        if (!$Incorpora->stablishConnection($connectionParams, $test)) {
            $errLog = $Incorpora->getLogMsg();
            $this->log['aut']['msg'] = 'ERROR INCORPORA: ' . $errLog['err'];
            $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR INCORPORA ', $errLog);
            $this->option = false;
            $this->mapStepNavigation();
        }
        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Incorpora connection stablished ');

        switch ($this->option) {
            case 'inc_crm': {
                    $this->getDataIncorpora($Incorpora, $ids, $inc_ids, $override);
                    break;
                }
            case 'crm_inc': {
                    $this->newDataIncorpora($Incorpora, $ids, $inc_ids);
                    break;
                }
            case 'crm_edit_inc': {
                    $this->editDataIncorpora($Incorpora, $ids, $inc_ids);
                    break;
                }
            default: {
                    break;
                }
        }

        $this->mapStepNavigation('');
        $this->view = "results";
    }

    /**
     * First, it retrieves the local data of the selected records. Then, it parses it to adjust to Incorpora
     * fields and format. And it sends that data to the WS, using its Incorpora ID.
     *
     * The results of that synchronization is saved in the logs.
     *
     * @param class $Incorpora It contains the functions to syncronize with Incorpora WS
     * @param array $crmIds
     * @param array $incIds
     * @return void
     */
    protected function editDataIncorpora($Incorpora, $crmIds, $incIds)
    {
        global $mod_strings;
        require_once 'modules/stic_Incorpora/utils/DataParser.php';

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Edit Data Incorpora option ');

        foreach ($crmIds as $beanId) {
            // If there is no incorpora ID, we don't proceed
            if ($incIds[$beanId]) {
                // Retrieving bean to be edit
                $bean = BeanFactory::getBean($this->returnModule, $beanId);

                // Transforming the data to be sent to Incorpora. If there is an error, it will be displayed on the log
                $DataParser = new DataParser($this->returnModule);
                if (!$parsedData = $DataParser->parseDataToIncorpora($bean, $this->incorporaUserParams)) {
                    $logParser = $DataParser->getLogMsg();
                    $this->log['logs'][$bean->id] = $logParser['logs'][$bean->id];
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR WITH EDIT DATA: ', $logParser['logs'][$bean->id]);
                    continue;
                }

                // Calling the Incorpora function to send the data to be edit. If there is an error, it will be displayed on the log
                if (!$Incorpora->editRecordIncorpora($parsedData, $incIds[$beanId])) {
                    $errLog = $Incorpora->getLogMsg();
                    $this->log['aut']['msg'] .= $mod_strings['LBL_RESULTS_INCORPORA_API_ERROR'] . $errLog['err'];
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR INCORPORA EDIT: ', $this->log['aut']['msg']);
                    continue;
                }

                // It copies the log information into the bean
                $logParser = $DataParser->parseResponseIncorpora($Incorpora->getLogMsg(), $bean, $this->option);

                // At this point, it could happen that the user doesn't have permission to edit the record. It will be displayed here.
                if ($logParser['aut']) {
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR INCORPORA RESPONSE:  ', $logParser);
                    $this->log = $logParser;
                    continue;
                }
                $this->log['logs'][$bean->id] = $logParser['logs'][$bean->id];
            } else {
                // No Incorpora ID
                $bean = BeanFactory::getBean($this->returnModule, $beanId);
                $this->log['logs'][$beanId]['msg'] = $mod_strings['LBL_RESULTS_SYNC_RECORD_WITHOUT_INCORPORA_ID'];
                $this->log['logs'][$beanId]['url'] = static::createLinkToDetailView($this->returnModule, $beanId, $bean->name);
                $this->log['logs'][$beanId]['cod'] = '999';
                $this->log['logs'][$beanId]['error'] = true;
                $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . $mod_strings['LBL_RESULTS_SYNC_RECORD_WITHOUT_INCORPORA_ID'], $beanId);
            }
        }
        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' Edit Data Incorpora option finished ', $this->log);
        $this->calculateSummary();
        $this->mapStepNavigation();
    }

    /**
     * First, it retrieves the local data of the selected records. Then, it parses it to adjust to Incorpora
     * fields and format. And it sends that data to the WS.  If the record already exists, it returns its Id.
     * If it doesn't exists and the parameters are correct, it creates the new record and return its new ID.
     *
     * The results of that synchronization is saved in the logs.
     *
     * @param class $Incorpora It contains the functions to syncronize with Incorpora WS
     * @param array $crmIds
     * @param array $incIds
     * @return void
     */
    protected function newDataIncorpora($Incorpora, $crmIds, $incIds)
    {
        global $mod_strings;
        require_once 'modules/stic_Incorpora/utils/DataParser.php';

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' New Record Incorpora option ');

        foreach ($crmIds as $beanId) {
            // In this case, if there is incorpora ID, we don't proceed
            if (!isset($incIds[$beanId]) || !$incIds[$beanId]) {
                // Retrieve record bean to be created
                $bean = BeanFactory::getBean($this->returnModule, $beanId);

                // Transforming the data to be sent to Incorpora. If there is an error, it will be displayed on the log
                $DataParser = new DataParser($this->returnModule);
                if (!$parsedData = $DataParser->parseDataToIncorpora($bean, $this->incorporaUserParams)) {
                    $logParser = $DataParser->getLogMsg();
                    $this->log['logs'][$bean->id] = $logParser['logs'][$bean->id];
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR WITH NEW DATA: ', $logParser['logs'][$bean->id]);
                    continue;
                }

                // Calling the Incorpora function to send the record to be created. If there is an error, it will be displayed on the log
                if (!$Incorpora->newRecordIncorpora($parsedData)) {
                    $errLog = $Incorpora->getLogMsg();
                    $this->log['aut']['msg'] .= $mod_strings['LBL_RESULTS_INCORPORA_API_ERROR'] . $errLog['err'];
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR INCORPORA NEW: ', $this->log['aut']['msg']);
                    continue;
                }

                // It copies the log information into the bean
                $logParser = $DataParser->parseResponseIncorpora($Incorpora->getLogMsg(), $bean, $this->option);

                // At this point, it could happen that the user doesn't have permission to edit the record. It will be displayed here.
                if (isset($logParser['aut']) && $logParser['aut']) {
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . ' ERROR INCORPORA RESPONSE:  ', $logParser);
                    $this->log = $logParser;
                    continue;
                }
                $this->log['logs'][$bean->id] = $logParser['logs'][$bean->id];
            } else {
                // Record has Incorpora ID
                $bean = BeanFactory::getBean($this->returnModule, $beanId);
                $this->log['logs'][$beanId]['msg'] = $mod_strings['LBL_RESULTS_NEW_RECORD_WITH_INCORPORA_ID'];
                $this->log['logs'][$beanId]['url'] = static::createLinkToDetailView($this->returnModule, $beanId, $bean->name);
                $this->log['logs'][$beanId]['cod'] = '999';
                $this->log['logs'][$beanId]['error'] = true;
                $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ' . $mod_strings['LBL_RESULTS_NEW_RECORD_WITH_INCORPORA_ID'], $beanId);
            }
        }

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' New Record Incorpora option finished ', $this->log);
        $this->calculateSummary();
        $this->mapStepNavigation();
    }

    /**
     * It uses the Incorpora Id to retrieve the data form the Incorpora remote database. Then it parses
     * that data into the local format and fields.
     *
     * If the ID
     *
     * @param [type] $Incorpora
     * @param [type] $crmIds
     * @param [type] $incIds
     * @param [type] $override
     * @return void
     */
    protected function getDataIncorpora($Incorpora, $crmIds, $incIds, $override)
    {
        global $current_user, $timedate, $mod_strings;
        require_once 'modules/stic_Incorpora/utils/DataParser.php';

        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' New Record Incorpora option ');

        foreach ($crmIds as $beanId) {
            // If there is no incorpora ID, we don't proceed
            if (isset($incIds[$beanId]) && $incIds[$beanId]) {
                // Calling the Incorpora function to get the record with the Incorpora ID. If there is an error, it will be displayed on the log
                if (!$incorporaRecord = $Incorpora->getRecordIncorpora($incIds[$beanId], $beanId)) {
                    $errLog = $Incorpora->getLogMsg();
                    $this->log['aut']['msg'] .= $mod_strings['LBL_RESULTS_INCORPORA_API_ERROR'] . $errLog['err'];
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' ERROR INCORPORA GET ', $this->log['aut']['msg']);
                    continue;
                }
                // $this->log['logs'][$beanId] = $Incorpora->getLogMsg();

                // Retrieve the bean where the data will be introduced
                $bean = BeanFactory::getBean($this->returnModule, $beanId);

                // In this case, if there is any response issue getting the issue, it will save only the log information into the bean
                $DataParser = new DataParser($this->returnModule);
                if ($logParser = $DataParser->parseResponseIncorpora($Incorpora->getLogMsg(), $bean, $this->option)) {
                    $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' There is some error with the information recieved ', $logParser);
                    if ($this->log['aut'] = $logParser['aut']) {
                        $GLOBALS['log']->error(__METHOD__ . ' ' . __LINE__ . ' There is some error with the authentication ', $this->log);
                        break;
                    }
                    $this->log['logs'][$bean->id] = $logParser['logs'][$bean->id];
                    $sufix = $bean->field_defs['inc_id_c'] ? '_c' : '';
                    $inc_synchronization_errors = 'inc_synchronization_errors' . $sufix;
                    $inc_synchronization_log = 'inc_synchronization_log' . $sufix;
                    $inc_synchronization_date = 'inc_synchronization_date' . $sufix;
                    $bean->$inc_synchronization_errors = true;
                    $bean->$inc_synchronization_log = $this->log['logs'][$bean->id]['msg'];
                    $syncDate = $timedate->fromDb(gmdate('Y-m-d H:i:s'));
                    $bean->$inc_synchronization_date = $timedate->asUser($syncDate, $current_user);
                    $bean->save();
                    $this->log['logs'][$beanId]['url'] = static::createLinkToDetailView($this->returnModule, $beanId, $bean->name);
                    $this->log['logs'][$beanId]['error'] = true;
                } else {
                    // If successful, save the data into the bean
                    $this->log['logs'][$beanId] = $Incorpora->getLogMsg();
                    $log = $DataParser->parseDataFromIncorpora($incorporaRecord, $bean, $override);
                    $this->log['logs'][$beanId] = array_merge($this->log['logs'][$beanId], $log['logs'][$beanId]);
                    $this->log['logs'][$beanId]['msg'] .= ' - ' .$mod_strings['LBL_RESULTS_GET_RECORD_SUCCESS'];
                }
            } else {
                $bean = BeanFactory::getBean($this->returnModule, $beanId);
                $this->log['logs'][$beanId]['msg'] = $mod_strings['LBL_RESULTS_SYNC_RECORD_WITHOUT_INCORPORA_ID'];
                $this->log['logs'][$beanId]['url'] = static::createLinkToDetailView($this->returnModule, $beanId, $bean->name);
                $this->log['logs'][$beanId]['cod'] = '999';
                $this->log['logs'][$beanId]['error'] = true;
            }
        }
        $GLOBALS['log']->debug(__METHOD__ . ' ' . __LINE__ . ' ' . ' New Record Incorpora option finished  ', $this->log);

        $this->calculateSummary();
        $this->mapStepNavigation();
    }

    /**
     * It prepares the common parameters that are used between views and actions
     *
     * @param varchar $nextStep Next action to call
     * @return void
     */
    protected function mapStepNavigation($nextStep = '')
    {
        $this->view_object_map['URL'] = "index.php";
        $this->view_object_map['ACTION'] = "$nextStep";
        $this->view_object_map['MODULE'] = $this->module;
        $this->view_object_map['RETURN_MODULE'] = $this->returnModule;
        $this->view_object_map['RETURN_ID'] = $this->return_id;
        $this->view_object_map['RETURN_ACTION'] = $this->return_action;
        $this->view_object_map['SUMMARY'] = $this->summary;
        $this->view_object_map['LOG'] = $this->log;
        // $GLOBALS['log']->fatal(__METHOD__.' '.__LINE__.' ', $this->log );
    }

    /**
     * It creates a link to the detail view of a specific record
     *
     * @param string $module
     * @param string $id
     * @param string $text The text that will appear on the link
     * @return void
     */
    protected static function createLinkToDetailView($module, $id, $text)
    {
        global $sugar_config;
        $site_url = rtrim($sugar_config['site_url'], "/"); // Elimina el carácter / si existe, luego se lo incluiremos siempre
        return "<a href=\"{$site_url}/index.php?module={$module}&action=DetailView&record={$id}\">$text</a>";
    }

    /**
     * It calculates how many record failed to syncronized, using the code retrieved by the Incorpora response. If the code is 0, means
     * that the record has been syncronized successfully
     *
     * @return void
     */
    protected function calculateSummary()
    {
        $this->summary['successful'] = 0;
        $this->summary['failed'] = 0;
        foreach ($this->log['logs'] as $row) {
            // Errors may come from Incorpora WS (cod > 0) or from the CRM (error = 1). Both should be considered in totals.
            if (($row['cod'] ?? 0) > 0 || ($row['error'] ?? 0) == 1) {
                $this->summary['failed']++;
            } else {
                $this->summary['successful']++;
            }
        }
    }

    /**
     * Function that retrieves the user preferences to connect to Incorpora WS. These are selected on the Employee profile module.
     *
     * NOTE: This only provides the label of the record, that will be used in the view syncoptions
     *
     * @return void
     */
    protected function setIncorporaUserParams()
    {
        global $current_user, $app_list_strings;
        require_once 'modules/stic_Incorpora/utils/IncorporaCredentials.php';

        // First building PRO details. We need to split the label and the code, in order to be shown properly on the UI.
        $incReferenceEntityGroup = explode("_", $current_user->inc_reference_entity_c);
        $incReferenceEntity = end($incReferenceEntityGroup);
        $this->view_object_map['INCORPORA_CONNECTION_PARAMS'] = array(
            'reference_group' => array(
                'label' => $app_list_strings['stic_incorpora_reference_group_list'][$current_user->inc_reference_group_c],
                'code' => $current_user->inc_reference_group_c,
            ),
            'reference_entity' => array(
                'label' => $app_list_strings['stic_incorpora_reference_entity_list'][$current_user->inc_reference_entity_c],
                'code' => $incReferenceEntity,
            ),
            'reference_officer' => array(
                'label' => $current_user->inc_reference_officer_c,
                'code' => $current_user->inc_reference_officer_c,
            ),
            'user' => array(
                'label' => $current_user->inc_incorpora_user_c,
            ),
        );
        // TEST details
        $testReferenceGroup = $incorporaCredentials['INCORPORA_TEST_USER_REFERENCE_GROUP'];
        $testReferenceEntity = $incorporaCredentials['INCORPORA_TEST_USER_REFERENCE_ENTITY'];
        $testReferenceOfficer = $incorporaCredentials['INCORPORA_TEST_USER_REFERENCE_OFFICER'];
        $testUser = $incorporaCredentials['INCORPORA_TEST_USER'];
        $testPassword = $incorporaCredentials['INCORPORA_TEST_PASSWORD'];
        $this->view_object_map['INCORPORA_TEST_CONNECTION_PARAMS'] = array(
            'reference_group' => array(
                'label' => $app_list_strings['stic_incorpora_reference_group_list'][$testReferenceGroup],
                'code' => $testReferenceGroup,
            ),
            'reference_entity' => array(
                'label' => $app_list_strings['stic_incorpora_reference_entity_list'][$testReferenceGroup . '_' . $testReferenceEntity],
                'code' => $testReferenceEntity,
            ),
            'reference_officer' => array(
                'label' => $testReferenceOfficer,
                'code' => $testReferenceOfficer,
            ),
            'user' => array(
                'label' => $testUser,
            ),
            'password' => array(
                'code' => $testPassword,
            ),
        );

        // We will only display the TEST params if the user is admin or SINERGIACRM
        if (strtolower($current_user->user_name) == 'admin' || strtolower($current_user->user_name) == 'sinergiacrm') {
            $this->view_object_map['IS_STIC_ADMIN'] = true;
        }
    }
}
