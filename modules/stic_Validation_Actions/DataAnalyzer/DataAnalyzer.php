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
 * Consistency process controller class
 */
class stic_DataAnalyzer
{
    /**
     * Execute the validation actions
     * @param Objeto  $scheduler of type scheduler
     * @param Array $actions of stic_Validation_Actions $ actions Array of action objects to execute
     * @return Boolean Overall result of the actions. If all are executed correctly returns true, otherwise returns false.
     */
    public function processActions($scheduler, $actions)
    {
        $GLOBALS['scheduler_id'] = $scheduler->id;

        // Overall result of the actions. If all are executed correctly returns true, otherwise returns false.
        $totalResult = true;
        // Array where we will store the validation actions that do not complete successfully
        $failedActions = array();
        // Retrieve the database object
        $db = DBManagerFactory::getInstance();

        // Index the actions by their priority
        $prioritizedActions = array();
        foreach ($actions as $action) {
            $priority = (empty($action->priority) ? 0 : $action->priority);
            if (!isset($prioritizedActions[$priority])) {
                $prioritizedActions[$priority] = array();
            }
            $prioritizedActions[$priority][] = $action;
        }
        // And order them from highest to lowest
        krsort($prioritizedActions, SORT_NUMERIC);

        // Go through the actions linked to the task
        foreach ($prioritizedActions as $priority => $pActions) 
        {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ":Retrieving priority actions [{$priority}] ...");
            foreach ($pActions as $action) 
            {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Executing action [{$action->name}] ...");
                $result = false; // result of the execution of the action. Default error.
                $initTime = microtime(true);
                try {
                    $func = $action->getFunctionObject();
                    $sqlString = $func->prepareSQL($action, $this->proposeSQL($action)); // General the SQL
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": [{$action->name}] Running query [{$sqlString}]...");
                    $dbResult = $db->query($sqlString); // Run the query

                    if ($dbResult === false) {
                        throw new Exception("Database error [" . $db->lastError() . "]");
                    }

                    if (!is_bool($dbResult)) 
                    {   
                        // Create an array with the records to validate
                        $records = array();
                        while ($row = $db->fetchByAssoc($dbResult)) {
                            array_push($records, $row);
                        }

                        // If the result of the query was an object, close it
                        if (is_object($dbResult)) {
                            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Closing query object.");
                            $dbResult->close();
                        }
                                        
                        // Remove stale validation results related to the running validation action
                        $func->removeObsoleteValidationResults($records, $db, $func->selector, $func->id);
                        
                        // Apply validation action
                        $result = $func->doAction($records, $action);
                        if ($result) {
                            // If the action was executed correctly, the date of the last execution is updated
                            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Action [{$action->id} - {$action->name}] has been successfully completed.");
                            global $timedate;
                            $action->last_execution = $timedate->getInstance()->now();
                            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Saving execution date...");
                            $action->save();
                        } else {
                            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Action [{$action->id} - {$action->name}]has ended with error.");
                        }
                    }
                } catch (Exception $e) {
                    $idError = uniqid();
                    $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Exception occurred in action execution [{$action->id} - {$action->name}]. IDError [{$idError}] Exception [{$e}]");
                    array_push($failedActions, $action);
                }
                // Update the general status. Although there is an error in the execution of one of the functions, we continue with the rest of the actions.
                $totalResult = $totalResult && $result;                                        
                $endTime = microtime(true);
                $totalTime = $endTime - $initTime;
                $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": Action [{$action->id} - {$action->name}] has taken [{$totalTime}] seconds. ");
            }
        }

        // Send email only if there are validation results on the same day
        global $current_user;
        $tzone = $current_user->getPreference('timezone') ?? $sugar_config['default_timezone'] ?? date_default_timezone_get();        
        $query = "SELECT count(*) as num_results FROM `stic_validation_results` WHERE deleted = 0 AND DATE_FORMAT(CONVERT_TZ(date_modified, '+00:00', '" . $tzone ."'),'%Y-%m-%d') LIKE CURDATE();";
        $result = $db->query($query);
        $row = $db->fetchByAssoc($result);
        if (isset($row['num_results']) && intval($row['num_results']) > 0) {
            $this->prepareAndSendEmail($scheduler, $failedActions);
        }
        return $totalResult;
    }

    /**
     * Create the query array proposed by the validator
     * @param Array $selector
     * @throws Exception If the selector is not of a valid type
     */
    protected function proposeSQL(stic_Validation_Actions $action)
    {
        $sqlArray = array();
        $func = $action->getFunctionObject();
        switch ($func->selector) {
            case DataCheckFunction::SELECTOR_INCREMENTAL:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . "Action with incremental type selector. Generating SQL proposal...");
                $sqlArray = $this->proposeIncrementalSQL($action);
                break;
            case DataCheckFunction::SELECTOR_SPECIFIC:
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . "Action with specific selector. No SQL proposal will be made.");
                break;
            default:
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . "Selector [{$func->selector}]invalid in action function [{$action->id} - {$action->name}]");
                throw new Exception(translate('LBL_FUNCTION_CONF_ERROR', 'stic_Validation_Actions'));
                break;
        }
        return $sqlArray;
    }

    /**
     * Create the query array proposed by the validator for incremental processes
     * @param Array $selector
     */
    protected function proposeIncrementalSQL(stic_Validation_Actions $action)
    {
        $func = $action->getFunctionObject();
        $arrayRet = array();
        if (!$func->module) {
            $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": The module to apply the filter to has not been specified.");
        } else {
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Creating filter for module [{$func->module}]");
            $module = BeanFactory::getBean($func->module);
            $where = '';
            $sqlIncremental = self::getSQLIncremental($action);

            // Add the date filter
            if ($sqlIncremental) {
                $where = "{$module->table_name}.$sqlIncremental";
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Filter by date added [" . $sqlIncremental . "] [{$where}]");
            } else {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": The date of the last execution does not exist. Not included in the filter.");
            }
            // Generate sql to retrieve data
            $arrayRet = $module->create_new_list_query('', $where, array(), array(), 0, '', true);
        }
        return $arrayRet;
    }

    /**
     * Returns the condition to convert an SQL to incremental
     * @param stic_Validation_Actions $action
     */
    public static function getSQLIncremental(stic_Validation_Actions $action)
    {
        // Add the date filter
        if ($action->last_execution) {
            global $timedate, $current_user;
            $timeformat = $timedate->get_date_time_format($current_user);
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Last execution date [{$action->last_execution}] Format [{$timeformat}].");
            $newTimeDate = $timedate->fromUser($action->last_execution, $current_user);
            $sqlDate = "date_modified > '" . $newTimeDate->asDB() . "'";
            $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Fecha SQL {$sqlDate}");
            return $sqlDate;
        }
        return null;
    }

    /**
     * Configure the email that will be sent to administrators
     * @param stic_Validation_Actions $action
     */
    public function prepareAndSendEmail($scheduler, $failedActions)
    {
        // Send email to admin users
        global $timedate, $current_user, $sugar_config;
        $userFormat = $timedate->get_date_format($current_user);
        $nowUserFormat = date($userFormat);
        $nowBDFormat = date('Y-m-d');

        $subject = "$scheduler->name - $nowUserFormat";
        $body = translate('LBL_SUCCESS', 'stic_Validation_Actions') . " $nowUserFormat:";
        $body .= '<br /><a href="' . $sugar_config["site_url"] . '/index.php?module=stic_Validation_Results&action=index&query=true&searchFormTab=advanced_search&execution_date_advanced_range_choice==&range_execution_date_advanced=' . $nowBDFormat . '">' . translate('LBL_RESULTS_LINK', 'stic_Validation_Actions') . " $nowUserFormat </a>";

        if (!empty($failedActions)){
            $body .= '<br /><br /><br />' . translate('LBL_ACTION_ERRORS', 'stic_Validation_Actions') . '<br />';

            foreach($failedActions as $action){
                $body .= "<br /> - <a href='" . $sugar_config['site_url'] . "/index.php?module=stic_Validation_Actions&action=DetailView&record=$action->id'>" . $action->name . "</a>";
            }

            $body .= '<br /><br />' . translate('LBL_EXCEPTION', 'stic_Validation_Actions');
        }

        try {
            $this->sendEmailToAdmins($subject, $body);
        } catch (Exception $e) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error {$e} sending the email.");
        }
        return null;
    }

    /**
     * Send email to administrators
     *
     * @param String $subject mail subject
     * @param String $subject mail body
     * @return void
     */
    protected function sendEmailToAdmins($subject, $body)
    {
        // Add destination addresses
        $recipients = $this->getRecipients();
        if (!empty($recipients)) {

            // Prepare mail
            require_once 'include/SugarPHPMailer.php';
            $emailObj = new Email();
            $defaults = $emailObj->getSystemDefaultEmail();

            $mail = new SugarPHPMailer();
            $mail->setMailerForSystem();
            $mail->From = $defaults['email'];
            $mail->FromName = $defaults['name'];

            foreach ($recipients as $address) {
                $mail->AddBCC($address);
            }
            
            $mail->Subject = $subject;
            $current_language = $GLOBALS['current_language'];
            $completeHTML = "
            <html lang=\"{$current_language}\">
                <head>
                    <title>{$this->Subject}</title>
                </head>
                <body style=\"font-family: Arial\">
                {$body}
                </body>
            </html>";
            $mail->Body = from_html($completeHTML);
            //$this->saveReport($this->name, $completeHTML);
            $mail->isHtml(true);
            $mail->prepForOutbound();
            $ret = true;
            if (!$ret = $mail->Send()) {
                $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ': There was an error sending the email to the admins.');
            } else {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ': Mail/s sent correctly.');
            }
        }
        return $ret;
    }

    /*
    * Returns an array with the list of addresses to send the mail
    * @return Array
    */
    protected function getRecipients()
    {
        $users = BeanFactory::getBean('Users');
        $adminList = $users->get_full_list('', "users.is_admin=1 AND users.status = 'Active'");

        $recipients = array();

        if ($adminList == null) {
            $GLOBALS['log']->fatal('Line ' . __LINE__ . ': ' . __METHOD__ . ': The destination address list could not be retrieved. The email cannot be sent');
        } else {
            // Indexem l'array per id
            foreach ($adminList as $user) {
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Addressee: {$user->id} - {$user->first_name} {$user->last_name}");
                if (!$user->emailAddress) {
                    $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error [{$user->id}] does not have an associated email address");
                } else {
                    $address = $user->emailAddress->getPrimaryAddress($user);
                    if (!$address) {
                        $GLOBALS['log']->error('Line ' . __LINE__ . ': ' . __METHOD__ . ": Error [{$user->id}] does not have an associated email address");
                    } else {
                        $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Addressee [{$user->first_name} {$user->last_name}] dirección [{$address}].");
                        $recipients[] = $address;
                    }
                }
            }
        }
        return $recipients;
    }
}

