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

require_once 'SticInclude/Utils.php';

/**
 * Class to check the following fields of organizations
 * - NIF/NIE/CIF
 * - Name
 */
class CheckAccountsBeanData extends DataCheckFunction
{
    public const NID_FIELD = 'stic_identification_number_c'; // Name of the field to review

    // Contains the FieldDefs of the Organization class
    protected $accountDef = null;

    public function __construct($functionDef)
    {
        parent::__construct($functionDef);
        $bean = BeanFactory::getBean('Accounts');
        $this->accountDef = $bean->field_defs;
    }

    /**
     * Función doAction.
     * Perform the action defined in the function
     * @param $records Set of records on which the validation action is to be applied
     * @param $actionBean stic_Validation_Actions Bean of the action in which the function is being executed.
     * @return boolean It will return true in case of success and false in case of error.
     */
    public function doAction($records, stic_Validation_Actions $actionBean)
    {
        global $sugar_config;
        // It will indicate if records have been found to validate.
        $count = 0;
        // It will indicate if records with errors have been found.
        $errors = 0;
        // It will indicate how many records have been updated.
        $nUpdated = 0;

        $fieldsToValidate = $this->fieldsToValidate;
        $objModel = BeanFactory::getBean($this->module);
        $fieldDefs = $objModel->field_defs;

        while ($row = array_pop($records)) 
        {
            $logName = "[{$row['id']} - {$row['name']}]";
            $bean = null;

            // Check the fields to review
            foreach ($fieldsToValidate as $field) 
            {
                if (!SticUtils::isValidField($field, $row[$field], $fieldDefs, false)) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Field [{$field}] with value [{$row[$field]}] wrong in [{$this->module}] {$logName}.");
                    $fieldName = translate($fieldDefs[$field]['vname'], $this->module);
                    $name = substr($fieldName, 0, -1) . ' ' . $this->getLabel('ERR_FIELD') . " [{$row[$field]}]";
                    $errorMsg = '<span style="color:red;">' . $name . '</span>';
                    $errorMsg2 = $field == 'name' ? '<br />- ID: <a href="' . $sugar_config["site_url"] . '/index.php?module=' . $this->module . '&return_module=' . $this->module . '&action=DetailView&record=' . $row['id'] . '">' . $row['id'] . '</a>' : '';
                    $data = array(
                        'name' => $this->getLabel('NAME') . ' - ' . $name,
                        'stic_validation_actions_id' => $actionBean->id,
                        'log' => '<div>' . $errorMsg . $errorMsg2 . '</div>',
                        'parent_type' => $this->functionDef['module'],
                        'parent_id' => $row['id'],
                        'reviewed' => 'no',   
                        'assigned_user_id' => $row['assigned_user_id'],
                    );
                    $this->logValidationResult($data);
                    $errors++;
                }
            }

            // If you have NIF / NIE / CIF check its validity
            if (!empty($row[self::NID_FIELD])) 
            {
                // Calculate the identifier by removing invalid characters
                $cleanedId = SticUtils::cleanNIForCIF($row[self::NID_FIELD]);
                $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": CIF O NIF [{$row[self::NID_FIELD]}] -> [{$cleanedId}].");

                // If the clean field is not valid, the error is reported
                if (!SticUtils::isValidNIForNIE($cleanedId) &&
                    !SticUtils::isValidCIF($cleanedId)) {
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Field [" . self::NID_FIELD . "] with value [{$row[self::NID_FIELD]}] wrong in [{$this->module}] {$logName}.");
                    $errorMsg = '<span style="color:red;">' . $this->getLabel('NO_VALID') . " [{$row[self::NID_FIELD]}]" . '</span>';
                    $data = array(
                        'name' => $this->getLabel('NAME') . ' - ' . $row['name'],
                        'stic_validation_actions_id' => $actionBean->id,
                        'log' => '<div>' . $errorMsg . '</div>',
                        'parent_type' => $this->functionDef['module'],
                        'parent_id' => $row['id'],
                        'reviewed' => 'no',   
                        'assigned_user_id' => $row['assigned_user_id'],
                    );
                    $this->logValidationResult($data);
                    $errors++;
                // If it is valid, but different from the original, it is updated
                } elseif ($cleanedId != $row[self::NID_FIELD]) {
                    $bean = null;
                    $bean = $this->loadBean($bean, $row);
                    $nidField = self::NID_FIELD;
                    $bean->$nidField = $cleanedId;
                    $GLOBALS['log']->debug('Line ' . __LINE__ . ': ' . __METHOD__ . ": Updating Organization to modify the field [" . self::NID_FIELD . "] ...");
                    $bean->save();

                    $errorMsg = $this->getLabel('UPDATED') . "<br />- [{$row[self::NID_FIELD]}] --> [{$cleanedId}]";
                    $data = array(
                        'name' => $this->getLabel('NAME') . ' - ' . $row['name'],
                        'stic_validation_actions_id' => $actionBean->id,
                        'log' => '<div>' . $errorMsg . '</div>',
                        'parent_type' => $this->functionDef['module'],
                        'parent_id' => $row['id'],
                        'reviewed' => 'not_necessary',                      
                        'assigned_user_id' => $row['assigned_user_id'],
                    );
                    $this->logValidationResult($data);

                    $nUpdated++;
                    $errors++; // It is not really an error, but in this way it eliminates the message that no errors were found when it is updated
                }
            }
            $count++; // Records processed
        }

        // Report_always
        global $current_user;
        if (!$count && $actionBean->report_always) {
            $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": No record has been retrieved to validate.");
            $errorMsg = $this->getLabel('NO_ROWS');
            $data = array(
                'name' => $errorMsg,
                'stic_validation_actions_id' => $actionBean->id,
                'log' => '<div>' . $errorMsg . '</div>',
                'reviewed' => 'not_necessary',              
                'assigned_user_id' => $current_user->id, // In this message we indicate the administrator user
            );
            $this->logValidationResult($data);
        } else if (!$errors && $actionBean->report_always) {
            $errorMsg = $this->getLabel('NO_ERRORS');
            $data = array(
                'name' => $errorMsg,
                'stic_validation_actions_id' => $actionBean->id,
                'log' => '<div>' . $errorMsg . '</div>',
                'reviewed' => 'not_necessary',       
                'assigned_user_id' => $current_user->id, // In this message we indicate the administrator user            
            );
            $this->logValidationResult($data);
        }

        $GLOBALS['log']->info('Line ' . __LINE__ . ': ' . __METHOD__ . ": [{$count}] records processed [{$errors}] invalid, [{$nUpdated}] updated.");
        return true;
    }
}
