<?php

namespace Step\Acceptance;

use \AcceptanceTester as Tester;

#[\AllowDynamicProperties]
class Workflow extends Tester
{
    public function navigateToWorkflow(): void
    {
        $this->visitPage('Administration', 'index');
        $this->click('#workflow_management');
        $this->waitForText('WORKFLOW');}

    /**
     * @param $module
     */
    public function selectWorkflowModule($module)
    {
        $this->selectOption('#flow_module', $module);
    }

    public function addCondition()
    {
        $this->click('#btn_ConditionLine');
    }

    /**
     * @param $row
     * @param $module
     * @param $field
     */
    public function setConditionModuleField($row, $module, $field)
    {
        $this->selectOption('#aow_conditions_module_path' . $row, $module);
        $this->selectOption('#aow_conditions_field' . $row, $field);
    }

    /**
     * @param $row
     * @param $operator
     * @param $type
     */
    public function setConditionOperator($row, $operator, $type)
    {
        $this->waitForElementVisible('#aow_conditions_operator[' . $row . ']');
        $this->selectOption('#aow_conditions_operator[' . $row . ']', $operator);

        $this->waitForElementVisible('#aow_conditions_value_type[' . $row . ']');
        $this->selectOption('#aow_conditions_value_type[' . $row . ']', $type);
    }

    /**
     * @param $row
     */
    public function setConditionOperatorDateTimeValue($row)
    {
        $calendarButton = '#aow_conditions_value'.$row.'_trigger';
        $calendarField = '#aow_conditions_value'.$row.'_date';
        $calendarDialog = '#aow_conditions_value'.$row.'_trigger_div';
        $this->click($calendarButton);
        $this->waitForElementVisible($calendarDialog);
        $this->click('.today > .selector', $calendarDialog);
        $this->cantSeeInField($calendarField, '');
    }

    public function getLastConditionRowNumber()
    {
        return $this->executeJS('return document.querySelectorAll(\'[id ^= aow_conditions_body]\').length - 1;');
    }
}
