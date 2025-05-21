<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\Integrations;

class GoogleCloudIntegrationsV1alphaTriggerConfig extends \Google\Collection
{
  protected $collection_key = 'startTasks';
  protected $alertConfigType = GoogleCloudIntegrationsV1alphaIntegrationAlertConfig::class;
  protected $alertConfigDataType = 'array';
  protected $cloudSchedulerConfigType = GoogleCloudIntegrationsV1alphaCloudSchedulerConfig::class;
  protected $cloudSchedulerConfigDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $errorCatcherId;
  protected $inputVariablesType = GoogleCloudIntegrationsV1alphaTriggerConfigVariables::class;
  protected $inputVariablesDataType = '';
  /**
   * @var string
   */
  public $label;
  /**
   * @var string
   */
  public $nextTasksExecutionPolicy;
  protected $outputVariablesType = GoogleCloudIntegrationsV1alphaTriggerConfigVariables::class;
  protected $outputVariablesDataType = '';
  protected $positionType = GoogleCloudIntegrationsV1alphaCoordinate::class;
  protected $positionDataType = '';
  /**
   * @var string[]
   */
  public $properties;
  protected $startTasksType = GoogleCloudIntegrationsV1alphaNextTask::class;
  protected $startTasksDataType = 'array';
  /**
   * @var string
   */
  public $trigger;
  /**
   * @var string
   */
  public $triggerId;
  /**
   * @var string
   */
  public $triggerNumber;
  /**
   * @var string
   */
  public $triggerType;

  /**
   * @param GoogleCloudIntegrationsV1alphaIntegrationAlertConfig[]
   */
  public function setAlertConfig($alertConfig)
  {
    $this->alertConfig = $alertConfig;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaIntegrationAlertConfig[]
   */
  public function getAlertConfig()
  {
    return $this->alertConfig;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaCloudSchedulerConfig
   */
  public function setCloudSchedulerConfig(GoogleCloudIntegrationsV1alphaCloudSchedulerConfig $cloudSchedulerConfig)
  {
    $this->cloudSchedulerConfig = $cloudSchedulerConfig;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaCloudSchedulerConfig
   */
  public function getCloudSchedulerConfig()
  {
    return $this->cloudSchedulerConfig;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setErrorCatcherId($errorCatcherId)
  {
    $this->errorCatcherId = $errorCatcherId;
  }
  /**
   * @return string
   */
  public function getErrorCatcherId()
  {
    return $this->errorCatcherId;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaTriggerConfigVariables
   */
  public function setInputVariables(GoogleCloudIntegrationsV1alphaTriggerConfigVariables $inputVariables)
  {
    $this->inputVariables = $inputVariables;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaTriggerConfigVariables
   */
  public function getInputVariables()
  {
    return $this->inputVariables;
  }
  /**
   * @param string
   */
  public function setLabel($label)
  {
    $this->label = $label;
  }
  /**
   * @return string
   */
  public function getLabel()
  {
    return $this->label;
  }
  /**
   * @param string
   */
  public function setNextTasksExecutionPolicy($nextTasksExecutionPolicy)
  {
    $this->nextTasksExecutionPolicy = $nextTasksExecutionPolicy;
  }
  /**
   * @return string
   */
  public function getNextTasksExecutionPolicy()
  {
    return $this->nextTasksExecutionPolicy;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaTriggerConfigVariables
   */
  public function setOutputVariables(GoogleCloudIntegrationsV1alphaTriggerConfigVariables $outputVariables)
  {
    $this->outputVariables = $outputVariables;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaTriggerConfigVariables
   */
  public function getOutputVariables()
  {
    return $this->outputVariables;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaCoordinate
   */
  public function setPosition(GoogleCloudIntegrationsV1alphaCoordinate $position)
  {
    $this->position = $position;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaCoordinate
   */
  public function getPosition()
  {
    return $this->position;
  }
  /**
   * @param string[]
   */
  public function setProperties($properties)
  {
    $this->properties = $properties;
  }
  /**
   * @return string[]
   */
  public function getProperties()
  {
    return $this->properties;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaNextTask[]
   */
  public function setStartTasks($startTasks)
  {
    $this->startTasks = $startTasks;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaNextTask[]
   */
  public function getStartTasks()
  {
    return $this->startTasks;
  }
  /**
   * @param string
   */
  public function setTrigger($trigger)
  {
    $this->trigger = $trigger;
  }
  /**
   * @return string
   */
  public function getTrigger()
  {
    return $this->trigger;
  }
  /**
   * @param string
   */
  public function setTriggerId($triggerId)
  {
    $this->triggerId = $triggerId;
  }
  /**
   * @return string
   */
  public function getTriggerId()
  {
    return $this->triggerId;
  }
  /**
   * @param string
   */
  public function setTriggerNumber($triggerNumber)
  {
    $this->triggerNumber = $triggerNumber;
  }
  /**
   * @return string
   */
  public function getTriggerNumber()
  {
    return $this->triggerNumber;
  }
  /**
   * @param string
   */
  public function setTriggerType($triggerType)
  {
    $this->triggerType = $triggerType;
  }
  /**
   * @return string
   */
  public function getTriggerType()
  {
    return $this->triggerType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaTriggerConfig::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaTriggerConfig');
