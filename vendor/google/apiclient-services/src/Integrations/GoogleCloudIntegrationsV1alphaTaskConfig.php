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

class GoogleCloudIntegrationsV1alphaTaskConfig extends \Google\Collection
{
  protected $collection_key = 'nextTasks';
  protected $conditionalFailurePoliciesType = GoogleCloudIntegrationsV1alphaConditionalFailurePolicies::class;
  protected $conditionalFailurePoliciesDataType = '';
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $errorCatcherId;
  /**
   * @var string
   */
  public $externalTaskType;
  protected $failurePolicyType = GoogleCloudIntegrationsV1alphaFailurePolicy::class;
  protected $failurePolicyDataType = '';
  /**
   * @var string
   */
  public $jsonValidationOption;
  protected $nextTasksType = GoogleCloudIntegrationsV1alphaNextTask::class;
  protected $nextTasksDataType = 'array';
  /**
   * @var string
   */
  public $nextTasksExecutionPolicy;
  protected $parametersType = GoogleCloudIntegrationsV1alphaEventParameter::class;
  protected $parametersDataType = 'map';
  protected $positionType = GoogleCloudIntegrationsV1alphaCoordinate::class;
  protected $positionDataType = '';
  protected $successPolicyType = GoogleCloudIntegrationsV1alphaSuccessPolicy::class;
  protected $successPolicyDataType = '';
  protected $synchronousCallFailurePolicyType = GoogleCloudIntegrationsV1alphaFailurePolicy::class;
  protected $synchronousCallFailurePolicyDataType = '';
  /**
   * @var string
   */
  public $task;
  /**
   * @var string
   */
  public $taskExecutionStrategy;
  /**
   * @var string
   */
  public $taskId;
  /**
   * @var string
   */
  public $taskTemplate;

  /**
   * @param GoogleCloudIntegrationsV1alphaConditionalFailurePolicies
   */
  public function setConditionalFailurePolicies(GoogleCloudIntegrationsV1alphaConditionalFailurePolicies $conditionalFailurePolicies)
  {
    $this->conditionalFailurePolicies = $conditionalFailurePolicies;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaConditionalFailurePolicies
   */
  public function getConditionalFailurePolicies()
  {
    return $this->conditionalFailurePolicies;
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
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
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
   * @param string
   */
  public function setExternalTaskType($externalTaskType)
  {
    $this->externalTaskType = $externalTaskType;
  }
  /**
   * @return string
   */
  public function getExternalTaskType()
  {
    return $this->externalTaskType;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaFailurePolicy
   */
  public function setFailurePolicy(GoogleCloudIntegrationsV1alphaFailurePolicy $failurePolicy)
  {
    $this->failurePolicy = $failurePolicy;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaFailurePolicy
   */
  public function getFailurePolicy()
  {
    return $this->failurePolicy;
  }
  /**
   * @param string
   */
  public function setJsonValidationOption($jsonValidationOption)
  {
    $this->jsonValidationOption = $jsonValidationOption;
  }
  /**
   * @return string
   */
  public function getJsonValidationOption()
  {
    return $this->jsonValidationOption;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaNextTask[]
   */
  public function setNextTasks($nextTasks)
  {
    $this->nextTasks = $nextTasks;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaNextTask[]
   */
  public function getNextTasks()
  {
    return $this->nextTasks;
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
   * @param GoogleCloudIntegrationsV1alphaEventParameter[]
   */
  public function setParameters($parameters)
  {
    $this->parameters = $parameters;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaEventParameter[]
   */
  public function getParameters()
  {
    return $this->parameters;
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
   * @param GoogleCloudIntegrationsV1alphaSuccessPolicy
   */
  public function setSuccessPolicy(GoogleCloudIntegrationsV1alphaSuccessPolicy $successPolicy)
  {
    $this->successPolicy = $successPolicy;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaSuccessPolicy
   */
  public function getSuccessPolicy()
  {
    return $this->successPolicy;
  }
  /**
   * @param GoogleCloudIntegrationsV1alphaFailurePolicy
   */
  public function setSynchronousCallFailurePolicy(GoogleCloudIntegrationsV1alphaFailurePolicy $synchronousCallFailurePolicy)
  {
    $this->synchronousCallFailurePolicy = $synchronousCallFailurePolicy;
  }
  /**
   * @return GoogleCloudIntegrationsV1alphaFailurePolicy
   */
  public function getSynchronousCallFailurePolicy()
  {
    return $this->synchronousCallFailurePolicy;
  }
  /**
   * @param string
   */
  public function setTask($task)
  {
    $this->task = $task;
  }
  /**
   * @return string
   */
  public function getTask()
  {
    return $this->task;
  }
  /**
   * @param string
   */
  public function setTaskExecutionStrategy($taskExecutionStrategy)
  {
    $this->taskExecutionStrategy = $taskExecutionStrategy;
  }
  /**
   * @return string
   */
  public function getTaskExecutionStrategy()
  {
    return $this->taskExecutionStrategy;
  }
  /**
   * @param string
   */
  public function setTaskId($taskId)
  {
    $this->taskId = $taskId;
  }
  /**
   * @return string
   */
  public function getTaskId()
  {
    return $this->taskId;
  }
  /**
   * @param string
   */
  public function setTaskTemplate($taskTemplate)
  {
    $this->taskTemplate = $taskTemplate;
  }
  /**
   * @return string
   */
  public function getTaskTemplate()
  {
    return $this->taskTemplate;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudIntegrationsV1alphaTaskConfig::class, 'Google_Service_Integrations_GoogleCloudIntegrationsV1alphaTaskConfig');
