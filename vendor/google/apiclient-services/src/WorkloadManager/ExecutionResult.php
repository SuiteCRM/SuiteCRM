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

namespace Google\Service\WorkloadManager;

class ExecutionResult extends \Google\Collection
{
  protected $collection_key = 'commands';
  protected $commandsType = Command::class;
  protected $commandsDataType = 'array';
  /**
   * @var string
   */
  public $documentationUrl;
  protected $resourceType = WorkloadmanagerResource::class;
  protected $resourceDataType = '';
  /**
   * @var string
   */
  public $rule;
  /**
   * @var string
   */
  public $severity;
  /**
   * @var string
   */
  public $type;
  protected $violationDetailsType = ViolationDetails::class;
  protected $violationDetailsDataType = '';
  /**
   * @var string
   */
  public $violationMessage;

  /**
   * @param Command[]
   */
  public function setCommands($commands)
  {
    $this->commands = $commands;
  }
  /**
   * @return Command[]
   */
  public function getCommands()
  {
    return $this->commands;
  }
  /**
   * @param string
   */
  public function setDocumentationUrl($documentationUrl)
  {
    $this->documentationUrl = $documentationUrl;
  }
  /**
   * @return string
   */
  public function getDocumentationUrl()
  {
    return $this->documentationUrl;
  }
  /**
   * @param WorkloadmanagerResource
   */
  public function setResource(WorkloadmanagerResource $resource)
  {
    $this->resource = $resource;
  }
  /**
   * @return WorkloadmanagerResource
   */
  public function getResource()
  {
    return $this->resource;
  }
  /**
   * @param string
   */
  public function setRule($rule)
  {
    $this->rule = $rule;
  }
  /**
   * @return string
   */
  public function getRule()
  {
    return $this->rule;
  }
  /**
   * @param string
   */
  public function setSeverity($severity)
  {
    $this->severity = $severity;
  }
  /**
   * @return string
   */
  public function getSeverity()
  {
    return $this->severity;
  }
  /**
   * @param string
   */
  public function setType($type)
  {
    $this->type = $type;
  }
  /**
   * @return string
   */
  public function getType()
  {
    return $this->type;
  }
  /**
   * @param ViolationDetails
   */
  public function setViolationDetails(ViolationDetails $violationDetails)
  {
    $this->violationDetails = $violationDetails;
  }
  /**
   * @return ViolationDetails
   */
  public function getViolationDetails()
  {
    return $this->violationDetails;
  }
  /**
   * @param string
   */
  public function setViolationMessage($violationMessage)
  {
    $this->violationMessage = $violationMessage;
  }
  /**
   * @return string
   */
  public function getViolationMessage()
  {
    return $this->violationMessage;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExecutionResult::class, 'Google_Service_WorkloadManager_ExecutionResult');
