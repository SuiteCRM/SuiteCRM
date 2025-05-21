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

class SqlserverValidation extends \Google\Collection
{
  protected $collection_key = 'validationDetails';
  /**
   * @var string
   */
  public $agentVersion;
  /**
   * @var string
   */
  public $instance;
  /**
   * @var string
   */
  public $projectId;
  protected $validationDetailsType = SqlserverValidationValidationDetail::class;
  protected $validationDetailsDataType = 'array';

  /**
   * @param string
   */
  public function setAgentVersion($agentVersion)
  {
    $this->agentVersion = $agentVersion;
  }
  /**
   * @return string
   */
  public function getAgentVersion()
  {
    return $this->agentVersion;
  }
  /**
   * @param string
   */
  public function setInstance($instance)
  {
    $this->instance = $instance;
  }
  /**
   * @return string
   */
  public function getInstance()
  {
    return $this->instance;
  }
  /**
   * @param string
   */
  public function setProjectId($projectId)
  {
    $this->projectId = $projectId;
  }
  /**
   * @return string
   */
  public function getProjectId()
  {
    return $this->projectId;
  }
  /**
   * @param SqlserverValidationValidationDetail[]
   */
  public function setValidationDetails($validationDetails)
  {
    $this->validationDetails = $validationDetails;
  }
  /**
   * @return SqlserverValidationValidationDetail[]
   */
  public function getValidationDetails()
  {
    return $this->validationDetails;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlserverValidation::class, 'Google_Service_WorkloadManager_SqlserverValidation');
