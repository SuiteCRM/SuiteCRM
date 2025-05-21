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

class TorsoValidation extends \Google\Model
{
  /**
   * @var string
   */
  public $agentVersion;
  /**
   * @var string
   */
  public $instanceName;
  /**
   * @var string
   */
  public $projectId;
  /**
   * @var string[]
   */
  public $validationDetails;
  /**
   * @var string
   */
  public $workloadType;

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
  public function setInstanceName($instanceName)
  {
    $this->instanceName = $instanceName;
  }
  /**
   * @return string
   */
  public function getInstanceName()
  {
    return $this->instanceName;
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
   * @param string[]
   */
  public function setValidationDetails($validationDetails)
  {
    $this->validationDetails = $validationDetails;
  }
  /**
   * @return string[]
   */
  public function getValidationDetails()
  {
    return $this->validationDetails;
  }
  /**
   * @param string
   */
  public function setWorkloadType($workloadType)
  {
    $this->workloadType = $workloadType;
  }
  /**
   * @return string
   */
  public function getWorkloadType()
  {
    return $this->workloadType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TorsoValidation::class, 'Google_Service_WorkloadManager_TorsoValidation');
