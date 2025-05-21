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

namespace Google\Service\MigrationCenterAPI;

class SoleTenancyPreferences extends \Google\Collection
{
  protected $collection_key = 'nodeTypes';
  /**
   * @var string
   */
  public $commitmentPlan;
  public $cpuOvercommitRatio;
  /**
   * @var string
   */
  public $hostMaintenancePolicy;
  protected $nodeTypesType = SoleTenantNodeType::class;
  protected $nodeTypesDataType = 'array';

  /**
   * @param string
   */
  public function setCommitmentPlan($commitmentPlan)
  {
    $this->commitmentPlan = $commitmentPlan;
  }
  /**
   * @return string
   */
  public function getCommitmentPlan()
  {
    return $this->commitmentPlan;
  }
  public function setCpuOvercommitRatio($cpuOvercommitRatio)
  {
    $this->cpuOvercommitRatio = $cpuOvercommitRatio;
  }
  public function getCpuOvercommitRatio()
  {
    return $this->cpuOvercommitRatio;
  }
  /**
   * @param string
   */
  public function setHostMaintenancePolicy($hostMaintenancePolicy)
  {
    $this->hostMaintenancePolicy = $hostMaintenancePolicy;
  }
  /**
   * @return string
   */
  public function getHostMaintenancePolicy()
  {
    return $this->hostMaintenancePolicy;
  }
  /**
   * @param SoleTenantNodeType[]
   */
  public function setNodeTypes($nodeTypes)
  {
    $this->nodeTypes = $nodeTypes;
  }
  /**
   * @return SoleTenantNodeType[]
   */
  public function getNodeTypes()
  {
    return $this->nodeTypes;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SoleTenancyPreferences::class, 'Google_Service_MigrationCenterAPI_SoleTenancyPreferences');
