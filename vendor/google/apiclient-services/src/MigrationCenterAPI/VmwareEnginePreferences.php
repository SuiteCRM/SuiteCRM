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

class VmwareEnginePreferences extends \Google\Model
{
  /**
   * @var string
   */
  public $commitmentPlan;
  public $cpuOvercommitRatio;
  public $memoryOvercommitRatio;
  public $storageDeduplicationCompressionRatio;

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
  public function setMemoryOvercommitRatio($memoryOvercommitRatio)
  {
    $this->memoryOvercommitRatio = $memoryOvercommitRatio;
  }
  public function getMemoryOvercommitRatio()
  {
    return $this->memoryOvercommitRatio;
  }
  public function setStorageDeduplicationCompressionRatio($storageDeduplicationCompressionRatio)
  {
    $this->storageDeduplicationCompressionRatio = $storageDeduplicationCompressionRatio;
  }
  public function getStorageDeduplicationCompressionRatio()
  {
    return $this->storageDeduplicationCompressionRatio;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VmwareEnginePreferences::class, 'Google_Service_MigrationCenterAPI_VmwareEnginePreferences');
