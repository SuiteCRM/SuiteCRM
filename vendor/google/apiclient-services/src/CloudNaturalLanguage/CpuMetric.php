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

namespace Google\Service\CloudNaturalLanguage;

class CpuMetric extends \Google\Model
{
  /**
   * @var string
   */
  public $coreNumber;
  /**
   * @var string
   */
  public $coreSec;
  /**
   * @var string
   */
  public $cpuType;
  /**
   * @var string
   */
  public $machineSpec;
  /**
   * @var string[]
   */
  public $trackingLabels;

  /**
   * @param string
   */
  public function setCoreNumber($coreNumber)
  {
    $this->coreNumber = $coreNumber;
  }
  /**
   * @return string
   */
  public function getCoreNumber()
  {
    return $this->coreNumber;
  }
  /**
   * @param string
   */
  public function setCoreSec($coreSec)
  {
    $this->coreSec = $coreSec;
  }
  /**
   * @return string
   */
  public function getCoreSec()
  {
    return $this->coreSec;
  }
  /**
   * @param string
   */
  public function setCpuType($cpuType)
  {
    $this->cpuType = $cpuType;
  }
  /**
   * @return string
   */
  public function getCpuType()
  {
    return $this->cpuType;
  }
  /**
   * @param string
   */
  public function setMachineSpec($machineSpec)
  {
    $this->machineSpec = $machineSpec;
  }
  /**
   * @return string
   */
  public function getMachineSpec()
  {
    return $this->machineSpec;
  }
  /**
   * @param string[]
   */
  public function setTrackingLabels($trackingLabels)
  {
    $this->trackingLabels = $trackingLabels;
  }
  /**
   * @return string[]
   */
  public function getTrackingLabels()
  {
    return $this->trackingLabels;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CpuMetric::class, 'Google_Service_CloudNaturalLanguage_CpuMetric');
