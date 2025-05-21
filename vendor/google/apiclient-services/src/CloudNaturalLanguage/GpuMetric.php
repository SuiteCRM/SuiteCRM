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

class GpuMetric extends \Google\Model
{
  /**
   * @var string
   */
  public $gpuSec;
  /**
   * @var string
   */
  public $gpuType;
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
  public function setGpuSec($gpuSec)
  {
    $this->gpuSec = $gpuSec;
  }
  /**
   * @return string
   */
  public function getGpuSec()
  {
    return $this->gpuSec;
  }
  /**
   * @param string
   */
  public function setGpuType($gpuType)
  {
    $this->gpuType = $gpuType;
  }
  /**
   * @return string
   */
  public function getGpuType()
  {
    return $this->gpuType;
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
class_alias(GpuMetric::class, 'Google_Service_CloudNaturalLanguage_GpuMetric');
