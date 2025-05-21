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

class RamMetric extends \Google\Model
{
  /**
   * @var string
   */
  public $gibSec;
  /**
   * @var string
   */
  public $machineSpec;
  public $memories;
  /**
   * @var string
   */
  public $ramType;
  /**
   * @var string[]
   */
  public $trackingLabels;

  /**
   * @param string
   */
  public function setGibSec($gibSec)
  {
    $this->gibSec = $gibSec;
  }
  /**
   * @return string
   */
  public function getGibSec()
  {
    return $this->gibSec;
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
  public function setMemories($memories)
  {
    $this->memories = $memories;
  }
  public function getMemories()
  {
    return $this->memories;
  }
  /**
   * @param string
   */
  public function setRamType($ramType)
  {
    $this->ramType = $ramType;
  }
  /**
   * @return string
   */
  public function getRamType()
  {
    return $this->ramType;
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
class_alias(RamMetric::class, 'Google_Service_CloudNaturalLanguage_RamMetric');
