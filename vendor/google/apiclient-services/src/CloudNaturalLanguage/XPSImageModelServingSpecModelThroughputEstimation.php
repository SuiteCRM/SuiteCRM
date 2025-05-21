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

class XPSImageModelServingSpecModelThroughputEstimation extends \Google\Model
{
  /**
   * @var string
   */
  public $computeEngineAcceleratorType;
  public $latencyInMilliseconds;
  public $nodeQps;
  /**
   * @var string
   */
  public $servomaticPartitionType;

  /**
   * @param string
   */
  public function setComputeEngineAcceleratorType($computeEngineAcceleratorType)
  {
    $this->computeEngineAcceleratorType = $computeEngineAcceleratorType;
  }
  /**
   * @return string
   */
  public function getComputeEngineAcceleratorType()
  {
    return $this->computeEngineAcceleratorType;
  }
  public function setLatencyInMilliseconds($latencyInMilliseconds)
  {
    $this->latencyInMilliseconds = $latencyInMilliseconds;
  }
  public function getLatencyInMilliseconds()
  {
    return $this->latencyInMilliseconds;
  }
  public function setNodeQps($nodeQps)
  {
    $this->nodeQps = $nodeQps;
  }
  public function getNodeQps()
  {
    return $this->nodeQps;
  }
  /**
   * @param string
   */
  public function setServomaticPartitionType($servomaticPartitionType)
  {
    $this->servomaticPartitionType = $servomaticPartitionType;
  }
  /**
   * @return string
   */
  public function getServomaticPartitionType()
  {
    return $this->servomaticPartitionType;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(XPSImageModelServingSpecModelThroughputEstimation::class, 'Google_Service_CloudNaturalLanguage_XPSImageModelServingSpecModelThroughputEstimation');
