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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1DedicatedResources extends \Google\Collection
{
  protected $collection_key = 'autoscalingMetricSpecs';
  protected $autoscalingMetricSpecsType = GoogleCloudAiplatformV1AutoscalingMetricSpec::class;
  protected $autoscalingMetricSpecsDataType = 'array';
  protected $machineSpecType = GoogleCloudAiplatformV1MachineSpec::class;
  protected $machineSpecDataType = '';
  /**
   * @var int
   */
  public $maxReplicaCount;
  /**
   * @var int
   */
  public $minReplicaCount;
  /**
   * @var int
   */
  public $requiredReplicaCount;
  /**
   * @var bool
   */
  public $spot;

  /**
   * @param GoogleCloudAiplatformV1AutoscalingMetricSpec[]
   */
  public function setAutoscalingMetricSpecs($autoscalingMetricSpecs)
  {
    $this->autoscalingMetricSpecs = $autoscalingMetricSpecs;
  }
  /**
   * @return GoogleCloudAiplatformV1AutoscalingMetricSpec[]
   */
  public function getAutoscalingMetricSpecs()
  {
    return $this->autoscalingMetricSpecs;
  }
  /**
   * @param GoogleCloudAiplatformV1MachineSpec
   */
  public function setMachineSpec(GoogleCloudAiplatformV1MachineSpec $machineSpec)
  {
    $this->machineSpec = $machineSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1MachineSpec
   */
  public function getMachineSpec()
  {
    return $this->machineSpec;
  }
  /**
   * @param int
   */
  public function setMaxReplicaCount($maxReplicaCount)
  {
    $this->maxReplicaCount = $maxReplicaCount;
  }
  /**
   * @return int
   */
  public function getMaxReplicaCount()
  {
    return $this->maxReplicaCount;
  }
  /**
   * @param int
   */
  public function setMinReplicaCount($minReplicaCount)
  {
    $this->minReplicaCount = $minReplicaCount;
  }
  /**
   * @return int
   */
  public function getMinReplicaCount()
  {
    return $this->minReplicaCount;
  }
  /**
   * @param int
   */
  public function setRequiredReplicaCount($requiredReplicaCount)
  {
    $this->requiredReplicaCount = $requiredReplicaCount;
  }
  /**
   * @return int
   */
  public function getRequiredReplicaCount()
  {
    return $this->requiredReplicaCount;
  }
  /**
   * @param bool
   */
  public function setSpot($spot)
  {
    $this->spot = $spot;
  }
  /**
   * @return bool
   */
  public function getSpot()
  {
    return $this->spot;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1DedicatedResources::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1DedicatedResources');
