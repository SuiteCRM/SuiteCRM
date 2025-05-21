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

class GoogleCloudAiplatformV1ResourcePool extends \Google\Model
{
  protected $autoscalingSpecType = GoogleCloudAiplatformV1ResourcePoolAutoscalingSpec::class;
  protected $autoscalingSpecDataType = '';
  protected $diskSpecType = GoogleCloudAiplatformV1DiskSpec::class;
  protected $diskSpecDataType = '';
  /**
   * @var string
   */
  public $id;
  protected $machineSpecType = GoogleCloudAiplatformV1MachineSpec::class;
  protected $machineSpecDataType = '';
  /**
   * @var string
   */
  public $replicaCount;
  /**
   * @var string
   */
  public $usedReplicaCount;

  /**
   * @param GoogleCloudAiplatformV1ResourcePoolAutoscalingSpec
   */
  public function setAutoscalingSpec(GoogleCloudAiplatformV1ResourcePoolAutoscalingSpec $autoscalingSpec)
  {
    $this->autoscalingSpec = $autoscalingSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1ResourcePoolAutoscalingSpec
   */
  public function getAutoscalingSpec()
  {
    return $this->autoscalingSpec;
  }
  /**
   * @param GoogleCloudAiplatformV1DiskSpec
   */
  public function setDiskSpec(GoogleCloudAiplatformV1DiskSpec $diskSpec)
  {
    $this->diskSpec = $diskSpec;
  }
  /**
   * @return GoogleCloudAiplatformV1DiskSpec
   */
  public function getDiskSpec()
  {
    return $this->diskSpec;
  }
  /**
   * @param string
   */
  public function setId($id)
  {
    $this->id = $id;
  }
  /**
   * @return string
   */
  public function getId()
  {
    return $this->id;
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
   * @param string
   */
  public function setReplicaCount($replicaCount)
  {
    $this->replicaCount = $replicaCount;
  }
  /**
   * @return string
   */
  public function getReplicaCount()
  {
    return $this->replicaCount;
  }
  /**
   * @param string
   */
  public function setUsedReplicaCount($usedReplicaCount)
  {
    $this->usedReplicaCount = $usedReplicaCount;
  }
  /**
   * @return string
   */
  public function getUsedReplicaCount()
  {
    return $this->usedReplicaCount;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ResourcePool::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ResourcePool');
