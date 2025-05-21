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

namespace Google\Service\Batch;

class AgentMetadata extends \Google\Model
{
  /**
   * @var string
   */
  public $creationTime;
  /**
   * @var string
   */
  public $creator;
  /**
   * @var string
   */
  public $imageVersion;
  /**
   * @var string
   */
  public $instance;
  /**
   * @var string
   */
  public $instanceId;
  /**
   * @var bool
   */
  public $instancePreemptionNoticeReceived;
  /**
   * @var string
   */
  public $machineType;
  /**
   * @var string[]
   */
  public $osRelease;
  /**
   * @var string
   */
  public $version;
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setCreationTime($creationTime)
  {
    $this->creationTime = $creationTime;
  }
  /**
   * @return string
   */
  public function getCreationTime()
  {
    return $this->creationTime;
  }
  /**
   * @param string
   */
  public function setCreator($creator)
  {
    $this->creator = $creator;
  }
  /**
   * @return string
   */
  public function getCreator()
  {
    return $this->creator;
  }
  /**
   * @param string
   */
  public function setImageVersion($imageVersion)
  {
    $this->imageVersion = $imageVersion;
  }
  /**
   * @return string
   */
  public function getImageVersion()
  {
    return $this->imageVersion;
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
  public function setInstanceId($instanceId)
  {
    $this->instanceId = $instanceId;
  }
  /**
   * @return string
   */
  public function getInstanceId()
  {
    return $this->instanceId;
  }
  /**
   * @param bool
   */
  public function setInstancePreemptionNoticeReceived($instancePreemptionNoticeReceived)
  {
    $this->instancePreemptionNoticeReceived = $instancePreemptionNoticeReceived;
  }
  /**
   * @return bool
   */
  public function getInstancePreemptionNoticeReceived()
  {
    return $this->instancePreemptionNoticeReceived;
  }
  /**
   * @param string
   */
  public function setMachineType($machineType)
  {
    $this->machineType = $machineType;
  }
  /**
   * @return string
   */
  public function getMachineType()
  {
    return $this->machineType;
  }
  /**
   * @param string[]
   */
  public function setOsRelease($osRelease)
  {
    $this->osRelease = $osRelease;
  }
  /**
   * @return string[]
   */
  public function getOsRelease()
  {
    return $this->osRelease;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
  /**
   * @param string
   */
  public function setZone($zone)
  {
    $this->zone = $zone;
  }
  /**
   * @return string
   */
  public function getZone()
  {
    return $this->zone;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(AgentMetadata::class, 'Google_Service_Batch_AgentMetadata');
