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

namespace Google\Service\Parallelstore;

class Instance extends \Google\Collection
{
  protected $collection_key = 'accessPoints';
  /**
   * @var string[]
   */
  public $accessPoints;
  /**
   * @var string
   */
  public $capacityGib;
  /**
   * @var string
   */
  public $createTime;
  /**
   * @var string
   */
  public $daosVersion;
  /**
   * @var string
   */
  public $deploymentType;
  /**
   * @var string
   */
  public $description;
  /**
   * @var string
   */
  public $directoryStripeLevel;
  /**
   * @var string
   */
  public $effectiveReservedIpRange;
  /**
   * @var string
   */
  public $fileStripeLevel;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $network;
  /**
   * @var string
   */
  public $reservedIpRange;
  /**
   * @var string
   */
  public $state;
  /**
   * @var string
   */
  public $updateTime;

  /**
   * @param string[]
   */
  public function setAccessPoints($accessPoints)
  {
    $this->accessPoints = $accessPoints;
  }
  /**
   * @return string[]
   */
  public function getAccessPoints()
  {
    return $this->accessPoints;
  }
  /**
   * @param string
   */
  public function setCapacityGib($capacityGib)
  {
    $this->capacityGib = $capacityGib;
  }
  /**
   * @return string
   */
  public function getCapacityGib()
  {
    return $this->capacityGib;
  }
  /**
   * @param string
   */
  public function setCreateTime($createTime)
  {
    $this->createTime = $createTime;
  }
  /**
   * @return string
   */
  public function getCreateTime()
  {
    return $this->createTime;
  }
  /**
   * @param string
   */
  public function setDaosVersion($daosVersion)
  {
    $this->daosVersion = $daosVersion;
  }
  /**
   * @return string
   */
  public function getDaosVersion()
  {
    return $this->daosVersion;
  }
  /**
   * @param string
   */
  public function setDeploymentType($deploymentType)
  {
    $this->deploymentType = $deploymentType;
  }
  /**
   * @return string
   */
  public function getDeploymentType()
  {
    return $this->deploymentType;
  }
  /**
   * @param string
   */
  public function setDescription($description)
  {
    $this->description = $description;
  }
  /**
   * @return string
   */
  public function getDescription()
  {
    return $this->description;
  }
  /**
   * @param string
   */
  public function setDirectoryStripeLevel($directoryStripeLevel)
  {
    $this->directoryStripeLevel = $directoryStripeLevel;
  }
  /**
   * @return string
   */
  public function getDirectoryStripeLevel()
  {
    return $this->directoryStripeLevel;
  }
  /**
   * @param string
   */
  public function setEffectiveReservedIpRange($effectiveReservedIpRange)
  {
    $this->effectiveReservedIpRange = $effectiveReservedIpRange;
  }
  /**
   * @return string
   */
  public function getEffectiveReservedIpRange()
  {
    return $this->effectiveReservedIpRange;
  }
  /**
   * @param string
   */
  public function setFileStripeLevel($fileStripeLevel)
  {
    $this->fileStripeLevel = $fileStripeLevel;
  }
  /**
   * @return string
   */
  public function getFileStripeLevel()
  {
    return $this->fileStripeLevel;
  }
  /**
   * @param string[]
   */
  public function setLabels($labels)
  {
    $this->labels = $labels;
  }
  /**
   * @return string[]
   */
  public function getLabels()
  {
    return $this->labels;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setNetwork($network)
  {
    $this->network = $network;
  }
  /**
   * @return string
   */
  public function getNetwork()
  {
    return $this->network;
  }
  /**
   * @param string
   */
  public function setReservedIpRange($reservedIpRange)
  {
    $this->reservedIpRange = $reservedIpRange;
  }
  /**
   * @return string
   */
  public function getReservedIpRange()
  {
    return $this->reservedIpRange;
  }
  /**
   * @param string
   */
  public function setState($state)
  {
    $this->state = $state;
  }
  /**
   * @return string
   */
  public function getState()
  {
    return $this->state;
  }
  /**
   * @param string
   */
  public function setUpdateTime($updateTime)
  {
    $this->updateTime = $updateTime;
  }
  /**
   * @return string
   */
  public function getUpdateTime()
  {
    return $this->updateTime;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Instance::class, 'Google_Service_Parallelstore_Instance');
