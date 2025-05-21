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

namespace Google\Service\VMMigrationService;

class AwsVmDetails extends \Google\Collection
{
  protected $collection_key = 'securityGroups';
  /**
   * @var string
   */
  public $architecture;
  /**
   * @var string
   */
  public $bootOption;
  /**
   * @var string
   */
  public $committedStorageMb;
  /**
   * @var int
   */
  public $cpuCount;
  /**
   * @var int
   */
  public $diskCount;
  /**
   * @var string
   */
  public $displayName;
  /**
   * @var string
   */
  public $instanceType;
  /**
   * @var int
   */
  public $memoryMb;
  /**
   * @var string
   */
  public $osDescription;
  /**
   * @var string
   */
  public $powerState;
  protected $securityGroupsType = AwsSecurityGroup::class;
  protected $securityGroupsDataType = 'array';
  /**
   * @var string
   */
  public $sourceDescription;
  /**
   * @var string
   */
  public $sourceId;
  /**
   * @var string[]
   */
  public $tags;
  /**
   * @var string
   */
  public $virtualizationType;
  /**
   * @var string
   */
  public $vmId;
  /**
   * @var string
   */
  public $vpcId;
  /**
   * @var string
   */
  public $zone;

  /**
   * @param string
   */
  public function setArchitecture($architecture)
  {
    $this->architecture = $architecture;
  }
  /**
   * @return string
   */
  public function getArchitecture()
  {
    return $this->architecture;
  }
  /**
   * @param string
   */
  public function setBootOption($bootOption)
  {
    $this->bootOption = $bootOption;
  }
  /**
   * @return string
   */
  public function getBootOption()
  {
    return $this->bootOption;
  }
  /**
   * @param string
   */
  public function setCommittedStorageMb($committedStorageMb)
  {
    $this->committedStorageMb = $committedStorageMb;
  }
  /**
   * @return string
   */
  public function getCommittedStorageMb()
  {
    return $this->committedStorageMb;
  }
  /**
   * @param int
   */
  public function setCpuCount($cpuCount)
  {
    $this->cpuCount = $cpuCount;
  }
  /**
   * @return int
   */
  public function getCpuCount()
  {
    return $this->cpuCount;
  }
  /**
   * @param int
   */
  public function setDiskCount($diskCount)
  {
    $this->diskCount = $diskCount;
  }
  /**
   * @return int
   */
  public function getDiskCount()
  {
    return $this->diskCount;
  }
  /**
   * @param string
   */
  public function setDisplayName($displayName)
  {
    $this->displayName = $displayName;
  }
  /**
   * @return string
   */
  public function getDisplayName()
  {
    return $this->displayName;
  }
  /**
   * @param string
   */
  public function setInstanceType($instanceType)
  {
    $this->instanceType = $instanceType;
  }
  /**
   * @return string
   */
  public function getInstanceType()
  {
    return $this->instanceType;
  }
  /**
   * @param int
   */
  public function setMemoryMb($memoryMb)
  {
    $this->memoryMb = $memoryMb;
  }
  /**
   * @return int
   */
  public function getMemoryMb()
  {
    return $this->memoryMb;
  }
  /**
   * @param string
   */
  public function setOsDescription($osDescription)
  {
    $this->osDescription = $osDescription;
  }
  /**
   * @return string
   */
  public function getOsDescription()
  {
    return $this->osDescription;
  }
  /**
   * @param string
   */
  public function setPowerState($powerState)
  {
    $this->powerState = $powerState;
  }
  /**
   * @return string
   */
  public function getPowerState()
  {
    return $this->powerState;
  }
  /**
   * @param AwsSecurityGroup[]
   */
  public function setSecurityGroups($securityGroups)
  {
    $this->securityGroups = $securityGroups;
  }
  /**
   * @return AwsSecurityGroup[]
   */
  public function getSecurityGroups()
  {
    return $this->securityGroups;
  }
  /**
   * @param string
   */
  public function setSourceDescription($sourceDescription)
  {
    $this->sourceDescription = $sourceDescription;
  }
  /**
   * @return string
   */
  public function getSourceDescription()
  {
    return $this->sourceDescription;
  }
  /**
   * @param string
   */
  public function setSourceId($sourceId)
  {
    $this->sourceId = $sourceId;
  }
  /**
   * @return string
   */
  public function getSourceId()
  {
    return $this->sourceId;
  }
  /**
   * @param string[]
   */
  public function setTags($tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return string[]
   */
  public function getTags()
  {
    return $this->tags;
  }
  /**
   * @param string
   */
  public function setVirtualizationType($virtualizationType)
  {
    $this->virtualizationType = $virtualizationType;
  }
  /**
   * @return string
   */
  public function getVirtualizationType()
  {
    return $this->virtualizationType;
  }
  /**
   * @param string
   */
  public function setVmId($vmId)
  {
    $this->vmId = $vmId;
  }
  /**
   * @return string
   */
  public function getVmId()
  {
    return $this->vmId;
  }
  /**
   * @param string
   */
  public function setVpcId($vpcId)
  {
    $this->vpcId = $vpcId;
  }
  /**
   * @return string
   */
  public function getVpcId()
  {
    return $this->vpcId;
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
class_alias(AwsVmDetails::class, 'Google_Service_VMMigrationService_AwsVmDetails');
