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

namespace Google\Service\Backupdr;

class ComputeInstanceBackupProperties extends \Google\Collection
{
  protected $collection_key = 'serviceAccount';
  /**
   * @var bool
   */
  public $canIpForward;
  /**
   * @var string
   */
  public $description;
  protected $diskType = AttachedDisk::class;
  protected $diskDataType = 'array';
  protected $guestAcceleratorType = AcceleratorConfig::class;
  protected $guestAcceleratorDataType = 'array';
  /**
   * @var string
   */
  public $keyRevocationActionType;
  /**
   * @var string[]
   */
  public $labels;
  /**
   * @var string
   */
  public $machineType;
  protected $metadataType = Metadata::class;
  protected $metadataDataType = '';
  /**
   * @var string
   */
  public $minCpuPlatform;
  protected $networkInterfaceType = NetworkInterface::class;
  protected $networkInterfaceDataType = 'array';
  protected $schedulingType = Scheduling::class;
  protected $schedulingDataType = '';
  protected $serviceAccountType = ServiceAccount::class;
  protected $serviceAccountDataType = 'array';
  /**
   * @var string
   */
  public $sourceInstance;
  protected $tagsType = Tags::class;
  protected $tagsDataType = '';

  /**
   * @param bool
   */
  public function setCanIpForward($canIpForward)
  {
    $this->canIpForward = $canIpForward;
  }
  /**
   * @return bool
   */
  public function getCanIpForward()
  {
    return $this->canIpForward;
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
   * @param AttachedDisk[]
   */
  public function setDisk($disk)
  {
    $this->disk = $disk;
  }
  /**
   * @return AttachedDisk[]
   */
  public function getDisk()
  {
    return $this->disk;
  }
  /**
   * @param AcceleratorConfig[]
   */
  public function setGuestAccelerator($guestAccelerator)
  {
    $this->guestAccelerator = $guestAccelerator;
  }
  /**
   * @return AcceleratorConfig[]
   */
  public function getGuestAccelerator()
  {
    return $this->guestAccelerator;
  }
  /**
   * @param string
   */
  public function setKeyRevocationActionType($keyRevocationActionType)
  {
    $this->keyRevocationActionType = $keyRevocationActionType;
  }
  /**
   * @return string
   */
  public function getKeyRevocationActionType()
  {
    return $this->keyRevocationActionType;
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
   * @param Metadata
   */
  public function setMetadata(Metadata $metadata)
  {
    $this->metadata = $metadata;
  }
  /**
   * @return Metadata
   */
  public function getMetadata()
  {
    return $this->metadata;
  }
  /**
   * @param string
   */
  public function setMinCpuPlatform($minCpuPlatform)
  {
    $this->minCpuPlatform = $minCpuPlatform;
  }
  /**
   * @return string
   */
  public function getMinCpuPlatform()
  {
    return $this->minCpuPlatform;
  }
  /**
   * @param NetworkInterface[]
   */
  public function setNetworkInterface($networkInterface)
  {
    $this->networkInterface = $networkInterface;
  }
  /**
   * @return NetworkInterface[]
   */
  public function getNetworkInterface()
  {
    return $this->networkInterface;
  }
  /**
   * @param Scheduling
   */
  public function setScheduling(Scheduling $scheduling)
  {
    $this->scheduling = $scheduling;
  }
  /**
   * @return Scheduling
   */
  public function getScheduling()
  {
    return $this->scheduling;
  }
  /**
   * @param ServiceAccount[]
   */
  public function setServiceAccount($serviceAccount)
  {
    $this->serviceAccount = $serviceAccount;
  }
  /**
   * @return ServiceAccount[]
   */
  public function getServiceAccount()
  {
    return $this->serviceAccount;
  }
  /**
   * @param string
   */
  public function setSourceInstance($sourceInstance)
  {
    $this->sourceInstance = $sourceInstance;
  }
  /**
   * @return string
   */
  public function getSourceInstance()
  {
    return $this->sourceInstance;
  }
  /**
   * @param Tags
   */
  public function setTags(Tags $tags)
  {
    $this->tags = $tags;
  }
  /**
   * @return Tags
   */
  public function getTags()
  {
    return $this->tags;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ComputeInstanceBackupProperties::class, 'Google_Service_Backupdr_ComputeInstanceBackupProperties');
